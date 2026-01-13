# ============================================================
# 0) Libraries ####
# ============================================================
library(tidyverse)
library(lubridate)
library(haven)        # read_dta
library(fastDummies)  # dummy_cols
library(lme4)         # glmer (mixed logit)
library(brms)         # Bayesian mixed quantiles (asymmetric_laplace)
#install.packages("rqPen")  # if needed
library(rqPen)        # quantile regression with lasso capabilities 
library(splines)
#install.packages("conquer")
library(conquer)
# ============================================================
# 1) Load and primary ID construction #####
# ============================================================
raw <- read_dta("/Users/awinn2/Box Sync/Inflation_Reduction_Act_Analysis/IRA Expenditure Data.dta") %>%
  mutate(
    across(where(is.labelled), haven::as_factor),
    across(where(is.character), ~ str_trim(.x))
  )

# sequential person IDs
patients_lut <- raw %>%
  distinct(deid_patientid) %>%
  arrange(deid_patientid) %>%
  mutate(new_id = row_number())

dat1 <- raw %>% left_join(patients_lut, by = "deid_patientid")

# application-within-person IDs
apps_lut <- dat1 %>%
  distinct(new_id, deid_applicationid) %>%
  arrange(new_id, deid_applicationid) %>%
  group_by(new_id) %>%
  mutate(app_id = row_number()) %>%
  ungroup()

dat2 <- dat1 %>% left_join(apps_lut, by = c("new_id","deid_applicationid"))

# ============================================================
#### 2) Dates and imputation  #####
# ============================================================
set.seed(123)
dat2 <- dat2 %>%
  mutate(
    start_date    = make_date(year = as.integer(start_year),
                              month = as.integer(start_month),
                              day   = as.integer(start_day)),
    exp_paid_date = make_date(year = as.integer(exp_paid_year),
                              month = as.integer(exp_paid_month),
                              day   = as.integer(exp_paid_day))
  ) %>%
  mutate(
    needs_impute    = (as.integer(exp_paid_year) == 1900),
    random_offset_d = sample(0:364, size = n(), replace = TRUE),
    imputed_date    = start_date + days(random_offset_d),
    exp_paid_date   = if_else(needs_impute, imputed_date, exp_paid_date),
    exp_paid_year   = year(exp_paid_date),
    exp_paid_month  = month(exp_paid_date),
    exp_paid_day    = day(exp_paid_date)
  ) %>%
  select(-needs_impute, -random_offset_d, -imputed_date)

# ============================================================
##### 3) Running total and numeric encodes  ####
# ============================================================
dat2 <- dat2 %>%
  arrange(new_id, app_id, exp_paid_date) %>%
  group_by(new_id, app_id) %>%
  mutate(total_spend = cumsum(coalesce(expenditure_approved_amount, 0))) %>%
  ungroup()

dat2 <- dat2 %>%
  mutate(
    sex_code    = as.integer(factor(gender)),
    employ_code = as.integer(factor(employment_status)),
    age_code    = as.integer(factor(age_group)),
    income_code = as.integer(factor(income_group)),
    eth_code    = as.integer(factor(ethnicity)),
    state_code  = as.integer(factor(state_name)),
    insur_code  = as.integer(factor(insurance_type))
  )

# ============================================================
# 4) HIV subset + automatic drug dummies (≥3% of rows) ####
# ============================================================
hiv <- dat2 %>%
  filter(fund_name == "HIV, AIDS and Prevention***") %>%
  mutate(
    drugname_norm    = stringr::str_squish(stringr::str_to_upper(drugname)),
    amount_remaining = pmax(0, 7500 - coalesce(expenditure_approved_amount, 0))
  )

hiv_all <- fastDummies::dummy_cols(
  hiv,
  select_columns = "drugname_norm",
  remove_selected_columns = FALSE,
  remove_first_dummy = FALSE,
  ignore_na = TRUE
)

rx_cols_all <- grep("^drugname_norm_", names(hiv_all), value = TRUE)
keep_rx <- rx_cols_all[colMeans(hiv_all[rx_cols_all] == 1, na.rm = TRUE) >= 0.03]

hiv <- hiv_all %>%
  select(-all_of(setdiff(rx_cols_all, keep_rx))) %>%
  rename_with(~ sub("^drugname_norm_", "rx_", .x), all_of(keep_rx)) %>%
  mutate(across(starts_with("rx_"), ~ as.integer(coalesce(.x, 0))))

# ============================================================
# 5) Balanced 12-month panel creation ####
# ============================================================
first_nonmiss <- function(x) { idx <- which(!is.na(x)); if (length(idx)) x[idx[1]] else NA }

build_month_panel <- function(df, months = 12, bucket_days = 30) {
  # create the "grid" or data skeleton
  keys <- df %>% distinct(new_id, deid_applicationid, app_id)
  grid <- tidyr::expand_grid(keys, month = 1:months)
  
  # time invariant factors (once per id×app)
  static_lut <- df %>%
    group_by(new_id, deid_applicationid, app_id) %>%
    summarise(
      sex_code    = first_nonmiss(sex_code),
      employ_code = first_nonmiss(employ_code),
      age_code    = first_nonmiss(age_code),
      income_code = first_nonmiss(income_code),
      eth_code    = first_nonmiss(eth_code),
      state_code  = first_nonmiss(state_code),
      insur_code  = first_nonmiss(insur_code),
      .groups = "drop"
    )
  
  # time varying factors 
  monthly <- df %>%
    mutate(month = pmin(months, pmax(1L, floor(as.numeric(exp_paid_date - start_date) / bucket_days) + 1L))) %>%
    group_by(new_id, deid_applicationid, app_id, month) %>%
    summarise(
      spend_month = sum(coalesce(expenditure_approved_amount, 0), na.rm = TRUE),
      across(starts_with("rx_"), ~ as.integer(any(coalesce(.x, 0L) == 1L))),
      .groups = "drop"
    )
  # fill the data skeleton - put some meat on those bones
  grid %>%
    left_join(monthly, by = c("new_id","deid_applicationid","app_id","month")) %>%
    mutate(
      spend_month = replace_na(spend_month, 0),
      across(starts_with("rx_"), ~ replace_na(.x, 0L))
    ) %>%
    left_join(static_lut, by = c("new_id","deid_applicationid","app_id")) %>%
    group_by(new_id, deid_applicationid, app_id) %>%
    arrange(month, .by_group = TRUE) %>%
    mutate(
      expenditure_approved_amount = cumsum(spend_month),
      total_spend = expenditure_approved_amount,
      across(starts_with("rx_"), ~ as.integer(cummax(.x) > 0)),
      new_spending = as.integer(expenditure_approved_amount >
                                  lag(expenditure_approved_amount, default = 0))
    ) %>%
    ungroup() %>%
    arrange(new_id, deid_applicationid, month)
}

month_panel <- build_month_panel(hiv, months = 12, bucket_days = 30)

# safety check: 12 rows per id×app
stopifnot(
  month_panel %>% count(new_id, deid_applicationid, app_id) %>% pull(n) %>% { all(. == 12) }
)

# ============================================================
# 6) Targets & features for modeling ####
# ============================================================
mp <- month_panel

# final spend per application (the outcome)
final_spend <- mp %>%
  group_by(new_id, deid_applicationid, app_id) %>%
  summarise(y_final = max(expenditure_approved_amount, na.rm = TRUE), .groups = "drop")

mp <- mp %>%
  left_join(final_spend, by = c("new_id","deid_applicationid","app_id")) %>%
  mutate(remaining_to_horizon = pmax(y_final - expenditure_approved_amount, 0)) %>%
  group_by(new_id, deid_applicationid, app_id) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(
    spend_prev  = lag(spend_month, default = 0),
    spend_vel   = spend_month - spend_prev,
    spend_to_dt = expenditure_approved_amount
  ) %>%
  ungroup()

# scale spend_to_dt once (so predict has the same column)
mu_sp  <- mean(mp$spend_to_dt, na.rm = TRUE)
sd_sp  <- sd(mp$spend_to_dt, na.rm = TRUE)


# factors for modeling
mp <- mp %>%
  mutate(
    sex_f    = factor(sex_code),
    employ_f = factor(employ_code),
    age_f    = factor(age_code),
    income_f = factor(income_code),
    eth_f    = factor(eth_code),
    state_f  = factor(state_code),
    insur_f  = factor(insur_code),
    month_f = factor(month),
    month2 = month*month,
    month3 = month*month*month,
    spend_to_dt_s = (spend_to_dt - mu_sp) / sd_sp
  )

#### now for the model mayhem #########
# would love to do a mixed effect quantile regression model, 
# and while this can be implemented via bayesian approach (tried and takes forever) 
# there aren't option for a frequentist approach. therfore, we 
# are going to try a 2 stage approach (similar to merf - mixed effects, random forest)
# however we will use a lasso for the second stage of to get the distribution.
# what we are going to do is... 
# stage 1a : Mixed effects model predicting the mean, 
# stage 1b : generate residuals 
# stage 2  : use quantile regression with a lasso to predict across the distribution with the outcome being that patients residual


# ============================================================
# 7) STAGE 1: Mixed-effects model to get partial-pooling offsets ----
# ============================================================

# make sure grouping factor exists
mp <- mp %>% mutate(new_id_f = factor(new_id),
                    month_num = as.numeric(month))

# mixed effect model (random intercept at the patient level)
# - time splines
# - spending - spending to date, spending last period and change in spending between quarters
# - core demographics (maybe take out state? idk)
# - random intercept per patient

mm1 <- lmer(
  y_final ~ month_f +
    spend_to_dt_s + spend_prev + spend_vel +
    sex_f + age_f + income_f + insur_f + state_f +
    (1 | new_id_f),
  data = mp, REML = TRUE
)

# prediction (no random effects/intercepts baked in)
eta_fix <- as.numeric(predict(mm1, newdata = mp, re.form = NA))

### need to add back in the REs (random intercepts) ###

# random effect at the patient level
# we have to reach into the stored model stuff from the re model (thankfull it is all easy to grab)
re_pat  <- ranef(mm1)$new_id_f
# make the RE data easy to combine with overall dataset 
re_map  <- tibble(new_id_f = rownames(re_pat), u_hat = re_pat[[1]])
# add this into the data
mp <- mp %>% left_join(re_map, by = "new_id_f") %>%
  mutate(u_hat = replace_na(u_hat, 0))

# Stage-1 mean for each row & residual to be modeled in Stage-2
mp <- mp %>%
  mutate(mu_hat     = eta_fix + u_hat,
         resid_final = y_final - mu_hat)

# Keep the SD of random intercept (needed for simulations)
sigma_u <- as.numeric(attr(VarCorr(mm1)$new_id_f, "stddev"))

# ============================================================
# 8) STAGE 2: Quantile LASSO on residuals  -----
# ============================================================

# ---- Build a design matrix for penalized Quantile Regression (no intercept here) ----
# Choose features for residual structure - drugs
rx_cols  <- grep("^rx_", names(mp), value = TRUE)

# Let's keep only RX dummies with ≥1% prevalence in the panel (kinda a suspenders and belt sort of thing):
keep_rx  <- rx_cols[colMeans(mp[rx_cols] == 1, na.rm = TRUE) >= 0.01]
feat_vec <- c("month_f", "spend_to_dt_s", "spend_prev", "spend_vel",
              "sex_f", "age_f", "income_f", "insur_f", "state_f",
              keep_rx)

mp_small <- mp[1:100000,]

# Model matrix 
X <- model.matrix(~ 0 + ., data = mp_small[, feat_vec, drop = FALSE])
y <- mp_small$resid_final

# Penalization setup:
# By default penalize everything. Optionally *don’t* penalize a few core drivers:
pfac <- rep(1, ncol(X)); names(pfac) <- colnames(X) 
pfac[ c("spend_to_dt_s","spend_prev","spend_vel","month_num") %in% colnames(X) ] <- 1

# Target quantiles (deciles but need to build out eventually)
taus <- seq(0.1, 0.9, by = 0.1)

# Cross-Validated Penalized Convolution-Type Smoothed Quantile Regression


cvc<- conquer::conquer.cv.reg(
  X, y,
  tau = .5 ,                 # pick your τ
  penalty = "lasso",
  kfolds = 3,                # speed knob
  numLambda = 10             # default is 50
)



# sanity checks
print(class(cvc))
print(names(cvc))        # should include "coeff.min", "coeff.1se", "lambda.min", ...



beta_min  <- cvc$coeff.min         # (p+1)-vector: intercept first
beta_1se  <- cvc$coeff.1se

# Refit the model at best lambda
fit_c <- conquer.reg(
  X, y,
  tau = 0.5,
  penalty = "lasso",
  lambda = cvc$lambda.min
)
# Examine results
summary(fit_c)
head(fit_c$coeff)
# Predict or extract coefficients
beta <- fit_c$coeff
yhat <- as.numeric(cbind(1, X) %*% beta)

test2 <- cbind(mp, yhat)
head(test2)
test2 <- test2 %>% mutate(prediction = yhat + mu_hat)
mean(test2$prediction )
median(test2$y_final)

library(ggplot2)


  
ggplot(test2 %>%
         filter(month ==1) %>%
         select(prediction, y_final) %>%
         pivot_longer(cols = everything(),
                      names_to = "variable",
                      values_to = "value"),
       aes(x = value, fill = variable)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("prediction" = "#1b9e77", "y_final" = "#d95f02")) +
  labs(x = "Spending", y = "Density",
       title = "Distribution of Predicted vs. Observed Spending") +
  theme_minimal(base_size = 13)
yhat
1 -424.74757
2 -354.24799
3 -283.74840
4 -213.24881
5 -142.74922
6  -72.24963

# ---- Predict residual quantiles and combine with Stage-1 mean ----
# rqPen returns a coefficient vector (intercept + betas) at lambda.min
pred_q_mat <- matrix(NA_real_, nrow = nrow(X), ncol = length(taus))
for (j in seq_along(taus)) {
  cf   <- coef(fits_q[[j]], lambda = fits_q[[j]]$lambda.min)  # intercept first
  beta <- as.numeric(cf[-1]); b0 <- as.numeric(cf[1])
  pred_q_mat[, j] <- b0 + as.numeric(X %*% beta)               # residual τ-quantile
}
colnames(pred_q_mat) <- paste0("qresid_", sprintf("%02d", taus*100))

# Add Stage-1 mean back to get TOTAL-SPEND quantiles
qfinal_mat <- sweep(pred_q_mat, 1, mp$mu_hat, `+`)
colnames(qfinal_mat) <- paste0("qfinal_", sprintf("%02d", taus*100))

# Bind to mp
mp <- bind_cols(mp, as.data.frame(qfinal_mat))

# Enforce non-crossing across τ (row-wise sort is a safe post-hoc fix)
qcols <- colnames(qfinal_mat)
mp[, qcols] <- t(apply(as.matrix(mp[, qcols, drop = FALSE]), 1, function(v) {
  if (all(is.na(v))) v else sort(v, na.last = TRUE)
}))

# Remaining-budget quantiles and monotone decrease over months within id/app
rem_cols <- sub("^qfinal_", "remain_q", qcols)
for (k in seq_along(qcols)) {
  mp[[rem_cols[k]]] <- pmax(mp[[qcols[k]]] - mp$expenditure_approved_amount, 0)
}
mp <- mp %>%
  group_by(new_id, deid_applicationid, app_id) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(across(all_of(rem_cols), ~ cummin(.x))) %>%
  ungroup()


# ============================================================
# 9) Quick sanity checks / outputs
# ============================================================
# how sparse is the selected model at τ=0.8?
cf80 <- coef(fits_q[[which(taus==0.8)]], lambda = fits_q[[which(taus==0.8)]]$lambda.min)
sum(cf80[-1] != 0)  # number of active features

# glance at a few rows
mp %>%
  select(new_id, deid_applicationid, app_id, month,
         expenditure_approved_amount, y_final,
         starts_with("qfinal_"), starts_with("remain_q")) %>%
  slice_head(n = 10)













# RX columns to include (all that survived ≥3%)
rx_cols <- grep("^rx_", names(mp), value = TRUE)
rx_keep <- rx_cols


### a frequentist quantile model 
 install.packages("lqmm")
library(lqmm)
library(dplyr)
library(purrr)

# Make sure the grouping id is a factor
mp <- mp %>%
  mutate(new_id_f = factor(new_id))   # patient-level random intercept

# Choose quantiles
taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# Formula:
# - fixed: simple, fast baseline (add more covariates once this runs clean)
# - random: random intercept by patient
fixed_f  <- y_final ~ month + month2 + month3 + new_spending + spend_to_dt_s
random_f <- ~ 1 # random intercept model


quantile1 <- lqmm(
  fixed  = fixed_f,
  random = random_f,
  group  = new_id_f,       # grouping factor
  tau    = .8,
  data   = mp,
  nK     = 7,              # Gauss–Hermite quadrature points (7–11 typical)
  type   = "normal",       # random effects ~ Normal
  control = lqmmControl(
    LP_max_iter = 100,     # linear programming iter
    verbose = TRUE
  )
)

#summary(quantile1)
coef(quantile1)         # fixed effects
quantile1$Psi           # RE variance (random intercept)
quantile1$Sigma         # residual scale

# 5) Predictions for every row (final-spend τ=0.8)
mp$qfinal_80 <- as.numeric(predict(quantile1, newdata = mp, type = "response"))



pred <- predict(
  object  = quantile1,  # your fitted lqmm model
  newdata = mp,         # data frame with same covariates as used in fitting
  type    = "response", # 
  level   =1       # conditional predictions
)
# Fit one model per tau
fits <- map(taus, ~ lqmm(
  fixed  = fixed_f,
  random = random_f,
  group  = new_id_f,       # grouping factor
  tau    = .x,
  data   = mp,
  nK     = 7,              # Gauss–Hermite quadrature points (7–11 typical)
  type   = "normal",       # random effects ~ Normal
  control = lqmmControl(
    LP_max_iter = 100,     # linear programming iter
    BFGS_max_iter = 100,   # optimizer iter
    verbose = FALSE
  )
))
names(fits) <- paste0("tau", sprintf("%02d", 100*taus))

# Predict quantiles of FINAL spend for each row
pred_q <- map(fits, ~ predict(.x, newdata = mp))
pred_q <- bind_cols(pred_q)
names(pred_q) <- paste0("qfinal_", sprintf("%02d", 100*taus))

# Attach to data
mp <- bind_cols(mp, pred_q)





##### now lets write the quantile model
library(brms)

family_asymmetric_laplace <- brmsfamily("asym_laplace", link = "identity")
# Outcome: y_final  (you already created this as the final/horizon total per app)
# Predictors: spend_to_dt (cumulative so far), factor(month)
# Random effects: (1 | new_id/app_id)

# 1) Formula
form_med <- bf(
  y_final ~ factor(month) + scale(spend_to_dt),
  #  + (1 | new_id/app_id)
  quantile = 0.5   # set τ here
)

# 2) Family: asymmetric Laplace with quantile = 0.5 (the median)
fam_med <- asym_laplace()

# 3) Mild regularizing priors (optional but helpful)
priors_med <- c(
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 10), class = "Intercept")
#  , prior(exponential(1), class = "sd")
)

# 4) Fit
fit_med <- brm(
  form_med, data = mp, family = asym_laplace(),
  prior = priors_med,
  chains = 2, cores = 6, iter = 2000, warmup = 1000,
  init = 0,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE),
  file = "fit_med_cached",
  refresh = 0
)
summary(fit_med)





















# ============================================================
# 7) Mixed-effects logistic hurdle (Part A)
# ============================================================
rhs_hurdle <- paste(
  "factor(month) +  scale(spend_to_dt) ",
  "sex_f ",
  sep = " + "
)
# add to 212: + factor(employ_f) + factor(age_f) + factor(income_f) + factor(eth_f)  + factor(insur_f)
# figure out how to get this in there between 212 and 213:   paste0("'", rx_keep, , collapse = " + "),

form_hurdle <- as.formula(
  paste0("new_spending ~ ", rhs_hurdle, " + (1 | new_id/app_id)")
)

fit_hurdle <- glmer(
  form_hurdle,
  data    = mp,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

mp$phat_change <- predict(fit_hurdle, type = "response")

# ============================================================
# 8) Multivariate brms (deciles) for remaining_to_horizon (Part B)
#     - Baseline (month == 1)
#     - Post-baseline (months >= 2, conditional on change==1 for training)
# ============================================================

# ---- helpers for multivariate setup ----
make_mv_data <- function(df, outcome, taus) {
  out <- df
  for (k in seq_along(taus)) {
    out[[paste0(outcome, "_", sprintf("%02d", as.integer(taus[k]*100)))]] <- df[[outcome]]
  }
  out
}

mvbind_formula_and_family <- function(outcome, rhs, taus) {
  resp_names <- paste0(outcome, "_", sprintf("%02d", as.integer(taus*100)))
  f <- as.formula(
    paste0("mvbind(", paste(resp_names, collapse = ", "), ") ~ ",
           rhs, " + (1 | new_id/app_id)")
  )
  fams <- lapply(taus, function(tau) asymmetric_laplace(quantile = tau))
  list(formula = f, families = fams, resp_names = resp_names)
}

qr_priors <- c(
  prior(normal(0, 1), class = "b"),
  prior(exponential(1), class = "sd"),
  prior(student_t(3, 0, 10), class = "Intercept")
)

fit_mv_qr <- function(df, outcome, rhs, taus, iter = 3000, seed = 2025) {
  spec <- mvbind_formula_and_family(outcome, rhs, taus)
  brm(
    formula   = spec$formula,
    data      = df,
    family    = spec$families,
    prior     = qr_priors,
    chains    = 4, cores = 4, iter = iter, seed = seed,
    set_rescor = FALSE,
    control   = list(adapt_delta = 0.95, max_treedepth = 12)
  )
}

predict_mv_qr <- function(fit, newdata, outcome, taus) {
  resp_names <- paste0(outcome, "_", sprintf("%02d", as.integer(taus*100)))
  mats <- lapply(resp_names, function(r)
    posterior_epred(fit, newdata = newdata, resp = r) # draws x N
  )
  preds <- lapply(mats, function(m) apply(m, 2, median)) # posterior medians
  tibble(!!!setNames(preds, paste0("remain_q", sprintf("%02d", as.integer(taus*100)))))
}

# ---- choose deciles ----
tau_vec <- seq(0.1, 0.9, by = 0.1)
dec_names <- paste0("remain_q", sprintf("%02d", as.integer(tau_vec*100)))

# ---- BASELINE (month == 1) ----
mp_base <- mp %>% filter(month == 1) %>% mutate(row_id = row_number())
rhs_base <- paste(
  "scale(spend_to_dt)",
  "sex_f ",

  sep = " + "
)
# add this to 292: + employ_f + age_f + income_f + eth_f + state_f + insur_f
# ad to 293   paste(rx_keep, collapse = " + "),
mp_base_mv <- make_mv_data(mp_base, outcome = "remaining_to_horizon", taus = tau_vec)

fit_base_mv <- fit_mv_qr(
  df = mp_base_mv,
  outcome = "remaining_to_horizon",
  rhs = rhs_base,
  taus = tau_vec
)


fit_mv_qr <- function(df, outcome, rhs, taus, iter = 3000, seed = 2025) {
  spec <- mvbind_formula_and_family(outcome, rhs, taus)
  brm(
    formula   = spec$formula,
    data      = df,
    family    = spec$families,
    prior     = qr_priors,
    chains    = 4, cores = 4, iter = iter, seed = seed,
    set_rescor = FALSE,
    control   = list(adapt_delta = 0.95, max_treedepth = 12)
  )
}



pred_base <- predict_mv_qr(
  fit = fit_base_mv,
  newdata = mp_base_mv,
  outcome = "remaining_to_horizon",
  taus = tau_vec
) %>% mutate(row_id = mp_base$row_id)

# ---- POST-BASELINE (months >= 2, train on change==1) ----
mp_post <- mp %>% filter(month >= 2, new_spending == 1L) %>% mutate(row_id = row_number())
rhs_post <- paste(
  "factor(month) + scale(spend_to_dt) + scale(spend_prev) + scale(spend_vel)",
  "sex_f + employ_f + age_f + income_f + eth_f + state_f + insur_f",
  paste(rx_keep, collapse = " + "),
  sep = " + "
)
mp_post_mv <- make_mv_data(mp_post, outcome = "remaining_to_horizon", taus = tau_vec)

fit_post_mv <- fit_mv_qr(
  df = mp_post_mv,
  outcome = "remaining_to_horizon",
  rhs = rhs_post,
  taus = tau_vec
)

pred_post <- predict_mv_qr(
  fit = fit_post_mv,
  newdata = mp_post_mv,
  outcome = "remaining_to_horizon",
  taus = tau_vec
) %>% mutate(row_id = mp_post$row_id)

# ============================================================
# 9) Merge predictions → all rows, enforce non-crossing, compute E[·] & final
# ============================================================
# initialize empty decile columns
for (nm in dec_names) mp[[nm]] <- NA_real_

# attach baseline deciles
mp <- mp %>%
  mutate(row_id_all = row_number()) %>%
  left_join(
    pred_base %>% select(row_id, all_of(dec_names)) %>%
      rename(row_id_all = row_id),
    by = "row_id_all"
  )

# attach post-baseline deciles (map rows with month>=2)
idx_post_global <- which(mp$month >= 2)
mp_post_all <- mp %>% filter(month >= 2) %>% mutate(row_id_post = row_number())
mp <- mp %>%
  left_join(
    pred_post %>% select(row_id, all_of(dec_names)) %>%
      rename(row_id_post = row_id),
    by = "row_id_post",
    suffix = c("", "_post")
  )

# prefer populated values: keep baseline where present, otherwise post-baseline
for (nm in dec_names) {
  post_nm <- paste0(nm, "_post")
  mp[[nm]] <- ifelse(is.na(mp[[nm]]), mp[[post_nm]], mp[[nm]])
  mp[[post_nm]] <- NULL
}

# non-crossing across deciles: row-wise sort
mat <- as.matrix(mp[, dec_names])
row_sort <- function(v) { if (all(is.na(v))) v else sort(v, na.last = TRUE) }
mat_nc <- t(apply(mat, 1, row_sort))
colnames(mat_nc) <- dec_names
mp[, dec_names] <- mat_nc

# expected remaining and predicted final at each decile
for (nm in dec_names) {
  mp[[paste0("E_remaining_", nm)]] <- mp$phat_change * mp[[nm]]
  mp[[paste0("pred_final_", nm)]]  <- mp$expenditure_approved_amount + mp[[paste0("E_remaining_", nm)]]
}

# final tidy output for analysis / Shiny
pred_out <- mp %>%
  select(new_id, deid_applicationid, app_id, month,
         expenditure_approved_amount, spend_month, phat_change,
         all_of(dec_names),
         all_of(paste0("E_remaining_", dec_names)),
         all_of(paste0("pred_final_", dec_names)))

# Example quick peek:
# glimpse(pred_out)



# ----- (Optional) Save result -----
# readr::write_rds(month_panel, "/path/to/month_panel.rds")
# arrow::write_parquet(month_panel, "/path/to/month_panel.parquet")
