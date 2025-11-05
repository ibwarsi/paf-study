### aaron's edits 
#### Dated: 10-22-2025
######################################################################################################################################
#### QUANTILE REGRESSION & MATRIX CREATION (hiv_data) ####
######################################################################################################################################
# starting with the HIV data 

#---------------------------
# Loading Libraries
#---------------------------
#installing essential libraries
install.packages("fastDummies")
install.packages("SparseM")
library(readxl)
library(SparseM)
library(quantreg) #for rq function
library(fastDummies)
library(ggplot2) #graphical plots
library(dplyr) #mutate function
library(tidyr) #making tidy tables
library(writexl) #writing & saving excel fikes
#Objective:
#This script focuses on creating matrices like Aaron suggested
#Using those to run regression modeling staring with base model: with just age & increementally adding predictors (models 1-4)
#Repeat the same experiment for other 4 datasets
#Predict spend & build visualization plots

#---------------------------
# Saving Essential Datasets
#---------------------------
#Saving essential datasets created in this script for faster recovery
# Save your dataframe as Excel file
#getwd()
#setwd("C:/Users/awars/OneDrive - University of Illinois Chicago/PSOP Sem 1/Aaron Winn Project/IRA Expenses Data/Shared Folder with Aaron/Poster R Analysis Datasets (Nov 2025)")
#write_xlsx(hiv_data2, "C:/Users/awars/OneDrive - University of Illinois Chicago/PSOP Sem 1/Aaron Winn Project/IRA Expenses Data/Shared Folder with Aaron/Poster R Analysis Datasets (Nov 2025)/hiv_data2.xlsx")
#write_xlsx(hiv_data, "C:/Users/awars/OneDrive - University of Illinois Chicago/PSOP Sem 1/Aaron Winn Project/IRA Expenses Data/Shared Folder with Aaron/Poster R Analysis Datasets (Nov 2025)/hiv_data.xlsx")


#-----------------------------------------------------------------------------------------------------
#1. Creating Matrix
#-----------------------------------------------------------------------------------------------------
#hiv_data_coef <- read_excel("data/hiv_data.xlsx")
hiv_data_coef <- hiv_data
names(hiv_data_coef)

# let's drop the coefficients from this dataset 
hiv_data2 <- hiv_data_coef[,1:38] 
#This line keeps only the first 38 columns (the “raw” variables) and drops any appended coefficient columns. It assumes those start after column 38.

# create dummies from the categorical data (will make the predictions easier)
dummy_mat <- model.matrix(~ Age_cat3 - 1, data = hiv_data2)
#model.matrix(~ Age_cat3 - 1) builds a one-hot (no intercept) indicator matrix for the categorical variable Age_cat3.
#If Age_cat3 has three levels, you’ll get three 0/1 columns—one per level.
#The “- 1” means “don’t include an intercept in the dummy matrix.” (Aaron adds an intercept later himself).


# make column names syntactically valid (spaces -> ., '>' -> '.')
# this makes life easier when you get the coefficient names 
colnames(dummy_mat) <- make.names(colnames(dummy_mat))
#make.names() converts column names into valid R names: spaces become . and symbols like > or < become . as well.
#This avoids headaches when matching names later (e.g., "Age 56-75" → "Age.56.75").

# put it together: Appends the dummy columns to your dataset. Now you have both the original variables and the dummy variables side by side.
hiv_data2 <- cbind(hiv_data2, as.data.frame(dummy_mat))


# add in an intercept term for future matrix multiplication 
hiv_data2$'(Intercept)' <- 1  #adds an explicit column named "(Intercept)" with value 1 for every row.
#crucial for the matrix multiplication step he does later: 
#take a patient matrix (predictors) and multiply by the coefficient matrix (which will include a row named "(Intercept)"). 
#Name alignment makes it plug-and-play.

names(hiv_data2)
hiv_data2$Age_cat3 <- NULL  #Drops the original categorical factor now that its information is encoded via dummy columns.

####New variables (hiv_data2):#### "Age_cat3Age..55""Age_cat3Age.56.75" "Age_cat3Age..75" "(Intercept)"

#-----------------------------------------------------------------------------------------------------
#2. Running Model 1 Quantile Regression for hiv_data with just age in the model [Aaron's Code]
#-----------------------------------------------------------------------------------------------------
#Approach:
#1. Run a quantile regression (Model 1) with only age group predictors (from your dummy-coded Age_cat3 variable).
#2. Extract the coefficients for all quantiles (τ = 0.01 → 0.99).
#3. Align those coefficients with the corresponding patient variables.
#4. Later (in the next block), multiply each patient’s predictor vector by the quantile-specific coefficients to get predicted total spending across quantiles.


# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

#counts how many patients fall into the “Age < 55” group vs. not.
table(hiv_data2$Age_cat3Age..55)
#    0     1 
#29309 29362 
#dummy variable Age_cat3Age..55 takes value 1 if the person is in that group, 0 otherwise.

############### Model 1 ###############
# run model for each quantile 
#######################################
#Using rq function (quantreg package)
library(quantreg)
#Qτ​(Spending)=β0​(τ)+β1​(τ)⋅I(Age56–75)+β2​(τ)⋅I(Age<55)
quantile_hiv_mod1 <- rq(total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55 , data = hiv_data2, tau = quantiles)
#two dummy variables representing age groups (< 55) and 56–75, with the third group (> 75) serving as the reference category.
#The result (quantile_hiv_mod1) contains 99 sets of β coefficients — one for each quantile τ.

#Extract coefficient matrix: Coefficients for a multiple quantile model
coef_multiple_mod1 <- coef(quantile_hiv_mod1)
# now we have our coefficents as rows and the columns as equations
print(coef_multiple_mod1)

#Extract variable names from coefficients
colset_union_mod1 <- rownames(coef_multiple_mod1)
#> colset_union
#[1] "(Intercept)"       "Age_cat3Age >75"  
#[3] "Age_cat3Age 56-75"

# now we need to make the data frame and coefficients 
# have dimensions correct (same number of rows in the coef as 
# there are columns in the patient data)


#######################################
# Align coefficient names and dataset columns
#######################################
# keep only those that exist in df and preserve df's column order
colset_mod1 <- intersect( colset_union_mod1, names(hiv_data2))

#Count unique patients and make an ID list
n_patients <- length(unique(hiv_data2$patient_id))
patient_ids <- sort(unique(hiv_data2$patient_id))

# Create a patient-by-variable matrix
patient_matrix <- matrix(0, nrow = n_patients, 
                         ncol =  length(colset_mod1) , 
                         dimnames = list(
                           id = as.character(patient_ids),
                           variable = colset_mod1
                         ))

# now we need to fill the matrix 
# sanity check: columns must exist
stopifnot(all(colset_mod1 %in% names(hiv_data2)))

# helper to coerce variables into numeric format
coerce_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x))  return(as.numeric(as.character(x)))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  return(as.numeric(x))
}

#Using Mutate function from dplyr package
library(dplyr)
# build a per-patient row with exactly the columns in `colset_mod1`
df_pat1 <- hiv_data2 %>%
  select(patient_id, all_of(colset_mod1)) %>%
  group_by(patient_id) %>%
  slice(1) %>%                        # if multiple rows per patient, take the first (adjust if needed)
  ungroup() %>%
  mutate(across(all_of(colset_mod1), coerce_num))

# align to the order in `patient_ids`
df_pat1 <- df_pat1[match(patient_ids, df_pat1$patient_id), ]

# safety check: all patient_ids found
if (any(is.na(df_pat1$patient_id))) {
  missing_ids <- patient_ids[is.na(df_pat1$patient_id)]
  stop(sprintf("Missing %d patient_id(s) in hiv_data2, e.g., %s",
               length(missing_ids), paste(head(missing_ids, 5), collapse = ", ")))
}

# fill in the created patient_matrix with correct dims:
patient_matrix[,] <- as.matrix(df_pat1[, colset_mod1, drop = FALSE])
rownames(patient_matrix) <- as.character(df_pat1$patient_id)
colnames(patient_matrix) <- colset_mod1

# Verify dimensions
dim(patient_matrix)
dim(coef_multiple_mod1)
#> dim(patient_matrix)
#[1] 48393     3
# dim(coef_multiple)
#[1]  3 99

#-----------------------------------------------------------------------------------------------------
# 3. Matrix Multiplication - Generating Percentile Predictions
#-----------------------------------------------------------------------------------------------------
# Aaron: "the magic of matrix multiplication!"
## Multiply patient predictors by quantile regression coefficients
percentiles <- patient_matrix[]%*%coef_multiple_mod1[,]
View(percentiles)

# Rename columns to indicate quantile number
colnames(percentiles) <- sprintf("percentile%02d", seq_len(ncol(percentiles)))

# # Convert to data.frame by patient_id
pred_df1 <- data.frame(
  patient_id = rownames(patient_matrix),
  percentiles,
  check.names = FALSE
)

# --- WIDE merge: add predictions as new columns to hiv_data2 ---
# Merge predictions back into mm_data2
hiv_data_with_predictions <- dplyr::left_join(hiv_data2, pred_df1, by = "patient_id")



#-----------------------------------------------------------------------------------------------------
# 4. Distribution Plot with $2K Cap Threshold and τ* Annotation (HIV - Model 1) - Ibrahim's edits
#-----------------------------------------------------------------------------------------------------
#Goal: Visualize the mean predicted total spending across quantiles τ = 0.01–0.99
#      to understand how spending grows across the patient cost distribution.
library(ggplot2)
library(dplyr)
library(tidyr)

# Step 1️⃣: Summarize mean predicted spending per quantile
mean_predicted_spend_hiv <- hiv_data_with_predictions %>%
  dplyr::select(starts_with("percentile")) %>%        # grab all predicted percentile columns #forcing dplyr package (previous error)
  dplyr::summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "quantile",
               values_to = "mean_predicted_spend") %>%
  mutate(tau = as.numeric(gsub("percentile", "", quantile)) / 100) #if error with mutate function - force-specify dplyr package

# Step 2️⃣: Automatically find the quantile τ* where mean predicted spend first exceeds $2,000
threshold_tau_hiv <- mean_predicted_spend_hiv %>%
  filter(mean_predicted_spend >= 2000) %>%
  slice_head(n = 1) %>%
  pull(tau)

cat("Quantile τ* where predicted mean spend exceeds $2,000 (HIV):", threshold_tau_hiv, "\n")

# Step 3️⃣: Plot predicted spending and highlight the $2K cap and τ* cutoff
ggplot(mean_predicted_spend_hiv, aes(x = tau, y = mean_predicted_spend)) +
  geom_line(linewidth = 1.2, color = "#0072B2") +                         # previous it was size --- changed to ---> linewidth (no error)
  geom_point(size = 1.5, color = "#0072B2") +
  geom_hline(yintercept = 2000, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = threshold_tau_hiv, color = "darkred", linetype = "dotted", linewidth = 1) +
  annotate("text", x = threshold_tau_hiv + 0.05, y = 2200,
           label = sprintf("τ* ≈ %.2f", threshold_tau_hiv),
           color = "darkred", size = 4.2, fontface = "bold") +
  annotate("text", x = 0.9, y = 2100, label = "$2K Cap Threshold", color = "red", size = 4) +
  labs(
    title = "Predicted Total Spending Across Quantiles (HIV - Model 1)",
    subtitle = "Base model with Age as the only predictor\nRed line = $2K cap, Dotted line = τ* where mean spend exceeds $2K",
    x = "Quantile (τ)",
    y = "Predicted Total Spending (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

#save in wd
ggsave("hiv_predict_spend_2kthresholdplot.png")


###############################################################################################################################
# Ibrahim starting over to recheck analysis (dated 11-4-25)
# Obj: R shiny plot based on quant reg & per-patient actuarial analysis
#------------------------------------------------------------------------------------------------------------------------------
# 5. # Aggregate Patient Predicts (creating Predict Column) - Based on percentile then IRA 2K/2.1K cap -  Ibrahim's edits
#------------------------------------------------------------------------------------------------------------------------------
#Obj: collapse 99 quantile predictions into one average or representative estimate of 
#..predicted total spending per patient — that’s what creates the pred_spend variable (and later the capped versions).

library(dplyr)
names(hiv_data)
names(hiv_data2)
# Step 1️⃣ — Compute mean predicted spend per patient from pred_df1
pred_df1_summary <- pred_df1 %>%
  mutate(pred_spend_quantreg = rowMeans(across(starts_with("percentile")), na.rm = TRUE)) %>%
  select(patient_id, pred_spend_quantreg)

# Step 2️⃣ — Merge (join) predictions into hiv_data using patient_id
hiv_data2 <- hiv_data2 %>%
  left_join(pred_df1_summary, by = "patient_id")

# Step 3️⃣ — Create capped predicted spending and high-spender flag
hiv_data2 <- hiv_data2 %>%
  mutate(
    pred_spendira_2025new = ifelse(pred_spend_quantreg > 2000, 2000, pred_spend_quantreg),
    pred_spendira_2026new = ifelse(pred_spend_quantreg > 2100, 2100, pred_spend_quantreg),
    spend_over_2k = ifelse(total_spending > 2000, 1, 0)
  )

# Step 4️⃣ — Sanity check
hiv_data2 %>%
  summarise(
    total_patients = n(),
    patients_with_predictions = sum(!is.na(pred_spend_quantreg)),
    missing_predictions = sum(is.na(pred_spend_quantreg)),
    prop_with_predictions = mean(!is.na(pred_spend_quantreg)) * 100
  )
#  total_patients       patients_with_predictions   missing_predictions     prop_with_predictions
#          58671                     58671                   0                   100
#Every patient now has a corresponding predicted spend value. Merge worked exactly as intended this time.
#total_patients: 58,671
#patients_with_predictions: 58,671
#missing_predictions: 0
#prop_with_predictions: 100%


#Checking descriptive:
hiv_data2 %>%
  summarise(
    mean_2025 = mean(pred_spendira_2025new, na.rm = TRUE),
    median_2025 = median(pred_spendira_2025new, na.rm = TRUE),
    sd_2025 = sd(pred_spendira_2025new, na.rm = TRUE),
    min_2025 = min(pred_spendira_2025new, na.rm = TRUE),
    max_2025 = max(pred_spendira_2025new, na.rm = TRUE),
    q25_2025 = quantile(pred_spendira_2025new, 0.25, na.rm = TRUE),
    q75_2025 = quantile(pred_spendira_2025new, 0.75, na.rm = TRUE),
    
    mean_2026 = mean(pred_spendira_2026new, na.rm = TRUE),
    median_2026 = median(pred_spendira_2026new, na.rm = TRUE),
    sd_2026 = sd(pred_spendira_2026new, na.rm = TRUE),
    min_2026 = min(pred_spendira_2026new, na.rm = TRUE),
    max_2026 = max(pred_spendira_2026new, na.rm = TRUE),
    q25_2026 = quantile(pred_spendira_2026new, 0.25, na.rm = TRUE),
    q75_2026 = quantile(pred_spendira_2026new, 0.75, na.rm = TRUE)
  )
#  mean_2025 median_2025 sd_2025 min_2025 max_2025 q25_2025 q75_2025 mean_2026 median_2026 sd_2026 min_2026 max_2026 q25_2026 q75_2026
#      2000        2000       0     2000     2000     2000     2000      2100        2100       0     2100     2100     2100     2100
 
#We noticed that each value was greater than or equal to 2k.
#Is qreg the right method for estimating the IRA Capped values?
#Perhaps lets try the policy-capped total spend per patient approach

#---------------------------------------------------------------------------------------------------------------------
# 6A. Re-estimating Policy-capped total spend per patient --> Applied on actual total_spending, not on predicted values
# ACTUAL vs. PREDICT Spend columns
#---------------------------------------------------------------------------------------------------------------------
#Approach: 
# - To estimate patient-level spending under the IRA cap ($2,000 in 2025 and $2,100 in 2026). 
# - Work with actual spending (total_spending) rather than model-predicted values.
# - Ensure patients who already spend below the cap retain their actual spending values.
# - Cap spending for patients above the threshold, simulating the policy-enforced upper limit.
# - Provide a realistic projection of post-policy savings and potential coverage expansion for the PAF.
# - Compare and see which approach (sections 5 vs. 6) could be used for R Shiny

hiv_data2 <- hiv_data2 %>%
  mutate(
    # Actual capped spend under IRA for 2025 and 2026
    #If actual spend ≤ cap → keep actual value
    #If actual spend > cap → capped at 2k (or 2.1k)
    actual_spend_ira_2025 = ifelse(total_spending > 2000, 2000, total_spending),
    actual_spend_ira_2026 = ifelse(total_spending > 2100, 2100, total_spending),
    
    # Predicted (quantile regression) spending is left untouched
    # Predicted spending (pred_spend_quantreg) is only used for modeling, not to overwrite observed behavior
    pred_spend_quantreg = pred_spend_quantreg,
    
    # Combine logic — predicted capped spend should also respect real-world variation
    pred_spendira_2025new = ifelse(pred_spend_quantreg < total_spending,
                                   ifelse(total_spending > 2000, 2000, total_spending),
                                   pred_spend_quantreg),
    
    pred_spendira_2026new = ifelse(pred_spend_quantreg < total_spending,
                                   ifelse(total_spending > 2100, 2100, total_spending),
                                   pred_spend_quantreg)
  )
# Check Distribution
hiv_data2 %>%
  summarise(
    mean_2025 = mean(actual_spend_ira_2025, na.rm = TRUE),
    median_2025 = median(actual_spend_ira_2025, na.rm = TRUE),
    min_2025 = min(actual_spend_ira_2025, na.rm = TRUE),
    max_2025 = max(actual_spend_ira_2025, na.rm = TRUE)
  )
#  mean_2025 median_2025 min_2025 max_2025
#  1457.389        2000     0.05     2000
# These values are more representative of patient-level caps.
# *Mean* reflects the average patient-level cost after applying the $2,000 limit.
# *Median* hitting the cap (2000) shows that half the patients spend at or below the threshold,
# *Min and Max* confirm no values exceed the policy limit.

library(ggplot2)
ggplot(hiv_data2, aes(x = total_spending, y = actual_spend_ira_2025)) +
  geom_point(alpha = 0.3, color = "#1B9E77") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 2000, linetype = "dotted", color = "#E64B35", size = 1) +
  labs(
    title = "Distribution of Baseline vs. Capped Spending (HIV Patients, 2025)",
    subtitle = "Points above the red line indicate spending capped under the IRA $2,000 limit",
    x = "Baseline Total Spending (USD)",
    y = "Spending After $2,000 Cap (USD)"
  ) +
  theme_minimal(base_size = 13)

#---------------------------------------------------------------------------------------------------------------------
# 6b. Plotting Section 6A
# ACTUAL vs. PREDICT Spend columns
#---------------------------------------------------------------------------------------------------------------------
# GOAL:
#To visualize predicted total spending across quantiles (τ = 0.01–0.99) before and after the IRA cap.
#This helps policymakers understand distributional impacts — i.e., which segments (low vs high spenders) benefit most.
#VARIABLES:
#pred_spend_quantreg: Predicted spending across τ from quantile regression.
#pred_spendira_2025new: Predicted spending capped at $2,000.
#actual_spend_ira_2025: Actual patient-level spending capped at $2,100 (estimated at 6A).

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# Step 1️⃣ — Create quantile data
quantile_levels <- seq(0.01, 0.99, by = 0.01)

quantile_spend <- tibble(
  tau = quantile_levels,
  pred_spend_2025 = quantile(hiv_data2$pred_spendira_2025new, probs = quantile_levels, na.rm = TRUE),
  actual_spend_2025 = quantile(hiv_data2$actual_spend_ira_2025, probs = quantile_levels, na.rm = TRUE)
)

# Step 2️⃣ — Reshape to long format for ggplot
quantile_long <- quantile_spend %>%
  pivot_longer(
    cols = c(pred_spend_2025, actual_spend_2025),
    names_to = "spend_type",
    values_to = "spending"
  ) %>%
  mutate(
    spend_type = recode(
      spend_type,
      "pred_spend_2025" = "Predicted Spending (Model-Based)",
      "actual_spend_2025" = "Observed Spending (Actual Data)"
    )
  )

# Step 3️⃣ — Plot: Quantile (τ) vs Spending
ggplot(quantile_long, aes(x = tau, y = spending, color = spend_type)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 2000, linetype = "dashed", color = "#E64B35", size = 1) +
  annotate("text", x = 0.1, y = 2000, label = "IRA Cap ($2,000)", color = "#E64B35", vjust = -1) +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Predicted vs Actual Spending Distribution Across Quantiles (τ)",
    subtitle = "Comparison of model-based and observed patient spending under 2025 IRA $2,000 cap",
    x = "Quantile (τ)",
    y = "Spending (USD)",
    color = "Spending Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )
#KEY INSIGHTS:
#Below median (τ < 0.5):
#Spending grows steadily; most patients’ costs are modest and below $2,000.

#Around median to 0.6:
#The observed (orange) distribution hits the cap — everyone from this point onward is protected from higher costs.

#Upper tail (τ > 0.6):
#The model (blue) predicts higher uncapped spending (≈ $2,500–$3,000+), showing where the IRA makes the largest impact.
#These are high-cost beneficiaries (likely with chronic or rare diseases) who benefit most from the cap.

#Summary Interpretation:
# - "This plot shows how the Inflation Reduction Act’s $2,000 cap changes the distribution of patient out-of-pocket spending.
# - The orange curve represents real-world capped spending, while the blue curve reflects our model’s predicted spending distribution without truncation. 
# - The flat portion of the orange line indicates that the policy successfully protects high-spending patients, while the gap between the curves represents total savings.
# - This visualization helps quantify who benefits most and by how much, beyond simple averages."



#-----------------------------------------------------------------------------------------------------
# 7A. MONTE CARLO SIM: microsim testing feasibility / sensitivity analysis
#-----------------------------------------------------------------------------------------------------
#Questions it can answer (better than point estimates)
#1. What’s the distribution of total post-cap spend per disease/state?
#2. How many new patients can we support with $X, with 90% confidence?
#3. What’s the probability that savings exceed $Y?
#4. How sensitive are results to disease mix, price inflation, or eligibility rules?

# Inputs
caps      <- c(`2025`=2000, `2026`=2100)
B         <- 5000  # iterations
by_vars   <- c("Age_Group")  # extend to add State, etc.

simulate_once <- function(df, cap, method=c("empirical","parametric")){
  method <- match.arg(method)
  # 1) sample spend
  if(method=="empirical"){
    # bootstrap patients within stratum
    draw <- df$total_spending[sample.int(nrow(df), nrow(df), replace=TRUE)]
  } else {
    # param: sample from your quantile curves or re-fit on a bootstrap
    # (left as hook to plug your quantile-regression coefficients)
    draw <- df$pred_spend_quantreg  # placeholder
  }
  # 2) apply policy
  post <- pmin(draw, cap)
  # 3) aggregate
  tibble(
    baseline = sum(draw, na.rm=TRUE),
    post     = sum(post, na.rm=TRUE),
    savings  = baseline - post,
    mean_post = mean(post, na.rm=TRUE)
  )
}

library(dplyr)
run_mc <- function(df, cap, B=2000, method="empirical"){
  bind_rows(replicate(B, simulate_once(df, cap, method), simplify=FALSE)) |>
    summarise(
      baseline_med = median(baseline), baseline_l = quantile(baseline, .025), baseline_u = quantile(baseline, .975),
      post_med     = median(post),     post_l     = quantile(post, .025),     post_u     = quantile(post, .975),
      savings_med  = median(savings),  savings_l  = quantile(savings, .025),  savings_u  = quantile(savings, .975),
      new_cap_med  = median(savings/cap),
      new_cap_l    = quantile(savings/cap, .025),
      new_cap_u    = quantile(savings/cap, .975),
      new_mean_med = median(savings/mean_post),
      new_mean_l   = quantile(savings/mean_post, .025),
      new_mean_u   = quantile(savings/mean_post, .975)
    )
}

results_mc <- hiv_data2 |> #placeholder for specifying dataset
  group_by(across(all_of(by_vars))) |>    #placeholder example: group_by(Disease_Top5 = "HIV, AIDS and Prevention") |>
  group_modify(~ bind_rows(
    run_mc(.x, cap=caps["2025"], B=2000, method="empirical") |> mutate(cap_year="2025"),
    run_mc(.x, cap=caps["2026"], B=2000, method="empirical") |> mutate(cap_year="2026")
  )) |>
  ungroup()

# inspect results
View(results_mc)

#Interpret
###| Age Group    | Key takeaways (based on `cap_year` = 2025/2026)                                                                                                                                                             |
#  | ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
#  | **Birth–18** | Lowest baseline and post-IRA spending (≈ $130 K → $80 K). Savings ≈ $50 K; ~25–30 new patients fundable. Coverage-gain ratios (0.62–0.69) show high uncertainty due to small sample size.                   |
#  | **19–35**    | Moderate baseline spend (≈ $26 M) dropping to ≈ $14 M post-IRA. Savings ≈ $12 M, enabling roughly 6 000 additional patients (2025) or 5 600 (2026). Coverage-gain ratio ≈ 0.8 → ~20 % improvement in reach. |
#  | **36–55**    | Baseline ≈ $46 M → $23 M post-IRA; savings ≈ $22 M. Cap-based reach ≈ 11 000 patients (2025) and 10 000 (2026). Coverage-gain ratios ≈ 0.93 and 0.86 → largest proportional benefit among adults.           |
#  | **56–75**    | Highest spending and savings: baseline ≈ $85 M → $43 M; savings ≈ $40 M. Adds ≈ 19–21 k new patients (huge reach). Coverage-gain ≈ 0.97 (2025) → nearly doubling reach; consistent CI → stable estimate.    |
#  | **Over 75**  | Lowest absolute savings among older adults (~$3.8 M–$3.9 M). Cap-based new patients ≈ 1.7–1.9 k. High coverage ratio (~0.85–0.92) shows moderate benefit but smaller population base.                       |
  

#-----------------------------------------------------------------------------------------------------
# 7B. MONTE CARLO SIM: VISUALIZATION coverage gain (y) by age group (x-axis) - TEST#1
#-----------------------------------------------------------------------------------------------------
# Total savings by age group
ggplot(results_mc, aes(x = Age_Group, y = savings_med/1e6,
                       fill = factor(cap_year))) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = savings_l/1e6, ymax = savings_u/1e6),
                position = position_dodge(width = 0.9), width = 0.3) +
  labs(y = "Total Savings (Million USD)",
       title = "Estimated IRA Policy Savings by Age Group (Monte Carlo Median ±95 % CI)",
       fill = "Cap Year") +
  theme_minimal(base_size = 13)

# Coverage gain
ggplot(results_mc, aes(x = Age_Group, y = new_mean_med,
                       group = cap_year, color = factor(cap_year))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = new_mean_l, ymax = new_mean_u), width = 0.2) +
  labs(y = "Coverage Gain (Mean-based)",
       title = "Coverage Expansion by Age Group Under IRA Caps",
       color = "Cap Year") +
  theme_minimal(base_size = 13)

#-----------------------------------------------------------------------------------------------------
# 7B. MONTE CARLO SIM2: VISUALIZATION coverage gain (y) by QUANTILES OF spending distribution (x-axis) - TEST#2
#-----------------------------------------------------------------------------------------------------
# Obj:

#Step 1️⃣ — Define quantile bins
quantile_bins <- seq(0.01, 1.00, by = 0.01)
hiv_data2 <- hiv_data2 %>%
  mutate(quantile_group = ntile(total_spending, 100))  # split into 100 quantiles

#Step 2️⃣ — Summarize spending within each quantile
quantile_summary <- hiv_data2 %>%
  group_by(quantile_group) %>%
  summarise(
    baseline_med = median(total_spending, na.rm = TRUE),
    postcap_med_2025 = median(actual_spend_ira_2025, na.rm = TRUE),
    savings_med_2025 = median(total_spending - actual_spend_ira_2025, na.rm = TRUE),
    coverage_gain_2025 = mean((total_spending - actual_spend_ira_2025) / 2000, na.rm = TRUE), # rough cap-based
    n_patients = n()
  ) %>%
  mutate(tau = quantile_bins)

# Step 3️⃣ — Visualize the results
# 3(A) Distribution of spending across quantiles
ggplot(quantile_summary, aes(x = tau)) +
  geom_line(aes(y = baseline_med, color = "Baseline Spending"), size = 1) +
  geom_line(aes(y = postcap_med_2025, color = "Post-Cap Spending"), size = 1) +
  geom_hline(yintercept = 2000, linetype = "dashed", color = "red") +
  labs(title = "Spending Distribution Across Quantiles (τ)",
       subtitle = "Median baseline vs. post-IRA capped spending by quantile",
       x = "Quantile (τ)",
       y = "Spending (USD)",
       color = "Spending Type") +
  theme_minimal(base_size = 13)


# 3(B) Savings or coverage gain curve
ggplot(quantile_summary, aes(x = tau, y = savings_med_2025)) +
  geom_line(color = "#E69F00", size = 1.3) +
  labs(title = "Policy-Induced Savings Across Quantiles (τ)",
       subtitle = "Distribution of savings under the $2,000 IRA cap",
       x = "Quantile (τ)",
       y = "Median Savings (USD)") +
  theme_minimal(base_size = 13)






#-----------------------------------------------------------------------------------------------------
# 8. R-shiny Plot (based on HIV 2k- Model 1) - Test 1
#-----------------------------------------------------------------------------------------------------
#Goal: Use a loop with tau values 0.1 to 0.99 to 
###############################################################################################################################



#Ibrahim's edits (10-24-25)
######################################################################################################################################
#### QUANTILE REGRESSION (using aaron's approach within all 5 different datasets) ####
######################################################################################################################################
#Datasets:
nrow(hiv_data)                # HIV dataset #58671
nrow(mm_data)                 # Multiple Myeloma dataset #15204
nrow(capros_data)           # Prostate Cancer dataset #17304
nrow(pulfib_data) # Pulmonary Fibrosis dataset #5556
nrow(cabr_data)      # Breast Cancer dataset #5586


################################################################
##Use "mm_data" for Quant Reg Model (Base Model 1) - MULTIPLE MYELOMA
################################################################

#-----------------------------------------------------------------------------------------------------
# 1. Creating Matrix for Multiple Myeloma (MM) Dataset - Model 1 (Age only)
#-----------------------------------------------------------------------------------------------------

# Load MM dataset (15,204 obs)
mm_data_coef <- mm_data
names(mm_data_coef)

# Drop coefficient columns if they exist (keeping first 38 columns for raw data)
mm_data2 <- mm_data_coef[, 1:38]

# --- Create dummy variables for Age_cat3 (no intercept) ---
mm_dummy_mat <- model.matrix(~ Age_cat3 - 1, data = mm_data2)

# Clean column names: converts symbols and spaces to dots
colnames(mm_dummy_mat) <- make.names(colnames(mm_dummy_mat))

# Add dummy variables to dataset
mm_data2 <- cbind(mm_data2, as.data.frame(mm_dummy_mat))

# Add intercept for matrix multiplication
mm_data2$'(Intercept)' <- 1

# Drop original Age_cat3 variable (we now have dummy columns)
mm_data2$Age_cat3 <- NULL

names(mm_data2)
#### New variables in mm_data2: #### 
# "Age_cat3Age..55", "Age_cat3Age.56.75", "Age_cat3Age..75", "(Intercept)"

#-----------------------------------------------------------------------------------------------------
# 2. Running Model 1 Quantile Regression for mm_data (only Age as predictor)
#-----------------------------------------------------------------------------------------------------

# Define quantiles from 0.01 to 0.99
quantiles <- seq(0.01, 0.99, by = 0.01)

# Quick check: how many patients are in Age <55 group
table(mm_data2$Age_cat3Age..55)

############### Model 1 (MM dataset, Age only) ###############

# Run quantile regression for each quantile
mm_quantile_mod1 <- rq(total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55, 
                       data = mm_data2, tau = quantiles)

# Extract coefficients for each quantile (rows = variables, columns = quantiles)
mm_coef_multiple_mod1 <- coef(mm_quantile_mod1)
print(mm_coef_multiple_mod1)

# Extract variable names from coefficients
mm_colset_union_mod1 <- rownames(mm_coef_multiple_mod1)

# Align coefficient names with dataset column names
mm_colset_mod1 <- intersect(mm_colset_union_mod1, names(mm_data2))

# Count unique patients and make ID list
mm_n_patients <- length(unique(mm_data2$patient_id))
mm_patient_ids <- sort(unique(mm_data2$patient_id))

# Create a patient-by-variable matrix (rows = patients, cols = model variables)
mm_patient_matrix <- matrix(0, 
                            nrow = mm_n_patients, 
                            ncol = length(mm_colset_mod1), 
                            dimnames = list(
                              id = as.character(mm_patient_ids),
                              variable = mm_colset_mod1
                            ))

# Sanity check: all necessary columns exist in data
stopifnot(all(mm_colset_mod1 %in% names(mm_data2)))

# Helper function to coerce variables into numeric format
coerce_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x))  return(as.numeric(as.character(x)))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  return(as.numeric(x))
}

# Build a one-row-per-patient dataset with the model variables
mm_df_pat1 <- mm_data2 %>%
  select(patient_id, all_of(mm_colset_mod1)) %>%
  group_by(patient_id) %>%
  slice(1) %>%  # In case of duplicates, take first record
  ungroup() %>%
  mutate(across(all_of(mm_colset_mod1), coerce_num))

# Align dataset order with patient_ids vector
mm_df_pat1 <- mm_df_pat1[match(mm_patient_ids, mm_df_pat1$patient_id), ]

# Safety check for missing patient IDs
if (any(is.na(mm_df_pat1$patient_id))) {
  missing_ids <- mm_patient_ids[is.na(mm_df_pat1$patient_id)]
  stop(sprintf("Missing %d patient_id(s) in mm_data2, e.g., %s",
               length(missing_ids), paste(head(missing_ids, 5), collapse = ", ")))
}

# Fill the empty patient matrix with numeric data
mm_patient_matrix[,] <- as.matrix(mm_df_pat1[, mm_colset_mod1, drop = FALSE])
rownames(mm_patient_matrix) <- as.character(mm_df_pat1$patient_id)
colnames(mm_patient_matrix) <- mm_colset_mod1

# Verify dimensions
dim(mm_patient_matrix)      # 13338   x     3
dim(mm_coef_multiple_mod1)  # 3       x    99

#-----------------------------------------------------------------------------------------------------
# 3. Matrix Multiplication - Generating Percentile Predictions
#-----------------------------------------------------------------------------------------------------

# Multiply patient predictors by quantile regression coefficients
mm_percentiles_mod1 <- mm_patient_matrix %*% mm_coef_multiple_mod1

# Rename columns to indicate quantile number
colnames(mm_percentiles_mod1) <- sprintf("mm_mod1_percentile%02d", seq_len(ncol(mm_percentiles_mod1)))

# Convert to data.frame with patient IDs
mm_pred_df1 <- data.frame(
  patient_id = rownames(mm_patient_matrix),
  mm_percentiles_mod1,
  check.names = FALSE
)

# Merge predictions back into mm_data2
mm_data_with_predictions <- dplyr::left_join(mm_data2, mm_pred_df1, by = "patient_id")

# Preview
head(mm_data_with_predictions[, c("patient_id", "mm_mod1_percentile01", "mm_mod1_percentile50", "mm_mod1_percentile99")])

#----------------------------------------------------------
# 4. Distribution Plot: Predicted Spending Across Quantiles
#----------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# ✅ STEP 1: Select the quantile prediction columns
# These should look like: mm_mod1_percentile01, mm_mod1_percentile02, ... mm_mod1_percentile99
mm_pred_cols <- grep("^mm_mod1_percentile", names(mm_data_with_predictions), value = TRUE)

# ✅ STEP 2: Reshape from wide to long format for plotting
mm_long <- mm_data_with_predictions %>%
  select(patient_id, all_of(mm_pred_cols)) %>%
  pivot_longer(
    cols = starts_with("mm_mod1_percentile"),
    names_to = "quantile",
    values_to = "predicted_spending"
  )

# ✅ STEP 3: Clean quantile labels (convert to numeric)
mm_long <- mm_long %>%
  mutate(
    quantile = as.numeric(gsub("mm_mod1_percentile", "", quantile)) / 100  # convert 1→0.01, 99→0.99
  )

# ✅ STEP 4: Plot average predicted spending per quantile
ggplot(mm_long, aes(x = quantile, y = predicted_spending)) +
  stat_summary(fun = mean, geom = "line", color = "#0072B2", linewidth = 1.2) +
  labs(
    title = "Predicted Total Spending Across Quantiles (Multiple Myeloma - Model 1)",
    x = "Quantile (τ)",
    y = "Mean Predicted Total Spending"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )






#########################################################################
##Use "capros_data" for Quant Reg Model (Base Model 1) - CARCINOMA PROSTRATE
#########################################################################

#-----------------------------------------------------------------------------------------------------
# 1. Preparing capros_data (Prostate Cancer)
#-----------------------------------------------------------------------------------------------------

library(readxl)
library(quantreg)
library(dplyr)

# Copy the dataset
cap_data_coef <- capros_data
names(cap_data_coef)

# Drop any appended coefficients, keep first 38 columns (raw variables)
cap_data2 <- cap_data_coef[, 1:38]

# Create dummy variables for Age_cat3 (no intercept here)
dummy_mat_cap <- model.matrix(~ Age_cat3 - 1, data = cap_data2)

# Clean column names (spaces, symbols)
colnames(dummy_mat_cap) <- make.names(colnames(dummy_mat_cap))

# Combine with original dataset
cap_data2 <- cbind(cap_data2, as.data.frame(dummy_mat_cap))

# Add intercept column for later matrix multiplication
cap_data2$"(Intercept)" <- 1

# Drop original Age_cat3 column
cap_data2$Age_cat3 <- NULL

#### New variables in cap_data2:
# "Age_cat3Age..55", "Age_cat3Age.56.75", "Age_cat3Age..75", "(Intercept)"

#-----------------------------------------------------------------------------------------------------
# 2. Running Model 1 Quantile Regression for cap_data (Age only)
#-----------------------------------------------------------------------------------------------------

quantiles <- seq(0.01, 0.99, by = 0.01)

# Quick count for sanity check
table(cap_data2$Age_cat3Age..55)

# Run quantile regression with Age groups as predictors
cap_mod1 <- rq(total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55,
               data = cap_data2, tau = quantiles)

# Extract coefficients matrix (rows = predictors, columns = quantiles)
coef_cap_mod1 <- coef(cap_mod1)
print(coef_cap_mod1)

# Align coefficient names with dataset columns
colset_union_cap_mod1 <- rownames(coef_cap_mod1)
colset_cap_mod1 <- intersect(colset_union_cap_mod1, names(cap_data2))

# Count unique patients
n_patients_cap <- length(unique(cap_data2$patient_id))
patient_ids_cap <- sort(unique(cap_data2$patient_id))

# Create empty patient-by-variable matrix
patient_matrix_cap <- matrix(0,
                             nrow = n_patients_cap,
                             ncol = length(colset_cap_mod1),
                             dimnames = list(
                               id = as.character(patient_ids_cap),
                               variable = colset_cap_mod1
                             )
)

# Sanity check
stopifnot(all(colset_cap_mod1 %in% names(cap_data2)))

# Helper: safely coerce to numeric
coerce_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) return(as.numeric(as.character(x)))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  return(as.numeric(x))
}

# Build per-patient dataset
df_cap_pat_mod1 <- cap_data2 %>%
  select(patient_id, all_of(colset_cap_mod1)) %>%
  group_by(patient_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(across(all_of(colset_cap_mod1), coerce_num))

# Align order of patient IDs
df_cap_pat_mod1 <- df_cap_pat_mod1[match(patient_ids_cap, df_cap_pat_mod1$patient_id), ]

# Safety check for missing IDs
if (any(is.na(df_cap_pat_mod1$patient_id))) {
  missing_ids <- patient_ids_cap[is.na(df_cap_pat_mod1$patient_id)]
  stop(sprintf("Missing %d patient_id(s) in cap_data2, e.g., %s",
               length(missing_ids), paste(head(missing_ids, 5), collapse = ", ")))
}

# Fill matrix
patient_matrix_cap[,] <- as.matrix(df_cap_pat_mod1[, colset_cap_mod1, drop = FALSE])
rownames(patient_matrix_cap) <- as.character(df_cap_pat_mod1$patient_id)
colnames(patient_matrix_cap) <- colset_cap_mod1

# Dimensions check
dim(patient_matrix_cap)
dim(coef_cap_mod1)

#-----------------------------------------------------------------------------------------------------
# 3. Predict Percentiles via Matrix Multiplication
#-----------------------------------------------------------------------------------------------------

cap_mod1_percentiles <- patient_matrix_cap %*% coef_cap_mod1
View(cap_mod1_percentiles)

# Rename percentile columns (e.g., cap_mod1_percentile01, …)
colnames(cap_mod1_percentiles) <- sprintf("cap_mod1_percentile%02d", seq_len(ncol(cap_mod1_percentiles)))

# Convert to dataframe keyed by patient_id
pred_df_cap_mod1 <- data.frame(
  patient_id = rownames(patient_matrix_cap),
  cap_mod1_percentiles,
  check.names = FALSE
)

# Merge predictions back into main dataset
cap_data_with_predictions <- left_join(cap_data2, pred_df_cap_mod1, by = "patient_id")

# View results
head(cap_data_with_predictions)











#-----------------------------------------------------------------------------------------------------
# 4. Quantile Distribution Plot: Predicted Total Spending Across Quantiles (Prostate Cancer - Model 1)
#-----------------------------------------------------------------------------------------------------

# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)

# --- Step 1: Reshape your predictions into a long format ---
# Identify percentile columns (generated from matrix multiplication)
cap_long <- cap_data_with_predictions %>%
  select(patient_id, starts_with("cap_mod1_percentile")) %>%
  pivot_longer(
    cols = starts_with("cap_mod1_percentile"),
    names_to = "quantile",
    values_to = "predicted_spending"
  ) %>%
  mutate(
    # Extract numeric quantile values from the column names
    quantile = as.numeric(gsub("cap_mod1_percentile", "", quantile)) / 100
  )

# --- Step 2: Plot predicted spending across quantiles ---
ggplot(cap_long, aes(x = quantile, y = predicted_spending)) +
  stat_summary(fun = mean, geom = "line", color = "#D55E00", linewidth = 1.2) +
  labs(
    title = "Predicted Total Spending Across Quantiles\n(Prostate Cancer - Model 1)",
    subtitle = "Base model with Age as the only predictor",
    x = "Quantile (τ)",
    y = "Mean Predicted Total Spending (USD)"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





#########################################################################
##Use pulfib_data (Pulmonary Fibrosis) for Quant Reg Model (Base Model 1)
#########################################################################

#-----------------------------------------------------------------------------------------------------
# 1. Preparing pulfib_data (Pulmonary Fibrosis)
#-----------------------------------------------------------------------------------------------------

library(readxl)
library(quantreg)
library(dplyr)
library(ggplot2)
library(tidyr)

# Copy the dataset
pulfib_data_coef <- pulfib_data
names(pulfib_data_coef)

# Drop any appended coefficient columns, keeping only first 38 raw variables
pulfib_data2 <- pulfib_data_coef[, 1:38]

# Create dummy variables for Age_cat3
dummy_mat_pulfib <- model.matrix(~ Age_cat3 - 1, data = pulfib_data2)

# Clean dummy column names (make syntactically valid)
colnames(dummy_mat_pulfib) <- make.names(colnames(dummy_mat_pulfib))

# Append dummy variables to the dataset
pulfib_data2 <- cbind(pulfib_data2, as.data.frame(dummy_mat_pulfib))

# Add intercept column
pulfib_data2$"(Intercept)" <- 1

# Drop original Age_cat3 variable
pulfib_data2$Age_cat3 <- NULL

#### New variables in pulfib_data2: ####
# "Age_cat3Age..55", "Age_cat3Age.56.75", "Age_cat3Age..75", "(Intercept)"

#-----------------------------------------------------------------------------------------------------
# 2. Running Model 1 Quantile Regression for pulfib_data (Age only)
#-----------------------------------------------------------------------------------------------------

# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

# Sanity check: how many patients fall into each age group
table(pulfib_data2$Age_cat3Age..55)

# Run quantile regression with Age group dummies
pulfib_mod1 <- rq(total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55,
                  data = pulfib_data2, tau = quantiles)

# Extract coefficient matrix (rows = predictors, columns = quantiles)
coef_pulfib_mod1 <- coef(pulfib_mod1)
print(coef_pulfib_mod1)

# Align coefficient names with dataset columns
colset_union_pulfib_mod1 <- rownames(coef_pulfib_mod1)
colset_pulfib_mod1 <- intersect(colset_union_pulfib_mod1, names(pulfib_data2))

# Count unique patients
n_patients_pulfib <- length(unique(pulfib_data2$patient_id))
patient_ids_pulfib <- sort(unique(pulfib_data2$patient_id))

# Create a patient-by-variable matrix
patient_matrix_pulfib <- matrix(
  0,
  nrow = n_patients_pulfib,
  ncol = length(colset_pulfib_mod1),
  dimnames = list(
    id = as.character(patient_ids_pulfib),
    variable = colset_pulfib_mod1
  )
)

# Sanity check: columns must exist
stopifnot(all(colset_pulfib_mod1 %in% names(pulfib_data2)))

# Helper function to safely convert values to numeric
coerce_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x))  return(as.numeric(as.character(x)))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  return(as.numeric(x))
}

# Build per-patient dataset with predictor columns
df_pulfib_pat_mod1 <- pulfib_data2 %>%
  select(patient_id, all_of(colset_pulfib_mod1)) %>%
  group_by(patient_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(across(all_of(colset_pulfib_mod1), coerce_num))

# Align to patient ID order
df_pulfib_pat_mod1 <- df_pulfib_pat_mod1[match(patient_ids_pulfib, df_pulfib_pat_mod1$patient_id), ]

# Check for missing IDs
if (any(is.na(df_pulfib_pat_mod1$patient_id))) {
  missing_ids <- patient_ids_pulfib[is.na(df_pulfib_pat_mod1$patient_id)]
  stop(sprintf("Missing %d patient_id(s) in pulfib_data2, e.g., %s",
               length(missing_ids), paste(head(missing_ids, 5), collapse = ", ")))
}

# Fill the patient matrix
patient_matrix_pulfib[,] <- as.matrix(df_pulfib_pat_mod1[, colset_pulfib_mod1, drop = FALSE])
rownames(patient_matrix_pulfib) <- as.character(df_pulfib_pat_mod1$patient_id)
colnames(patient_matrix_pulfib) <- colset_pulfib_mod1

# Dimension check
dim(patient_matrix_pulfib)
dim(coef_pulfib_mod1)

#-----------------------------------------------------------------------------------------------------
# 3. Predict Percentiles via Matrix Multiplication
#-----------------------------------------------------------------------------------------------------

# Matrix multiplication: patient predictors × quantile-specific coefficients
pulfib_mod1_percentiles <- patient_matrix_pulfib %*% coef_pulfib_mod1

# Rename percentile columns
colnames(pulfib_mod1_percentiles) <- sprintf("pulfib_mod1_percentile%02d", seq_len(ncol(pulfib_mod1_percentiles)))

# Convert to dataframe keyed by patient_id
pred_df_pulfib_mod1 <- data.frame(
  patient_id = rownames(patient_matrix_pulfib),
  pulfib_mod1_percentiles,
  check.names = FALSE
)

# Merge predictions with main dataset
pulfib_data_with_predictions <- left_join(pulfib_data2, pred_df_pulfib_mod1, by = "patient_id")

# View results
head(pulfib_data_with_predictions)

#-----------------------------------------------------------------------------------------------------
# 4. Quantile Distribution Plot: Predicted Total Spending Across Quantiles (Pulmonary Fibrosis - Model 1)
#-----------------------------------------------------------------------------------------------------

# Reshape predictions to long format
pulfib_long <- pulfib_data_with_predictions %>%
  select(patient_id, starts_with("pulfib_mod1_percentile")) %>%
  pivot_longer(
    cols = starts_with("pulfib_mod1_percentile"),
    names_to = "quantile",
    values_to = "predicted_spending"
  ) %>%
  mutate(
    quantile = as.numeric(gsub("pulfib_mod1_percentile", "", quantile)) / 100
  )

# Plot average predicted spending across quantiles
ggplot(pulfib_long, aes(x = quantile, y = predicted_spending)) +
  stat_summary(fun = mean, geom = "line", color = "#009E73", linewidth = 1.2) +
  labs(
    title = "Predicted Total Spending Across Quantiles\n(Pulmonary Fibrosis - Model 1)",
    subtitle = "Base model with Age as the only predictor",
    x = "Quantile (τ)",
    y = "Mean Predicted Total Spending (USD)"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )







#########################################################################
##Use cabr_data (Carcinoma Breast) for Quant Reg Model (Base Model 1)
#########################################################################
#-----------------------------------------------------------------------------------------------------
# 1. Preparing cabr_data (Carcinoma Breast)
#-----------------------------------------------------------------------------------------------------

library(readxl)
library(quantreg)
library(dplyr)
library(ggplot2)
library(tidyr)

# Copy the dataset
cabr_data_coef <- cabr_data
names(cabr_data_coef)

# Drop any appended coefficient columns, keeping only the first 38 raw variables
cabr_data2 <- cabr_data_coef[, 1:38]

# Create dummy variables for Age_cat3 (no intercept)
dummy_mat_cabr <- model.matrix(~ Age_cat3 - 1, data = cabr_data2)

# Clean dummy variable names
colnames(dummy_mat_cabr) <- make.names(colnames(dummy_mat_cabr))

# Append dummy variables to dataset
cabr_data2 <- cbind(cabr_data2, as.data.frame(dummy_mat_cabr))

# Add intercept column for later matrix multiplication
cabr_data2$"(Intercept)" <- 1

# Drop original Age_cat3 column (replaced by dummies)
cabr_data2$Age_cat3 <- NULL

#### New variables in cabr_data2: ####
# "Age_cat3Age..55", "Age_cat3Age.56.75", "Age_cat3Age..75", "(Intercept)"

#-----------------------------------------------------------------------------------------------------
# 2. Running Model 1 Quantile Regression for cabr_data (Age only)
#-----------------------------------------------------------------------------------------------------

# Define quantiles (τ = 0.01 → 0.99)
quantiles <- seq(0.01, 0.99, by = 0.01)

# Sanity check for group distribution
table(cabr_data2$Age_cat3Age..55)

# Run quantile regression with Age groups as predictors
cabr_mod1 <- rq(total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55,
                data = cabr_data2, tau = quantiles)

# Extract coefficient matrix
coef_cabr_mod1 <- coef(cabr_mod1)
print(coef_cabr_mod1)

# Align coefficient names with dataset column names
colset_union_cabr_mod1 <- rownames(coef_cabr_mod1)
colset_cabr_mod1 <- intersect(colset_union_cabr_mod1, names(cabr_data2))

# Count unique patients and get IDs
n_patients_cabr <- length(unique(cabr_data2$patient_id))
patient_ids_cabr <- sort(unique(cabr_data2$patient_id))

# Create patient-by-variable matrix
patient_matrix_cabr <- matrix(
  0,
  nrow = n_patients_cabr,
  ncol = length(colset_cabr_mod1),
  dimnames = list(
    id = as.character(patient_ids_cabr),
    variable = colset_cabr_mod1
  )
)

# Sanity check
stopifnot(all(colset_cabr_mod1 %in% names(cabr_data2)))

# Helper: safely convert all variables to numeric
coerce_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) return(as.numeric(as.character(x)))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  return(as.numeric(x))
}

# Build per-patient dataset
df_cabr_pat_mod1 <- cabr_data2 %>%
  select(patient_id, all_of(colset_cabr_mod1)) %>%
  group_by(patient_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(across(all_of(colset_cabr_mod1), coerce_num))

# Align order of patient IDs
df_cabr_pat_mod1 <- df_cabr_pat_mod1[match(patient_ids_cabr, df_cabr_pat_mod1$patient_id), ]

# Check for missing IDs
if (any(is.na(df_cabr_pat_mod1$patient_id))) {
  missing_ids <- patient_ids_cabr[is.na(df_cabr_pat_mod1$patient_id)]
  stop(sprintf("Missing %d patient_id(s) in cabr_data2, e.g., %s",
               length(missing_ids), paste(head(missing_ids, 5), collapse = ", ")))
}

# Fill the patient matrix with numeric values
patient_matrix_cabr[,] <- as.matrix(df_cabr_pat_mod1[, colset_cabr_mod1, drop = FALSE])
rownames(patient_matrix_cabr) <- as.character(df_cabr_pat_mod1$patient_id)
colnames(patient_matrix_cabr) <- colset_cabr_mod1

# Check dimensions
dim(patient_matrix_cabr)
dim(coef_cabr_mod1)

#-----------------------------------------------------------------------------------------------------
# 3. Predict Percentiles via Matrix Multiplication
#-----------------------------------------------------------------------------------------------------

# Perform matrix multiplication: patient predictors × quantile-specific coefficients
cabr_mod1_percentiles <- patient_matrix_cabr %*% coef_cabr_mod1

# Rename percentile columns
colnames(cabr_mod1_percentiles) <- sprintf("cabr_mod1_percentile%02d", seq_len(ncol(cabr_mod1_percentiles)))

# Convert to data frame keyed by patient_id
pred_df_cabr_mod1 <- data.frame(
  patient_id = rownames(patient_matrix_cabr),
  cabr_mod1_percentiles,
  check.names = FALSE
)

# Merge predictions back into the main dataset
cabr_data_with_predictions <- left_join(cabr_data2, pred_df_cabr_mod1, by = "patient_id")

# Preview final dataset
head(cabr_data_with_predictions)

#-----------------------------------------------------------------------------------------------------
# 4. Quantile Distribution Plot: Predicted Total Spending Across Quantiles (Carcinoma Breast - Model 1)
#-----------------------------------------------------------------------------------------------------

# Reshape predictions into long format for ggplot
cabr_long <- cabr_data_with_predictions %>%
  select(patient_id, starts_with("cabr_mod1_percentile")) %>%
  pivot_longer(
    cols = starts_with("cabr_mod1_percentile"),
    names_to = "quantile",
    values_to = "predicted_spending"
  ) %>%
  mutate(
    quantile = as.numeric(gsub("cabr_mod1_percentile", "", quantile)) / 100
  )

# Plot average predicted spending across quantiles
ggplot(cabr_long, aes(x = quantile, y = predicted_spending)) +
  stat_summary(fun = mean, geom = "line", color = "#CC79A7", linewidth = 1.2) +
  labs(
    title = "Predicted Total Spending Across Quantiles\n(Carcinoma Breast - Model 1)",
    subtitle = "Base model with Age as the only predictor",
    x = "Quantile (τ)",
    y = "Mean Predicted Total Spending (USD)"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#########################################################################
# Observations/Interpretations from the Plots #
#########################################################################

# | Disease                     | τ (Quantile) where mean predicted spending ≈ $2,000 | General shape                            |
# | --------------------------- | --------------------------------------------------- | ---------------------------------------- |
# | **Multiple Myeloma**        | ≈ 0.15–0.20                                         | Very steep, high spenders dominate early |
# | **Pulmonary Fibrosis**      | ≈ 0.25                                              | Smoother, rising mid-tail                |
# | **Breast Cancer**           | ≈ 0.35–0.40                                         | Gradual increase, larger tail above 10k  |
# | **Prostate                  | ≈ 0.25–0.30                                         | Moderate rise                            |
  

#########################################################################
# Understanding High Spend Subjects within Disease Areas (>$2K) --> with predict spend & visualization
#########################################################################
#Approach:Identify, for each disease, the quantile τ* at which the predicted mean total spending crosses $2,000.
#Then, use that τ* as the disease-specific cutoff to estimate what proportion of patients are expected to exceed $2,000.

#Importance: Data-driven τ* (recommended)
#Accurately identifies where $2,000 sits in the conditional distribution; lets you report “X% of patients are likely above the cap” per disease

#1. Data-Driven Threshold Detection:
#Instead of hard-coding percentile90, we find the first quantile where the predicted mean spending exceeds $2,000.
#1️⃣ Collapse predictions across patients to get mean predicted spending per quantile:
mean_spend_by_tau <- mm_data_with_predictions %>%
  select(starts_with("mm_mod1_percentile")) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "quantile",
               values_to = "mean_spending") %>%
  mutate(tau = as.numeric(gsub("mm_mod1_percentile", "", quantile)) / 100)

#2️⃣ Locate τ* where mean spending ≈ $2,000:
threshold_tau <- mean_spend_by_tau %>%
  filter(mean_spending >= 2000) %>%
  slice_head(n = 1) %>%
  pull(tau)
print(threshold_tau)
###################
# print(threshold_tau)
#[1] 0.22
###################

#3️⃣ Use this τ* to classify high-spend patients:
colname <- sprintf("mm_mod1_percentile%02d", threshold_tau * 100)
mm_data_with_predictions$predicted_spend <- mm_data_with_predictions[[colname]]
mm_data_with_predictions$likely_over_2k <- ifelse(mm_data_with_predictions$predicted_spend > 2000, 1, 0)
mean(mm_data_with_predictions$likely_over_2k)
###################
# mean(mm_data_with_predictions$likely_over_2k)
#[1] 0.58603
###################
#Interpretation:
#“The $2,000 out-of-pocket cap falls near the 20th quantile for Multiple Myeloma 
#but near the 35th quantile for Breast Cancer, suggesting heterogeneity in spending patterns across diseases.”

#43️⃣Visualization: Add a vertical line showing where $2k occurs:

ggplot(mean_spend_by_tau, aes(x = tau, y = mean_spending)) +
  geom_line(size = 1, color = "#0072B2") +
  geom_hline(yintercept = 2000, color = "red", linetype = "dashed") +
  geom_vline(xintercept = threshold_tau, color = "darkred", linetype = "dotted") +
  annotate("text", x = threshold_tau + 0.05, y = 2200,
           label = sprintf("τ ≈ %.2f", threshold_tau), color = "darkred") +
  labs(title = "Predicted Mean Spending by Quantile (MM)",
       x = "Quantile (τ)",
       y = "Mean Predicted Spending (USD)") +
  theme_minimal(base_size = 13)



#names(hiv_data2)

######################################################################################################################################
#### QUANTILE REGRESSION for-loop hiv_data2 - Add Predictors (MULTIVARIABLE MODELS 2, 3, 4) 
######################################################################################################################################

# starting with the HIV data 
install.packages("fastDummies")
install.packages("SparseM")
library(readxl)
library(SparseM)
library(quantreg)
library(dplyr)
library(fastDummies)

unique(hiv_data2$Income_cat3)


#-----------------------------------------------------------------------------------------------------
# 1. Creating Clean Income Category Dummies (Consistent with Age naming system)
#-----------------------------------------------------------------------------------------------------

# 1️⃣ Check unique income levels
unique(hiv_data2$Income_cat3)
# Expected:
# lessthan_47,999
# btw_48,000-71,999
# greaterthan_72,000

# 2️⃣ Create dummy variables from Income_cat3
income_dummy_mat <- model.matrix(~ Income_cat3 - 1, data = hiv_data2)

# 3️⃣ Clean column names for readability (use dots to match Age_cat3 naming style)
colnames(income_dummy_mat) <- make.names(
  gsub("greaterthan_72,000", "Income.72plus",
       gsub("btw_48,000-71,999", "Income.48.71",
            gsub("lessthan_47,999", "Income..47",
                 gsub("Income_cat3", "Income_cat3", 
                      colnames(income_dummy_mat),
                      fixed = TRUE),
                 fixed = TRUE),
            fixed = TRUE),
       fixed = TRUE)
)

#Sanity check: col names of income
colnames(income_dummy_mat)
#[1] "Income_cat3Income..47"    "Income_cat3Income.48.71"  "Income_cat3Income.72plus"

# 4️⃣ Combine the dummies with the main dataset
hiv_data2 <- cbind(hiv_data2, as.data.frame(income_dummy_mat))

# 5️⃣ Drop the original categorical variable to avoid duplication
hiv_data2$Income_cat3 <- NULL

# 6️⃣ Verify that new variables were created properly
names(hiv_data2)[grepl("Income_cat3", names(hiv_data2))]
#Correct vars: [1] "Income_cat3Income..47"    "Income_cat3Income.48.71"  "Income_cat3Income.72plus"
names(hiv_data2)


#-----------------------------------------------------------------------------------------------------
# 2. FOR-LOOP: Incrementally add predictors to HIV quantile regression models and visualize results
#-----------------------------------------------------------------------------------------------------
library(quantreg)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

#-----------------------------------------------------------------------------------------------------
# 1️⃣ Define model formulas with new variable names (Age + Income dummies + Gender + Ethnicity)
#-----------------------------------------------------------------------------------------------------
model_formulas <- list(
  hiv_mod1 = total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55,
  hiv_mod2 = total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55 + Gender_Male,
  hiv_mod3 = total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55 + Gender_Male + Ethnicity_White,
  hiv_mod4 = total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55 + Gender_Male + Ethnicity_White +
    Income_cat3Income..47 + Income_cat3Income.48.71
  # Note: Income_cat3Income.72plus is the reference group (baseline)
)

# Create lists to store model outputs
model_results <- list()
mean_predicted_spend_list <- list()

#-----------------------------------------------------------------------------------------------------
# 2️⃣ Run all models incrementally — FIXED to ensure all models output properly
#-----------------------------------------------------------------------------------------------------
for (i in seq_along(model_formulas)) {
  
  model_name <- names(model_formulas)[i]
  formula <- model_formulas[[i]]
  message(paste0("\nRunning ", model_name, "..."))
  
  # Run quantile regression
  qr_model <- suppressWarnings(rq(formula, data = hiv_data2, tau = quantiles))
  coef_matrix <- coef(qr_model)
  
  # --- Fix alignment of coefficients and patient data ---
  colset <- intersect(rownames(coef_matrix), names(hiv_data2))
  if ("(Intercept)" %in% rownames(coef_matrix) && !"(Intercept)" %in% names(hiv_data2)) {
    hiv_data2$'(Intercept)' <- 1
  }
  if (!"(Intercept)" %in% colset && "(Intercept)" %in% rownames(coef_matrix)) {
    colset <- c("(Intercept)", colset)
  }
  
  # Build patient matrix
  n_patients <- length(unique(hiv_data2$patient_id))
  patient_ids <- sort(unique(hiv_data2$patient_id))
  
  df_pat <- hiv_data2 %>%
    select(patient_id, all_of(colset)) %>%
    group_by(patient_id) %>%
    slice(1) %>%
    ungroup()
  df_pat <- df_pat[match(patient_ids, df_pat$patient_id), ]
  patient_matrix <- as.matrix(df_pat[, colset, drop = FALSE])
  rownames(patient_matrix) <- as.character(df_pat$patient_id)
  
  # Ensure matrix alignment
  coef_matrix <- coef_matrix[rownames(coef_matrix) %in% colnames(patient_matrix), , drop = FALSE]
  coef_matrix <- coef_matrix[match(colnames(patient_matrix), rownames(coef_matrix)), , drop = FALSE]
  
  # Predict for all quantiles
  percentiles <- patient_matrix %*% coef_matrix
  colnames(percentiles) <- sprintf("percentile%02d", seq_len(ncol(percentiles)))
  
  pred_df <- data.frame(patient_id = rownames(patient_matrix), percentiles, check.names = FALSE)
  data_with_pred <- left_join(hiv_data2, pred_df, by = "patient_id")
  
  # ✅ Guarantee model tagging before storing
  mean_predicted_spend <- data_with_pred %>%
    select(starts_with("percentile")) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
    pivot_longer(cols = everything(),
                 names_to = "quantile",
                 values_to = "mean_predicted_spend") %>%
    mutate(
      tau = as.numeric(gsub("percentile", "", quantile)) / 100,
      model = factor(model_name,
                     levels = c("hiv_mod1", "hiv_mod2", "hiv_mod3", "hiv_mod4"),
                     labels = c("Model 1: Age only",
                                "Model 2: + Gender",
                                "Model 3: + Ethnicity",
                                "Model 4: + Income"))
    )
  
  # Store properly
  mean_predicted_spend_list[[model_name]] <- mean_predicted_spend
}

#-----------------------------------------------------------------------------------------------------
# 3️⃣ Combine all model results for plotting
#-----------------------------------------------------------------------------------------------------
mean_predicted_spend_all <- bind_rows(mean_predicted_spend_list)

# Sanity check
table(mean_predicted_spend_all$model)

#-----------------------------------------------------------------------------------------------------
# 4️⃣ Visualization: Show all 4 HIV Models
#-----------------------------------------------------------------------------------------------------
ggplot(mean_predicted_spend_all, aes(x = tau, y = mean_predicted_spend,
                                     color = model, group = model)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.1, alpha = 0.7) +
  geom_hline(yintercept = 2000, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.8, y = 2100, label = "$2K Cap Threshold", color = "red", size = 4) +
  labs(
    title = "Predicted Total Spending Across Quantiles (HIV Data)",
    subtitle = "Models 1–4: Incremental Predictors (Age, Gender, Ethnicity, Income)",
    x = "Quantile (τ)",
    y = "Mean Predicted Total Spending (USD)",
    color = "Model"
  ) +
  scale_color_manual(values = c(
    "Model 1: Age only"   = "#0072B2",
    "Model 2: + Gender"   = "#009E73",
    "Model 3: + Ethnicity"= "#E69F00",
    "Model 4: + Income"   = "#D55E00"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 5️⃣How much are the savings in total spend?
#→ total_savings = projected aggregate savings across all patients if the $2K cap is applied.
hiv_data_with_predictions$savings <- pmax(0, hiv_data_with_predictions$total_spending - 2000)
hiv_data_with_predictions$spend_after_cap <- pmin(hiv_data_with_predictions$total_spending, 2000)
total_savings <- sum(hiv_data_with_predictions$savings, na.rm = TRUE)

# 6️⃣How many more patients can be provided with funds?
#If the Patient Assistance Fund (PAF) reallocated these savings:
# 1. Assume average patient cost (based on mean predicted spend per disease area).
avg_spend_per_patient <- mean(hiv_data_with_predictions$total_spending, na.rm = TRUE)

#2. Then estimate additional patients who could be supported:
additional_patients_supported <- total_savings / avg_spend_per_patient

# 6️⃣Visualization for Savings Impac: extend your quantile plot with a shaded “Savings Zone”:
# ✅ Quantile Spending Visualization with “Savings Zone” Shading

ggplot(mean_predicted_spend_all,
       aes(x = tau, y = mean_predicted_spend,
           color = model, group = model)) +
  
  # ---- Shaded Savings Zone (area above $2K) ----
geom_ribbon(
  data = subset(mean_predicted_spend_all, mean_predicted_spend > 2000),
  aes(ymin = 2000, ymax = mean_predicted_spend, fill = model),
  alpha = 0.2, color = NA
) +
  
  # ---- Model lines ----
geom_line(size = 1.2) +
  geom_point(size = 1.1, alpha = 0.7) +
  
  # ---- 2K threshold line ----
geom_hline(yintercept = 2000, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.8, y = 2100,
           label = "$2K Cap Threshold", color = "red", size = 4) +
  
  # ---- Labels and legend ----
labs(
  title = "Predicted Total Spending Across Quantiles (HIV Data)",
  subtitle = "Models 1–4: Incremental Predictors (Age, Gender, Ethnicity, Income)",
  x = "Quantile (τ)",
  y = "Mean Predicted Total Spending (USD)",
  color = "Model",
  fill = "Model"
) +
  
  # ---- Color + Fill Consistency ----
scale_color_manual(values = c(
  "Model 1: Age only"    = "#0072B2",
  "Model 2: + Gender"    = "#009E73",
  "Model 3: + Ethnicity" = "#E69F00",
  "Model 4: + Income"    = "#D55E00"
)) +
  scale_fill_manual(values = c(
    "Model 1: Age only"    = "#0072B2",
    "Model 2: + Gender"    = "#009E73",
    "Model 3: + Ethnicity" = "#E69F00",
    "Model 4: + Income"    = "#D55E00"
  )) +
  
  # ---- Theme ----
theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )




######################################################################################################################################
#### PREDICT SPEND - 5 DISEASE AREAS (BASED ON IRA CAPS 2K (2025) & 2.1K (2026))
#####################################################################################################################################

#------------------------------------------------------------
# Coverage Expansion Table for IRA Caps ($2000 and $2100)
#------------------------------------------------------------

# Define the IRA caps
ira_caps <- c(2000, 2100)

# Helper function to compute coverage metrics per dataset
compute_coverage_metrics <- function(data, disease_name, ira_cap) {
  
  # Ensure variable exists
  if (!"total_spending" %in% names(data)) stop(paste("Missing total_spending in", disease_name))
  
  n_patients <- length(unique(data$patient_id))
  total_baseline <- sum(data$total_spending, na.rm = TRUE)
  mean_spend <- mean(data$total_spending, na.rm = TRUE)
  
  # Apply IRA cap
  data$spend_after_IRA <- pmin(data$total_spending, ira_cap)
  
  total_IRA <- sum(data$spend_after_IRA, na.rm = TRUE)
  mean_spend_IRA <- mean(data$spend_after_IRA, na.rm = TRUE)
  
  total_savings <- total_baseline - total_IRA
  n_patients_IRA <- total_savings / mean_spend_IRA
  coverage_gain_percent <- ((n_patients_IRA - n_patients) / n_patients) * 100
  
  tibble(
    Disease_Top5 = disease_name,
    IRA_cap = ira_cap,
    n_patients = n_patients,
    total_baseline = total_baseline,
    mean_spend = mean_spend,
    mean_spend_IRA = mean_spend_IRA,
    n_patients_IRA = n_patients_IRA,
    coverage_gain_percent = coverage_gain_percent
  )
}

#------------------------------------------------------------
# Apply the function to all disease datasets
#------------------------------------------------------------
disease_datasets <- list(
  "HIV, AIDS and Prevention" = hiv_data_with_predictions,
  "Multiple Myeloma" = mm_data_with_predictions,
  "Prostate Cancer" = cap_data_with_predictions,
  "Pulmonary Fibrosis" = pulfib_data_with_predictions,
  "Breast Cancer" = cabr_data_with_predictions
)

# Generate results for both years (2025: $2000, 2026: $2100)
coverage_expansion_results <- purrr::map_dfr(
  ira_caps, 
  ~ purrr::map_dfr(names(disease_datasets), function(disease) {
    compute_coverage_metrics(disease_datasets[[disease]], disease, .x)
  })
)

#------------------------------------------------------------
# Preview the table
#------------------------------------------------------------
coverage_expansion_results %>%
  arrange(IRA_cap, Disease_Top5) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  knitr::kable()

#------------------------------------------------------------
# Visualization for This Table
#------------------------------------------------------------
ggplot(coverage_expansion_results, 
       aes(x = reorder(Disease_Top5, coverage_gain_percent),
           y = coverage_gain_percent, fill = as.factor(IRA_cap))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  labs(
    title = "Projected Coverage Expansion under IRA Cap",
    x = "Disease Area",
    y = "Coverage Gain (%)",
    fill = "IRA Cap ($)"
  ) +
  theme_minimal(base_size = 13)



######################################################################################################################################
#### Combined Visualization: Mean Predicted Spending Across Quantiles by Disease
#####################################################################################################################################
#Dataset names
mm_data_with_predictions
hiv_data_with_predictions
cap_data_with_predictions
pulfib_data_with_predictions
cabr_data_with_predictions

#understanding the naming system within each dataset before combing
###| Dataset                        | Example Column Prefix    |
###| ------------------------------ | ------------------------ |
#  | `hiv_data_with_predictions`    | `percentile`             |
#  | `mm_data_with_predictions`     | `mm_mod1_percentile`     |
#  | `cap_data_with_predictions`    | `cap_mod1_percentile`    |
#  | `pulfib_data_with_predictions` | `pulfib_mod1_percentile` |
#  | `cabr_data_with_predictions`   | `cabr_mod1_percentile`   |
#The code below automatically detects the percentile columns for each dataset — regardless of prefix — &..
#...then builds the combined visualization with your consistent formatting.  

#-----------------------------------------------------------------------------------------------------
# 1️⃣ Load Libraries
#-----------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------
# 2️⃣ Flexible Summarization Function (Auto-detects percentile columns)
#-----------------------------------------------------------------------------------------------------
summarize_disease_spending <- function(data, disease_name) {
  # Detect any columns that contain 'percentile'
  percentile_cols <- grep("percentile", names(data), value = TRUE)
  
  if (length(percentile_cols) == 0) {
    message(paste("⚠️ Skipping", disease_name, "- no percentile columns found."))
    return(NULL)
  }
  
  # Summarize across quantiles
  data %>%
    select(all_of(percentile_cols)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "quantile",
      values_to = "mean_predicted_spend"
    ) %>%
    mutate(
      # Extract numeric quantile values from column names
      tau = as.numeric(gsub(".*percentile", "", quantile)) / 100,
      Disease = disease_name
    )
}

#-----------------------------------------------------------------------------------------------------
# 3️⃣ Apply Function to Each Dataset (Auto-handles naming differences)
#-----------------------------------------------------------------------------------------------------
mean_spending_all <- bind_rows(
  summarize_disease_spending(hiv_data_with_predictions, "HIV, AIDS and Prevention"),
  summarize_disease_spending(mm_data_with_predictions, "Multiple Myeloma"),
  summarize_disease_spending(cap_data_with_predictions, "Prostate Cancer"),
  summarize_disease_spending(pulfib_data_with_predictions, "Pulmonary Fibrosis"),
  summarize_disease_spending(cabr_data_with_predictions, "Breast Cancer")
)

#-----------------------------------------------------------------------------------------------------
# 4️⃣ Visualization: Mean Predicted Spending Across Quantiles (All Diseases)
#-----------------------------------------------------------------------------------------------------
disease_colors <- c(
  "HIV, AIDS and Prevention" = "#0072B2",   # Blue
  "Multiple Myeloma" = "#E69F00",           # Orange
  "Prostate Cancer" = "#D55E00",            # Red
  "Pulmonary Fibrosis" = "#009E73",         # Green
  "Breast Cancer" = "#CC79A7"               # Pink/Purple
)

ggplot(mean_spending_all, aes(x = tau, y = mean_predicted_spend, color = Disease, group = Disease)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.3) +
  scale_color_manual(values = disease_colors) +
  labs(
    title = "Mean Predicted Total Spending Across Quantiles (τ = 0.01–0.99)",
    subtitle = "Comparison of Spending Distributions by Disease Area",
    x = "Quantile (τ)",
    y = "Mean Predicted Total Spending ($)",
    color = "Disease Area"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_hline(yintercept = 2000, linetype = "solid", color = "black", size = 1) +
  annotate("text", x = 0.01, y = 2150, label = "IRA Cap ($2,000)",  #raise/lower y to move away from data lines
           color = "black", size = 4, 
           fontface = "italic", hjust = 0, vjust = -0.5)




