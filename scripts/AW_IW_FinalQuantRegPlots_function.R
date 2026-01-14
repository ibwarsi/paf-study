### aaron's edits 
#### Dated: 12 - 2 - 2025
######################################################################################################################################
#### QUANTILE REGRESSION & MATRIX CREATION (hiv_data) ####
######################################################################################################################################

# previously, we were only able to include a 1 covariate due to convergence issues
# instead of manually testing different covariates, i think an automatic variables selection approach 
# makes the most sense. So, let's use the Lasso here to make our lives easier. 

# starting with the HIV data 
#install.packages("fastDummies")
#install.packages("SparseM")
library(readxl)
library(SparseM)
library(quantreg)
library(dplyr)
library(fastDummies)
library(rqPen)      # new
library(caret)  # for nearZeroVar, optional



#### so since we need to do the exact same thing for the other datasets, we 
### can take the above code and then build a function so we can do the same thing again. 
# created a function see this if you haven't done this before: https://www.dataquest.io/blog/write-functions-in-r/


# ===================================================================
# MAIN FUNCTION: Fast Quantile Regression LASSO using cv.hqreg
# Purpose: Fit a LASSO-penalized quantile regression model across many 
#          quantiles (e.g., 5th to 95th percentile) and return predicted 
#          spending at each quantile for every patient.
# ===================================================================

run_quantile_lasso_predictions <- function(
    data, # Either a data frame OR a path to an Excel file
    response_var = "total_spending", # Name of the outcome column (continuous $)
    id_var = "patient_id", # Unique patient identifier (not used in model, just kept)
    formula = ~ Gender + Employment_Status + Income_Group +
      Ethnicity  + Insurance_Type + Age_cat3, # Predictors to include
    quantiles = seq(0.1, 0.95, by = 0.15), # Which percentiles to model (default: fewer for speed)
    penalty = "LASSO", # "LASSO" = pure L1 penalty; can also use "ENET"
    alpha = 1, # 1 = pure LASSO; 0.5 = 50/50 LASSO+ridge; etc.
    seed = 123, # For reproducible cross-validation folds
    nfolds = 3, # CV folds (reduced for speed; 10 is the default for the package)
    ncores = 1, # Start with 1 (sequential) to avoid parallel bugs; set higher once tested
    save_coef = TRUE, # Do you want the coefficient table saved to disk?
    output_prefix = "hiv" # Prefix for output files and final data-frame name
) {
  
  # ---------------------------------------------------------------
  # Load required packages (stops with message if missing)
  # ---------------------------------------------------------------
  if (!requireNamespace("hqreg", quietly = TRUE)) {
    stop("Package 'hqreg' is required. Install with: install.packages('hqreg')")
  }
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("Package 'tidyverse' is required. Install with: install.packages('tidyverse')")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required for Excel files. Install with: install.packages('readxl')")
  }
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package 'parallel' is required for ncores > 1.")
  }
  
  library(tidyverse) # dplyr, tidyr, etc.
  library(hqreg) # The fast quantile regression package
  library(readxl) # Only needed if reading Excel
  library(parallel) # For ncores detection (if you set ncores = detectCores() - 1)
  
  # ---------------------------------------------------------------
  # 1. Load the data if the user passed a file path instead of a data frame
  # ---------------------------------------------------------------
  if (is.character(data)) {
    message("Reading Excel file: ", data)
    df <- read_excel(data) # Load the Excel sheet
  } else {
    df <- data # Already a data frame in memory
  }
  
  # Nice name for messages / filenames
  dataset_name <- if (is.character(data)) {
    tools::file_path_sans_ext(basename(data))
  } else {
    deparse(substitute(data))
  }
  
  # ---------------------------------------------------------------
  # 2. Convert all predictor variables in the formula to factors
  # (necessary for correct dummy coding)
  # ---------------------------------------------------------------
  predictor_names <- all.vars(formula) # Extracts variable names from the formula
  df <- df %>%
    mutate(across(all_of(predictor_names), factor)) # Forces them to be factors
  
  # Extract the response vector
  y <- df[[response_var]]
  
  # ---------------------------------------------------------------
  # 3. Create the design matrix X (includes intercept automatically)
  # model.matrix() turns factors into dummy variables
  # ---------------------------------------------------------------
  X_full <- model.matrix(formula, data = df) # Includes the intercept column
  # note: the model will automatically detect the intercept column and ignore it
  # we include this because it makes things easier for prediction
  message("Design matrix created: ", nrow(X_full), " rows × ", ncol(X_full),
          " columns (including intercept)")
  
  # ---------------------------------------------------------------
  # 4. Fit cross-validated quantile LASSO: LOOP over each tau (package limitation)
  #    Each cv.hqreg() is fast; parallel works per tau if ncores > 1
  # ---------------------------------------------------------------
  set.seed(seed) # Makes CV folds reproducible
  
  message("Running cv.hqreg for ", length(quantiles),
          " quantiles (go grab a coffee)...")
  
  # Pre-allocate for results
  cv_models <- vector("list", length(quantiles)) # One CV model per tau
  coef_list <- vector("list", length(quantiles)) # Coefs per tau
  
  for (i in seq_along(quantiles)) {
    tau_i <- quantiles[i]
    message("  Fitting tau = ", sprintf("%.2f", tau_i), " (", i, "/", length(quantiles), ")")
    
    cv_models[[i]] <- cv.hqreg(
      X = X_full, # Design matrix (with intercept)
      y = y, # Outcome vector
      FUN = "hqreg", # FIXED: Explicitly specify to avoid parallel worker errors
      method = "quantile", # FIXED: Explicit for quantile loss
      tau = tau_i, # SINGLE scalar tau (package doesn't support vectors)
      alpha = if (penalty == "LASSO") 1 else alpha, # 1 = pure LASSO
      nfolds = nfolds, # Number of cross-validation folds
      seed = seed + i - 1, # Vary seed slightly per tau for independence
      ncores = ncores, # Parallel across folds (if >1)
      nlambda = 30 # FIXED: Fewer lambda values for speed (default ~100)
      # lambda.min.ratio = 0.01  # Uncomment for even shorter path if needed
    )
    
    coef_list[[i]] <- coef(cv_models[[i]]) # Coefs at lambda.min for this tau
  }
  
  # ---------------------------------------------------------------
  # 5. Extract the coefficient matrix
  # - One column per quantile
  # - One row per predictor (including intercept)
  # ---------------------------------------------------------------
  message("Extracting coefficients for all quantiles...")
  
  coef_mat <- do.call(cbind, coef_list) # Combine into a regular matrix
  colnames(coef_mat) <- paste0("tau_", quantiles) # Name columns by quantile
  rownames(coef_mat) <- names(coef_list[[1]]) # Predictor names (incl. intercept)
  
  # Optionally save the coefficient cube to disk
  if (save_coef) {
    coef_file <- paste0("coef_cube_", penalty, "_", output_prefix, ".rds")
    saveRDS(coef_mat, file = coef_file)
    message("Coefficient matrix saved → ", coef_file)
  }
  
  # ---------------------------------------------------------------
  # 6. Generate predicted values at every quantile for every patient
  # Loop over models (since predict() is per-cv_model)
  # ---------------------------------------------------------------
  message("Generating predictions for every patient at every quantile...")
  
  predicted_quantiles <- matrix(0, nrow = nrow(X_full), ncol = length(quantiles)) # Pre-allocate
  for (i in seq_along(quantiles)) {
    predicted_quantiles[, i] <- predict(
      cv_models[[i]],
      X = X_full,
      type = "response" # Gives the actual predicted dollar amount
    )
  }
  
  # Clean column names: p10, p25, ..., p85
  colnames(predicted_quantiles) <- sprintf("p%02d", round(quantiles * 100))
  
  # ---------------------------------------------------------------
  # 7. Attach predictions back to the original data frame
  # ---------------------------------------------------------------
  result_df <- df %>%
    bind_cols(as.data.frame(predicted_quantiles))
  
  # ---------------------------------------------------------------
  # 8. Put the final data frame into the global environment with a nice name
  # ---------------------------------------------------------------
  output_name <- paste0(output_prefix, "_with_quantile_predictions")
  assign(output_name, result_df, envir = .GlobalEnv)
  
  # ---------------------------------------------------------------
  # Final success message
  # ---------------------------------------------------------------
  message("\n=== ALL DONE (powered by hqreg) ===")
  message("Output data frame created → ", output_name)
  message(" • Rows: ", nrow(result_df))
  message(" • New prediction columns (", ncol(predicted_quantiles), "): ",
          paste(head(colnames(predicted_quantiles), 3), collapse = ", "),
          " ... ",
          paste(tail(colnames(predicted_quantiles), 2), collapse = ", "))
  message(" • Example: p50 = predicted median spending, p90 = predicted 90th percentile, etc.\n")
  
  # ---------------------------------------------------------------
  # Return everything useful invisibly (so it doesn't print unless called)
  # ---------------------------------------------------------------
  invisible(list(
    data_with_predictions = result_df, # Full data + predictions
    coefficients = coef_mat, # Coefficient matrix (for tables/figures)
    cv_models = cv_models, # List of per-tau CV objects
    quantiles_used = quantiles
  ))
}
# Test call (on only a few quantiles)
start_time <- Sys.time()
run_quantile_lasso_predictions(
  data = "data/hiv_data.xlsx",
  quantiles = seq(0.01, 0.99, by = 0.01), # test quantiles
  formula = ~ Age_cat3 + Gender + Employment_Status + Income_Group  , 
  nfolds = 3, # Quick CV
  output_prefix = "hiv2025_fixed"
)
end_time <- Sys.time()
print(start_time)
print(end_time)

# note Insurance_Type...  at least tripled the time
# note Ethnicity... 60x the time



# Example 2: From a data frame already in memory
# hiv_new <- read_excel("data/hiv_2024.xlsx")
# run_quantile_lasso_predictions(
#   data = hiv_new,
#   quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90),  # only key quantiles
#   penalty = "ENET",
#   alpha = 0.1,                                 # mostly ridge, small LASSO
#   output_prefix = "hiv2024_enet"
# )

# After running, you will have a new object in your environment, e.g.:
# hiv2025_with_quantile_predictions
# It contains the original data + columns p05, p10, ..., p95 with predicted spending



#-----------------------------------------------------------------------------------------------------
# 4. Distribution Plot with $2K Cap Threshold and τ* Annotation (HIV - Model 1)
#-----------------------------------------------------------------------------------------------------
#Goal: Visualize the mean predicted total spending across quantiles τ = 0.01–0.99
#      to understand how spending grows across the patient cost distribution.
library(ggplot2)
library(dplyr)
library(tidyr)

# Step 1️⃣: Summarize mean predicted spending per quantile
mean_predicted_spend_hiv <- hiv_data_with_predictions %>%
  select(starts_with("percentile")) %>%        # grab all predicted percentile columns
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "quantile",
               values_to = "mean_predicted_spend") %>%
  mutate(tau = as.numeric(gsub("percentile", "", quantile)) / 100)

# Step 2️⃣: Automatically find the quantile τ* where mean predicted spend first exceeds $2,000
threshold_tau_hiv <- mean_predicted_spend_hiv %>%
  filter(mean_predicted_spend >= 2000) %>%
  slice_head(n = 1) %>%
  pull(tau)

cat("Quantile τ* where predicted mean spend exceeds $2,000 (HIV):", threshold_tau_hiv, "\n")

# Step 3️⃣: Plot predicted spending and highlight the $2K cap and τ* cutoff
ggplot(mean_predicted_spend_hiv, aes(x = tau, y = mean_predicted_spend)) +
  geom_line(size = 1.2, color = "#0072B2") +
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
    y = "Mean Predicted Total Spending (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )




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






