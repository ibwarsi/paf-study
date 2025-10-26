### aaron's edits 
#### Dated: 10-22-2025
######################################################################################################################################
#### QUANTILE REGRESSION & MATRIX CREATION (hiv_data) ####
######################################################################################################################################
#Objective:
#This script focuses on creating matrices like Aaron suggested
#Using those to run regression modeling staring with base model: with just age & increementally adding predictors (models 1-4)
#Repeat the same experiment for other 4 datasets
#Predict spend & build visualization plots

# starting with the HIV data 
install.packages("fastDummies")
install.packages("SparseM")
library(readxl)
library(SparseM)
library(quantreg)
library(dplyr)
library(fastDummies)

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
dim(coef_multiple)
#> dim(patient_matrix)
#[1] 48393     3
# dim(coef_multiple)
#[1]  3 99

#-----------------------------------------------------------------------------------------------------
# 3. Matrix Multiplication - Generating Percentile Predictions
#-----------------------------------------------------------------------------------------------------
# Aaron: "the magic of matrix multiplication!"
## Multiply patient predictors by quantile regression coefficients
percentiles <- patient_matrix[]%*%coef_multiple[,]
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




