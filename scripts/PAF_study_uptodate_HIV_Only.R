### aaron's edits 
# starting with the HIV data 

library(readxl)
library(quantreg)
library(dplyr)
library(fastDummies)



hiv_data_coef <- read_excel("data/hiv_data.xlsx")

names(hiv_data_coef)

# let's drop the coefficients from this dataset 
hiv_data <- hiv_data_coef[,1:38] 
# create dummies from the categorical data (will make the predictions easier)
dummy_mat <- model.matrix(~ Age_cat3 - 1, data = hiv_data)
# make column names syntactically valid (spaces -> ., '>' -> '.')
# this makes life easier when you get the coefficient names 
colnames(dummy_mat) <- make.names(colnames(dummy_mat))
# put it together 
hiv_data <- cbind(hiv_data, as.data.frame(dummy_mat))
# add in an intercept term for future matrix multiplication 
hiv_data$'(Intercept)' <- 1 
names(hiv_data)
hiv_data$Age_cat3 <- NULL
#-----------------------------------------------------------------------------------------------------
#1. Running Quantile Regression for Each Disease Group
#-----------------------------------------------------------------------------------------------------

# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

table(hiv_data$Age_cat3Age..55)

# run model for each quantile 
quantile_regression <- rq(total_spending ~ Age_cat3Age.56.75 + Age_cat3Age..55 , data = hiv_data, tau = quantiles)

# Coefficients for a multiple quantile model
coef_multiple <- coef(quantile_regression)
# now we have our coefficents as rows and the columns as equations
print(coef_multiple)

colset_union <- rownames(coef_multiple)
#> colset_union
#[1] "(Intercept)"       "Age_cat3Age >75"  
#[3] "Age_cat3Age 56-75"


# now we need to make the data frame and coefficients 
# have dimensions correct (same number of rows in the coef as 
# there are columns in the patient data)


# keep only those that exist in df and preserve df's column order
colset <- intersect( colset_union, names(hiv_data))

n_patients <- length(unique(hiv_data$patient_id))
patient_ids <- sort(unique(hiv_data$patient_id))

# create a matrix of patient characteristics
patient_matrix <- matrix(0, nrow = n_patients, 
                            ncol =  length(colset) , 
                         dimnames = list(
                           id = as.character(patient_ids),
                           variable = colset
                         )
                         )

# now we need to fill the matrix 
# sanity check: columns must exist
stopifnot(all(colset %in% names(hiv_data)))

# helper to coerce to numeric safely
coerce_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x))  return(as.numeric(as.character(x)))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  return(as.numeric(x))
}

# build a per-patient row with exactly the columns in `colset`
df_pat <- hiv_data %>%
  select(patient_id, all_of(colset)) %>%
  group_by(patient_id) %>%
  slice(1) %>%                        # if multiple rows per patient, take the first (adjust if needed)
  ungroup() %>%
  mutate(across(all_of(colset), coerce_num))

# align to the order in `patient_ids`
df_pat <- df_pat[match(patient_ids, df_pat$patient_id), ]

# safety check: all patient_ids found
if (any(is.na(df_pat$patient_id))) {
  missing_ids <- patient_ids[is.na(df_pat$patient_id)]
  stop(sprintf("Missing %d patient_id(s) in hiv_data, e.g., %s",
               length(missing_ids), paste(head(missing_ids, 5), collapse = ", ")))
}

# fill in the created patient_matrix with correct dims:
patient_matrix[,] <- as.matrix(df_pat[, colset, drop = FALSE])
rownames(patient_matrix) <- as.character(df_pat$patient_id)
colnames(patient_matrix) <- colset

dim(patient_matrix)
dim(coef_multiple)


# the magic of matrix multiplication!
percentiles <- patient_matrix[]%*%coef_multiple[,]

# renames the columns
colnames(percentiles) <- sprintf("percentile%02d", seq_len(ncol(percentiles)))

# Turn into a data.frame keyed by patient_id
pred_df <- data.frame(
  patient_id = rownames(patient_matrix),
  percentiles,
  check.names = FALSE
)

# --- WIDE merge: add predictions as new columns to hiv_data ---
hiv_data_with_predictions <- dplyr::left_join(hiv_data, pred_df, by = "patient_id")

