##################### Analysis Step 3: Collapsing vars & Quantile Regression #####################
#Analytical Plan
#Here’s a summarized breakdown of the **next steps** based on your meeting with Aaron Winn:
#####################################################################################################################################################
### **Next Steps in the Analytical Plan**
#Datset: ira_spells_agg2_top5dis
#Dataset Information (IMP): dataset contains 102,321 observations (rows)
#1. Collapse variables from table 1 to solve CONVERGENCE ISSUE within Quantile regression
#2. Remove variable: 'insurance_type' (Noticed paf_abs_2 code worked)
#3. Create multiple Quant Reg models
######################################################################################################################
names(ira_spells_agg2_top5dis)
#Assessing levels within specific vars of interest
unique(ira_spells_agg2_top5dis$Insurance_Type)
unique(ira_spells_agg2_top5dis$Age_Group)
unique(ira_spells_agg2_top5dis$disease_clean)


# Recoding variables within the dataset ira_spells_agg2_top5dis
library(dplyr)
ira_spells_agg2_top5dis <- ira_spells_agg2_top5dis %>%
  #------------------------
# 1. INSURANCE TYPE → Medicare vs. Not Medicare
#------------------------
mutate(
  Insurance_Medicare = case_when(
    Insurance_Type == "Medicare" ~ 1,
    TRUE ~ 0
  )
) %>%
  
  #------------------------
# 2. GENDER → Male vs. Not Male
#------------------------
mutate(
  Gender_Male = case_when(
    Gender == "Male" ~ 1,
    TRUE ~ 0
  )
) %>%
  
  #------------------------
# 3. EMPLOYMENT STATUS → Employed vs. Unemployed
#------------------------
mutate(
  Employment_Employed = case_when(
    Employment_Status == "Employed" ~ 1,
    TRUE ~ 0
  )
) %>%
  
  #------------------------
# 4. AGE GROUP → 3 Categories
#------------------------
mutate(
  Age_cat3 = case_when(
    Age_Group %in% c("Birth to 18", "19 to 35", "36 to 55") ~ "Age <55",
    Age_Group == "56 to 75" ~ "Age 56-75",
    Age_Group == "Over 75" ~ "Age >75",
    TRUE ~ NA_character_
  )
) %>%
  
  #------------------------
# 5. INCOME GROUP → 3 Categories
#------------------------
mutate(
  Income_cat3 = case_when(
    Income_Group %in% c("Less than $23,999", "$24,000 - $47,999") ~ "lessthan_47,999",
    Income_Group %in% c("$48,000 - $71,999") ~ "btw_48,000-71,999",
    Income_Group %in% c("$72,000 - $95,999", "$96,000 - $119,999", "$120,000 or More") ~ "greaterthan_72,000",
    TRUE ~ NA_character_
  )
) %>%
  
  #------------------------
# 6. ETHNICITY → White vs. Non-White
#------------------------
mutate(
  Ethnicity_White = case_when(
    Ethnicity == "White" ~ 1,
    TRUE ~ 0
  )
)

#Sanity check for the new predictor variables
nrow(ira_spells_agg2_top5dis) #dataset contains 102,321 observations (rows)
unique(ira_spells_agg2_top5dis$Insurance_Medicare)
table(ira_spells_agg2_top5dis$Insurance_Medicare) #comparing old table observations with new below
table(ira_spells_agg2_top5dis$Insurance_Medicare) #new table observations
table(ira_spells_agg2_top5dis$Gender_Male)
table(ira_spells_agg2_top5dis$Employment_Employed)
table(ira_spells_agg2_top5dis$Age_cat3)
table(ira_spells_agg2_top5dis$Income_cat3) #Has NA values and total obs lower than expected (102,181)
unique(ira_spells_agg2_top5dis$Income_cat3)
table(ira_spells_agg2_top5dis$Ethnicity_White)
unique(ira_spells_agg2_top5dis$disease_clean)


#Reconfiguring Income_cat3 variable
# Count the number of NA values in the 'Income_cat3' variable
sum(is.na(ira_spells_agg2_top5dis$Income_cat3)) #40 missing (NAs)
#merge NAs to this category: "btw_48,000-71,999"
# Replace NA values in Income_cat3 with "Unknown"
ira_spells_agg2_top5dis <- ira_spells_agg2_top5dis %>%
  mutate(Income_cat3 = replace(Income_cat3, is.na(Income_cat3), "btw_48,000-71,999"))
#Sanity check for NAs
sum(is.na(ira_spells_agg2_top5dis$Income_cat3)) #0 missing (NAs)
# Check the updated unique values in Income_cat3
unique(ira_spells_agg2_top5dis$Income_cat3)
table(ira_spells_agg2_top5dis$Income_cat3)


#------------------------
# Transforming into Categorical Vars & Assigning Order: Convert the new variables to factors
#------------------------
#The first level will be considered the reference category, and the rest will follow.
#Using levels: define the order and categories that a factor variable can take --> [ordinal variable]
#using labels refer to human-readable names for the levels in orderly fashion.
table(ira_spells_agg2_top5dis$disease_clean) #frequency of top-5 diseases below
#Breast Cancer HIV, AIDS and Prevention         Multiple Myeloma          Prostate Cancer       Pulmonary Fibrosis 
#5586                    58671                    15204                    17304                     5556 
#Assigning Pulmonary Fibrosis as the reference category, with increasing order of frequencies.

ira_spells_agg2_top5dis <- ira_spells_agg2_top5dis %>%
  mutate(
    Age_cat3 = factor(Age_cat3, levels = c("Age <55", "Age 56-75", "Age >75")),
    #"Age <55" is the reference category, "Age 56-75" comes second, and "Age >75" is the last one. This order will affect how the model interprets these levels.
    
    Income_cat3 = factor(Income_cat3, levels = c("lessthan_47,999", "btw_48,000-71,999", "greaterthan_72,000")),
    #"lessthan_47,999" being the first (ref cat), and "greaterthan_72,000" being the last.
    
    Insurance_Medicare = factor(Insurance_Medicare, labels = c("Not Medicare", "Medicare")),
    #Here, 0 gets the label "Not Medicare", and 1 gets "Medicare". The order of 0 and 1 is important.
    #When using labels = c("Not Medicare", "Medicare"), the first label corresponds to the first level (0), and the second label corresponds to the second level (1).
    
    Gender_Male = factor(Gender_Male, labels = c("Female", "Male")),
    #labels "Female" to 0 and "Male" to 1.
    
    Employment_Employed = factor(Employment_Employed, labels = c("Unemployed", "Employed")),
    #"Unemployed" is assigned to 0, and "Employed" is assigned to 1.
    
    Ethnicity_White = factor(Ethnicity_White, labels = c("Non-White", "White")),
    #"Non-White" corresponds to 0 and "White" corresponds to 1.
    
    Disease_Top5 = factor(disease_clean, 
                          levels = c("Pulmonary Fibrosis", 
                                     "Breast Cancer", 
                                     "Multiple Myeloma",
                                     "Prostate Cancer",
                                     "HIV, AIDS and Prevention"
                          ))
    
  )

#------------------------
# Sanity check: Check new variable summaries
#------------------------
summary(select(ira_spells_agg2_top5dis,
               Insurance_Medicare,
               Gender_Male,
               Employment_Employed,
               Age_cat3,
               Income_cat3,
               Ethnicity_White,
               Disease_Top5))
#Frequencies are consistent with expected ones
#Good to proceed with Quantile Regression

######################################################################################################################################
#### QUANTILE REGRESSION PROCESS ###
#disease_focus: Top-5 diseases of interest: Breast Cancer/HIV, AIDS, and Prevention/Multiple Myeloma/Prostate Cancer/Pulmonary Fibrosis
#dataset: ira_spells_agg2_top5dis
#outcome var: total_spending
#Predictor vars: Age_cat3 + Gender_Male + Ethnicity_White + Employment_Employed + Income_cat3 + Disease_Top5
######################################################################################################################################
#Start with median (tau=0.5)
library(quantreg)
#Attempt # 1
qr_1 <- lapply(quantiles, function(tau) {
  rq(total_spending ~ Age_cat3 + Gender_Male + Ethnicity_White 
     + Employment_Employed + Income_cat3 + Disease_Top5,
     data = ira_spells_agg2_top5dis, tau = 0.5)
})
# Check the results
summary(qr_1)
#Worked yet with warnings

#Attempt # 2
#------------------------
# Quantile Regression Model # 1 (Tau=0.5)
#------------------------
#Trying alternative methods: This method works! Try to replicate it!
qr_q50 <- rq(total_spending ~ Age_cat3 + Gender_Male + Ethnicity_White 
             + Employment_Employed + Income_cat3 + Disease_Top5,
             data = ira_spells_agg2_top5dis, tau = 0.5)
# Check the results
summary(qr_q50)
#Making Exportable Tables
# Load necessary library
library(broom)
library(DT)
# Summarize the quantile regression model (tau = 0.5)
qr_q50table1<- tidy(qr_q50, conf.int = TRUE, conf.level = 0.95)
# Round the numeric columns to 2 digits
qr_q50table1 <- qr_q50table1 %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
# View the summary of regression results
print(qr_q50table1)
# create an interactive HTML table in the Viewer
datatable(
  qr_q50table1,
  caption = "Quantile Regression Summary (tau = 0.5) for Total Spending",
  options = list(
    pageLength = 20,   # Display 10 rows per page
    scrollX = TRUE,    # Enable horizontal scrolling if the table is wide
    autoWidth = TRUE    # Adjust column width automatically
  ),
  rownames = FALSE
)
################################################################################

#------------------------
# Quantile Regression Model # 2 (Tau=0.5)
#Approach: Reducing Predictors (remove Employment_Employed  - may be related to income - suggesting collinearity)
#predictors: Age_cat3 + Gender_Male + Ethnicity_White + Income_cat3 + Disease_Top5
#------------------------
qr_q50_model1.2<- rq(total_spending ~ Age_cat3 + Gender_Male + 
                       Ethnicity_White +  Income_cat3 + Disease_Top5,
                     data = ira_spells_agg2_top5dis, tau = 0.5)
# Check the results
summary(qr_q50_model1.2) 
#Interpretation of model1.2: change in the intercept by 2 points (minimal impact)
#...not significant change in estimates of the predictor vars.

#------------------------
# Quantile Regression Model # 3 (Tau=0.1)
#Approach: Same as Model 1.2 (previous model) but changing the tau to 0.1 from 0.5
#------------------------
qr_q50_model1.3<- rq(total_spending ~ Age_cat3 + Gender_Male + 
                       Ethnicity_White +  Income_cat3 + Disease_Top5,
                     data = ira_spells_agg2_top5dis, tau = 0.1)
# Check the results
summary(qr_q50_model1.3) 

#------------------------
# Quantile Regression Model # 4 (Tau=0.5)
#Approach: Same as Model 1.2 (same median tau, removing income_cat3 var)
#------------------------
qr_q50_model1.4<- rq(total_spending ~ Age_cat3 + Gender_Male + 
                       Ethnicity_White +  Disease_Top5,
                     data = ira_spells_agg2_top5dis, tau = 0.5)
# Check the results
summary(qr_q50_model1.4) 

#------------------------
# MULTI-COLLINEARITY ASSESSMENT using VIF
#Approach: The Variance Inflation Factor (VIF) is used to assess multicollinearity in regression models. 
#...VIF measures how much the variance of the estimated regression coefficients is inflated due to correlation with other predictors.
#------------------------
install.packages("car")
library(car)
# Example: Fit a linear regression model
lm_model_viftest1<- lm(total_spending ~ Age_cat3 + Gender_Male + Ethnicity_White + 
                         Disease_Top5, data = ira_spells_agg2_top5dis)
# Check VIF for the model
vif(lm_model_viftest1)
#Interpretation: VIF values 1.02 to 1.13;
#Since VIF > 1: There is some correlation, but it's not a cause for concern yet.
#Additional context: ...VIF > 5-10: Indicates moderate to high multicollinearity. A VIF greater than 10 is often considered problematic, meaning that there is substantial collinearity between the predictors.

#Next step: Since there are no problematic VIF values (i.e., no values above 10), proceed with quantile regression models without further adjustment for multicollinearity.
#...If seeing issues with model convergence or non-uniqueness, the cause is likely something other than multicollinearity.


#------------------------
# MODEL PERFORMANCE Using AIC BIC
#Approach: Compare the AIC and BIC values for models with tau value of 0.5; using models 1,2,4
#attempt failed: tried comparing model performance using AIC. BIC measures: didnt work --> next step to plot residuals 
#------------------------
library(quantreg)
quantreg::AIC(qr_q50, qr_q50_model1.2, qr_q50_model1.4)
AIC(qr_q50, qr_q50_model1.2, qr_q50_model1.4)
BIC(qr_q50, qr_q50_model1.2, qr_q50_model1.4)


#------------------------
# RESIDUAL ANALYSIS
#Plot Residuals vs. Fitted Values
#helps check if there are any patterns or issues like heteroscedasticity
#------------------------
# Plot residuals vs. fitted values for quantile regression model
residuals_qr <- residuals(qr_q50_model1.4)
fitted_qr <- fitted(qr_q50_model1.4)

plot(fitted_qr, residuals_qr,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")  # Add horizontal line at zero

#Plotting Histogram
hist(residuals_qr, main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightblue", breaks = 30)
#For quantile regression, residuals should ideally be normally distributed (depending on the quantile).


#--------------------------------------------------------------------------------------------------------#
##------------------------
# Running Multiple Quantile Regression Models (Use Model 1.4 predictors)
#Approach: Running quantile regression for multiple quantiles (Q1 to Q99)
#------------------------
quantiles_mod1.4<- seq(0.01, 0.99, by = 0.01)
qr_results<- lapply(quantiles, function(tau) {
  rq(total_spending ~ Age_cat3 + Gender_Male + 
       Ethnicity_White +  Disease_Top5,
     data = ira_spells_agg2_top5dis, tau = tau)
})
# Check the results
summary(qr_results)
#Tidying tables
library(broom)
library(dplyr)
qr_tables <- lapply(qr_results, function(model) tidy(model, conf.int = TRUE))
# Add quantile index to each
qr_tables <- Map(function(tbl, q) { tbl$quantile <- q; tbl }, qr_tables, 1:length(qr_results))
# Combine into one data frame
qr_combined <- bind_rows(qr_tables)

#-----------------------------------------------------------------------------------------------------
#Running Same Code above : Attempt 2

#--------------------------------------------------------------------------------------------------------#
# Quantile Regression: Full Range (τ = 0.01 → 0.99) using Model 1.4 Predictors
#--------------------------------------------------------------------------------------------------------#

# Load required libraries
library(quantreg)
library(broom)
library(dplyr)
library(DT)

# Define quantiles (1% to 99%)
quantiles_mod1.4 <- seq(0.01, 0.99, by = 0.01)

# Run quantile regression models for each τ
qr_results <- lapply(quantiles_mod1.4, function(tau) {
  rq(total_spending ~ Age_cat3 + Gender_Male + 
       Ethnicity_White + Disease_Top5,
     data = ira_spells_agg2_top5dis, tau = tau)
})

# Tidy model results with confidence intervals
qr_tables <- lapply(seq_along(qr_results), function(i) {
  broom::tidy(qr_results[[i]], conf.int = TRUE) %>%
    mutate(tau = quantiles_mod1.4[i])
})

# Combine all results into one dataframe
qr_combined <- bind_rows(qr_tables)

# Round numeric columns for cleaner viewing
qr_combined <- qr_combined %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# View interactive HTML table (can export directly from Viewer)
DT::datatable(
  qr_combined,
  caption = "Quantile Regression Results across τ = 0.01 to 0.99 (Model 1.4 Predictors)",
  options = list(
    pageLength = 50,    # display 50 rows per page (adjustable)
    scrollX = TRUE,     # enable horizontal scroll for wide tables
    autoWidth = TRUE,   # adjust column widths automatically
    dom = 'Bftip'       # include export buttons (Copy, CSV, Excel)
  ),
  rownames = FALSE
)

# Optional: Export results to Excel
# writexl::write_xlsx(qr_combined, "Quantile_Regression_Results_Model1.4.xlsx")

#### Dated: 10-22-2025
######################################################################################################################################
#### QUANTILE REGRESSION within 5 different datasets ####
######################################################################################################################################
#Approach (post-Aaron discussion):
#1. Create 5 different datasets: stratify on basis of disease_clean (top-5 disease conditions)
#2. Use one predictor, Age_cat3 to build a model for extracting 2 coefficients: B0 and B1.
#3. Extract the coefficients (intercepts and the coefficient for Age_cat3) for each quantile and create corresponding columns in the respective dataset for each quantile (τ from 0.01 to 0.99).
#3.... loop through each quantile (τ = 0.01 to 0.99), run quantile regression for each quantile, and extract the coefficients (B0 for the intercept and B1 for Age_cat3) for each quantile.
# Load necessary libraries
library(quantreg)    # For quantile regression
library(broom)       # For tidying up regression output
library(dplyr)       # For data manipulation
library(DT)          # For interactive tables

# Define the list of the top-5 diseases of interest
disease_focus <- c("HIV, AIDS and Prevention", 
                   "Multiple Myeloma", 
                   "Prostate Cancer", 
                   "Pulmonary Fibrosis", 
                   "Breast Cancer")

# Subset the dataset for each disease category
hiv_data <- ira_spells_agg2_top5dis %>%
  filter(Disease_Top5 == "HIV, AIDS and Prevention")

mm_data <- ira_spells_agg2_top5dis %>%
  filter(Disease_Top5 == "Multiple Myeloma")

capros_data <- ira_spells_agg2_top5dis %>%
  filter(Disease_Top5 == "Prostate Cancer")

pulfib_data <- ira_spells_agg2_top5dis %>%
  filter(Disease_Top5 == "Pulmonary Fibrosis")

cabr_data <- ira_spells_agg2_top5dis %>%
  filter(Disease_Top5 == "Breast Cancer")

# Check how many records in each dataset
nrow(hiv_data)                # HIV dataset #58671
nrow(mm_data)                 # Multiple Myeloma dataset #15204
nrow(capros_data)           # Prostate Cancer dataset #17304
nrow(pulfib_data) # Pulmonary Fibrosis dataset #5556
nrow(cabr_data)      # Breast Cancer dataset #5586
#Observations were correct per my table from before

unique(ira_spells_agg2_top5dis$disease_clean)
#5 unique Datasets
#1. hiv_data: Contains only HIV, AIDS, and Prevention patients (58,671 observations).
#2. mm_data: Contains only Multiple Myeloma patients (15,204 observations).
#3. capros_data: Contains only Prostate Cancer patients (17,304 observations).
#4. pulfib_data: Contains only Pulmonary Fibrosis patients (5,556 observations).
#5. cabr_data: Contains only Breast Cancer patients (5,586 observations).

#-----------------------------------------------------------------------------------------------------
#1. Running Quantile Regression for Each Disease Group
#-----------------------------------------------------------------------------------------------------
library(quantreg)
library(dplyr)

# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

# Loop through each dataset and run quantile regression for each quantile
qr_results_hiv <- lapply(quantiles, function(tau) {
  rq(total_spending ~ Age_cat3, data = hiv_data, tau = tau)
})

qr_results_mm <- lapply(quantiles, function(tau) {
  rq(total_spending ~ Age_cat3, data = mm_data, tau = tau)
})

qr_results_capros <- lapply(quantiles, function(tau) {
  rq(total_spending ~ Age_cat3, data = capros_data, tau = tau)
})

qr_results_pulfib <- lapply(quantiles, function(tau) {
  rq(total_spending ~ Age_cat3, data = pulfib_data, tau = tau)
})

qr_results_cabr <- lapply(quantiles, function(tau) {
  rq(total_spending ~ Age_cat3, data = cabr_data, tau = tau)
})

#-----------------------------------------------------------------------------------------------------
#2. Re-Run Quantile Regression for Multiple Quantiles: Start with HIV dataset
#run quantile regression for each quantile (τ from 0.01 to 0.99) and extract the coefficients.
#-----------------------------------------------------------------------------------------------------
library(quantreg)
library(dplyr)

# Define the quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

# Run quantile regression for each quantile
qr_results_hiv <- lapply(quantiles, function(tau) {
  qr_model <- rq(total_spending ~ Age_cat3, data = hiv_data, tau = tau)  # Quantile regression for each tau
  return(coef(qr_model))  # Extract coefficients
})

#-----------------------------------------------------------------------------------------------------
#3. Extract Coefficients and Add to HIV Dataset: MODEL 1
#-----------------------------------------------------------------------------------------------------
#Approach: Extract the intercept (B0) and the coefficient for Age_cat3 (B1) for each quantile and store them in the hiv_data dataset. 
#...create two columns for each quantile: one for the intercept and one for the Age_cat3 coefficient. Variables to be added in the model incrementally:
#1. "Age_cat3
#2. Gender_Male",  
#3. "Ethnicity_White"
#4. "Income_cat3",

# Load necessary libraries
library(quantreg)
library(dplyr)

# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

# Run quantile regression for each quantile (τ = 0.01 to τ = 0.99)
qr_results_hiv <- lapply(quantiles, function(tau) {
  qr_model <- rq(total_spending ~ Age_cat3, data = hiv_data, tau = tau)  # Quantile regression for each tau
  return(coef(qr_model))  # Extract coefficients (Intercept and Age_cat3)
})

# Convert the list of coefficients into a data frame
coefficients_df <- do.call(rbind, qr_results_hiv)

# Convert to a data frame and add tau values (quantiles)
coefficients_df <- as.data.frame(coefficients_df)
coefficients_df$tau <- quantiles  # Add tau value for each row

# Rename the columns for clarity
colnames(coefficients_df) <- c("Intercept", "Age_cat3", "tau")

# Add the coefficients as new columns in the hiv_data dataset
for (i in 1:length(quantiles)) {
  tau <- quantiles[i]
  
  # Create column names for Intercept and Age_cat3 coefficients at each quantile
  intercept_col <- paste("Intercept_tau_", tau, sep = "")
  age_cat3_col <- paste("Age_cat3_tau_", tau, sep = "")
  
  # Add the coefficients to hiv_data
  hiv_data[[intercept_col]] <- coefficients_df[i, "Intercept"]
  hiv_data[[age_cat3_col]] <- coefficients_df[i, "Age_cat3"]
}

# Check if the new columns are added successfully
head(hiv_data)
colnames(hiv_data)

#Interpretation: [columns 39-236] Successfully created columns with Intercept and Age-Intercept from 0-99 quantiles. 
#At lower quantiles (e.g., τ = 0.1), the coefficient represents the effect of Age_cat3 on low spenders (those who spend less).
#At higher quantiles (e.g., τ = 0.9), the coefficient represents the effect of Age_cat3 on high spenders (those who spend more).
#Next steps:
#(a) Visualize the distribution
#(b) Add parameters in the model
#(c) Repeat the process for the other 4 disease areas


#-----------------------------------------------------------------------------------------------------
#4. Visualizing the Effect of Age_cat3 Across Quantiles:
#-----------------------------------------------------------------------------------------------------
#Approach: visualize how the coefficient for Age_cat3 changes across different quantiles (τ = 0.01 to τ = 0.99). 
#...plot the coefficients for Age_cat3_tau_0.01, ..., Age_cat3_tau_0.99 to see if the effect of age varies at different spending levels.
#a. Extract the coefficients.
#b. Combine them with the quantile (tau) information.
#c. Reshape the data into long format.
#d. Plot the coefficients of Age_cat3 across different quantiles.
library(ggplot2)


#-----------------------------------------------------------------------------------------------------
#5. MODEL 2: Adding "Gender_Male" predictor to the first model:
#-----------------------------------------------------------------------------------------------------
#Other variables to be incrementally added in subsequent models: 
#2. Gender_Male",  
#3. "Ethnicity_White"
#4. "Income_cat3",

# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

# Run quantile regression for each quantile (τ = 0.01 to τ = 0.99) with Age_cat3 and Gender_Male
qr_results_hiv_mod2 <- lapply(quantiles, function(tau) {
  qr_model <- rq(total_spending ~ Age_cat3 + Gender_Male, data = hiv_data, tau = tau)  # Added Gender_Male as predictor
  return(coef(qr_model))  # Extract coefficients (Intercept, Age_cat3, Gender_Male)
})

# Convert the list of coefficients into a data frame for Model 2
coefficients_df_mod2 <- do.call(rbind, qr_results_hiv_mod2)

# Convert the coefficients to a data frame and add tau values (quantiles)
coefficients_df_mod2 <- as.data.frame(coefficients_df_mod2)
coefficients_df_mod2$tau <- quantiles  # Add tau value for each row

# Rename the columns for clarity with the "mod2" convention
colnames(coefficients_df_mod2) <- c("Intercept_mod2", "Age_cat3_mod2", "Gender_Male_mod2", "tau")

# View the first few rows to verify the correct structure
head(coefficients_df_mod2)

# Add the coefficients as new columns in the hiv_data dataset for each quantile (mod2 for Model 2)
for (i in 1:length(quantiles)) {
  tau <- quantiles[i]
  
  # Create column names for Intercept, Age_cat3, and Gender_Male coefficients at each quantile
  intercept_col <- paste("Intercept_mod2_tau_", tau, sep = "")  # Updated to model 2 with Gender_Male predictor
  age_cat3_col <- paste("Age_cat3_mod2_tau_", tau, sep = "")    # Updated to model 2 with Gender_Male predictor
  gender_male_col <- paste("Gender_Male_mod2_tau_", tau, sep = "")  # Updated to model 2 with Gender_Male predictor
  
  # Add the coefficients to hiv_data
  hiv_data[[intercept_col]] <- coefficients_df_mod2[i, "Intercept_mod2"]
  hiv_data[[age_cat3_col]] <- coefficients_df_mod2[i, "Age_cat3_mod2"]
  hiv_data[[gender_male_col]] <- coefficients_df_mod2[i, "Gender_Male_mod2"]
}

# Check if the new columns are added successfully
head(hiv_data)
colnames(hiv_data)  # To ensure new variables were added correctly



#-----------------------------------------------------------------------------------------------------
#6. MODEL 3: Adding "Ethnicity_White" predictor to model 2.
#-----------------------------------------------------------------------------------------------------
#Other variables to be incrementally added in subsequent models: 
#3. "Ethnicity_White"
#4. "Income_cat3",

# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

# Run quantile regression for each quantile (τ = 0.01 to τ = 0.99) with Age_cat3, Gender_Male, and Ethnicity_White
qr_results_hiv_mod3 <- lapply(quantiles, function(tau) {
  qr_model <- rq(total_spending ~ Age_cat3 + Gender_Male + Ethnicity_White, data = hiv_data, tau = tau)  # Added Ethnicity_White as predictor
  return(coef(qr_model))  # Extract coefficients (Intercept, Age_cat3, Gender_Male, Ethnicity_White)
})

# Convert the list of coefficients into a data frame for Model 3
coefficients_df_mod3 <- do.call(rbind, qr_results_hiv_mod3)

# Convert the coefficients to a data frame and add tau values (quantiles)
coefficients_df_mod3 <- as.data.frame(coefficients_df_mod3)
coefficients_df_mod3$tau <- quantiles  # Add tau value for each row

# Rename the columns for clarity with the "mod3" convention
colnames(coefficients_df_mod3) <- c("Intercept_mod3", "Age_cat3_mod3", "Gender_Male_mod3", "Ethnicity_White_mod3", "tau")

# View the first few rows to verify the correct structure
head(coefficients_df_mod3)

# Add the coefficients as new columns in the hiv_data dataset for each quantile (mod3 for Model 3)
for (i in 1:length(quantiles)) {
  tau <- quantiles[i]
  
  # Create column names for Intercept, Age_cat3, Gender_Male, and Ethnicity_White coefficients at each quantile
  intercept_col <- paste("Intercept_mod3_tau_", tau, sep = "")  # Updated to model 3 with Ethnicity_White predictor
  age_cat3_col <- paste("Age_cat3_mod3_tau_", tau, sep = "")    # Updated to model 3 with Ethnicity_White predictor
  gender_male_col <- paste("Gender_Male_mod3_tau_", tau, sep = "")  # Updated to model 3 with Ethnicity_White predictor
  ethnicity_white_col <- paste("Ethnicity_White_mod3_tau_", tau, sep = "")  # Updated to model 3 with Ethnicity_White predictor
  
  # Add the coefficients to hiv_data
  hiv_data[[intercept_col]] <- coefficients_df_mod3[i, "Intercept_mod3"]
  hiv_data[[age_cat3_col]] <- coefficients_df_mod3[i, "Age_cat3_mod3"]
  hiv_data[[gender_male_col]] <- coefficients_df_mod3[i, "Gender_Male_mod3"]
  hiv_data[[ethnicity_white_col]] <- coefficients_df_mod3[i, "Ethnicity_White_mod3"]
}

# Check if the new columns are added successfully
head(hiv_data)
colnames(hiv_data)  # To ensure new variables were added correctly


#-----------------------------------------------------------------------------------------------------
#6. MODEL 4: Adding "Ethnicity_White" predictor to model 3.
#-----------------------------------------------------------------------------------------------------
#Other variables to be incrementally added in subsequent models: 
#4. "Income_cat3",

#-----------------------------------------------------------------------------------------------------
#5. MODEL 4: Adding "Income_cat3" predictor to Model 3
#-----------------------------------------------------------------------------------------------------
# The model now includes: Age_cat3, Gender_Male, Ethnicity_White, and Income_cat3 as predictors.
# Let's run the quantile regression for each quantile (τ = 0.01 to τ = 0.99)

# Define quantiles (Q1 to Q99)
quantiles <- seq(0.01, 0.99, by = 0.01)

# Run quantile regression for each quantile (τ = 0.01 to τ = 0.99) with Age_cat3, Gender_Male, Ethnicity_White, and Income_cat3
qr_results_hiv_mod4 <- lapply(quantiles, function(tau) {
  qr_model <- rq(total_spending ~ Age_cat3 + Gender_Male + Ethnicity_White + Income_cat3, data = hiv_data, tau = tau)  # Added Income_cat3 as predictor
  return(coef(qr_model))  # Extract coefficients (Intercept, Age_cat3, Gender_Male, Ethnicity_White, Income_cat3)
})

# Convert the list of coefficients into a data frame for Model 4
coefficients_df_mod4 <- do.call(rbind, qr_results_hiv_mod4)

# Convert the coefficients to a data frame and add tau values (quantiles)
coefficients_df_mod4 <- as.data.frame(coefficients_df_mod4)
coefficients_df_mod4$tau <- quantiles  # Add tau value for each row

# Rename the columns for clarity with the "mod4" convention
colnames(coefficients_df_mod4) <- c("Intercept_mod4", "Age_cat3_mod4", "Gender_Male_mod4", "Ethnicity_White_mod4", "Income_cat3_mod4", "tau")

# View the first few rows to verify the correct structure
head(coefficients_df_mod4)

# Add the coefficients as new columns in the hiv_data dataset for each quantile (mod4 for Model 4)
for (i in 1:length(quantiles)) {
  tau <- quantiles[i]
  
  # Create column names for Intercept, Age_cat3, Gender_Male, Ethnicity_White, and Income_cat3 coefficients at each quantile
  intercept_col <- paste("Intercept_mod4_tau_", tau, sep = "")  # Updated to model 4 with Income_cat3 predictor
  age_cat3_col <- paste("Age_cat3_mod4_tau_", tau, sep = "")    # Updated to model 4 with Income_cat3 predictor
  gender_male_col <- paste("Gender_Male_mod4_tau_", tau, sep = "")  # Updated to model 4 with Income_cat3 predictor
  ethnicity_white_col <- paste("Ethnicity_White_mod4_tau_", tau, sep = "")  # Updated to model 4 with Income_cat3 predictor
  income_cat3_col <- paste("Income_cat3_mod4_tau_", tau, sep = "")  # Updated to model 4 with Income_cat3 predictor
  
  # Add the coefficients to hiv_data
  hiv_data[[intercept_col]] <- coefficients_df_mod4[i, "Intercept_mod4"]
  hiv_data[[age_cat3_col]] <- coefficients_df_mod4[i, "Age_cat3_mod4"]
  hiv_data[[gender_male_col]] <- coefficients_df_mod4[i, "Gender_Male_mod4"]
  hiv_data[[ethnicity_white_col]] <- coefficients_df_mod4[i, "Ethnicity_White_mod4"]
  hiv_data[[income_cat3_col]] <- coefficients_df_mod4[i, "Income_cat3_mod4"]
}
# Check if the new columns are added successfully
head(hiv_data)
colnames(hiv_data)  # To ensure new variables were added correctly

#-----------------------------------------------------------------------------------------------------
#8. Model Comparison & Visualization
#-----------------------------------------------------------------------------------------------------
#Approach: Compare the Coefficients Across Models
#1. Extract coefficients from each model (Model 1, Model 2, Model 3, Model 4)
#2. Extract Coefficients for Model Comparison
#3. Visualization of Coefficients Across Models
#4. 

# Model 1 (just Age_cat3)
#Renaming model 1 data frame as below for consistency
coefficients_df_mod1 <- coefficients_df
# Rename columns for clarity (mod1)
colnames(coefficients_df) <- c("Intercept_mod1", "Age_cat3_mod1", "tau")
#Further renaming Model 1 for consistency
coefficients_mod1 <- coefficients_df[, c("tau", "Intercept_mod1", "Age_cat3_mod1")]

# Model 2 (Age_cat3 + Gender_Male)
coefficients_mod2 <- coefficients_df_mod2[, c("tau", "Intercept_mod2", "Age_cat3_mod2", "Gender_Male_mod2")]

# Model 3 (Age_cat3 + Gender_Male + Ethnicity_White)
coefficients_mod3 <- coefficients_df_mod3[, c("tau", "Intercept_mod3", "Age_cat3_mod3", "Gender_Male_mod3", "Ethnicity_White_mod3")]

# Model 4 (Age_cat3 + Gender_Male + Ethnicity_White + Income_cat3)
coefficients_mod4 <- coefficients_df_mod4[, c("tau", "Intercept_mod4", "Age_cat3_mod4", "Gender_Male_mod4", "Ethnicity_White_mod4", "Income_cat3_mod4")]

# Ensure consistency across all models
# All models should have the same number of rows corresponding to the quantiles (tau), i.e., 99 rows for Q1 to Q99
# Check if all models have the same number of rows (quantiles)
nrow(coefficients_mod1)  # Should be 99 (for tau from 0.01 to 0.99)
nrow(coefficients_mod2)  # Should be 99
nrow(coefficients_mod3)  # Should be 99
nrow(coefficients_mod4)  # Should be 99
# Check coefficients for each model
head(coefficients_mod1)  # Ensure coefficients for Model 1
head(coefficients_mod2)  # Ensure coefficients for Model 2
head(coefficients_mod3)  # Ensure coefficients for Model 3
head(coefficients_mod4)  # Ensure coefficients for Model 4

# Combine all the coefficients into one data frame for comparison
coefficients_comb_mod_1_4 <- bind_rows(
  mutate(coefficients_mod1, model = "Model 1"),
  mutate(coefficients_mod2, model = "Model 2"),
  mutate(coefficients_mod3, model = "Model 3"),
  mutate(coefficients_mod4, model = "Model 4")
)

# View the first few rows of the combined coefficients table
head(coefficients_comb_mod_1_4)

# Check for NA values in the combined data frame
sum(is.na(coefficients_comb_mod_1_4))  # Check for NA values

####################################################################################
#dataset created named: "coefficients_comb_mod_1_4"
#This dataset has 
#Data Visual for hiv_data
#####################################################################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

###### MODEL 1 ######
#Appraoch: Test for model 1 to see if it works --> Update the Excel sheet
# Create a plot of Age_cat3 coefficients across quantiles (for Model 1 to Model 4)
ggplot(coefficients_comb_mod_1_4, aes(x = tau, y = Age_cat3_mod1, color = model)) +
  geom_line(size = 1) +   # Line plot
  geom_point(size = 2) +  # Points on the line to indicate the actual coefficients
  labs(title = "Effect of Age_cat3 (model 1) on Total Spending Across Quantiles (τ = 0.01 to 0.99)",
       x = "Quantile (τ)",
       y = "Coefficient for Age_cat3") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +  # x-axis with quantile labels (τ)
  theme_minimal() +   # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


###### Downloading Dataset #####
getwd()

# Install and load the writexl package if you don't have it installed
# install.packages("writexl")
library(writexl)

# Export the dataset to an Excel file
write_xlsx(coefficients_comb_mod_1_4, "C:/Users/awars/OneDrive - University of Illinois Chicago/PSOP Sem 1/Aaron Winn Project/IRA Expenses Data/Shared Folder with Aaron/coefficients_comb_mod_1_4.xlsx")



#-----------------------------------------------------------------------------------------------------
##Re-organizing the dataset for visualization, 'coefficients_comb_mod_1_4'
#-----------------------------------------------------------------------------------------------------
#Approach: I built a clean Excel file (Manually) with columns and imported it using escel reader
#model | tau | Intercept | Age | Gender | Ethnicity | Income,
#I can directly visualize how the Age coefficient changes across quantiles and models using ggplot2.
#
library(dplyr)
library(tidyr)
#upload this visual data code
# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# 1️⃣ Read your Excel file
library(readxl)
combined_quantregs_10_24_25_ <- read_excel("C:/Users/awars/OneDrive - University of Illinois Chicago/PSOP Sem 1/Aaron Winn Project/IRA Expenses Data/Shared Folder with Aaron/hiv_dataset/combined_quantregs_(10-24-25).xlsx")
View(combined_quantregs_10_24_25_)
coefficients_data_hiv <- combined_quantregs_10_24_25_

# 2️⃣ Ensure correct data types
coefficients_data_hiv <- coefficients_data_hiv %>%
  mutate(
    tau = as.numeric(tau),     # make sure tau is numeric for plotting
    model = as.factor(model)   # treat model as categorical
  )

# 3️⃣ Plot: Effect of Age across quantiles (τ = 0.01 to 0.99)
ggplot(coefficients_data_hiv, aes(x = tau, y = Age, color = model, group = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Effect of Age on Total Spending Across Quantiles (τ = 0.01–0.99)",
    x = "Quantile (τ)",
    y = "Coefficient for Age",
    color = "Model"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


