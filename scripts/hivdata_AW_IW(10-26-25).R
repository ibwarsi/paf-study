### aaron's pre-poster proposed edits 
#### Dated: 10-26-2025
######################################################################################################################################
#### PREDICT SPEND TABLE (start with... hiv_data) ####
######################################################################################################################################
#Problem:
# Revision of what was earlier done in script: hivdata_aaron. 
#^Earlier: taking regression-predicted averages or log-spending models to infer “proportion over 2k” — that’s the two-step approach.
#^...It biases results downward, because log-models compress high spending outliers.
#Solution:
# Now Apply the $2,000 cap at the individual (patient) level, instead of aggregated or regression-predicted shortcut.
#Aaron: “keep the actual spending if the person’s spend was under 2k but if it was over 2k then set it at 2k.” 
#^This respects real patient-level variation.

# Approach:
#For each disease dataset: Apply the IRA cap (if total_spending > cap, set to cap).
# Compute:
#Baseline total spend (sum(total_spending))
#After-IRA total spend (sum(total_spend_after_IRA))
#Mean spend before/after cap
#Number of patients
#Savings (baseline - after_IRA)
#Estimated extra patients supported (savings / 2000)
#% Coverage gain (extra_patients / n_patients * 100)


# -------------------------------------------------------------
# 1️⃣ Load libraries
# -------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(writexl)
library(scales)

# -------------------------------------------------------------
# 2️⃣ Define a function to apply IRA cap and summarize results
# -------------------------------------------------------------
summarize_ira_effect <- function(df, disease_name = "HIV, AIDS and Prevention", ira_caps = c(2000, 2100)) {
  
  results <- lapply(ira_caps, function(cap) {
    
    # Apply the IRA cap at the patient level
    df_cap <- df %>%
      mutate(
        total_spend_after_IRA = ifelse(total_spending > cap, cap, total_spending)
      )
    
    # Computing core table metrics
    total_baseline <- sum(df_cap$total_spending, na.rm = TRUE)
    total_after_IRA <- sum(df_cap$total_spend_after_IRA, na.rm = TRUE)
    n_patients <- n_distinct(df_cap$patient_id)
    
    mean_spend <- mean(df_cap$total_spending, na.rm = TRUE)
    mean_spend_IRA <- mean(df_cap$total_spend_after_IRA, na.rm = TRUE)
    
    # Calculate savings and expanded coverage
    savings <- total_baseline - total_after_IRA
    new_patients_supported_cap1 <- savings / cap
    new_patients_supported_mean2 <- savings / mean_spend_IRA
    coverage_gain_percent1 <- (new_patients_supported_cap1 / n_patients) * 100
    coverage_gain_percent2 <- (new_patients_supported_mean2 / n_patients) * 100
    
    # Return summary tibble
    tibble(
      Disease_Top5 = disease_name,
      IRA_cap = cap,
      n_patients = n_patients,
      total_baseline = total_baseline,
      total_after_IRA = total_after_IRA,
      mean_spend = mean_spend,
      mean_spend_IRA = mean_spend_IRA,
      savings = savings,
      new_patients_supported_cap1 = new_patients_supported_cap1,
      new_patients_supported_mean2 = new_patients_supported_mean2, 
      coverage_gain_percent1 = coverage_gain_percent1,
      coverage_gain_percent2 = coverage_gain_percent2
    )
  })
  
  bind_rows(results)
}

# -------------------------------------------------------------
# 3️⃣ Run the function for HIV dataset
# -------------------------------------------------------------
ira_summary_hiv <- summarize_ira_effect(hiv_data_with_predictions, "HIV, AIDS and Prevention")

# Round numbers for readability
ira_summary_hiv <- ira_summary_hiv %>%
  mutate(across(where(is.numeric), round, 2))

print(ira_summary_hiv)
names(ira_summary_hiv)
# -------------------------------------------------------------

###############################################################################
#Repeating the hiv plots not using regression predicts this time
#Patient-Level Spending Before and After IRA Cap (HIV Dataset)
#-----------------------------------------------------------------------------------------------------
# 4️⃣ Policy-Impact Visualization: HIV Dataset (Patient-Level Spending)
#-----------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(scales)

# 1️⃣ Create capped spending variables at $2,000 (2025) and $2,100 (2026)
hiv_data_capped <- hiv_data2 %>%
  mutate(
    total_spend_baseline = total_spending,
    total_spend_after_2000 = ifelse(total_spending > 2000, 2000, total_spending),
    total_spend_after_2100 = ifelse(total_spending > 2100, 2100, total_spending)
  )

# 2️⃣ Reshape to long format for easy plotting
hiv_long <- hiv_data_capped %>%
  select(patient_id, total_spend_baseline, total_spend_after_2000, total_spend_after_2100) %>%
  tidyr::pivot_longer(
    cols = starts_with("total_spend"),
    names_to = "spend_type",
    values_to = "spending"
  ) %>%
  mutate(
    spend_type = factor(spend_type,
                        levels = c("total_spend_baseline",
                                   "total_spend_after_2000",
                                   "total_spend_after_2100"),
                        labels = c("Baseline Spending",
                                   "After $2,000 Cap (2025)",
                                   "After $2,100 Cap (2026)"))
  )

# 3️⃣ Plot: Distribution of Spending Before and After IRA Cap
ggplot(hiv_long, aes(x = spending, fill = spend_type)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "#E64B35", size = 1) +
  geom_vline(xintercept = 2100, linetype = "dashed", color = "#4DBBD5", size = 1) +
  annotate("text", x = 2000, y = 0.003, label = "2025 IRA Cap ($2,000)",
           color = "#E64B35", hjust = -0.1, vjust = -1, fontface = "bold") +
  annotate("text", x = 2100, y = 0.002, label = "2026 IRA Cap ($2,100)",
           color = "#4DBBD5", hjust = -0.1, vjust = -1, fontface = "bold") +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3")) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Impact of IRA Out-of-Pocket Cap on Total Spending (HIV Patients)",
    subtitle = "Patient-level distribution of total spending before and after applying IRA caps",
    x = "Total Spending (USD)",
    y = "Density",
    fill = "Spending Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -------------------------------------------------------------
# 5️⃣ Export the summary table
# -------------------------------------------------------------
write_xlsx(ira_summary_hiv, "IRA_HIV_summary_two_methods.xlsx")


######################################################################################################################################
#### PREDICT SPEND TABLE (Other diseases: ) ####
######################################################################################################################################
#Repeating the same code for the hiv_data2, mm_data, cap_data, pulfib_data, cabr_data datasets 

#-----------------------------------------------------------------------------------------------------
# 1️⃣ Libraries
#-----------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(purrr)

#-----------------------------------------------------------------------------------------------------
# 2️⃣ Define Function: Compute Savings, New Patients Supported, and Coverage Gain
#-----------------------------------------------------------------------------------------------------
summarize_IRA_policy <- function(data, disease_name, cap_values = c(2000, 2100)) {
  
  # Ensure numeric
  data$total_spending <- as.numeric(data$total_spending)
  
  n_patients <- n_distinct(data$patient_id)
  total_baseline <- sum(data$total_spending, na.rm = TRUE)
  mean_spend <- mean(data$total_spending, na.rm = TRUE)
  
  results <- map_dfr(cap_values, function(cap) {
    
    # Apply per-person cap
    data <- data %>%
      mutate(spend_after_IRA = ifelse(total_spending > cap, cap, total_spending))
    
    total_after_IRA <- sum(data$spend_after_IRA, na.rm = TRUE)
    mean_spend_IRA <- mean(data$spend_after_IRA, na.rm = TRUE)
    
    # Savings (total_baseline - total_after_IRA)
    savings <- total_baseline - total_after_IRA
    
    # Two methods of estimating new patients supported
    new_patients_supported_cap1 <- savings / cap
    new_patients_supported_mean2 <- savings / mean_spend_IRA
    
    # Coverage expansion in percentage terms
    coverage_gain_percent1 <- (new_patients_supported_cap1 / n_patients) * 100
    coverage_gain_percent2 <- (new_patients_supported_mean2 / n_patients) * 100
    
    tibble(
      Disease_Top5 = disease_name,
      IRA_cap = cap,
      n_patients = n_patients,
      total_baseline = total_baseline,
      total_after_IRA = total_after_IRA,
      mean_spend = mean_spend,
      mean_spend_IRA = mean_spend_IRA,
      savings = savings,
      new_patients_supported_cap1 = new_patients_supported_cap1,
      new_patients_supported_mean2 = new_patients_supported_mean2,
      coverage_gain_percent1 = coverage_gain_percent1,
      coverage_gain_percent2 = coverage_gain_percent2
    )
  })
  
  return(results)
}

#-----------------------------------------------------------------------------------------------------
# 3️⃣ Apply to All 5 Datasets
#-----------------------------------------------------------------------------------------------------

IRA_summary_all <- bind_rows(
  summarize_IRA_policy(hiv_data2, "HIV, AIDS and Prevention"),
  summarize_IRA_policy(mm_data2, "Multiple Myeloma"),
  summarize_IRA_policy(cap_data2, "Prostate Cancer"),
  summarize_IRA_policy(pulfib_data2, "Pulmonary Fibrosis"),
  summarize_IRA_policy(cabr_data2, "Breast Cancer")
)

# Round for readability
IRA_summary_all <- IRA_summary_all %>%
  mutate(across(where(is.numeric), round, 2))

# View final policy summary table
print(IRA_summary_all)


##################################################################################################################################
################ Created Plots for all 5 disease areas #####################
##################################################################################################################################

#-----------------------------------------------------------------------------------------------------
# 📘 1️⃣ Function: Create IRA Spending Distribution Plot
#-----------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)

plot_IRA_distribution <- function(data, disease_name) {
  
  # Add capped spending columns
  data_capped <- data %>%
    mutate(
      total_spend_baseline = total_spending,
      total_spend_after_2000 = ifelse(total_spending > 2000, 2000, total_spending),
      total_spend_after_2100 = ifelse(total_spending > 2100, 2100, total_spending)
    )
  
  # Reshape for plotting
  long_data <- data_capped %>%
    select(patient_id, total_spend_baseline, total_spend_after_2000, total_spend_after_2100) %>%
    pivot_longer(
      cols = starts_with("total_spend"),
      names_to = "spend_type",
      values_to = "spending"
    ) %>%
    mutate(
      spend_type = factor(
        spend_type,
        levels = c("total_spend_baseline", "total_spend_after_2000", "total_spend_after_2100"),
        labels = c("Baseline Spending", "After $2,000 Cap (2025)", "After $2,100 Cap (2026)")
      )
    )
  
  # Generate plot
  ggplot(long_data, aes(x = spending, fill = spend_type)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 2000, linetype = "dashed", color = "#E64B35", size = 1) +
    geom_vline(xintercept = 2100, linetype = "dashed", color = "#4DBBD5", size = 1) +
    annotate("text", x = 2000, y = Inf, label = "2025 IRA Cap ($2,000)",
             color = "#E64B35", hjust = -0.1, vjust = 2, fontface = "bold") +
    annotate("text", x = 2100, y = Inf, label = "2026 IRA Cap ($2,100)",
             color = "#4DBBD5", hjust = -0.1, vjust = 4, fontface = "bold") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3")) +
    scale_x_continuous(labels = dollar_format(prefix = "$")) +
    labs(
      title = paste0("Impact of IRA Out-of-Pocket Cap on Total Spending (", disease_name, ")"),
      subtitle = "Patient-level distribution of total spending before and after applying IRA caps",
      x = "Total Spending (USD)",
      y = "Density",
      fill = "Spending Type"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#-----------------------------------------------------------------------------------------------------
# 📊 2️⃣ Generate Plots for All 5 Disease Areas
#-----------------------------------------------------------------------------------------------------

plots_list <- list(
  HIV = plot_IRA_distribution(hiv_data2, "HIV, AIDS and Prevention"),
  MM = plot_IRA_distribution(mm_data2, "Multiple Myeloma"),
  CAP = plot_IRA_distribution(cap_data2, "Prostate Cancer"),
  PULFIB = plot_IRA_distribution(pulfib_data2, "Pulmonary Fibrosis"),
  CABR = plot_IRA_distribution(cabr_data2, "Breast Cancer")
)

# Display all plots one by one
plots_list$HIV
plots_list$MM
plots_list$CAP
plots_list$PULFIB
plots_list$CABR

# Optional: Save them to files (for your poster or slides)
ggsave("IRA_HIV_distribution.png", plots_list$HIV, width = 8, height = 5, dpi = 300)
ggsave("IRA_MM_distribution.png", plots_list$MM, width = 8, height = 5, dpi = 300)
ggsave("IRA_CAP_distribution.png", plots_list$CAP, width = 8, height = 5, dpi = 300)
ggsave("IRA_PULFIB_distribution.png", plots_list$PULFIB, width = 8, height = 5, dpi = 300)
ggsave("IRA_CABR_distribution.png", plots_list$CABR, width = 8, height = 5, dpi = 300)











######################################################################################################################################
#### Generate Summary Table 1 Across Top-5 Diseases
#Understanding variables for Table One
names(hiv_data2)
table(hiv_data2$Ethnicity)
unique(hiv_data2$Age_cat3Age..55)
unique(hiv_data2$Age_cat3Age.56.75)
unique(hiv_data2$Age_cat3Age..75)
unique(hiv_data2$Gender_Male)
table(hiv_data2$Gender_Male)
table(hiv_data2$Employment_Employed)
table(hiv_data2$Income_Group)
unique(hiv_data2$Income_cat3Income..47)
unique(hiv_data2$Income_cat3Income.48.71)
unique(hiv_data2$Income_cat3Income.72plus)
table(hiv_data2$Drug_Group)
################################################################################################################################
library(dplyr)
library(purrr)
library(janitor)
library(writexl)

summarize_disease_exposures <- function(data, disease_name) {
  
  # ---- Standardize Variables ----
  data <- data %>%
    mutate(
      Gender_Male = ifelse(Gender_Male %in% c("Male", "1", 1, TRUE), 1, 0),
      Ethnicity_White = ifelse(Ethnicity %in% c("White", "Caucasian"), 1, 0),
      Employment_Employed = ifelse(Employment_Employed %in% c("Employed", "1", 1, TRUE), 1, 0)
    )
  
  # ---- Income Recoding ----
  data <- data %>%
    mutate(
      income_bracket = case_when(
        grepl("Less than", Income_Group, ignore.case = TRUE) ~ "Under_24K",
        grepl("\\$24,000 - \\$47,999", Income_Group) ~ "24K_47K",
        grepl("\\$48,000 - \\$71,999", Income_Group) ~ "48K_71K",
        grepl("\\$72,000 - \\$95,999", Income_Group) ~ "72K_95K",
        grepl("\\$96,000 - \\$119,999", Income_Group) ~ "96K_119K",
        grepl("\\$120,000 or More", Income_Group) ~ "120K_plus",
        TRUE ~ "Unknown"
      )
    )
  
  # ---- Summarize ----
  data %>%
    summarise(
      Disease_Top5 = disease_name,
      n_patients = n_distinct(patient_id),
      total_spend = sum(total_spending, na.rm = TRUE),
      mean_spend = mean(total_spending, na.rm = TRUE),
      
      # Demographics
      Age_under55 = sum(Age_cat3Age..55, na.rm = TRUE),
      Age_56_75 = sum(Age_cat3Age.56.75, na.rm = TRUE),
      Age_over75 = sum(Age_cat3Age..75, na.rm = TRUE),
      Gender_Male = sum(Gender_Male, na.rm = TRUE),
      Ethnicity_White = sum(Ethnicity_White, na.rm = TRUE),
      Employed = sum(Employment_Employed, na.rm = TRUE),
      Unemployed = n() - sum(Employment_Employed, na.rm = TRUE),
      
      # Income Brackets
      Income_Under_24K = sum(income_bracket == "Under_24K", na.rm = TRUE),
      Income_24_47K = sum(income_bracket == "24K_47K", na.rm = TRUE),
      Income_48_71K = sum(income_bracket == "48K_71K", na.rm = TRUE),
      Income_72_95K = sum(income_bracket == "72K_95K", na.rm = TRUE),
      Income_96_119K = sum(income_bracket == "96K_119K", na.rm = TRUE),
      Income_120K_plus = sum(income_bracket == "120K_plus", na.rm = TRUE),
      Income_Unknown = sum(income_bracket == "Unknown", na.rm = TRUE),
      
      # Drug Group
      Drug_PartB = sum(Drug_Group == "Part B", na.rm = TRUE),
      Drug_PartD = sum(Drug_Group == "Part D", na.rm = TRUE),
      Drug_Insurance = sum(Drug_Group == "Insurance Premium", na.rm = TRUE),
      Drug_Group_Unique = n_distinct(Drug_Group, na.rm = TRUE)
    )
}

