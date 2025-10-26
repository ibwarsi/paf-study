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