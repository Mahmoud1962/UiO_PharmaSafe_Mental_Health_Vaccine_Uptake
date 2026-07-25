# Load required libraries
# library(tidyverse)
library(survival)
library(ggsurvfit)
library(ggplot2)
library(readr)
library(lubridate)
library(survminer)
library(mitools)
library(mice)
library(purrr)
library(broom)
library(cmprsk)
library(dplyr)

to_add_old_covid <- old_covid[, c("person_id", "mental", "mixed", "depression_pure","anxiety_pure",
                                 "bipolar_pure","PTSD_pure", "OCD_pure","ADHD_pure",
                                 "eligible_dose2_date","eligible_booster1_date",
                                 "eligible_booster2_date","received_valid_dose2",
                                 "received_valid_booster1","received_valid_booster2",
                                 "is_age_eligible_primary","is_age_eligible_booster1",
                                 "is_age_eligible_booster2")]
to_add_primary_vacc <- primary_vacc[,c("person_id", "time_to_event_primary","status_primary",
                                       "time_to_event_booster1","status_booster1",
                                       "time_to_event_booster2","status_booster2")]
to_add_old_covid <- to_add_old_covid[to_add_old_covid$person_id %in% to_add_primary_vacc$person_id,]
to_add <- merge(to_add_old_covid, to_add_primary_vacc, all = T)
old_covid_imp <- old_covid_imp_iptw
to_add <- to_add[to_add$is_age_eligible_primary == 1,]
for(i in 1) {
  old_covid_imp_iptw[[i]]$mental <- NULL
  old_covid_imp_iptw[[i]]$anxiety_pure <- NULL
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][old_covid_imp_iptw[[i]]$person_id %in% to_add$person_id,]
  old_covid_imp_iptw[[i]] <- merge(old_covid_imp_iptw[[i]], to_add, all = T)
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]] %>% group_by(old_covid_imp_iptw[[i]]) %>%
    mutate(anxiety_pure = max(anxiety_pure))
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][!duplicated(old_covid_imp_iptw[[i]]$person_id),]
}
# ============================================
# USE FIRST IMPUTATION ONLY FOR PLOTTING
# ============================================
data_plot <- old_covid_imp_iptw[[1]]
# ============================================
# PREPARE DATA
# ============================================

# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_primary, as.factor(status_primary)) ~ mental,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Mental Health Conditions", "Any Mental Health Condition")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Mental Health Conditions" = "orange", 
               "Any Mental Health Condition" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Mental Health Conditions" = "orange", 
                                "Any Mental Health Condition" = "royalblue2")
                     ) +
  theme(text = element_text(size = 20),          # all text
                    axis.title = element_text(size = 20),
                    axis.text = element_text(size = 20),
                    legend.title = element_text(size = 20),
                    legend.text = element_text(size = 18),
                    strip.text = element_text(size = 16),
                    plot.title = element_text(size = 16),
                    plot.subtitle = element_text(size = 14)
                )
  

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_primary_mental.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# # ============================================
# 
# # Function to extract cumulative incidence from one imputation
# extract_cuminc <- function(data, times_days = c(182.625, 365.25, 547.875)) {
#   
#   data <- data %>%
#     mutate(status_primary_factor = factor(status_primary,
#                                   levels = c(0, 1, 2, 3),
#                                   labels = c("censored", "vaccination", "infection", "death")))
#   
#   fit <- survfit(Surv(time_to_event_primary, status_primary_factor) ~ mental,
#                  data = data,
#                  weights = iptw_weights)
#   
#   summ <- summary(fit, times = times_days)
#   
#   # Extract vaccination cumulative incidence (usually column 2)
#   n_times <- length(times_days)
#   n_strata <- 2
#   
#   results <- data.frame(
#     time_days = rep(times_days, n_strata),
#     time_years = rep(times_days / 365.25, n_strata),
#     mental = rep(c("No Mental Health Conditions", "Any Mental Health Condition"), each = n_times),
#     cuminc = as.vector(summ$pstate[, 2]),
#     se = as.vector(summ$std.err[, 2])
#   )
#   
#   return(results)
# }
# 
# # Apply to all imputations
# all_results <- map(old_covid_imp_iptw, extract_cuminc)
# 
# # Add imputation number
# for(i in 1:length(all_results)) {
#   all_results[[i]]$imp <- i
# }
# 
# # Combine
# combined_results <- bind_rows(all_results)
# 
# # ============================================
# # POOLED ESTIMATES USING RUBIN'S RULES
# # ============================================
# pooled_results <- combined_results %>%
#   group_by(time_days, mental) %>%
#   summarise(
#     # Point estimate (mean across imputations)
#     cuminc_pooled = mean(cuminc),
#     
#     # Within-imputation variance
#     within_var = mean(se^2),
#     
#     # Between-imputation variance
#     between_var = var(cuminc),
#     
#     # Number of imputations
#     m = n(),
#     
#     .groups = "drop"
#   ) %>%
#   mutate(
#     # Total variance (Rubin's Rules)
#     total_var = within_var + (1 + 1/m) * between_var,
#     
#     # Pooled standard error
#     se_pooled = sqrt(total_var),
#     
#     # 95% confidence intervals
#     ci_lower = cuminc_pooled - 1.96 * se_pooled,
#     ci_upper = cuminc_pooled + 1.96 * se_pooled,
#     
#     # Constrain to [0,1]
#     ci_lower = pmax(0, ci_lower),
#     ci_upper = pmin(1, ci_upper),
#     
#     # Time in years for readability
#     time_years = time_days / 365.25
#   )
# 
# # ============================================
# # CREATE RESULTS TABLE FOR REPORTING
# # ============================================
# results_table <- pooled_results %>%
#   arrange(mental, time_years) %>%
#   mutate(
#     Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
#   ) %>%
#   select(Mental_status_primary = mental,
#          Time_Years = time_years,
#          Cumulative_Incidence = Estimate_CI)
# 
# # Save to CSV
# save(all_results, file = "CumInc_Results_Fit_Mental.rdata")
# write.csv(results_table, "pooled_results_table.csv", row.names = FALSE)
####################################################################################
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_primary, as.factor(status_primary)) ~ mixed,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Psychiatric Comorbidity", "Psychiatric Comorbidity")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Psychiatric Comorbidity" = "orange", 
               "Psychiatric Comorbidity" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Psychiatric Comorbidity" = "orange", 
                                "Psychiatric Comorbidity" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
                        axis.title = element_text(size = 20),
                        axis.text = element_text(size = 20),
                        legend.title = element_text(size = 20),
                        legend.text = element_text(size = 18),
                        strip.text = element_text(size = 16),
                        plot.title = element_text(size = 16),
                        plot.subtitle = element_text(size = 14)
                  )

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_primary_mixed.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(182.625, 365.25, 547.875)) {
  
  data <- data %>%
    mutate(status_primary_factor = factor(status_primary,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_primary, status_primary_factor) ~ mixed,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    mixed = rep(c("No Psychiatric Comorbidity", "Psychiatric Comorbidity"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, mixed) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(mixed, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(Mixed_status_primary = mixed,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_Mixed.rdata")
write.csv(results_table, "pooled_results_table_mixed.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_primary, as.factor(status_primary)) ~ depression_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Depression", "Depression")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Depression" = "orange", 
               "Depression" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Depression" = "orange", 
                                "Depression" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_primary_depression_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(182.625, 365.25, 547.875)) {
  
  data <- data %>%
    mutate(status_primary_factor = factor(status_primary,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_primary, status_primary_factor) ~ depression_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    depression_pure = rep(c("No Depression", "Depression"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, depression_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(depression_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(depression_pure_status_primary = depression_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_depression_pure.rdata")
write.csv(results_table, "pooled_results_table_depression_pure.csv", row.names = FALSE)
###############################################################################
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_primary, as.factor(status_primary)) ~ anxiety_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Anxiety", "Anxiety")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Anxiety" = "orange", 
               "Anxiety" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Anxiety" = "orange", 
                                "Anxiety" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_primary_Anxiety_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(182.625, 365.25, 547.875)) {
  
  data <- data %>%
    mutate(status_primary_factor = factor(status_primary,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_primary, status_primary_factor) ~ anxiety_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    anxiety_pure = rep(c("No Anxiety", "Anxiety"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, anxiety_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(anxiety_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(anxiety_pure_status_primary = anxiety_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_anxiety_pure.rdata")
write.csv(results_table, "pooled_results_table_anxiety_pure.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_primary, as.factor(status_primary)) ~ bipolar_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Bipolar", "Bipolar")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Bipolar" = "orange", 
               "Bipolar" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Bipolar" = "orange", 
                                "Bipolar" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_primary_Bipolar_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(182.625, 365.25, 547.875)) {
  
  data <- data %>%
    mutate(status_primary_factor = factor(status_primary,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_primary, status_primary_factor) ~ bipolar_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    bipolar_pure = rep(c("No Bipolar", "Bipolar"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, bipolar_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(bipolar_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(bipolar_pure_status_primary = bipolar_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_bipolar_pure.rdata")
write.csv(results_table, "pooled_results_table_bipolar_pure.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_primary, as.factor(status_primary)) ~ PTSD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No PTSD", "PTSD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No PTSD" = "orange", 
               "PTSD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No PTSD" = "orange", 
                                "PTSD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_primary_PTSD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(182.625, 365.25, 547.875)) {
  
  data <- data %>%
    mutate(status_primary_factor = factor(status_primary,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_primary, status_primary_factor) ~ PTSD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    PTSD_pure = rep(c("No PTSD", "PTSD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, PTSD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(PTSD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(PTSD_pure_status_primary = PTSD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_PTSD_pure.rdata")
write.csv(results_table, "pooled_results_table_PTSD_pure.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_primary, as.factor(status_primary)) ~ OCD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No OCD", "OCD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No OCD" = "orange", 
               "OCD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No OCD" = "orange", 
                                "OCD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_primary_OCD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(182.625, 365.25, 547.875)) {
  
  data <- data %>%
    mutate(status_primary_factor = factor(status_primary,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_primary, status_primary_factor) ~ OCD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    OCD_pure = rep(c("No OCD", "OCD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, OCD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(OCD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(OCD_pure_status_primary = OCD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_OCD_pure.rdata")
write.csv(results_table, "pooled_results_table_OCD_pure.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_primary, as.factor(status_primary)) ~ ADHD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No ADHD", "ADHD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No ADHD" = "orange", 
               "ADHD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No ADHD" = "orange", 
                                "ADHD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_primary_ADHD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(182.625, 365.25, 547.875)) {
  
  data <- data %>%
    mutate(status_primary_factor = factor(status_primary,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_primary, status_primary_factor) ~ ADHD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    ADHD_pure = rep(c("No ADHD", "ADHD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, ADHD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(ADHD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(ADHD_pure_status_primary = ADHD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_ADHD_pure.rdata")
write.csv(results_table, "pooled_results_table_ADHD_pure.csv", row.names = FALSE)
##############################################################################################

old_covid_imp_iptw <- old_covid_imp
to_add <- to_add[to_add$is_age_eligible_booster1 == 1,]
for(i in 1) {
  old_covid_imp_iptw[[i]]$mental <- NULL
  old_covid_imp_iptw[[i]]$anxiety_pure <- NULL
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][old_covid_imp_iptw[[i]]$person_id %in% to_add$person_id,]
  old_covid_imp_iptw[[i]] <- merge(old_covid_imp_iptw[[i]], to_add, all = T)
}


# ============================================
# USE FIRST IMPUTATION ONLY FOR PLOTTING
# ============================================
data_plot <- old_covid_imp_iptw[[1]]

# ============================================
# PREPARE DATA
# ============================================

# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster1, as.factor(status_booster1)) ~ mental,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Mental Health Conditions", "Any Mental Health Condition")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Mental Health Conditions" = "orange", 
               "Any Mental Health Condition" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Mental Health Conditions" = "orange", 
                                "Any Mental Health Condition" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster1_mental.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster1_factor = factor(status_booster1,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster1, status_booster1_factor) ~ mental,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    mental = rep(c("No Mental Health Conditions", "Any Mental Health Condition"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, mental) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(mental, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(Mental_status_booster1 = mental,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_Mental_Booster1.rdata")
write.csv(results_table, "pooled_results_table_Booster1.csv", row.names = FALSE)
####################################################################################
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster1, as.factor(status_booster1)) ~ mixed,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Psychiatric Comorbidity", "Psychiatric Comorbidity")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Psychiatric Comorbidity" = "orange", 
               "Psychiatric Comorbidity" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Psychiatric Comorbidity" = "orange", 
                                "Psychiatric Comorbidity" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster1_mixed.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster1_factor = factor(status_booster1,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster1, status_booster1_factor) ~ mixed,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    mixed = rep(c("No Psychiatric Comorbidity", "Psychiatric Comorbidity"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, mixed) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(mixed, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(Mixed_status_booster1 = mixed,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_Mixed_Booster1.rdata")
write.csv(results_table, "pooled_results_table_mixed_Booster1.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster1, as.factor(status_booster1)) ~ depression_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Depression", "Depression")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Depression" = "orange", 
               "Depression" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Depression" = "orange", 
                                "Depression" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster1_depression_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster1_factor = factor(status_booster1,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster1, status_booster1_factor) ~ depression_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    depression_pure = rep(c("No Depression", "Depression"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, depression_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(depression_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(depression_pure_status_booster1 = depression_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_depression_pure_Booster1.rdata")
write.csv(results_table, "pooled_results_table_depression_pure_Booster1.csv", row.names = FALSE)
###############################################################################
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster1, as.factor(status_booster1)) ~ anxiety_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Anxiety", "Anxiety")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Anxiety" = "orange", 
               "Anxiety" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Anxiety" = "orange", 
                                "Anxiety" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster1_Anxiety_pure.pdf", plot = p, width = 10, height = 6, dpi = 300)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster1_factor = factor(status_booster1,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster1, status_booster1_factor) ~ anxiety_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    anxiety_pure = rep(c("No Anxiety", "Anxiety"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, anxiety_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(anxiety_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(anxiety_pure_status_booster1 = anxiety_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_anxiety_pure_Booster1.rdata")
write.csv(results_table, "pooled_results_table_anxiety_pure_Booster1.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster1, as.factor(status_booster1)) ~ bipolar_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Bipolar", "Bipolar")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Bipolar" = "orange", 
               "Bipolar" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Bipolar" = "orange", 
                                "Bipolar" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster1_Bipolar_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster1_factor = factor(status_booster1,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster1, status_booster1_factor) ~ bipolar_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    bipolar_pure = rep(c("No Bipolar", "Bipolar"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, bipolar_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(bipolar_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(bipolar_pure_status_booster1 = bipolar_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_bipolar_pure_Booster1.rdata")
write.csv(results_table, "pooled_results_table_bipolar_pure_Booster1.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster1, as.factor(status_booster1)) ~ PTSD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No PTSD", "PTSD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No PTSD" = "orange", 
               "PTSD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No PTSD" = "orange", 
                                "PTSD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster1_PTSD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster1_factor = factor(status_booster1,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster1, status_booster1_factor) ~ PTSD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    PTSD_pure = rep(c("No PTSD", "PTSD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, PTSD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(PTSD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(PTSD_pure_status_booster1 = PTSD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_PTSD_pure_Booster1.rdata")
write.csv(results_table, "pooled_results_table_PTSD_pure_Booster1.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster1, as.factor(status_booster1)) ~ OCD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No OCD", "OCD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No OCD" = "orange", 
               "OCD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No OCD" = "orange", 
                                "OCD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster1_OCD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster1_factor = factor(status_booster1,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster1, status_booster1_factor) ~ OCD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    OCD_pure = rep(c("No OCD", "OCD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, OCD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(OCD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(OCD_pure_status_booster1 = OCD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_OCD_pure_Booster1.rdata")
write.csv(results_table, "pooled_results_table_OCD_pure_Booster1.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster1, as.factor(status_booster1)) ~ ADHD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No ADHD", "ADHD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No ADHD" = "orange", 
               "ADHD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No ADHD" = "orange", 
                                "ADHD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster1_ADHD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster1_factor = factor(status_booster1,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster1, status_booster1_factor) ~ ADHD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    ADHD_pure = rep(c("No ADHD", "ADHD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, ADHD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(ADHD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(ADHD_pure_status_booster1 = ADHD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_ADHD_pure_Booster1.rdata")
write.csv(results_table, "pooled_results_table_ADHD_pure_Booster1.csv", row.names = FALSE)
##############################################################################################

old_covid_imp_iptw <- old_covid_imp
to_add <- to_add[to_add$is_age_eligible_booster2 == 1,]
for(i in 1) {
  old_covid_imp_iptw[[i]]$mental <- NULL
  old_covid_imp_iptw[[i]]$anxiety_pure <- NULL
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][old_covid_imp_iptw[[i]]$person_id %in% to_add$person_id,]
  old_covid_imp_iptw[[i]] <- merge(old_covid_imp_iptw[[i]], to_add, all = T)
}


# ============================================
# USE FIRST IMPUTATION ONLY FOR PLOTTING
# ============================================
data_plot <- old_covid_imp_iptw[[1]]

# ============================================
# PREPARE DATA
# ============================================

# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster2, as.factor(status_booster2)) ~ mental,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Mental Health Conditions", "Any Mental Health Condition")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Mental Health Conditions" = "orange", 
               "Any Mental Health Condition" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Mental Health Conditions" = "orange", 
                                "Any Mental Health Condition" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))


# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster2_mental.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster2_factor = factor(status_booster2,
                                           levels = c(0, 1, 2, 3),
                                           labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster2, status_booster2_factor) ~ mental,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    mental = rep(c("No Mental Health Conditions", "Any Mental Health Condition"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, mental) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(mental, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(Mental_status_booster2 = mental,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_Mental_booster2.rdata")
write.csv(results_table, "pooled_results_table_booster2.csv", row.names = FALSE)
####################################################################################
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster2, as.factor(status_booster2)) ~ mixed,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Psychiatric Comorbidity", "Psychiatric Comorbidity")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Psychiatric Comorbidity" = "orange", 
               "Psychiatric Comorbidity" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Psychiatric Comorbidity" = "orange", 
                                "Psychiatric Comorbidity" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))


# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster2_mixed.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster2_factor = factor(status_booster2,
                                           levels = c(0, 1, 2, 3),
                                           labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster2, status_booster2_factor) ~ mixed,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    mixed = rep(c("No Psychiatric Comorbidity", "Psychiatric Comorbidity"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, mixed) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(mixed, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(Mixed_status_booster2 = mixed,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_Mixed_booster2.rdata")
write.csv(results_table, "pooled_results_table_mixed_booster2.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster2, as.factor(status_booster2)) ~ depression_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Depression", "Depression")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Depression" = "orange", 
               "Depression" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Depression" = "orange", 
                                "Depression" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))


# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster2_depression_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster2_factor = factor(status_booster2,
                                           levels = c(0, 1, 2, 3),
                                           labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster2, status_booster2_factor) ~ depression_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    depression_pure = rep(c("No Depression", "Depression"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, depression_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(depression_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(depression_pure_status_booster2 = depression_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_depression_pure_booster2.rdata")
write.csv(results_table, "pooled_results_table_depression_pure_booster2.csv", row.names = FALSE)
###############################################################################
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster2, as.factor(status_booster2)) ~ anxiety_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Anxiety", "Anxiety")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Anxiety" = "orange", 
               "Anxiety" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Anxiety" = "orange", 
                                "Anxiety" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))


# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster2_Anxiety_pure.pdf", plot = p, width = 10, height = 6, dpi = 300)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster2_factor = factor(status_booster2,
                                           levels = c(0, 1, 2, 3),
                                           labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster2, status_booster2_factor) ~ anxiety_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    anxiety_pure = rep(c("No Anxiety", "Anxiety"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, anxiety_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(anxiety_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(anxiety_pure_status_booster2 = anxiety_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_anxiety_pure_booster2.rdata")
write.csv(results_table, "pooled_results_table_anxiety_pure_booster2.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster2, as.factor(status_booster2)) ~ bipolar_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No Bipolar", "Bipolar")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No Bipolar" = "orange", 
               "Bipolar" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No Bipolar" = "orange", 
                                "Bipolar" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))


# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster2_Bipolar_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
  # ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster2_factor = factor(status_booster2,
                                           levels = c(0, 1, 2, 3),
                                           labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster2, status_booster2_factor) ~ bipolar_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    bipolar_pure = rep(c("No Bipolar", "Bipolar"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, bipolar_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(bipolar_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(bipolar_pure_status_booster2 = bipolar_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_bipolar_pure_booster2.rdata")
write.csv(results_table, "pooled_results_table_bipolar_pure_booster2.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster2, as.factor(status_booster2)) ~ PTSD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No PTSD", "PTSD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No PTSD" = "orange", 
               "PTSD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No PTSD" = "orange", 
                                "PTSD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))


# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster2_PTSD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster2_factor = factor(status_booster2,
                                           levels = c(0, 1, 2, 3),
                                           labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster2, status_booster2_factor) ~ PTSD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    PTSD_pure = rep(c("No PTSD", "PTSD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, PTSD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(PTSD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(PTSD_pure_status_booster2 = PTSD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_PTSD_pure_booster2.rdata")
write.csv(results_table, "pooled_results_table_PTSD_pure_booster2.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster2, as.factor(status_booster2)) ~ OCD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No OCD", "OCD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No OCD" = "orange", 
               "OCD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No OCD" = "orange", 
                                "OCD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster2_OCD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster2_factor = factor(status_booster2,
                                           levels = c(0, 1, 2, 3),
                                           labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster2, status_booster2_factor) ~ OCD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    OCD_pure = rep(c("No OCD", "OCD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, OCD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(OCD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(OCD_pure_status_booster2 = OCD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_OCD_pure_booster2.rdata")
write.csv(results_table, "pooled_results_table_OCD_pure_booster2.csv", row.names = FALSE)
###############################################################################3
# ============================================
# FIT WEIGHTED AALEN-JOHANSEN ESTIMATOR
# ============================================
fit_aj <- survfit(Surv(time_to_event_booster2, as.factor(status_booster2)) ~ ADHD_pure,
                  data = data_plot,
                  weights = iptw_weights)

# ============================================
# CREATE PLOT WITH RISK TABLE
# ============================================

time_points_days <- c(0, 182.625, 365.25, 547.875, 730.5, 913.125,1095.75)
time_labels_years <- c("0", "0.5", "1", "1.5", "2", "2.5", "3")

names(fit_aj$strata) <- c("No ADHD", "ADHD")
attr(fit_aj, "strata") <- names(fit_aj$strata)
p <- ggcuminc(fit_aj, outcome = 1) +
  labs(title = "",
       subtitle = "",
       y = "Cumulative Incidence",
       x = "Time (days)") +
  add_confidence_interval() +
  add_risktable(times = time_points_days,
                title = "Number at Risk",
                size = 6,
                risktable_stats = c(
                  "{ceiling(n.risk)}",
                  "{ceiling(n.event)}"),
                stats_label = c(
                  "At risk",
                  "Events"
                )) +
  scale_fill_manual(
    values = c("No ADHD" = "orange", 
               "ADHD" = "royalblue2"),
    guide = "none"  # Remove fill legend if desired
  ) +
  scale_color_manual(values = c("No ADHD" = "orange", 
                                "ADHD" = "royalblue2")
  ) +
  theme(text = element_text(size = 20),          # all text
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# ============================================
# DISPLAY AND SAVE PLOT
# ============================================
print(p)
ggsave("cuminc_plot_booster2_ADHD_pure.pdf", plot = p, width = 10, height = 6, dpi = 600)
# ggsave("cuminc_plot_first_imputation.png", plot = p, width = 9, height = 7, dpi = 300)

# ============================================
# POOL RESULTS ACROSS ALL 30 IMPUTATIONS
# ============================================

# Function to extract cumulative incidence from one imputation
extract_cuminc <- function(data, times_days = c(365.25, 1825.25, 3652.5)) {
  
  data <- data %>%
    mutate(status_booster2_factor = factor(status_booster2,
                                           levels = c(0, 1, 2, 3),
                                           labels = c("censored", "vaccination", "infection", "death")))
  
  fit <- survfit(Surv(time_to_event_booster2, status_booster2_factor) ~ ADHD_pure,
                 data = data,
                 weights = iptw_weights)
  
  summ <- summary(fit, times = times_days)
  
  # Extract vaccination cumulative incidence (usually column 2)
  n_times <- length(times_days)
  n_strata <- 2
  
  results <- data.frame(
    time_days = rep(times_days, n_strata),
    time_years = rep(times_days / 365.25, n_strata),
    ADHD_pure = rep(c("No ADHD", "ADHD"), each = n_times),
    cuminc = as.vector(summ$pstate[, 2]),
    se = as.vector(summ$std.err[, 2])
  )
  
  return(results)
}

# Apply to all imputations
all_results <- map(old_covid_imp_iptw, extract_cuminc)

# Add imputation number
for(i in 1:length(all_results)) {
  all_results[[i]]$imp <- i
}

# Combine
combined_results <- bind_rows(all_results)

# ============================================
# POOLED ESTIMATES USING RUBIN'S RULES
# ============================================
pooled_results <- combined_results %>%
  group_by(time_days, ADHD_pure) %>%
  summarise(
    # Point estimate (mean across imputations)
    cuminc_pooled = mean(cuminc),
    
    # Within-imputation variance
    within_var = mean(se^2),
    
    # Between-imputation variance
    between_var = var(cuminc),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's Rules)
    total_var = within_var + (1 + 1/m) * between_var,
    
    # Pooled standard error
    se_pooled = sqrt(total_var),
    
    # 95% confidence intervals
    ci_lower = cuminc_pooled - 1.96 * se_pooled,
    ci_upper = cuminc_pooled + 1.96 * se_pooled,
    
    # Constrain to [0,1]
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper),
    
    # Time in years for readability
    time_years = time_days / 365.25
  )

# ============================================
# CREATE RESULTS TABLE FOR REPORTING
# ============================================
results_table <- pooled_results %>%
  arrange(ADHD_pure, time_years) %>%
  mutate(
    Estimate_CI = sprintf("%.3f (%.3f-%.3f)", cuminc_pooled, ci_lower, ci_upper)
  ) %>%
  select(ADHD_pure_status_booster2 = ADHD_pure,
         Time_Years = time_years,
         Cumulative_Incidence = Estimate_CI)

# Save to CSV
save(all_results, file = "CumInc_Results_Fit_ADHD_pure_booster2.rdata")
write.csv(results_table, "pooled_results_table_ADHD_pure_booster2.csv", row.names = FALSE)
