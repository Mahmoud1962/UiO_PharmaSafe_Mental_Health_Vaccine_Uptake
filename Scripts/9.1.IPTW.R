################################################################################
# Script: 9.1.IPTW.R
#
# Purpose:
#   Construct inverse probability of treatment weights ffor all subpopulations, 
#   and trim extreme weights to stabalize them.
################################################################################
library(mice)
library(dplyr)
library(broom)
library(WeightIt)

cov_preg_covid_risk <- readRDS("Path_to_Cov_Imputed_Pregnancy_Covid_Risk_SA.rds")
preg_covid_risk_imp <- complete(cov_preg_covid_risk, action = "all")

# Function to apply IPTW and return a survey design object
apply_iptw <- function(data) {
  # Calculate weights (example: ATE weights for binary treatment)
  w <- weightit(mental ~ age_at_enrollment_categorized + county_2021 + country_of_birth
                + smoking + parity + profession + income_2021_cat,
                data = data,
                method = "ps",        # Propensity score
                estimand = "ATE",     # or "ATT", "ATC"
                stabilize = TRUE)    # Stabilized weights recommended
  
  # Add weights to dataset
  data$iptw_weights <- w$weights
  
  return(data)
}

# Apply to all imputed datasets
implist_weighted <- lapply(preg_covid_risk_imp, apply_iptw)

# Check mean weights across datasets
weight_means <- sapply(implist_weighted, function(x) mean(x$iptw_weights))
summary(weight_means)

# Check max weights
weight_max <- sapply(implist_weighted, function(x) max(x$iptw_weights))
summary(weight_max)

weight_stats <- data.frame(
  imputation = 1:45,
  mean = sapply(implist_weighted, function(x) mean(x$iptw_weights)),
  sd = sapply(implist_weighted, function(x) sd(x$iptw_weights)),
  min = sapply(implist_weighted, function(x) min(x$iptw_weights)),
  max = sapply(implist_weighted, function(x) max(x$iptw_weights)),
  p99 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.99)),
  p95 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.95)),
  p90 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.90))
)

# Summary across imputations
summary(weight_stats)

# Visualize
library(ggplot2)
ggplot(stack(implist_weighted[[1]], select = "iptw_weights"), aes(x = values)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_log10() +  # Weights often right-skewed
  labs(title = "Weight Distribution (Imputation 1)", x = "IPTW Weights (log scale)")

weight_flags <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(implist_weighted, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(implist_weighted, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(implist_weighted, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(implist_weighted, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(implist_weighted, nrow)
)

weight_flags$ess_percent <- weight_flags$ess / weight_flags$n * 100
weight_flags$ess_lt_50 <- weight_flags$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags)

# Coefficient of variation across imputations
cv_across_imps <- sd(weight_stats$mean) / mean(weight_stats$mean)
# If > 0.1-0.2, weights are sensitive to imputation model

preg_covid_risk_imp <- implist_weighted
save(preg_covid_risk_imp, file = "Pregnant_Covid_Risk_Weights_SA.rdata")
#######################################################################################################################
cov_preg_covid_norisk <- readRDS("Path_to_Cov_Imputed_Pregnancy_Covid_Norisk_SA.rds")
preg_covid_norisk_imp <- complete(cov_preg_covid_norisk, action = "all")

# Function to apply IPTW and return a survey design object
apply_iptw <- function(data) {
  # Calculate weights (example: ATE weights for binary treatment)
  w <- weightit(mental ~ age_at_enrollment_categorized + county_2021 + country_of_birth
                + smoking + parity + profession + income_2021_cat,
                data = data,
                method = "ps",        # Propensity score
                estimand = "ATE",     # or "ATT", "ATC"
                stabilize = TRUE)    # Stabilized weights recommended
  
  # Add weights to dataset
  data$iptw_weights <- w$weights
  
  return(data)
}

# Apply to all imputed datasets
implist_weighted <- lapply(preg_covid_norisk_imp, apply_iptw)

# Check mean weights across datasets
weight_means <- sapply(implist_weighted, function(x) mean(x$iptw_weights))
summary(weight_means)

# Check max weights
weight_max <- sapply(implist_weighted, function(x) max(x$iptw_weights))
summary(weight_max)

weight_stats <- data.frame(
  imputation = 1:45,
  mean = sapply(implist_weighted, function(x) mean(x$iptw_weights)),
  sd = sapply(implist_weighted, function(x) sd(x$iptw_weights)),
  min = sapply(implist_weighted, function(x) min(x$iptw_weights)),
  max = sapply(implist_weighted, function(x) max(x$iptw_weights)),
  p99 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.99)),
  p95 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.95)),
  p90 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.90))
)

# Summary across imputations
summary(weight_stats)

# Visualize
library(ggplot2)
ggplot(stack(implist_weighted[[1]], select = "iptw_weights"), aes(x = values)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_log10() +  # Weights often right-skewed
  labs(title = "Weight Distribution (Imputation 1)", x = "IPTW Weights (log scale)")

weight_flags <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(implist_weighted, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(implist_weighted, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(implist_weighted, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(implist_weighted, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(implist_weighted, nrow)
)

weight_flags$ess_percent <- weight_flags$ess / weight_flags$n * 100
weight_flags$ess_lt_50 <- weight_flags$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags)

# Coefficient of variation across imputations
cv_across_imps <- sd(weight_stats$mean) / mean(weight_stats$mean)
# If > 0.1-0.2, weights are sensitive to imputation model

preg_covid_norisk_imp <- implist_weighted
save(preg_covid_norisk_imp, file = "Pregnant_Covid_NoRisk_Weights_SA.rdata")
########################################################################################################################
cov_preg_flu_norisk <- readRDS("Path_to_Cov_Imputed_Pregnancy_Influenza_Norisk_SA.rds")
preg_flu_norisk_imp <- complete(cov_preg_flu_norisk, action = "all")

# Function to apply IPTW and return a survey design object
apply_iptw <- function(data) {
  # Calculate weights (example: ATE weights for binary treatment)
  w <- weightit(mental ~ age_at_enrollment_categorized + county_2017 + country_of_birth
                + smoking + parity + profession + income_2017_cat,
                data = data,
                method = "ps",        # Propensity score
                estimand = "ATE",     # or "ATT", "ATC"
                stabilize = TRUE)    # Stabilized weights recommended
  
  # Add weights to dataset
  data$iptw_weights <- w$weights
  
  return(data)
}

# Apply to all imputed datasets
implist_weighted <- lapply(preg_flu_norisk_imp, apply_iptw)

# Check mean weights across datasets
weight_means <- sapply(implist_weighted, function(x) mean(x$iptw_weights))
summary(weight_means)

# Check max weights
weight_max <- sapply(implist_weighted, function(x) max(x$iptw_weights))
summary(weight_max)

weight_stats <- data.frame(
  imputation = 1:45,
  mean = sapply(implist_weighted, function(x) mean(x$iptw_weights)),
  sd = sapply(implist_weighted, function(x) sd(x$iptw_weights)),
  min = sapply(implist_weighted, function(x) min(x$iptw_weights)),
  max = sapply(implist_weighted, function(x) max(x$iptw_weights)),
  p99 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.99)),
  p95 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.95)),
  p90 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.90))
)

# Summary across imputations
summary(weight_stats)

# Visualize
library(ggplot2)
ggplot(stack(implist_weighted[[1]], select = "iptw_weights"), aes(x = values)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_log10() +  # Weights often right-skewed
  labs(title = "Weight Distribution (Imputation 1)", x = "IPTW Weights (log scale)")

weight_flags <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(implist_weighted, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(implist_weighted, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(implist_weighted, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(implist_weighted, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(implist_weighted, nrow)
)

weight_flags$ess_percent <- weight_flags$ess / weight_flags$n * 100
weight_flags$ess_lt_50 <- weight_flags$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags)

# Coefficient of variation across imputations
cv_across_imps <- sd(weight_stats$mean) / mean(weight_stats$mean)
# If > 0.1-0.2, weights are sensitive to imputation model

preg_flu_norisk_imp <- implist_weighted
save(preg_flu_norisk_imp, file = "Pregnant_Influenza_NoRisk_Weights_SA.rdata")
####################################################################################################################
cov_preg_flu_risk <- readRDS("Path_to_Cov_Imputed_Pregnancy_Influenza_risk_SA.rds")
preg_flu_risk_imp <- complete(cov_preg_flu_risk, action = "all")

# Function to apply IPTW and return a survey design object
apply_iptw <- function(data) {
  # Calculate weights (example: ATE weights for binary treatment)
  w <- weightit(mental ~ age_at_enrollment_categorized + county_2017 + country_of_birth
                + smoking + parity + profession + income_2017_cat,
                data = data,
                method = "ps",        # Propensity score
                estimand = "ATE",     # or "ATT", "ATC"
                stabilize = TRUE)    # Stabilized weights recommended
  
  # Add weights to dataset
  data$iptw_weights <- w$weights
  
  return(data)
}

# Apply to all imputed datasets
implist_weighted <- lapply(preg_flu_risk_imp, apply_iptw)

# Check mean weights across datasets
weight_means <- sapply(implist_weighted, function(x) mean(x$iptw_weights))
summary(weight_means)

# Check max weights
weight_max <- sapply(implist_weighted, function(x) max(x$iptw_weights))
summary(weight_max)

weight_stats <- data.frame(
  imputation = 1:45,
  mean = sapply(implist_weighted, function(x) mean(x$iptw_weights)),
  sd = sapply(implist_weighted, function(x) sd(x$iptw_weights)),
  min = sapply(implist_weighted, function(x) min(x$iptw_weights)),
  max = sapply(implist_weighted, function(x) max(x$iptw_weights)),
  p99 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.99)),
  p95 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.95)),
  p90 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.90))
)

# Summary across imputations
summary(weight_stats)

# Visualize
library(ggplot2)
ggplot(stack(implist_weighted[[1]], select = "iptw_weights"), aes(x = values)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_log10() +  # Weights often right-skewed
  labs(title = "Weight Distribution (Imputation 1)", x = "IPTW Weights (log scale)")

weight_flags <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(implist_weighted, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(implist_weighted, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(implist_weighted, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(implist_weighted, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(implist_weighted, nrow)
)

weight_flags$ess_percent <- weight_flags$ess / weight_flags$n * 100
weight_flags$ess_lt_50 <- weight_flags$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags)

# Coefficient of variation across imputations
cv_across_imps <- sd(weight_stats$mean) / mean(weight_stats$mean)
# If > 0.1-0.2, weights are sensitive to imputation model

preg_flu_risk_imp <- implist_weighted
save(preg_flu_risk_imp, file = "Pregnant_Influenza_Risk_Weights_SA.rdata")
###############################################################################################
cov_old_flu <- readRDS("Path_to_Cov_Imputed_Old_Influenza_SA.rds")
old_flu_imp <- complete(cov_old_flu, action = "all")

# Function to apply IPTW and return a survey design object
apply_iptw <- function(data) {
  # Calculate weights (example: ATE weights for binary treatment)
  w <- weightit(Any_MH ~ age_at_enrollment_categorized + county_2017 + country_of_birth
                + risk_factor + income_2017_cat,
                data = data,
                method = "ps",        # Propensity score
                estimand = "ATE",     # or "ATT", "ATC"
                stabilize = TRUE)    # Stabilized weights recommended
  
  # Add weights to dataset
  data$iptw_weights <- w$weights
  
  return(data)
}

# Apply to all imputed datasets
implist_weighted <- lapply(old_flu_imp, apply_iptw)

# Check mean weights across datasets
weight_means <- sapply(implist_weighted, function(x) mean(x$iptw_weights))
summary(weight_means)

# Check max weights
weight_max <- sapply(implist_weighted, function(x) max(x$iptw_weights))
summary(weight_max)

weight_stats <- data.frame(
  imputation = 1:30,
  mean = sapply(implist_weighted, function(x) mean(x$iptw_weights)),
  sd = sapply(implist_weighted, function(x) sd(x$iptw_weights)),
  min = sapply(implist_weighted, function(x) min(x$iptw_weights)),
  max = sapply(implist_weighted, function(x) max(x$iptw_weights)),
  p99 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.99)),
  p95 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.95)),
  p90 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.90))
)

# Summary across imputations
summary(weight_stats)

# Visualize
library(ggplot2)
ggplot(stack(implist_weighted[[1]], select = "iptw_weights"), aes(x = values)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_log10() +  # Weights often right-skewed
  labs(title = "Weight Distribution (Imputation 1)", x = "IPTW Weights (log scale)")

weight_flags <- data.frame(
  imputation = 1:30,
  max_gt_10 = sapply(implist_weighted, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(implist_weighted, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(implist_weighted, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(implist_weighted, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(implist_weighted, nrow)
)

weight_flags$ess_percent <- weight_flags$ess / weight_flags$n * 100
weight_flags$ess_lt_50 <- weight_flags$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags)

# Coefficient of variation across imputations
cv_across_imps <- sd(weight_stats$mean) / mean(weight_stats$mean)
# If > 0.1-0.2, weights are sensitive to imputation model

old_flu_imp <- implist_weighted
save(old_flu_imp, file = "Old_Influenza_Weights_SA.rdata")
##########################################################################################################################
cov_old_covid <- readRDS("Path_to_Cov_Imputed_Old_Covid_SA.rds")
old_covid_imp <- complete(cov_old_covid, action = "all")

# Function to apply IPTW and return a survey design object
apply_iptw <- function(data) {
  # Calculate weights (example: ATE weights for binary treatment)
  w <- weightit(mental ~ age_at_enrollment_categorized + county_2021 + country_of_birth
                + risk_factor + income_2021_cat,
                data = data,
                method = "ps",        # Propensity score
                estimand = "ATE",     # or "ATT", "ATC"
                stabilize = TRUE)    # Stabilized weights recommended
  
  # Add weights to dataset
  data$iptw_weights <- w$weights
  
  return(data)
}

# Apply to all imputed datasets
implist_weighted <- lapply(old_covid_imp, apply_iptw)

# Check mean weights across datasets
weight_means <- sapply(implist_weighted, function(x) mean(x$iptw_weights))
summary(weight_means)

# Check max weights
weight_max <- sapply(implist_weighted, function(x) max(x$iptw_weights))
summary(weight_max)

weight_stats <- data.frame(
  imputation = 1:30,
  mean = sapply(implist_weighted, function(x) mean(x$iptw_weights)),
  sd = sapply(implist_weighted, function(x) sd(x$iptw_weights)),
  min = sapply(implist_weighted, function(x) min(x$iptw_weights)),
  max = sapply(implist_weighted, function(x) max(x$iptw_weights)),
  p99 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.99)),
  p95 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.95)),
  p90 = sapply(implist_weighted, function(x) quantile(x$iptw_weights, 0.90))
)

# Summary across imputations
summary(weight_stats)

# Visualize
library(ggplot2)
ggplot(stack(implist_weighted[[1]], select = "iptw_weights"), aes(x = values)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_log10() +  # Weights often right-skewed
  labs(title = "Weight Distribution (Imputation 1)", x = "IPTW Weights (log scale)")

weight_flags <- data.frame(
  imputation = 1:30,
  max_gt_10 = sapply(implist_weighted, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(implist_weighted, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(implist_weighted, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(implist_weighted, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(implist_weighted, nrow)
)

weight_flags$ess_percent <- weight_flags$ess / weight_flags$n * 100
weight_flags$ess_lt_50 <- weight_flags$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags)

# Coefficient of variation across imputations
cv_across_imps <- sd(weight_stats$mean) / mean(weight_stats$mean)
# If > 0.1-0.2, weights are sensitive to imputation model

old_covid_imp <- implist_weighted
save(old_covid_imp, file = "Old_COVID_Weights_SA.rdata")
##########################################################################################
# Now let's trim the extreme weights
# Old Covid weights do not need trimming as the mean_to_max ratio was less than 10.

# Start with old flu
# Load required libraries
library(WeightIt)
library(cobalt)
library(mice)  # if you need to work with the mids object

# SEQUENTIAL SCRIPT FOR TRIMMING ALL 45 WEIGHTIT OBJECTS

# Step 1: Create a function to trim weights at 1st and 99th percentiles
trim_weights <- function(w.out, lower_percentile = 0.01, upper_percentile = 0.99) {
  # Extract original weights
  weights_orig <- w.out$iptw_weights
  
  # Calculate percentiles
  lower_bound <- quantile(weights_orig, lower_percentile)
  upper_bound <- quantile(weights_orig, upper_percentile)
  
  # Trim the weights
  weights_trimmed <- ifelse(weights_orig < lower_bound, lower_bound,
                            ifelse(weights_orig > upper_bound, upper_bound, weights_orig))
  
  # Create a new weightit object with trimmed weights
  w.out_trimmed <- w.out
  w.out_trimmed$weights <- weights_trimmed
  
  # Add info about trimming
  attr(w.out_trimmed, "trim_info") <- list(
    original_range = range(weights_orig),
    trimmed_range = range(weights_trimmed),
    percent_trimmed_lower = mean(weights_orig < lower_bound) * 100,
    percent_trimmed_upper = mean(weights_orig > upper_bound) * 100,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_trimmed_lower = sum(weights_orig < lower_bound),
    n_trimmed_upper = sum(weights_orig > upper_bound)
  )
  
  
  return(w.out_trimmed)
  return(statistics)
}

# Step 2: Apply trimming to all 45 weightit objects
weightit_trimmed_list <- lapply(old_flu_imp, trim_weights)

# Step 3: Compare original vs trimmed weights for each imputation
# Check results for first few imputations
for(i in 1:min(3, length(weightit_trimmed_list))) {
  cat("\n=== Imputation", i, "===\n")
  cat("Original weights - Range:", 
      round(range(old_flu_imp[[i]]$iptw_weights)[1], 2), "to", 
      round(range(old_flu_imp[[i]]$iptw_weights)[2], 2), "\n")
  cat("Trimmed weights - Range:", 
      round(range(weightit_trimmed_list[[i]]$weights)[1], 2), "to", 
      round(range(weightit_trimmed_list[[i]]$weights)[2], 2), "\n")
  # Access trim info
  trim_info <- attr(weightit_trimmed_list[[i]], "trim_info")
  cat("Trimmed lower:", trim_info$n_trimmed_lower, "obs (", 
      round(trim_info$percent_trimmed_lower, 2), "%)\n")
  cat("Trimmed upper:", trim_info$n_trimmed_upper, "obs (", 
      round(trim_info$percent_trimmed_upper, 2), "%)\n")
}

weight_flags_before <- data.frame(
  imputation = 1:30,
  max_gt_10 = sapply(old_flu_imp, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(old_flu_imp, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(old_flu_imp, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(old_flu_imp, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(old_flu_imp, nrow)
)

weight_flags_before$ess_percent <- weight_flags_before$ess / weight_flags_before$n * 100
weight_flags_before$ess_lt_50 <- weight_flags_before$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_before)

weight_flags_after <- data.frame(
  imputation = 1:30,
  max_gt_10 = sapply(weightit_trimmed_list, function(x) max(x$weights) > 10),
  max_mean_ratio = sapply(weightit_trimmed_list, function(x) max(x$weights) / mean(x$weights)),
  cv = sapply(weightit_trimmed_list, function(x) sd(x$weights) / mean(x$weights)),
  ess = sapply(weightit_trimmed_list, function(x) (sum(x$weights)^2) / sum(x$weights^2)),
  n = sapply(weightit_trimmed_list, nrow)
)

weight_flags_after$ess_percent <- weight_flags_after$ess / weight_flags_after$n * 100
weight_flags_after$ess_lt_50 <- weight_flags_after$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_after)
old_flu_imp_iptw <- weightit_trimmed_list
save(old_flu_imp_iptw, file = "Old_Influenza_Weight_Corrected_SA.rdata")

###################################
# Pregnant flu risk
# Step 1: Create a function to trim weights at 1st and 99th percentiles
trim_weights <- function(w.out, lower_percentile = 0.01, upper_percentile = 0.99) {
  # Extract original weights
  weights_orig <- w.out$iptw_weights
  
  # Calculate percentiles
  lower_bound <- quantile(weights_orig, lower_percentile)
  upper_bound <- quantile(weights_orig, upper_percentile)
  
  # Trim the weights
  weights_trimmed <- ifelse(weights_orig < lower_bound, lower_bound,
                            ifelse(weights_orig > upper_bound, upper_bound, weights_orig))
  
  # Create a new weightit object with trimmed weights
  w.out_trimmed <- w.out
  w.out_trimmed$weights <- weights_trimmed
  
  # Add info about trimming
  attr(w.out_trimmed, "trim_info") <- list(
    original_range = range(weights_orig),
    trimmed_range = range(weights_trimmed),
    percent_trimmed_lower = mean(weights_orig < lower_bound) * 100,
    percent_trimmed_upper = mean(weights_orig > upper_bound) * 100,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_trimmed_lower = sum(weights_orig < lower_bound),
    n_trimmed_upper = sum(weights_orig > upper_bound)
  )
  
  
  return(w.out_trimmed)

}

# Step 2: Apply trimming to all 45 weightit objects
weightit_trimmed_list <- lapply(preg_flu_risk_imp, trim_weights)

# Step 3: Compare original vs trimmed weights for each imputation
# Check results for first few imputations
for(i in 1:min(3, length(weightit_trimmed_list))) {
  cat("\n=== Imputation", i, "===\n")
  cat("Original weights - Range:", 
      round(range(preg_flu_risk_imp[[i]]$iptw_weights)[1], 2), "to", 
      round(range(preg_flu_risk_imp[[i]]$iptw_weights)[2], 2), "\n")
  cat("Trimmed weights - Range:", 
      round(range(weightit_trimmed_list[[i]]$weights)[1], 2), "to", 
      round(range(weightit_trimmed_list[[i]]$weights)[2], 2), "\n")
  # Access trim info
  trim_info <- attr(weightit_trimmed_list[[i]], "trim_info")
  cat("Trimmed lower:", trim_info$n_trimmed_lower, "obs (", 
      round(trim_info$percent_trimmed_lower, 2), "%)\n")
  cat("Trimmed upper:", trim_info$n_trimmed_upper, "obs (", 
      round(trim_info$percent_trimmed_upper, 2), "%)\n")
}

weight_flags_before <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(preg_flu_risk_imp, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(preg_flu_risk_imp, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(preg_flu_risk_imp, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(preg_flu_risk_imp, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(preg_flu_risk_imp, nrow)
)

weight_flags_before$ess_percent <- weight_flags_before$ess / weight_flags_before$n * 100
weight_flags_before$ess_lt_50 <- weight_flags_before$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_before)

weight_flags_after <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(weightit_trimmed_list, function(x) max(x$weights) > 10),
  max_mean_ratio = sapply(weightit_trimmed_list, function(x) max(x$weights) / mean(x$weights)),
  cv = sapply(weightit_trimmed_list, function(x) sd(x$weights) / mean(x$weights)),
  ess = sapply(weightit_trimmed_list, function(x) (sum(x$weights)^2) / sum(x$weights^2)),
  n = sapply(weightit_trimmed_list, nrow)
)

weight_flags_after$ess_percent <- weight_flags_after$ess / weight_flags_after$n * 100
weight_flags_after$ess_lt_50 <- weight_flags_after$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_after)
preg_flu_risk_imp_iptw <- weightit_trimmed_list
save(preg_flu_risk_imp_iptw, file = "Pregnant_Influenza_Risk_Weight_Corrected_SA.rdata")
###################################
# Pregnant flu norisk
# Step 1: Create a function to trim weights at 1st and 99th percentiles
trim_weights <- function(w.out, lower_percentile = 0.01, upper_percentile = 0.99) {
  # Extract original weights
  weights_orig <- w.out$iptw_weights
  
  # Calculate percentiles
  lower_bound <- quantile(weights_orig, lower_percentile)
  upper_bound <- quantile(weights_orig, upper_percentile)
  
  # Trim the weights
  weights_trimmed <- ifelse(weights_orig < lower_bound, lower_bound,
                            ifelse(weights_orig > upper_bound, upper_bound, weights_orig))
  
  # Create a new weightit object with trimmed weights
  w.out_trimmed <- w.out
  w.out_trimmed$weights <- weights_trimmed
  
  # Add info about trimming
  attr(w.out_trimmed, "trim_info") <- list(
    original_range = range(weights_orig),
    trimmed_range = range(weights_trimmed),
    percent_trimmed_lower = mean(weights_orig < lower_bound) * 100,
    percent_trimmed_upper = mean(weights_orig > upper_bound) * 100,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_trimmed_lower = sum(weights_orig < lower_bound),
    n_trimmed_upper = sum(weights_orig > upper_bound)
  )
  
  
  return(w.out_trimmed)
  
}

# Step 2: Apply trimming to all 45 weightit objects
weightit_trimmed_list <- lapply(preg_flu_norisk_imp, trim_weights)

# Step 3: Compare original vs trimmed weights for each imputation
# Check results for first few imputations
for(i in 1:min(3, length(weightit_trimmed_list))) {
  cat("\n=== Imputation", i, "===\n")
  cat("Original weights - Range:", 
      round(range(preg_flu_norisk_imp[[i]]$iptw_weights)[1], 2), "to", 
      round(range(preg_flu_norisk_imp[[i]]$iptw_weights)[2], 2), "\n")
  cat("Trimmed weights - Range:", 
      round(range(weightit_trimmed_list[[i]]$weights)[1], 2), "to", 
      round(range(weightit_trimmed_list[[i]]$weights)[2], 2), "\n")
  # Access trim info
  trim_info <- attr(weightit_trimmed_list[[i]], "trim_info")
  cat("Trimmed lower:", trim_info$n_trimmed_lower, "obs (", 
      round(trim_info$percent_trimmed_lower, 2), "%)\n")
  cat("Trimmed upper:", trim_info$n_trimmed_upper, "obs (", 
      round(trim_info$percent_trimmed_upper, 2), "%)\n")
}

weight_flags_before <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(preg_flu_norisk_imp, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(preg_flu_norisk_imp, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(preg_flu_norisk_imp, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(preg_flu_norisk_imp, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(preg_flu_norisk_imp, nrow)
)

weight_flags_before$ess_percent <- weight_flags_before$ess / weight_flags_before$n * 100
weight_flags_before$ess_lt_50 <- weight_flags_before$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_before)

weight_flags_after <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(weightit_trimmed_list, function(x) max(x$weights) > 10),
  max_mean_ratio = sapply(weightit_trimmed_list, function(x) max(x$weights) / mean(x$weights)),
  cv = sapply(weightit_trimmed_list, function(x) sd(x$weights) / mean(x$weights)),
  ess = sapply(weightit_trimmed_list, function(x) (sum(x$weights)^2) / sum(x$weights^2)),
  n = sapply(weightit_trimmed_list, nrow)
)

weight_flags_after$ess_percent <- weight_flags_after$ess / weight_flags_after$n * 100
weight_flags_after$ess_lt_50 <- weight_flags_after$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_after)
preg_flu_norisk_imp_iptw <- weightit_trimmed_list
save(preg_flu_norisk_imp_iptw, file = "Pregnant_Influenza_NoRisk_Weight_Corrected_SA.rdata")
############################################3
# Pregnant covid norisk
# Step 1: Create a function to trim weights at 1st and 99th percentiles
trim_weights <- function(w.out, lower_percentile = 0.01, upper_percentile = 0.99) {
  # Extract original weights
  weights_orig <- w.out$iptw_weights
  
  # Calculate percentiles
  lower_bound <- quantile(weights_orig, lower_percentile)
  upper_bound <- quantile(weights_orig, upper_percentile)
  
  # Trim the weights
  weights_trimmed <- ifelse(weights_orig < lower_bound, lower_bound,
                            ifelse(weights_orig > upper_bound, upper_bound, weights_orig))
  
  # Create a new weightit object with trimmed weights
  w.out_trimmed <- w.out
  w.out_trimmed$weights <- weights_trimmed
  
  # Add info about trimming
  attr(w.out_trimmed, "trim_info") <- list(
    original_range = range(weights_orig),
    trimmed_range = range(weights_trimmed),
    percent_trimmed_lower = mean(weights_orig < lower_bound) * 100,
    percent_trimmed_upper = mean(weights_orig > upper_bound) * 100,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_trimmed_lower = sum(weights_orig < lower_bound),
    n_trimmed_upper = sum(weights_orig > upper_bound)
  )
  
  
  return(w.out_trimmed)
  
}

# Step 2: Apply trimming to all 45 weightit objects
weightit_trimmed_list <- lapply(preg_covid_norisk_imp, trim_weights)

# Step 3: Compare original vs trimmed weights for each imputation
# Check results for first few imputations
for(i in 1:min(3, length(weightit_trimmed_list))) {
  cat("\n=== Imputation", i, "===\n")
  cat("Original weights - Range:", 
      round(range(preg_covid_norisk_imp[[i]]$iptw_weights)[1], 2), "to", 
      round(range(preg_covid_norisk_imp[[i]]$iptw_weights)[2], 2), "\n")
  cat("Trimmed weights - Range:", 
      round(range(weightit_trimmed_list[[i]]$weights)[1], 2), "to", 
      round(range(weightit_trimmed_list[[i]]$weights)[2], 2), "\n")
  # Access trim info
  trim_info <- attr(weightit_trimmed_list[[i]], "trim_info")
  cat("Trimmed lower:", trim_info$n_trimmed_lower, "obs (", 
      round(trim_info$percent_trimmed_lower, 2), "%)\n")
  cat("Trimmed upper:", trim_info$n_trimmed_upper, "obs (", 
      round(trim_info$percent_trimmed_upper, 2), "%)\n")
}

weight_flags_before <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(preg_covid_norisk_imp, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(preg_covid_norisk_imp, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(preg_covid_norisk_imp, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(preg_covid_norisk_imp, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(preg_covid_norisk_imp, nrow)
)

weight_flags_before$ess_percent <- weight_flags_before$ess / weight_flags_before$n * 100
weight_flags_before$ess_lt_50 <- weight_flags_before$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_before)

weight_flags_after <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(weightit_trimmed_list, function(x) max(x$weights) > 10),
  max_mean_ratio = sapply(weightit_trimmed_list, function(x) max(x$weights) / mean(x$weights)),
  cv = sapply(weightit_trimmed_list, function(x) sd(x$weights) / mean(x$weights)),
  ess = sapply(weightit_trimmed_list, function(x) (sum(x$weights)^2) / sum(x$weights^2)),
  n = sapply(weightit_trimmed_list, nrow)
)

weight_flags_after$ess_percent <- weight_flags_after$ess / weight_flags_after$n * 100
weight_flags_after$ess_lt_50 <- weight_flags_after$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_after)
preg_covid_norisk_imp_iptw <- weightit_trimmed_list
save(preg_covid_norisk_imp_iptw, file = "Pregnant_COVID_NoRisk_Weight_Corrected_SA.rdata")
##################################3
# Pregnant covid risk
# Step 1: Create a function to trim weights at 1st and 99th percentiles
trim_weights <- function(w.out, lower_percentile = 0.01, upper_percentile = 0.99) {
  # Extract original weights
  weights_orig <- w.out$iptw_weights
  
  # Calculate percentiles
  lower_bound <- quantile(weights_orig, lower_percentile)
  upper_bound <- quantile(weights_orig, upper_percentile)
  
  # Trim the weights
  weights_trimmed <- ifelse(weights_orig < lower_bound, lower_bound,
                            ifelse(weights_orig > upper_bound, upper_bound, weights_orig))
  
  # Create a new weightit object with trimmed weights
  w.out_trimmed <- w.out
  w.out_trimmed$weights <- weights_trimmed
  
  # Add info about trimming
  attr(w.out_trimmed, "trim_info") <- list(
    original_range = range(weights_orig),
    trimmed_range = range(weights_trimmed),
    percent_trimmed_lower = mean(weights_orig < lower_bound) * 100,
    percent_trimmed_upper = mean(weights_orig > upper_bound) * 100,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_trimmed_lower = sum(weights_orig < lower_bound),
    n_trimmed_upper = sum(weights_orig > upper_bound)
  )
  
  
  return(w.out_trimmed)
  
}

# Step 2: Apply trimming to all 45 weightit objects
weightit_trimmed_list <- lapply(preg_covid_risk_imp, trim_weights)

# Step 3: Compare original vs trimmed weights for each imputation
# Check results for first few imputations
for(i in 1:min(3, length(weightit_trimmed_list))) {
  cat("\n=== Imputation", i, "===\n")
  cat("Original weights - Range:", 
      round(range(preg_covid_risk_imp[[i]]$iptw_weights)[1], 2), "to", 
      round(range(preg_covid_risk_imp[[i]]$iptw_weights)[2], 2), "\n")
  cat("Trimmed weights - Range:", 
      round(range(weightit_trimmed_list[[i]]$weights)[1], 2), "to", 
      round(range(weightit_trimmed_list[[i]]$weights)[2], 2), "\n")
  # Access trim info
  trim_info <- attr(weightit_trimmed_list[[i]], "trim_info")
  cat("Trimmed lower:", trim_info$n_trimmed_lower, "obs (", 
      round(trim_info$percent_trimmed_lower, 2), "%)\n")
  cat("Trimmed upper:", trim_info$n_trimmed_upper, "obs (", 
      round(trim_info$percent_trimmed_upper, 2), "%)\n")
}

weight_flags_before <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(preg_covid_risk_imp, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(preg_covid_risk_imp, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(preg_covid_risk_imp, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(preg_covid_risk_imp, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(preg_covid_risk_imp, nrow)
)

weight_flags_before$ess_percent <- weight_flags_before$ess / weight_flags_before$n * 100
weight_flags_before$ess_lt_50 <- weight_flags_before$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_before)

weight_flags_after <- data.frame(
  imputation = 1:45,
  max_gt_10 = sapply(weightit_trimmed_list, function(x) max(x$weights) > 10),
  max_mean_ratio = sapply(weightit_trimmed_list, function(x) max(x$weights) / mean(x$weights)),
  cv = sapply(weightit_trimmed_list, function(x) sd(x$weights) / mean(x$weights)),
  ess = sapply(weightit_trimmed_list, function(x) (sum(x$weights)^2) / sum(x$weights^2)),
  n = sapply(weightit_trimmed_list, nrow)
)

weight_flags_after$ess_percent <- weight_flags_after$ess / weight_flags_after$n * 100
weight_flags_after$ess_lt_50 <- weight_flags_after$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_after)
preg_covid_risk_imp_iptw <- weightit_trimmed_list
save(preg_covid_risk_imp_iptw, file = "Pregnant_COVID_Risk_Weight_Corrected_SA.rdata")
##################################################################################
# Step 1: Create a function to trim weights at 1st and 99th percentiles
trim_weights <- function(w.out, lower_percentile = 0.01, upper_percentile = 0.99) {
  # Extract original weights
  weights_orig <- w.out$iptw_weights
  
  # Calculate percentiles
  lower_bound <- quantile(weights_orig, lower_percentile)
  upper_bound <- quantile(weights_orig, upper_percentile)
  
  # Trim the weights
  weights_trimmed <- ifelse(weights_orig < lower_bound, lower_bound,
                            ifelse(weights_orig > upper_bound, upper_bound, weights_orig))
  
  # Create a new weightit object with trimmed weights
  w.out_trimmed <- w.out
  w.out_trimmed$weights <- weights_trimmed
  
  # Add info about trimming
  attr(w.out_trimmed, "trim_info") <- list(
    original_range = range(weights_orig),
    trimmed_range = range(weights_trimmed),
    percent_trimmed_lower = mean(weights_orig < lower_bound) * 100,
    percent_trimmed_upper = mean(weights_orig > upper_bound) * 100,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_trimmed_lower = sum(weights_orig < lower_bound),
    n_trimmed_upper = sum(weights_orig > upper_bound)
  )
  
  
  return(w.out_trimmed)
  return(statistics)
}

# Step 2: Apply trimming to all 45 weightit objects
weightit_trimmed_list <- lapply(old_covid_imp, trim_weights)

# Step 3: Compare original vs trimmed weights for each imputation
# Check results for first few imputations
for(i in 1:min(3, length(weightit_trimmed_list))) {
  cat("\n=== Imputation", i, "===\n")
  cat("Original weights - Range:", 
      round(range(old_flu_imp[[i]]$iptw_weights)[1], 2), "to", 
      round(range(old_flu_imp[[i]]$iptw_weights)[2], 2), "\n")
  cat("Trimmed weights - Range:", 
      round(range(weightit_trimmed_list[[i]]$weights)[1], 2), "to", 
      round(range(weightit_trimmed_list[[i]]$weights)[2], 2), "\n")
  # Access trim info
  trim_info <- attr(weightit_trimmed_list[[i]], "trim_info")
  cat("Trimmed lower:", trim_info$n_trimmed_lower, "obs (", 
      round(trim_info$percent_trimmed_lower, 2), "%)\n")
  cat("Trimmed upper:", trim_info$n_trimmed_upper, "obs (", 
      round(trim_info$percent_trimmed_upper, 2), "%)\n")
}

weight_flags_before <- data.frame(
  imputation = 1:30,
  max_gt_10 = sapply(old_flu_imp, function(x) max(x$iptw_weights) > 10),
  max_mean_ratio = sapply(old_flu_imp, function(x) max(x$iptw_weights) / mean(x$iptw_weights)),
  cv = sapply(old_flu_imp, function(x) sd(x$iptw_weights) / mean(x$iptw_weights)),
  ess = sapply(old_flu_imp, function(x) (sum(x$iptw_weights)^2) / sum(x$iptw_weights^2)),
  n = sapply(old_flu_imp, nrow)
)

weight_flags_before$ess_percent <- weight_flags_before$ess / weight_flags_before$n * 100
weight_flags_before$ess_lt_50 <- weight_flags_before$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_before)

weight_flags_after <- data.frame(
  imputation = 1:30,
  max_gt_10 = sapply(weightit_trimmed_list, function(x) max(x$weights) > 10),
  max_mean_ratio = sapply(weightit_trimmed_list, function(x) max(x$weights) / mean(x$weights)),
  cv = sapply(weightit_trimmed_list, function(x) sd(x$weights) / mean(x$weights)),
  ess = sapply(weightit_trimmed_list, function(x) (sum(x$weights)^2) / sum(x$weights^2)),
  n = sapply(weightit_trimmed_list, nrow)
)

weight_flags_after$ess_percent <- weight_flags_after$ess / weight_flags_after$n * 100
weight_flags_after$ess_lt_50 <- weight_flags_after$ess_percent < 50

# How many imputations exceed thresholds?
summary(weight_flags_after)
old_covid_imp_iptw <- weightit_trimmed_list
save(old_covid_imp_iptw, file = "Old_COVID19_Weight_Corrected_SA.rdata")




