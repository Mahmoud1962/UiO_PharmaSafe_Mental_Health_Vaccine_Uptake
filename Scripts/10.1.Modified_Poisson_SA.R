################################################################################
# Script: 10.1.Modified_Poisson.R
#
# Purpose:
#   Fit modified poisson models with robust variance estimators for each 
#   subpopulation across all mental health conditions. Results from all 
#   imputations are pooled using Rubin's rules. This is done on populations
#   where the exposure to mental health conditions was defined during all 
#   available lookback time.
################################################################################
library(lmtest)
library(sandwich)
library(dplyr)
library(tidyr)

load("Path_to_Pregnant_COVID_Risk_Weight_Corrected.rdata")
load("Path_to_Pregnant_Risk_COVID_Population_MH_corrected.rdata")
pregnant_covid_risk <- pregnant_covid_risk %>% group_by(Preg_id) %>%
  mutate(mental = max(mental),
         mixed = max(mixed),
         depression_mh = max(depression_mh),
         anxiety_mh = max(anxiety_mh),
         bipolar_mh = max(bipolar_mh),
         PTSD_mh = max(PTSD_mh),
         OCD_mh = max(OCD_mh),
         ADHD_mh = max(ADHD_mh),
         vaccinated = max(vaccinated))
pregnant_covid_risk <- pregnant_covid_risk[!duplicated(pregnant_covid_risk$Preg_id),]

load("Path_to_Pregnant_NoRisk_COVID_Population_MH_corrected.rdata")
pregnant_covid_norisk <- pregnant_covid_norisk %>% group_by(Preg_id) %>%
  mutate(mental = max(mental),
         mixed = max(mixed),
         depression_mh = max(depression_mh),
         anxiety_mh = max(anxiety_mh),
         bipolar_mh = max(bipolar_mh),
         PTSD_mh = max(PTSD_mh),
         OCD_mh = max(OCD_mh),
         ADHD_mh = max(ADHD_mh),
         vaccinated = max(vaccinated))
pregnant_covid_norisk <- pregnant_covid_norisk[!duplicated(pregnant_covid_norisk$Preg_id),]

load("Path_to_Pregnant_NoRisk_Influenza_Population_MH_corrected.rdata")
pregnant_flu_norisk <- pregnant_flu_norisk %>% group_by(Preg_id) %>%
  mutate(mental = max(mental),
         mixed = max(mixed),
         depression_mh = max(depression_mh),
         anxiety_mh = max(anxiety_mh),
         bipolar_mh = max(bipolar_mh),
         PTSD_mh = max(PTSD_mh),
         OCD_mh = max(OCD_mh),
         ADHD_mh = max(ADHD_mh),
         season1 = max(season1),
         season2 = max(season2),
         season3 = max(season3),
         all.seasons = max(all.seasons),
         vaccinated = max(vaccinated))
pregnant_flu_norisk <- pregnant_flu_norisk[!duplicated(pregnant_flu_norisk$Preg_id),]

load("Path_to_Pregnant_Risk_Influenza_Population_MH_corrected.rdata")
pregnant_flu_risk <- pregnant_flu_risk %>% group_by(Preg_id) %>%
  mutate(mental = max(mental),
         mixed = max(mixed),
         depression_mh = max(depression_mh),
         anxiety_mh = max(anxiety_mh),
         bipolar_mh = max(bipolar_mh),
         PTSD_mh = max(PTSD_mh),
         OCD_mh = max(OCD_mh),
         ADHD_mh = max(ADHD_mh),
         season1 = max(season1),
         season2 = max(season2),
         season3 = max(season3),
         all.seasons = max(all.seasons),
         vaccinated = max(vaccinated))
pregnant_flu_risk <- pregnant_flu_risk[!duplicated(pregnant_flu_risk$Preg_id),]
pregnant_flu_risk <- pregnant_flu_risk[pregnant_flu_risk$eligible_risk == 1,]
pregnant_flu_norisk <- pregnant_flu_norisk[pregnant_flu_norisk$eligible_norisk == 1,]

load("Path_to_Old_Influenza_Population_MH_corrected.rdata")
old_flu <- old_flu %>% group_by(person_id) %>%
  mutate(mental = max(mental),
         mixed = max(mixed),
         depression_mh = max(depression_mh),
         anxiety_mh = max(anxiety_mh),
         bipolar_mh = max(bipolar_mh),
         PTSD_mh = max(PTSD_mh),
         OCD_mh = max(OCD_mh),
         ADHD_mh = max(ADHD_mh),
         season1 = max(season1),
         season2 = max(season2),
         season3 = max(season3),
         all.seasons = max(all.seasons),
        vaccinated = max(vaccinated))
old_flu <- old_flu[!duplicated(old_flu$person_id),]
old_flu <- old_flu[old_flu$eligible == 1,]

load("Path_to_Old_COVID_Population_MH_corrected..rdata")
old_covid <- old_covid %>% group_by(person_id) %>%
  mutate(mental = max(mental),
         mixed = max(mixed),
         depression_mh = max(depression_mh),
         anxiety_mh = max(anxiety_mh),
         bipolar_mh = max(bipolar_mh),
         PTSD_mh = max(PTSD_mh),
         OCD_mh = max(OCD_mh),
         ADHD_mh = max(ADHD_mh),
         is_age_eligible_primary = max(is_age_eligible_primary),
         is_age_eligible_booster1 = max(is_age_eligible_booster1),
         is_age_eligible_booster2 = max(is_age_eligible_booster2),
         received_valid_booster1 = max(received_valid_booster1),
         received_valid_dose2 = max(received_valid_dose2),
         received_valid_booster2 = max(received_valid_booster2))


old_covid <- old_covid[!duplicated(old_covid$person_id),]
old_covid <- old_covid[old_covid$is_age_eligible_primary == 1 | 
                         old_covid$is_age_eligible_booster1 == 1 |
                         old_covid$is_age_eligible_booster2 == 1,]

old_covid$mental <- 0
old_flu$mental <- 0
pregnant_covid_norisk$mental <- 0
pregnant_covid_risk$mental <- 0
pregnant_flu_norisk$mental <- 0
pregnant_flu_risk$mental <- 0

old_covid$depression_pure <- 0
old_flu$depression_pure <- 0
pregnant_covid_norisk$depression_pure <- 0
pregnant_covid_risk$depression_pure <- 0
pregnant_flu_norisk$depression_pure <- 0
pregnant_flu_risk$depression_pure <- 0

old_covid$anxiety_pure <- 0
old_flu$anxiety_pure <- 0
pregnant_covid_norisk$anxiety_pure <- 0
pregnant_covid_risk$anxiety_pure <- 0
pregnant_flu_norisk$anxiety_pure <- 0
pregnant_flu_risk$anxiety_pure <- 0

old_covid$bipolar_pure <- 0
old_flu$bipolar_pure <- 0
pregnant_covid_norisk$bipolar_pure <- 0
pregnant_covid_risk$bipolar_pure <- 0
pregnant_flu_norisk$bipolar_pure <- 0
pregnant_flu_risk$bipolar_pure <- 0

old_covid$PTSD_pure <- 0
old_flu$PTSD_pure <- 0
pregnant_covid_norisk$PTSD_pure <- 0
pregnant_covid_risk$PTSD_pure <- 0
pregnant_flu_norisk$PTSD_pure <- 0
pregnant_flu_risk$PTSD_pure <- 0

old_covid$OCD_pure <- 0
old_flu$OCD_pure <- 0
pregnant_covid_norisk$OCD_pure <- 0
pregnant_covid_risk$OCD_pure <- 0
pregnant_flu_norisk$OCD_pure <- 0
pregnant_flu_risk$OCD_pure <- 0

old_covid$ED_pure <- 0
old_flu$ED_pure <- 0
pregnant_covid_norisk$ED_pure <- 0
pregnant_covid_risk$ED_pure <- 0
pregnant_flu_norisk$ED_pure <- 0
pregnant_flu_risk$ED_pure <- 0

old_covid$ADHD_pure <- 0
old_flu$ADHD_pure <- 0
pregnant_covid_norisk$ADHD_pure <- 0
pregnant_covid_risk$ADHD_pure <- 0
pregnant_flu_norisk$ADHD_pure <- 0
pregnant_flu_risk$ADHD_pure <- 0

old_covid$mixed <- as.integer(rowSums(old_covid[, c("depression_mh", "anxiety_mh", "bipolar_mh", "PTSD_mh", "OCD_mh", "ED_mh", "ADHD_mh")] == 1) >= 2)
old_flu$mixed <- as.integer(rowSums(old_flu[, c("depression_mh", "anxiety_mh", "bipolar_mh", "PTSD_mh", "OCD_mh", "ED_mh", "ADHD_mh")] == 1) >= 2)
pregnant_covid_norisk$mixed <- as.integer(rowSums(pregnant_covid_norisk[, c("depression_mh", "anxiety_mh", "bipolar_mh", "PTSD_mh", "OCD_mh", "ED_mh", "ADHD_mh")] == 1) >= 2)
pregnant_covid_risk$mixed <- as.integer(rowSums(pregnant_covid_risk[, c("depression_mh", "anxiety_mh", "bipolar_mh", "PTSD_mh", "OCD_mh", "ED_mh", "ADHD_mh")] == 1) >= 2)
pregnant_flu_norisk$mixed <- as.integer(rowSums(pregnant_flu_norisk[, c("depression_mh", "anxiety_mh", "bipolar_mh", "PTSD_mh", "OCD_mh", "ED_mh", "ADHD_mh")] == 1) >= 2)
pregnant_flu_risk$mixed <- as.integer(rowSums(pregnant_flu_risk[, c("depression_mh", "anxiety_mh", "bipolar_mh", "PTSD_mh", "OCD_mh", "ED_mh", "ADHD_mh")] == 1) >= 2)

old_covid$depression_pure[old_covid$depression_mh == 1 & old_covid$mixed == 0] <- 1
old_flu$depression_pure[old_flu$depression_mh == 1 & old_flu$mixed == 0] <- 1
pregnant_covid_norisk$depression_pure[pregnant_covid_norisk$depression_mh == 1 & pregnant_covid_norisk$mixed == 0] <- 1
pregnant_covid_risk$depression_pure[pregnant_covid_risk$depression_mh == 1 & pregnant_covid_risk$mixed == 0] <- 1
pregnant_flu_norisk$depression_pure[pregnant_flu_norisk$depression_mh == 1 & pregnant_flu_norisk$mixed == 0] <- 1
pregnant_flu_risk$depression_pure[pregnant_flu_risk$depression_mh == 1 & pregnant_flu_risk$mixed == 0] <- 1

old_covid$anxiety_pure[old_covid$anxiety_mh == 1 & old_covid$mixed == 0] <- 1
old_flu$anxiety_pure[old_flu$anxiety_mh == 1 & old_flu$mixed == 0] <- 1
pregnant_covid_norisk$anxiety_pure[pregnant_covid_norisk$anxiety_mh == 1 & pregnant_covid_norisk$mixed == 0] <- 1
pregnant_covid_risk$anxiety_pure[pregnant_covid_risk$anxiety_mh == 1 & pregnant_covid_risk$mixed == 0] <- 1
pregnant_flu_norisk$anxiety_pure[pregnant_flu_norisk$anxiety_mh == 1 & pregnant_flu_norisk$mixed == 0] <- 1
pregnant_flu_risk$anxiety_pure[pregnant_flu_risk$anxiety_mh == 1 & pregnant_flu_risk$mixed == 0] <- 1

old_covid$bipolar_pure[old_covid$bipolar_mh == 1 & old_covid$mixed == 0] <- 1
old_flu$bipolar_pure[old_flu$bipolar_mh == 1 & old_flu$mixed == 0] <- 1
pregnant_covid_norisk$bipolar_pure[pregnant_covid_norisk$bipolar_mh == 1 & pregnant_covid_norisk$mixed == 0] <- 1
pregnant_covid_risk$bipolar_pure[pregnant_covid_risk$bipolar_mh == 1 & pregnant_covid_risk$mixed == 0] <- 1
pregnant_flu_norisk$bipolar_pure[pregnant_flu_norisk$bipolar_mh == 1 & pregnant_flu_norisk$mixed == 0] <- 1
pregnant_flu_risk$bipolar_pure[pregnant_flu_risk$bipolar_mh == 1 & pregnant_flu_risk$mixed == 0] <- 1

old_covid$PTSD_pure[old_covid$PTSD_mh == 1 & old_covid$mixed == 0] <- 1
old_flu$PTSD_pure[old_flu$PTSD_mh == 1 & old_flu$mixed == 0] <- 1
pregnant_covid_norisk$PTSD_pure[pregnant_covid_norisk$PTSD_mh == 1 & pregnant_covid_norisk$mixed == 0] <- 1
pregnant_covid_risk$PTSD_pure[pregnant_covid_risk$PTSD_mh == 1 & pregnant_covid_risk$mixed == 0] <- 1
pregnant_flu_norisk$PTSD_pure[pregnant_flu_norisk$PTSD_mh == 1 & pregnant_flu_norisk$mixed == 0] <- 1
pregnant_flu_risk$PTSD_pure[pregnant_flu_risk$PTSD_mh == 1 & pregnant_flu_risk$mixed == 0] <- 1

old_covid$OCD_pure[old_covid$OCD_mh == 1 & old_covid$mixed == 0] <- 1
old_flu$OCD_pure[old_flu$OCD_mh == 1 & old_flu$mixed == 0] <- 1
pregnant_covid_norisk$OCD_pure[pregnant_covid_norisk$OCD_mh == 1 & pregnant_covid_norisk$mixed == 0] <- 1
pregnant_covid_risk$OCD_pure[pregnant_covid_risk$OCD_mh == 1 & pregnant_covid_risk$mixed == 0] <- 1
pregnant_flu_norisk$OCD_pure[pregnant_flu_norisk$OCD_mh == 1 & pregnant_flu_norisk$mixed == 0] <- 1
pregnant_flu_risk$OCD_pure[pregnant_flu_risk$OCD_mh == 1 & pregnant_flu_risk$mixed == 0] <- 1

old_covid$ED_pure[old_covid$ED_mh == 1 & old_covid$mixed == 0] <- 1
old_flu$ED_pure[old_flu$ED_mh == 1 & old_flu$mixed == 0] <- 1
pregnant_covid_norisk$ED_pure[pregnant_covid_norisk$ED_mh == 1 & pregnant_covid_norisk$mixed == 0] <- 1
pregnant_covid_risk$ED_pure[pregnant_covid_risk$ED_mh == 1 & pregnant_covid_risk$mixed == 0] <- 1
pregnant_flu_norisk$ED_pure[pregnant_flu_norisk$ED_mh == 1 & pregnant_flu_norisk$mixed == 0] <- 1
pregnant_flu_risk$ED_pure[pregnant_flu_risk$ED_mh == 1 & pregnant_flu_risk$mixed == 0] <- 1

old_covid$ADHD_pure[old_covid$ADHD_mh == 1 & old_covid$mixed == 0] <- 1
old_flu$ADHD_pure[old_flu$ADHD_mh == 1 & old_flu$mixed == 0] <- 1
pregnant_covid_norisk$ADHD_pure[pregnant_covid_norisk$ADHD_mh == 1 & pregnant_covid_norisk$mixed == 0] <- 1
pregnant_covid_risk$ADHD_pure[pregnant_covid_risk$ADHD_mh == 1 & pregnant_covid_risk$mixed == 0] <- 1
pregnant_flu_norisk$ADHD_pure[pregnant_flu_norisk$ADHD_mh == 1 & pregnant_flu_norisk$mixed == 0] <- 1
pregnant_flu_risk$ADHD_pure[pregnant_flu_risk$ADHD_mh == 1 & pregnant_flu_risk$mixed == 0] <- 1


old_covid$mental[old_covid$depression_mh == 1 | old_covid$anxiety_mh == 1 | old_covid$bipolar_mh == 1 |
                   old_covid$PTSD_mh == 1 | old_covid$OCD_mh == 1 | old_covid$ED_mh == 1 |
                   old_covid$ADHD_mh == 1] <- 1

old_flu$mental[old_flu$depression_mh == 1 | old_flu$anxiety_mh == 1 | old_flu$bipolar_mh == 1 |
                 old_flu$PTSD_mh == 1 | old_flu$OCD_mh == 1 | old_flu$ED_mh == 1 |
                 old_flu$ADHD_mh == 1] <- 1

pregnant_flu_norisk$mental[pregnant_flu_norisk$depression_mh == 1 | pregnant_flu_norisk$anxiety_mh == 1 | pregnant_flu_norisk$bipolar_mh == 1 |
                             pregnant_flu_norisk$PTSD_mh == 1 | pregnant_flu_norisk$OCD_mh == 1 | pregnant_flu_norisk$ED_mh == 1 |
                             pregnant_flu_norisk$ADHD_mh == 1] <- 1

pregnant_flu_risk$mental[pregnant_flu_risk$depression_mh == 1 | pregnant_flu_risk$anxiety_mh == 1 | pregnant_flu_risk$bipolar_mh == 1 |
                           pregnant_flu_risk$PTSD_mh == 1 | pregnant_flu_risk$OCD_mh == 1 | pregnant_flu_risk$ED_mh == 1 |
                           pregnant_flu_risk$ADHD_mh == 1] <- 1

pregnant_covid_risk$mental[pregnant_covid_risk$depression_mh == 1 | pregnant_covid_risk$anxiety_mh == 1 | pregnant_covid_risk$bipolar_mh == 1 |
                             pregnant_covid_risk$PTSD_mh == 1 | pregnant_covid_risk$OCD_mh == 1 | pregnant_covid_risk$ED_mh == 1 |
                             pregnant_covid_risk$ADHD_mh == 1] <- 1

pregnant_covid_norisk$mental[pregnant_covid_norisk$depression_mh == 1 | pregnant_covid_norisk$anxiety_mh == 1 | pregnant_covid_norisk$bipolar_mh == 1 |
                               pregnant_covid_norisk$PTSD_mh == 1 | pregnant_covid_norisk$OCD_mh == 1 | pregnant_covid_norisk$ED_mh == 1 |
                               pregnant_covid_norisk$ADHD_mh == 1] <- 1


# Step 1: Make sure each dataframe has its weights
# If you have separate weightit objects, add weights to dataframes:

to_add <- pregnant_covid_risk[, c("person_id", "Preg_id", "mental", "mixed", "depression_pure",
                                  "anxiety_pure", "bipolar_pure",
                                  "PTSD_pure", "OCD_pure","ADHD_pure")]
for(i in 1:50) {
  preg_covid_risk_imp_iptw[[i]] <- merge(preg_covid_risk_imp_iptw[[i]], to_add, all = T)
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_covid_risk_any_mh <- list()
model_list_preg_covid_risk_mixed <- list()
model_list_preg_covid_risk_depression <- list()
model_list_preg_covid_risk_anxiety <- list()
model_list_preg_covid_risk_bipolar <- list()
model_list_preg_covid_risk_PTSD <- list()
model_list_preg_covid_risk_OCD <- list()
model_list_preg_covid_risk_ADHD <- list()

for(i in 1:50) {
  # Run weighted modified Poisson
  model_list_preg_covid_risk_any_mh[[i]] <- glm(as.numeric(vaccinated) ~ mental + county_2021 +
                                                  age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                         family = poisson(link = "log"),
                         data = preg_covid_risk_imp_iptw[[i]],
                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_risk_mixed[[i]] <- glm(as.numeric(vaccinated) ~ mixed + county_2021 +
                                                 age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_covid_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_risk_depression[[i]] <- glm(as.numeric(vaccinated) ~ depression_pure + county_2021 +
                                                 age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_covid_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_risk_anxiety[[i]] <- glm(as.numeric(vaccinated) ~ anxiety_pure + county_2021 +
                                                 age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_covid_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_risk_bipolar[[i]] <- glm(as.numeric(vaccinated) ~ bipolar_pure + county_2021 +
                                                   age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_covid_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_risk_PTSD[[i]] <- glm(as.numeric(vaccinated) ~ PTSD_pure + county_2021 +
                                                   age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_covid_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_risk_OCD[[i]] <- glm(as.numeric(vaccinated) ~ OCD_pure + county_2021 +
                                                  age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_covid_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_risk_ADHD[[i]] <- glm(as.numeric(vaccinated) ~ ADHD_pure + county_2021 +
                                                   age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_covid_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_covid_risk_any_mh <- lapply(model_list_preg_covid_risk_any_mh, extract_robust)
results_preg_covid_risk_depression <- lapply(model_list_preg_covid_risk_depression, extract_robust)
results_preg_covid_risk_anxiety <- lapply(model_list_preg_covid_risk_anxiety, extract_robust)
# results_preg_covid_risk_bipolar <- lapply(model_list_preg_covid_risk_bipolar, extract_robust)
results_preg_covid_risk_mixed <- lapply(model_list_preg_covid_risk_mixed, extract_robust)
results_preg_covid_risk_PTSD <- lapply(model_list_preg_covid_risk_PTSD, extract_robust)
results_preg_covid_risk_OCD <- lapply(model_list_preg_covid_risk_OCD, extract_robust)
results_preg_covid_risk_ADHD <- lapply(model_list_preg_covid_risk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_covid_risk_any_mh_df <- bind_rows(results_preg_covid_risk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_risk_mixed_df <- bind_rows(results_preg_covid_risk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_risk_depression_df <- bind_rows(results_preg_covid_risk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_risk_anxiety_df <- bind_rows(results_preg_covid_risk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
# results_preg_covid_risk_bipolar_df <- bind_rows(results_preg_covid_risk_bipolar, .id = "imputation") %>%
  # mutate(imputation = as.numeric(imputation))
results_preg_covid_risk_PTSD_df <- bind_rows(results_preg_covid_risk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_risk_OCD_df <- bind_rows(results_preg_covid_risk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_risk_ADHD_df <- bind_rows(results_preg_covid_risk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_covid_risk_any_mh <- results_preg_covid_risk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_risk_mixed <- results_preg_covid_risk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_risk_depression <- results_preg_covid_risk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_risk_anxiety <- results_preg_covid_risk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )
# 
# pooled_results_preg_covid_risk_bipolar <- results_preg_covid_risk_bipolar_df %>%
#   group_by(term) %>%
#   summarise(
#     # Mean estimate across imputations
#     Q_bar = mean(estimate),
#     
#     # Within-imputation variance (mean of robust variances)
#     U_bar = mean(var_robust),
#     
#     # Between-imputation variance
#     B = var(estimate),
#     
#     # Number of imputations
#     m = n(),
#     
#     .groups = "drop"
#   ) %>%
#   mutate(
#     # Total variance (Rubin's rule)
#     T_var = U_bar + (1 + 1/m) * B,
#     
#     # Pooled SE
#     pooled_se = sqrt(T_var),
#     
#     # Degrees of freedom
#     df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
#     
#     # Confidence intervals (on log scale)
#     lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
#     upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
#     
#     # P-value
#     p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
#     
#     # Exponentiate to get Relative Risks
#     RR = exp(Q_bar),
#     RR_lower = exp(lower_ci_log),
#     RR_upper = exp(upper_ci_log)
#   )

pooled_results_preg_covid_risk_PTSD <- results_preg_covid_risk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_risk_OCD <- results_preg_covid_risk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_risk_ADHD <- results_preg_covid_risk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_preg_covid_risk_ADHD, file = "Pooled results/Pregnant_COVID_Risk_ADHD_SA.rdata")
save(pooled_results_preg_covid_risk_any_mh, file = "Pooled results/Pregnant_COVID_Risk_AnyMentalHealth_SA.rdata")
save(pooled_results_preg_covid_risk_mixed, file = "Pooled results/Pregnant_COVID_Risk_Mixed_SA.rdata")
save(pooled_results_preg_covid_risk_depression, file = "Pooled results/Pregnant_COVID_Risk_Depression_SA.rdata")
save(pooled_results_preg_covid_risk_anxiety, file = "Pooled results/Pregnant_COVID_Risk_Anxiety_SA.rdata")
# save(pooled_results_preg_covid_risk_bipolar, file = "Pooled results/Pregnant_COVID_Risk_Bipolar_SA.rdata")
save(pooled_results_preg_covid_risk_OCD, file = "Pooled results/Pregnant_COVID_Risk_OCD_SA.rdata")
save(pooled_results_preg_covid_risk_PTSD, file = "Pooled results/Pregnant_COVID_Risk_PTSD_SA.rdata")
###########################################################################
to_add <- pregnant_covid_norisk[, c("person_id", "Preg_id", "mental", "mixed", "depression_pure",
                                  "anxiety_pure", "bipolar_pure",
                                  "PTSD_pure", "OCD_pure","ADHD_pure")]
for(i in 1:45) {
  preg_covid_norisk_imp_iptw[[i]] <- merge(preg_covid_norisk_imp_iptw[[i]], to_add, all = T)
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_covid_norisk_any_mh <- list()
model_list_preg_covid_norisk_mixed <- list()
model_list_preg_covid_norisk_depression <- list()
model_list_preg_covid_norisk_anxiety <- list()
model_list_preg_covid_norisk_bipolar <- list()
model_list_preg_covid_norisk_PTSD <- list()
model_list_preg_covid_norisk_OCD <- list()
model_list_preg_covid_norisk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_covid_norisk_any_mh[[i]] <- glm(as.numeric(vaccinated) ~ mental + county_2021 +
                                                  age_at_enrollment_categorized + country_of_birth
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_covid_norisk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_norisk_mixed[[i]] <- glm(as.numeric(vaccinated) ~ mixed + county_2021 +
                                                 age_at_enrollment_categorized + country_of_birth
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_covid_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_norisk_depression[[i]] <- glm(as.numeric(vaccinated) ~ depression_pure + county_2021 +
                                                      age_at_enrollment_categorized + country_of_birth
                                                    + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                    family = poisson(link = "log"),
                                                    data = preg_covid_norisk_imp_iptw[[i]],
                                                    weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_norisk_anxiety[[i]] <- glm(as.numeric(vaccinated) ~ anxiety_pure + county_2021 +
                                                   age_at_enrollment_categorized + country_of_birth
                                                 + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                 family = poisson(link = "log"),
                                                 data = preg_covid_norisk_imp_iptw[[i]],
                                                 weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_norisk_bipolar[[i]] <- glm(as.numeric(vaccinated) ~ bipolar_pure + county_2021 +
                                                   age_at_enrollment_categorized + country_of_birth
                                                 + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                                 family = poisson(link = "log"),
                                                 data = preg_covid_norisk_imp_iptw[[i]],
                                                 weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_norisk_PTSD[[i]] <- glm(as.numeric(vaccinated) ~ PTSD_pure + county_2021 +
                                                age_at_enrollment_categorized + country_of_birth
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_covid_norisk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_norisk_OCD[[i]] <- glm(as.numeric(vaccinated) ~ OCD_pure + county_2021 +
                                               age_at_enrollment_categorized + country_of_birth
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_covid_norisk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_covid_norisk_ADHD[[i]] <- glm(as.numeric(vaccinated) ~ ADHD_pure + county_2021 +
                                                age_at_enrollment_categorized + country_of_birth
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2021_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_covid_norisk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_covid_norisk_any_mh <- lapply(model_list_preg_covid_norisk_any_mh, extract_robust)
results_preg_covid_norisk_depression <- lapply(model_list_preg_covid_norisk_depression, extract_robust)
results_preg_covid_norisk_anxiety <- lapply(model_list_preg_covid_norisk_anxiety, extract_robust)
results_preg_covid_norisk_bipolar <- lapply(model_list_preg_covid_norisk_bipolar, extract_robust)
results_preg_covid_norisk_mixed <- lapply(model_list_preg_covid_norisk_mixed, extract_robust)
results_preg_covid_norisk_PTSD <- lapply(model_list_preg_covid_norisk_PTSD, extract_robust)
results_preg_covid_norisk_OCD <- lapply(model_list_preg_covid_norisk_OCD, extract_robust)
results_preg_covid_norisk_ADHD <- lapply(model_list_preg_covid_norisk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_covid_norisk_any_mh_df <- bind_rows(results_preg_covid_norisk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_norisk_mixed_df <- bind_rows(results_preg_covid_norisk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_norisk_depression_df <- bind_rows(results_preg_covid_norisk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_norisk_anxiety_df <- bind_rows(results_preg_covid_norisk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_norisk_bipolar_df <- bind_rows(results_preg_covid_norisk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_norisk_PTSD_df <- bind_rows(results_preg_covid_norisk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_norisk_OCD_df <- bind_rows(results_preg_covid_norisk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_covid_norisk_ADHD_df <- bind_rows(results_preg_covid_norisk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_covid_norisk_any_mh <- results_preg_covid_norisk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_norisk_mixed <- results_preg_covid_norisk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_norisk_depression <- results_preg_covid_norisk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_norisk_anxiety <- results_preg_covid_norisk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_norisk_bipolar <- results_preg_covid_norisk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_norisk_PTSD <- results_preg_covid_norisk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_norisk_OCD <- results_preg_covid_norisk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_covid_norisk_ADHD <- results_preg_covid_norisk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_preg_covid_norisk_ADHD, file = "Pooled results/Pregnant_COVID_NoRisk_ADHD_SA.rdata")
save(pooled_results_preg_covid_norisk_any_mh, file = "Pooled results/Pregnant_COVID_NoRisk_AnyMentalHealth_SA.rdata")
save(pooled_results_preg_covid_norisk_mixed, file = "Pooled results/Pregnant_COVID_NoRisk_Mixed_SA.rdata")
save(pooled_results_preg_covid_norisk_depression, file = "Pooled results/Pregnant_COVID_NoRisk_Depression_SA.rdata")
save(pooled_results_preg_covid_norisk_anxiety, file = "Pooled results/Pregnant_COVID_NoRisk_Anxiety_SA.rdata")
save(pooled_results_preg_covid_norisk_bipolar, file = "Pooled results/Pregnant_COVID_NoRisk_Bipolar_SA.rdata")
save(pooled_results_preg_covid_norisk_OCD, file = "Pooled results/Pregnant_COVID_NoRisk_OCD_SA.rdata")
save(pooled_results_preg_covid_norisk_PTSD, file = "Pooled results/Pregnant_COVID_NoRisk_PTSD_SA.rdata")
###########################################################################
to_add <- old_flu[, c("person_id", "mental", "mixed", "depression_pure",
                                    "anxiety_pure", "bipolar_pure",
                                    "PTSD_pure", "OCD_pure","ADHD_pure")]
for(i in 1:30) {
  old_flu_imp_iptw[[i]] <- merge(old_flu_imp_iptw[[i]], to_add, all = T)
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_old_flu_any_mh <- list()
model_list_old_flu_mixed <- list()
model_list_old_flu_depression <- list()
model_list_old_flu_anxiety <- list()
model_list_old_flu_bipolar <- list()
model_list_old_flu_PTSD <- list()
model_list_old_flu_OCD <- list()
model_list_old_flu_ADHD <- list()

for(i in 1:30) {
  # Run weighted modified Poisson
  model_list_old_flu_any_mh[[i]] <- glm(as.numeric(all.seasons) ~ mental + county_2017 +
                                                    country_of_birth + risk_factor 
                                                  +  income_2017_cat + age_at_enrollment_categorized, 
                                                  family = poisson(link = "log"),
                                                  data = old_flu_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_mixed[[i]] <- glm(as.numeric(all.seasons) ~ mixed + county_2017 +
                                                   country_of_birth + risk_factor 
                                                 +  income_2017_cat + age_at_enrollment_categorized, 
                                                 family = poisson(link = "log"),
                                                 data = old_flu_imp_iptw[[i]],
                                                 weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_depression[[i]] <- glm(as.numeric(all.seasons) ~ depression_pure + county_2017 +
                                                        country_of_birth + risk_factor 
                                                      +  income_2017_cat + age_at_enrollment_categorized, 
                                                      family = poisson(link = "log"),
                                                      data = old_flu_imp_iptw[[i]],
                                                      weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_anxiety[[i]] <- glm(as.numeric(all.seasons) ~ anxiety_pure + county_2017 +
                                                     country_of_birth + risk_factor 
                                                   +  income_2017_cat + age_at_enrollment_categorized, 
                                                   family = poisson(link = "log"),
                                                   data = old_flu_imp_iptw[[i]],
                                                   weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_bipolar[[i]] <- glm(as.numeric(all.seasons) ~ bipolar_pure + county_2017 +
                                                     country_of_birth + risk_factor 
                                                   +  income_2017_cat + age_at_enrollment_categorized, 
                                                   family = poisson(link = "log"),
                                                   data = old_flu_imp_iptw[[i]],
                                                   weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_PTSD[[i]] <- glm(as.numeric(all.seasons) ~ PTSD_pure + county_2017 +
                                                  country_of_birth + risk_factor 
                                                +  income_2017_cat + age_at_enrollment_categorized, 
                                                family = poisson(link = "log"),
                                                data = old_flu_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_OCD[[i]] <- glm(as.numeric(all.seasons) ~ OCD_pure + + county_2017 +
                                                 country_of_birth + risk_factor 
                                               +  income_2017_cat + age_at_enrollment_categorized, 
                                               family = poisson(link = "log"),
                                               data = old_flu_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_ADHD[[i]] <- glm(as.numeric(all.seasons) ~ ADHD_pure + county_2017 +
                                                  country_of_birth + risk_factor 
                                                +  income_2017_cat + age_at_enrollment_categorized, 
                                                family = poisson(link = "log"),
                                                data = old_flu_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_old_flu_any_mh <- lapply(model_list_old_flu_any_mh, extract_robust)
results_old_flu_depression <- lapply(model_list_old_flu_depression, extract_robust)
results_old_flu_anxiety <- lapply(model_list_old_flu_anxiety, extract_robust)
results_old_flu_bipolar <- lapply(model_list_old_flu_bipolar, extract_robust)
results_old_flu_mixed <- lapply(model_list_old_flu_mixed, extract_robust)
results_old_flu_PTSD <- lapply(model_list_old_flu_PTSD, extract_robust)
results_old_flu_OCD <- lapply(model_list_old_flu_OCD, extract_robust)
results_old_flu_ADHD <- lapply(model_list_old_flu_ADHD, extract_robust)

# Combine into one dataframe
results_old_flu_any_mh_df <- bind_rows(results_old_flu_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_mixed_df <- bind_rows(results_old_flu_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_depression_df <- bind_rows(results_old_flu_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_anxiety_df <- bind_rows(results_old_flu_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_bipolar_df <- bind_rows(results_old_flu_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_PTSD_df <- bind_rows(results_old_flu_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_OCD_df <- bind_rows(results_old_flu_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_ADHD_df <- bind_rows(results_old_flu_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_old_flu_any_mh <- results_old_flu_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_mixed <- results_old_flu_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_depression <- results_old_flu_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_anxiety <- results_old_flu_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_bipolar <- results_old_flu_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_PTSD <- results_old_flu_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_OCD <- results_old_flu_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_ADHD <- results_old_flu_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_old_flu_ADHD, file = "Pooled results/Old_Influenza_ADHD_SA.rdata")
save(pooled_results_old_flu_any_mh, file = "Pooled results/Old_Influenza_AnyMentalHealth_SA.rdata")
save(pooled_results_old_flu_mixed, file = "Pooled results/Old_Influenza_Mixed_SA.rdata")
save(pooled_results_old_flu_depression, file = "Pooled results/Old_Influenza_Depression_SA.rdata")
save(pooled_results_old_flu_anxiety, file = "Pooled results/Old_Influenza_Anxiety_SA.rdata")
save(pooled_results_old_flu_bipolar, file = "Pooled results/Old_Influenza_Bipolar_SA.rdata")
save(pooled_results_old_flu_OCD, file = "Pooled results/Old_Influenza_OCD_SA.rdata")
save(pooled_results_old_flu_PTSD, file = "Pooled results/Old_Influenza_PTSD_SA.rdata")
#################################################
to_add <- pregnant_flu_risk[, c("person_id", "Preg_id", "mental", "mixed", "depression_pure",
                                    "anxiety_pure", "bipolar_pure",
                                    "PTSD_pure", "OCD_pure","ADHD_pure")]
for(i in 1:45) {
  preg_flu_risk_imp_iptw[[i]] <- merge(preg_flu_risk_imp_iptw[[i]], to_add, all = T)
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_risk_any_mh <- list()
model_list_preg_flu_risk_mixed <- list()
model_list_preg_flu_risk_depression <- list()
model_list_preg_flu_risk_anxiety <- list()
model_list_preg_flu_risk_bipolar <- list()
model_list_preg_flu_risk_PTSD <- list()
model_list_preg_flu_risk_OCD <- list()
model_list_preg_flu_risk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_risk_any_mh[[i]] <- glm(as.numeric(all.seasons) ~ mental + county_2017 +
                                                    country_of_birth + age_at_enrollment_categorized
                                                  + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                  family = poisson(link = "log"),
                                                  data = preg_flu_risk_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_mixed[[i]] <- glm(as.numeric(all.seasons) ~ mixed + county_2017 +
                                                   country_of_birth + age_at_enrollment_categorized
                                                 + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                 family = poisson(link = "log"),
                                                 data = preg_flu_risk_imp_iptw[[i]],
                                                 weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_depression[[i]] <- glm(as.numeric(all.seasons) ~ depression_pure + county_2017 +
                                                        country_of_birth + age_at_enrollment_categorized
                                                      + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                      family = poisson(link = "log"),
                                                      data = preg_flu_risk_imp_iptw[[i]],
                                                      weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_anxiety[[i]] <- glm(as.numeric(all.seasons) ~ anxiety_pure + county_2017 +
                                                     country_of_birth + age_at_enrollment_categorized
                                                   + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                   family = poisson(link = "log"),
                                                   data = preg_flu_risk_imp_iptw[[i]],
                                                   weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_bipolar[[i]] <- glm(as.numeric(all.seasons) ~ bipolar_pure + county_2017 +
                                                     country_of_birth + age_at_enrollment_categorized
                                                   + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                   family = poisson(link = "log"),
                                                   data = preg_flu_risk_imp_iptw[[i]],
                                                   weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_PTSD[[i]] <- glm(as.numeric(all.seasons) ~ PTSD_pure + county_2017 +
                                                  country_of_birth + age_at_enrollment_categorized
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_flu_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_OCD[[i]] <- glm(as.numeric(all.seasons) ~ OCD_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_risk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_ADHD[[i]] <- glm(as.numeric(all.seasons) ~ ADHD_pure + county_2017 +
                                                  country_of_birth + age_at_enrollment_categorized
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_flu_risk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_risk_any_mh <- lapply(model_list_preg_flu_risk_any_mh, extract_robust)
results_preg_flu_risk_depression <- lapply(model_list_preg_flu_risk_depression, extract_robust)
results_preg_flu_risk_anxiety <- lapply(model_list_preg_flu_risk_anxiety, extract_robust)
results_preg_flu_risk_bipolar <- lapply(model_list_preg_flu_risk_bipolar, extract_robust)
results_preg_flu_risk_mixed <- lapply(model_list_preg_flu_risk_mixed, extract_robust)
results_preg_flu_risk_PTSD <- lapply(model_list_preg_flu_risk_PTSD, extract_robust)
results_preg_flu_risk_OCD <- lapply(model_list_preg_flu_risk_OCD, extract_robust)
results_preg_flu_risk_ADHD <- lapply(model_list_preg_flu_risk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_risk_any_mh_df <- bind_rows(results_preg_flu_risk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_mixed_df <- bind_rows(results_preg_flu_risk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_depression_df <- bind_rows(results_preg_flu_risk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_anxiety_df <- bind_rows(results_preg_flu_risk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_bipolar_df <- bind_rows(results_preg_flu_risk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_PTSD_df <- bind_rows(results_preg_flu_risk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_OCD_df <- bind_rows(results_preg_flu_risk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_ADHD_df <- bind_rows(results_preg_flu_risk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_risk_any_mh <- results_preg_flu_risk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_mixed <- results_preg_flu_risk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_depression <- results_preg_flu_risk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_anxiety <- results_preg_flu_risk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_bipolar <- results_preg_flu_risk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_PTSD <- results_preg_flu_risk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_OCD <- results_preg_flu_risk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_ADHD <- results_preg_flu_risk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_preg_flu_risk_ADHD, file = "Pooled results/Pregnant_Influenza_Risk_ADHD_SA.rdata")
save(pooled_results_preg_flu_risk_any_mh, file = "Pooled results/Pregnant_Influenza_Risk_AnyMentalHealth_SA.rdata")
save(pooled_results_preg_flu_risk_mixed, file = "Pooled results/Pregnant_Influenza_Risk_Mixed_SA.rdata")
save(pooled_results_preg_flu_risk_depression, file = "Pooled results/Pregnant_Influenza_Risk_Depression_SA.rdata")
save(pooled_results_preg_flu_risk_anxiety, file = "Pooled results/Pregnant_Influenza_Risk_Anxiety_SA.rdata")
save(pooled_results_preg_flu_risk_bipolar, file = "Pooled results/Pregnant_Influenza_Risk_Bipolar_SA.rdata")
save(pooled_results_preg_flu_risk_OCD, file = "Pooled results/Pregnant_Influenza_Risk_OCD_SA.rdata")
save(pooled_results_preg_flu_risk_PTSD, file = "Pooled results/Pregnant_Influenza_Risk_PTSD_SA.rdata")
##################################################################
to_add <- pregnant_flu_norisk[, c("person_id", "Preg_id", "mental", "mixed", "depression_pure",
                                "anxiety_pure", "bipolar_pure",
                                "PTSD_pure", "OCD_pure","ADHD_pure")]
for(i in 1:45) {
  preg_flu_norisk_imp_iptw[[i]] <- merge(preg_flu_norisk_imp_iptw[[i]], to_add, all = T)
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_norisk_any_mh <- list()
model_list_preg_flu_norisk_mixed <- list()
model_list_preg_flu_norisk_depression <- list()
model_list_preg_flu_norisk_anxiety <- list()
model_list_preg_flu_norisk_bipolar <- list()
model_list_preg_flu_norisk_PTSD <- list()
model_list_preg_flu_norisk_OCD <- list()
model_list_preg_flu_norisk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_any_mh[[i]] <- glm(as.numeric(all.seasons) ~ mental + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_norisk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_mixed[[i]] <- glm(as.numeric(all.seasons) ~ mixed + county_2017 +
                                               country_of_birth + age_at_enrollment_categorized
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_flu_norisk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_depression[[i]] <- glm(as.numeric(all.seasons) ~ depression_pure + county_2017 +
                                                    country_of_birth + age_at_enrollment_categorized
                                                  + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                  family = poisson(link = "log"),
                                                  data = preg_flu_norisk_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_anxiety[[i]] <- glm(as.numeric(all.seasons) ~ anxiety_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_bipolar[[i]] <- glm(as.numeric(all.seasons) ~ bipolar_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_PTSD[[i]] <- glm(as.numeric(all.seasons) ~ PTSD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_norisk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_OCD[[i]] <- glm(as.numeric(all.seasons) ~ OCD_pure + county_2017 +
                                             country_of_birth + age_at_enrollment_categorized
                                           + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                           family = poisson(link = "log"),
                                           data = preg_flu_norisk_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_ADHD[[i]] <- glm(as.numeric(all.seasons) ~ ADHD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_norisk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_norisk_any_mh <- lapply(model_list_preg_flu_norisk_any_mh, extract_robust)
results_preg_flu_norisk_depression <- lapply(model_list_preg_flu_norisk_depression, extract_robust)
results_preg_flu_norisk_anxiety <- lapply(model_list_preg_flu_norisk_anxiety, extract_robust)
results_preg_flu_norisk_bipolar <- lapply(model_list_preg_flu_norisk_bipolar, extract_robust)
results_preg_flu_norisk_mixed <- lapply(model_list_preg_flu_norisk_mixed, extract_robust)
results_preg_flu_norisk_PTSD <- lapply(model_list_preg_flu_norisk_PTSD, extract_robust)
results_preg_flu_norisk_OCD <- lapply(model_list_preg_flu_norisk_OCD, extract_robust)
results_preg_flu_norisk_ADHD <- lapply(model_list_preg_flu_norisk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_norisk_any_mh_df <- bind_rows(results_preg_flu_norisk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_mixed_df <- bind_rows(results_preg_flu_norisk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_depression_df <- bind_rows(results_preg_flu_norisk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_anxiety_df <- bind_rows(results_preg_flu_norisk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_bipolar_df <- bind_rows(results_preg_flu_norisk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_PTSD_df <- bind_rows(results_preg_flu_norisk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_OCD_df <- bind_rows(results_preg_flu_norisk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_ADHD_df <- bind_rows(results_preg_flu_norisk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_norisk_any_mh <- results_preg_flu_norisk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_mixed <- results_preg_flu_norisk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_depression <- results_preg_flu_norisk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_anxiety <- results_preg_flu_norisk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_bipolar <- results_preg_flu_norisk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_PTSD <- results_preg_flu_norisk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_OCD <- results_preg_flu_norisk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_ADHD <- results_preg_flu_norisk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_preg_flu_norisk_ADHD, file = "Pooled results/Pregnant_Influenza_NoRisk_ADHD_SA.rdata")
save(pooled_results_preg_flu_norisk_any_mh, file = "Pooled results/Pregnant_Influenza_NoRisk_AnyMentalHealth_SA.rdata")
save(pooled_results_preg_flu_norisk_mixed, file = "Pooled results/Pregnant_Influenza_NoRisk_Mixed_SA.rdata")
save(pooled_results_preg_flu_norisk_depression, file = "Pooled results/Pregnant_Influenza_NoRisk_Depression_SA.rdata")
save(pooled_results_preg_flu_norisk_anxiety, file = "Pooled results/Pregnant_Influenza_NoRisk_Anxiety_SA.rdata")
save(pooled_results_preg_flu_norisk_bipolar, file = "Pooled results/Pregnant_Influenza_NoRisk_Bipolar_SA.rdata")
save(pooled_results_preg_flu_norisk_OCD, file = "Pooled results/Pregnant_Influenza_NoRisk_OCD_SA.rdata")
save(pooled_results_preg_flu_norisk_PTSD, file = "Pooled results/Pregnant_Influenza_NoRisk_PTSD_SA.rdata")
##################################################################
to_add <- pregnant_flu_risk[, c("person_id", "season1", "season2", "season3")]
for(i in 1:45) {
  preg_flu_risk_imp_iptw[[i]] <- merge(preg_flu_risk_imp_iptw[[i]], to_add, all = T)
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_risk_any_mh <- list()
model_list_preg_flu_risk_mixed <- list()
model_list_preg_flu_risk_depression <- list()
model_list_preg_flu_risk_anxiety <- list()
model_list_preg_flu_risk_bipolar <- list()
model_list_preg_flu_risk_PTSD <- list()
model_list_preg_flu_risk_OCD <- list()
model_list_preg_flu_risk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_risk_any_mh[[i]] <- glm(as.numeric(season1) ~ mental + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_risk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_mixed[[i]] <- glm(as.numeric(season1) ~ mixed + county_2017 +
                                               country_of_birth + age_at_enrollment_categorized
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_flu_risk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_depression[[i]] <- glm(as.numeric(season1) ~ depression_pure + county_2017 +
                                                    country_of_birth + age_at_enrollment_categorized
                                                  + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                  family = poisson(link = "log"),
                                                  data = preg_flu_risk_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_anxiety[[i]] <- glm(as.numeric(season1) ~ anxiety_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_risk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_bipolar[[i]] <- glm(as.numeric(season1) ~ bipolar_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_risk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_PTSD[[i]] <- glm(as.numeric(season1) ~ PTSD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_risk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_OCD[[i]] <- glm(as.numeric(season1) ~ OCD_pure + county_2017 +
                                             country_of_birth + age_at_enrollment_categorized
                                           + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                           family = poisson(link = "log"),
                                           data = preg_flu_risk_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_ADHD[[i]] <- glm(as.numeric(season1) ~ ADHD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_risk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_risk_any_mh <- lapply(model_list_preg_flu_risk_any_mh, extract_robust)
results_preg_flu_risk_depression <- lapply(model_list_preg_flu_risk_depression, extract_robust)
results_preg_flu_risk_anxiety <- lapply(model_list_preg_flu_risk_anxiety, extract_robust)
results_preg_flu_risk_bipolar <- lapply(model_list_preg_flu_risk_bipolar, extract_robust)
results_preg_flu_risk_mixed <- lapply(model_list_preg_flu_risk_mixed, extract_robust)
results_preg_flu_risk_PTSD <- lapply(model_list_preg_flu_risk_PTSD, extract_robust)
results_preg_flu_risk_OCD <- lapply(model_list_preg_flu_risk_OCD, extract_robust)
results_preg_flu_risk_ADHD <- lapply(model_list_preg_flu_risk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_risk_any_mh_df <- bind_rows(results_preg_flu_risk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_mixed_df <- bind_rows(results_preg_flu_risk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_depression_df <- bind_rows(results_preg_flu_risk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_anxiety_df <- bind_rows(results_preg_flu_risk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_bipolar_df <- bind_rows(results_preg_flu_risk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_PTSD_df <- bind_rows(results_preg_flu_risk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_OCD_df <- bind_rows(results_preg_flu_risk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_ADHD_df <- bind_rows(results_preg_flu_risk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_risk_any_mh <- results_preg_flu_risk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_mixed <- results_preg_flu_risk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_depression <- results_preg_flu_risk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_anxiety <- results_preg_flu_risk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_bipolar <- results_preg_flu_risk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_PTSD <- results_preg_flu_risk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_OCD <- results_preg_flu_risk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_ADHD <- results_preg_flu_risk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_ADHD_seasosn1 <- pooled_results_preg_flu_risk_ADHD
pooled_results_preg_flu_risk_any_mh_seasosn1 <- pooled_results_preg_flu_risk_any_mh
pooled_results_preg_flu_risk_mixed_seasosn1 <- pooled_results_preg_flu_risk_mixed
pooled_results_preg_flu_risk_depression_seasosn1 <- pooled_results_preg_flu_risk_depression
pooled_results_preg_flu_risk_anxiety_seasosn1 <- pooled_results_preg_flu_risk_anxiety
pooled_results_preg_flu_risk_bipolar_seasosn1 <- pooled_results_preg_flu_risk_bipolar
pooled_results_preg_flu_risk_OCD_seasosn1 <- pooled_results_preg_flu_risk_OCD
pooled_results_preg_flu_risk_PTSD_seasosn1 <- pooled_results_preg_flu_risk_PTSD

save(pooled_results_preg_flu_risk_ADHD_seasosn1, file = "Pooled results/Pregnant_Influenza_Risk_ADHD_Season1_SA.rdata")
save(pooled_results_preg_flu_risk_any_mh_seasosn1, file = "Pooled results/Pregnant_Influenza_Risk_AnyMentalHealth_Season1_SA.rdata")
save(pooled_results_preg_flu_risk_mixed_seasosn1, file = "Pooled results/Pregnant_Influenza_Risk_Mixed_Season1_SA.rdata")
save(pooled_results_preg_flu_risk_depression_seasosn1, file = "Pooled results/Pregnant_Influenza_Risk_Depression_Season1_SA.rdata")
save(pooled_results_preg_flu_risk_anxiety_seasosn1, file = "Pooled results/Pregnant_Influenza_Risk_Anxiety_Season1_SA.rdata")
save(pooled_results_preg_flu_risk_bipolar_seasosn1, file = "Pooled results/Pregnant_Influenza_Risk_Bipolar_Season1_SA.rdata")
save(pooled_results_preg_flu_risk_OCD_seasosn1, file = "Pooled results/Pregnant_Influenza_Risk_OCD_Season1_SA.rdata")
save(pooled_results_preg_flu_risk_PTSD_seasosn1, file = "Pooled results/Pregnant_Influenza_Risk_PTSD_Season1_SA.rdata")
##############################

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_risk_any_mh <- list()
model_list_preg_flu_risk_mixed <- list()
model_list_preg_flu_risk_depression <- list()
model_list_preg_flu_risk_anxiety <- list()
model_list_preg_flu_risk_bipolar <- list()
model_list_preg_flu_risk_PTSD <- list()
model_list_preg_flu_risk_OCD <- list()
model_list_preg_flu_risk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_risk_any_mh[[i]] <- glm(as.numeric(season2) ~ mental + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_risk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_mixed[[i]] <- glm(as.numeric(season2) ~ mixed + county_2017 +
                                               country_of_birth + age_at_enrollment_categorized
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_flu_risk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_depression[[i]] <- glm(as.numeric(season2) ~ depression_pure + county_2017 +
                                                    country_of_birth + age_at_enrollment_categorized
                                                  + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                  family = poisson(link = "log"),
                                                  data = preg_flu_risk_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_anxiety[[i]] <- glm(as.numeric(season2) ~ anxiety_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_risk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_bipolar[[i]] <- glm(as.numeric(season2) ~ bipolar_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_risk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_PTSD[[i]] <- glm(as.numeric(season2) ~ PTSD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_risk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_OCD[[i]] <- glm(as.numeric(season2) ~ OCD_pure + county_2017 +
                                             country_of_birth + age_at_enrollment_categorized
                                           + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                           family = poisson(link = "log"),
                                           data = preg_flu_risk_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_ADHD[[i]] <- glm(as.numeric(season2) ~ ADHD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_risk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_risk_any_mh <- lapply(model_list_preg_flu_risk_any_mh, extract_robust)
results_preg_flu_risk_depression <- lapply(model_list_preg_flu_risk_depression, extract_robust)
results_preg_flu_risk_anxiety <- lapply(model_list_preg_flu_risk_anxiety, extract_robust)
results_preg_flu_risk_bipolar <- lapply(model_list_preg_flu_risk_bipolar, extract_robust)
results_preg_flu_risk_mixed <- lapply(model_list_preg_flu_risk_mixed, extract_robust)
results_preg_flu_risk_PTSD <- lapply(model_list_preg_flu_risk_PTSD, extract_robust)
results_preg_flu_risk_OCD <- lapply(model_list_preg_flu_risk_OCD, extract_robust)
results_preg_flu_risk_ADHD <- lapply(model_list_preg_flu_risk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_risk_any_mh_df <- bind_rows(results_preg_flu_risk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_mixed_df <- bind_rows(results_preg_flu_risk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_depression_df <- bind_rows(results_preg_flu_risk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_anxiety_df <- bind_rows(results_preg_flu_risk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_bipolar_df <- bind_rows(results_preg_flu_risk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_PTSD_df <- bind_rows(results_preg_flu_risk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_OCD_df <- bind_rows(results_preg_flu_risk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_ADHD_df <- bind_rows(results_preg_flu_risk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_risk_any_mh <- results_preg_flu_risk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_mixed <- results_preg_flu_risk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_depression <- results_preg_flu_risk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_anxiety <- results_preg_flu_risk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_bipolar <- results_preg_flu_risk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_PTSD <- results_preg_flu_risk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_OCD <- results_preg_flu_risk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_ADHD <- results_preg_flu_risk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_ADHD_season2 <- pooled_results_preg_flu_risk_ADHD
pooled_results_preg_flu_risk_any_mh_season2 <- pooled_results_preg_flu_risk_any_mh
pooled_results_preg_flu_risk_mixed_season2 <- pooled_results_preg_flu_risk_mixed
pooled_results_preg_flu_risk_depression_season2 <- pooled_results_preg_flu_risk_depression
pooled_results_preg_flu_risk_anxiety_season2 <- pooled_results_preg_flu_risk_anxiety
pooled_results_preg_flu_risk_bipolar_season2 <- pooled_results_preg_flu_risk_bipolar
pooled_results_preg_flu_risk_OCD_season2 <- pooled_results_preg_flu_risk_OCD
pooled_results_preg_flu_risk_PTSD_season2 <- pooled_results_preg_flu_risk_PTSD

save(pooled_results_preg_flu_risk_ADHD_season2, file = "Pooled results/Pregnant_Influenza_Risk_ADHD_season2_SA.rdata")
save(pooled_results_preg_flu_risk_any_mh_season2, file = "Pooled results/Pregnant_Influenza_Risk_AnyMentalHealth_season2_SA.rdata")
save(pooled_results_preg_flu_risk_mixed_season2, file = "Pooled results/Pregnant_Influenza_Risk_Mixed_season2_SA.rdata")
save(pooled_results_preg_flu_risk_depression_season2, file = "Pooled results/Pregnant_Influenza_Risk_Depression_season2_SA.rdata")
save(pooled_results_preg_flu_risk_anxiety_season2, file = "Pooled results/Pregnant_Influenza_Risk_Anxiety_season2_SA.rdata")
save(pooled_results_preg_flu_risk_bipolar_season2, file = "Pooled results/Pregnant_Influenza_Risk_Bipolar_season2_SA.rdata")
save(pooled_results_preg_flu_risk_OCD_season2, file = "Pooled results/Pregnant_Influenza_Risk_OCD_season2_SA.rdata")
save(pooled_results_preg_flu_risk_PTSD_season2, file = "Pooled results/Pregnant_Influenza_Risk_PTSD_season2_SA.rdata")
###################################

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_risk_any_mh <- list()
model_list_preg_flu_risk_mixed <- list()
model_list_preg_flu_risk_depression <- list()
model_list_preg_flu_risk_anxiety <- list()
model_list_preg_flu_risk_bipolar <- list()
model_list_preg_flu_risk_PTSD <- list()
model_list_preg_flu_risk_OCD <- list()
model_list_preg_flu_risk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_risk_any_mh[[i]] <- glm(as.numeric(season3) ~ mental + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_risk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_mixed[[i]] <- glm(as.numeric(season3) ~ mixed + county_2017 +
                                               country_of_birth + age_at_enrollment_categorized
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_flu_risk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_depression[[i]] <- glm(as.numeric(season3) ~ depression_pure + county_2017 +
                                                    country_of_birth + age_at_enrollment_categorized
                                                  + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                  family = poisson(link = "log"),
                                                  data = preg_flu_risk_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_anxiety[[i]] <- glm(as.numeric(season3) ~ anxiety_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_risk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_bipolar[[i]] <- glm(as.numeric(season3) ~ bipolar_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_risk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_PTSD[[i]] <- glm(as.numeric(season3) ~ PTSD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_risk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_OCD[[i]] <- glm(as.numeric(season3) ~ OCD_pure + county_2017 +
                                             country_of_birth + age_at_enrollment_categorized
                                           + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                           family = poisson(link = "log"),
                                           data = preg_flu_risk_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_risk_ADHD[[i]] <- glm(as.numeric(season3) ~ ADHD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_risk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_risk_any_mh <- lapply(model_list_preg_flu_risk_any_mh, extract_robust)
results_preg_flu_risk_depression <- lapply(model_list_preg_flu_risk_depression, extract_robust)
results_preg_flu_risk_anxiety <- lapply(model_list_preg_flu_risk_anxiety, extract_robust)
results_preg_flu_risk_bipolar <- lapply(model_list_preg_flu_risk_bipolar, extract_robust)
results_preg_flu_risk_mixed <- lapply(model_list_preg_flu_risk_mixed, extract_robust)
results_preg_flu_risk_PTSD <- lapply(model_list_preg_flu_risk_PTSD, extract_robust)
results_preg_flu_risk_OCD <- lapply(model_list_preg_flu_risk_OCD, extract_robust)
results_preg_flu_risk_ADHD <- lapply(model_list_preg_flu_risk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_risk_any_mh_df <- bind_rows(results_preg_flu_risk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_mixed_df <- bind_rows(results_preg_flu_risk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_depression_df <- bind_rows(results_preg_flu_risk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_anxiety_df <- bind_rows(results_preg_flu_risk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_bipolar_df <- bind_rows(results_preg_flu_risk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_PTSD_df <- bind_rows(results_preg_flu_risk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_OCD_df <- bind_rows(results_preg_flu_risk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_risk_ADHD_df <- bind_rows(results_preg_flu_risk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_risk_any_mh <- results_preg_flu_risk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_mixed <- results_preg_flu_risk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_depression <- results_preg_flu_risk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_anxiety <- results_preg_flu_risk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_bipolar <- results_preg_flu_risk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_PTSD <- results_preg_flu_risk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_OCD <- results_preg_flu_risk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_ADHD <- results_preg_flu_risk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_risk_ADHD_season3 <- pooled_results_preg_flu_risk_ADHD
pooled_results_preg_flu_risk_any_mh_season3 <- pooled_results_preg_flu_risk_any_mh
pooled_results_preg_flu_risk_mixed_season3 <- pooled_results_preg_flu_risk_mixed
pooled_results_preg_flu_risk_depression_season3 <- pooled_results_preg_flu_risk_depression
pooled_results_preg_flu_risk_anxiety_season3 <- pooled_results_preg_flu_risk_anxiety
pooled_results_preg_flu_risk_bipolar_season3 <- pooled_results_preg_flu_risk_bipolar
pooled_results_preg_flu_risk_OCD_season3 <- pooled_results_preg_flu_risk_OCD
pooled_results_preg_flu_risk_PTSD_season3 <- pooled_results_preg_flu_risk_PTSD

save(pooled_results_preg_flu_risk_ADHD_season3, file = "Pooled results/Pregnant_Influenza_Risk_ADHD_season3_SA.rdata")
save(pooled_results_preg_flu_risk_any_mh_season3, file = "Pooled results/Pregnant_Influenza_Risk_AnyMentalHealth_season3_SA.rdata")
save(pooled_results_preg_flu_risk_mixed_season3, file = "Pooled results/Pregnant_Influenza_Risk_Mixed_season3_SA.rdata")
save(pooled_results_preg_flu_risk_depression_season3, file = "Pooled results/Pregnant_Influenza_Risk_Depression_season3_SA.rdata")
save(pooled_results_preg_flu_risk_anxiety_season3, file = "Pooled results/Pregnant_Influenza_Risk_Anxiety_season3_SA.rdata")
save(pooled_results_preg_flu_risk_bipolar_season3, file = "Pooled results/Pregnant_Influenza_Risk_Bipolar_season3_SA.rdata")
save(pooled_results_preg_flu_risk_OCD_season3, file = "Pooled results/Pregnant_Influenza_Risk_OCD_season3_SA.rdata")
save(pooled_results_preg_flu_risk_PTSD_season3, file = "Pooled results/Pregnant_Influenza_Risk_PTSD_season3_SA.rdata")
################################################################
to_add <- pregnant_flu_norisk[, c("person_id","Preg_id", "mental", "mixed", "depression_pure",
                                  "anxiety_pure", "bipolar_pure",
                                  "PTSD_pure", "OCD_pure","ADHD_pure", "season1", "season2", "season3")]

for(i in 1:45) {
  preg_flu_norisk_imp_iptw[[i]] <- merge(preg_flu_norisk_imp_iptw[[i]], to_add, all = T)
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_norisk_any_mh <- list()
model_list_preg_flu_norisk_mixed <- list()
model_list_preg_flu_norisk_depression <- list()
model_list_preg_flu_norisk_anxiety <- list()
model_list_preg_flu_norisk_bipolar <- list()
model_list_preg_flu_norisk_PTSD <- list()
model_list_preg_flu_norisk_OCD <- list()
model_list_preg_flu_norisk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_any_mh[[i]] <- glm(as.numeric(all.seasons) ~ mental + county_2017 +
                                                  country_of_birth + age_at_enrollment_categorized
                                                + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                family = poisson(link = "log"),
                                                data = preg_flu_norisk_imp_iptw[[i]],
                                                weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_mixed[[i]] <- glm(as.numeric(all.seasons) ~ mixed + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_depression[[i]] <- glm(as.numeric(all.seasons) ~ depression_pure + county_2017 +
                                                      country_of_birth + age_at_enrollment_categorized
                                                    + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                    family = poisson(link = "log"),
                                                    data = preg_flu_norisk_imp_iptw[[i]],
                                                    weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_anxiety[[i]] <- glm(as.numeric(all.seasons) ~ anxiety_pure + county_2017 +
                                                   country_of_birth + age_at_enrollment_categorized
                                                 + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                 family = poisson(link = "log"),
                                                 data = preg_flu_norisk_imp_iptw[[i]],
                                                 weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_bipolar[[i]] <- glm(as.numeric(all.seasons) ~ bipolar_pure + county_2017 +
                                                   country_of_birth + age_at_enrollment_categorized
                                                 + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                 family = poisson(link = "log"),
                                                 data = preg_flu_norisk_imp_iptw[[i]],
                                                 weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_PTSD[[i]] <- glm(as.numeric(all.seasons) ~ PTSD_pure + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_norisk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_OCD[[i]] <- glm(as.numeric(all.seasons) ~ OCD_pure + county_2017 +
                                               country_of_birth + age_at_enrollment_categorized
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_flu_norisk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_ADHD[[i]] <- glm(as.numeric(all.seasons) ~ ADHD_pure + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_norisk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_norisk_any_mh <- lapply(model_list_preg_flu_norisk_any_mh, extract_robust)
results_preg_flu_norisk_depression <- lapply(model_list_preg_flu_norisk_depression, extract_robust)
results_preg_flu_norisk_anxiety <- lapply(model_list_preg_flu_norisk_anxiety, extract_robust)
results_preg_flu_norisk_bipolar <- lapply(model_list_preg_flu_norisk_bipolar, extract_robust)
results_preg_flu_norisk_mixed <- lapply(model_list_preg_flu_norisk_mixed, extract_robust)
results_preg_flu_norisk_PTSD <- lapply(model_list_preg_flu_norisk_PTSD, extract_robust)
results_preg_flu_norisk_OCD <- lapply(model_list_preg_flu_norisk_OCD, extract_robust)
results_preg_flu_norisk_ADHD <- lapply(model_list_preg_flu_norisk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_norisk_any_mh_df <- bind_rows(results_preg_flu_norisk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_mixed_df <- bind_rows(results_preg_flu_norisk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_depression_df <- bind_rows(results_preg_flu_norisk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_anxiety_df <- bind_rows(results_preg_flu_norisk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_bipolar_df <- bind_rows(results_preg_flu_norisk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_PTSD_df <- bind_rows(results_preg_flu_norisk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_OCD_df <- bind_rows(results_preg_flu_norisk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_ADHD_df <- bind_rows(results_preg_flu_norisk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_norisk_any_mh <- results_preg_flu_norisk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_mixed <- results_preg_flu_norisk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_depression <- results_preg_flu_norisk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_anxiety <- results_preg_flu_norisk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_bipolar <- results_preg_flu_norisk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_PTSD <- results_preg_flu_norisk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_OCD <- results_preg_flu_norisk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_ADHD <- results_preg_flu_norisk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_preg_flu_norisk_ADHD, file = "Pooled results/Pregnant_Influenza_NoRisk_ADHD_SA.rdata")
save(pooled_results_preg_flu_norisk_any_mh, file = "Pooled results/Pregnant_Influenza_NoRisk_AnyMentalHealth_SA.rdata")
save(pooled_results_preg_flu_norisk_mixed, file = "Pooled results/Pregnant_Influenza_NoRisk_Mixed_SA.rdata")
save(pooled_results_preg_flu_norisk_depression, file = "Pooled results/Pregnant_Influenza_NoRisk_Depression_SA.rdata")
save(pooled_results_preg_flu_norisk_anxiety, file = "Pooled results/Pregnant_Influenza_NoRisk_Anxiety_SA.rdata")
save(pooled_results_preg_flu_norisk_bipolar, file = "Pooled results/Pregnant_Influenza_NoRisk_Bipolar_SA.rdata")
save(pooled_results_preg_flu_norisk_OCD, file = "Pooled results/Pregnant_Influenza_NoRisk_OCD_SA.rdata")
save(pooled_results_preg_flu_norisk_PTSD, file = "Pooled results/Pregnant_Influenza_NoRisk_PTSD_SA.rdata")


####################################################3
# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_norisk_any_mh <- list()
model_list_preg_flu_norisk_mixed <- list()
model_list_preg_flu_norisk_depression <- list()
model_list_preg_flu_norisk_anxiety <- list()
model_list_preg_flu_norisk_bipolar <- list()
model_list_preg_flu_norisk_PTSD <- list()
model_list_preg_flu_norisk_OCD <- list()
model_list_preg_flu_norisk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_any_mh[[i]] <- glm(as.numeric(season1) ~ mental + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_norisk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_mixed[[i]] <- glm(as.numeric(season1) ~ mixed + county_2017 +
                                               country_of_birth + age_at_enrollment_categorized
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_flu_norisk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_depression[[i]] <- glm(as.numeric(season1) ~ depression_pure + county_2017 +
                                                    country_of_birth + age_at_enrollment_categorized
                                                  + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                  family = poisson(link = "log"),
                                                  data = preg_flu_norisk_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_anxiety[[i]] <- glm(as.numeric(season1) ~ anxiety_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_bipolar[[i]] <- glm(as.numeric(season1) ~ bipolar_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_PTSD[[i]] <- glm(as.numeric(season1) ~ PTSD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_norisk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_OCD[[i]] <- glm(as.numeric(season1) ~ OCD_pure + county_2017 +
                                             country_of_birth + age_at_enrollment_categorized
                                           + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                           family = poisson(link = "log"),
                                           data = preg_flu_norisk_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_ADHD[[i]] <- glm(as.numeric(season1) ~ ADHD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_norisk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_norisk_any_mh <- lapply(model_list_preg_flu_norisk_any_mh, extract_robust)
results_preg_flu_norisk_depression <- lapply(model_list_preg_flu_norisk_depression, extract_robust)
results_preg_flu_norisk_anxiety <- lapply(model_list_preg_flu_norisk_anxiety, extract_robust)
results_preg_flu_norisk_bipolar <- lapply(model_list_preg_flu_norisk_bipolar, extract_robust)
results_preg_flu_norisk_mixed <- lapply(model_list_preg_flu_norisk_mixed, extract_robust)
results_preg_flu_norisk_PTSD <- lapply(model_list_preg_flu_norisk_PTSD, extract_robust)
results_preg_flu_norisk_OCD <- lapply(model_list_preg_flu_norisk_OCD, extract_robust)
results_preg_flu_norisk_ADHD <- lapply(model_list_preg_flu_norisk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_norisk_any_mh_df <- bind_rows(results_preg_flu_norisk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_mixed_df <- bind_rows(results_preg_flu_norisk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_depression_df <- bind_rows(results_preg_flu_norisk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_anxiety_df <- bind_rows(results_preg_flu_norisk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_bipolar_df <- bind_rows(results_preg_flu_norisk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_PTSD_df <- bind_rows(results_preg_flu_norisk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_OCD_df <- bind_rows(results_preg_flu_norisk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_ADHD_df <- bind_rows(results_preg_flu_norisk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_norisk_any_mh <- results_preg_flu_norisk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_mixed <- results_preg_flu_norisk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_depression <- results_preg_flu_norisk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_anxiety <- results_preg_flu_norisk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_bipolar <- results_preg_flu_norisk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_PTSD <- results_preg_flu_norisk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_OCD <- results_preg_flu_norisk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_ADHD <- results_preg_flu_norisk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_ADHD_seasosn1 <- pooled_results_preg_flu_norisk_ADHD
pooled_results_preg_flu_norisk_any_mh_seasosn1 <- pooled_results_preg_flu_norisk_any_mh
pooled_results_preg_flu_norisk_mixed_seasosn1 <- pooled_results_preg_flu_norisk_mixed
pooled_results_preg_flu_norisk_depression_seasosn1 <- pooled_results_preg_flu_norisk_depression
pooled_results_preg_flu_norisk_anxiety_seasosn1 <- pooled_results_preg_flu_norisk_anxiety
pooled_results_preg_flu_norisk_bipolar_seasosn1 <- pooled_results_preg_flu_norisk_bipolar
pooled_results_preg_flu_norisk_OCD_seasosn1 <- pooled_results_preg_flu_norisk_OCD
pooled_results_preg_flu_norisk_PTSD_seasosn1 <- pooled_results_preg_flu_norisk_PTSD

save(pooled_results_preg_flu_norisk_ADHD_seasosn1, file = "Pooled results/Pregnant_Influenza_NoRisk_ADHD_Season1_SA.rdata")
save(pooled_results_preg_flu_norisk_any_mh_seasosn1, file = "Pooled results/Pregnant_Influenza_NoRisk_AnyMentalHealth_Season1_SA.rdata")
save(pooled_results_preg_flu_norisk_mixed_seasosn1, file = "Pooled results/Pregnant_Influenza_NoRisk_Mixed_Season1_SA.rdata")
save(pooled_results_preg_flu_norisk_depression_seasosn1, file = "Pooled results/Pregnant_Influenza_NoRisk_Depression_Season1_SA.rdata")
save(pooled_results_preg_flu_norisk_anxiety_seasosn1, file = "Pooled results/Pregnant_Influenza_NoRisk_Anxiety_Season1_SA.rdata")
save(pooled_results_preg_flu_norisk_bipolar_seasosn1, file = "Pooled results/Pregnant_Influenza_NoRisk_Bipolar_Season1_SA.rdata")
save(pooled_results_preg_flu_norisk_OCD_seasosn1, file = "Pooled results/Pregnant_Influenza_NoRisk_OCD_Season1_SA.rdata")
save(pooled_results_preg_flu_norisk_PTSD_seasosn1, file = "Pooled results/Pregnant_Influenza_NoRisk_PTSD_Season1_SA.rdata")
##############################

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_norisk_any_mh <- list()
model_list_preg_flu_norisk_mixed <- list()
model_list_preg_flu_norisk_depression <- list()
model_list_preg_flu_norisk_anxiety <- list()
model_list_preg_flu_norisk_bipolar <- list()
model_list_preg_flu_norisk_PTSD <- list()
model_list_preg_flu_norisk_OCD <- list()
model_list_preg_flu_norisk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_any_mh[[i]] <- glm(as.numeric(season2) ~ mental + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_norisk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_mixed[[i]] <- glm(as.numeric(season2) ~ mixed + county_2017 +
                                               country_of_birth + age_at_enrollment_categorized
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_flu_norisk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_depression[[i]] <- glm(as.numeric(season2) ~ depression_pure + county_2017 +
                                                    country_of_birth + age_at_enrollment_categorized
                                                  + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                  family = poisson(link = "log"),
                                                  data = preg_flu_norisk_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_anxiety[[i]] <- glm(as.numeric(season2) ~ anxiety_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_bipolar[[i]] <- glm(as.numeric(season2) ~ bipolar_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_PTSD[[i]] <- glm(as.numeric(season2) ~ PTSD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_norisk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_OCD[[i]] <- glm(as.numeric(season2) ~ OCD_pure + county_2017 +
                                             country_of_birth + age_at_enrollment_categorized
                                           + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                           family = poisson(link = "log"),
                                           data = preg_flu_norisk_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_ADHD[[i]] <- glm(as.numeric(season2) ~ ADHD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_norisk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_norisk_any_mh <- lapply(model_list_preg_flu_norisk_any_mh, extract_robust)
results_preg_flu_norisk_depression <- lapply(model_list_preg_flu_norisk_depression, extract_robust)
results_preg_flu_norisk_anxiety <- lapply(model_list_preg_flu_norisk_anxiety, extract_robust)
results_preg_flu_norisk_bipolar <- lapply(model_list_preg_flu_norisk_bipolar, extract_robust)
results_preg_flu_norisk_mixed <- lapply(model_list_preg_flu_norisk_mixed, extract_robust)
results_preg_flu_norisk_PTSD <- lapply(model_list_preg_flu_norisk_PTSD, extract_robust)
results_preg_flu_norisk_OCD <- lapply(model_list_preg_flu_norisk_OCD, extract_robust)
results_preg_flu_norisk_ADHD <- lapply(model_list_preg_flu_norisk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_norisk_any_mh_df <- bind_rows(results_preg_flu_norisk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_mixed_df <- bind_rows(results_preg_flu_norisk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_depression_df <- bind_rows(results_preg_flu_norisk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_anxiety_df <- bind_rows(results_preg_flu_norisk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_bipolar_df <- bind_rows(results_preg_flu_norisk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_PTSD_df <- bind_rows(results_preg_flu_norisk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_OCD_df <- bind_rows(results_preg_flu_norisk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_ADHD_df <- bind_rows(results_preg_flu_norisk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_norisk_any_mh <- results_preg_flu_norisk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_mixed <- results_preg_flu_norisk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_depression <- results_preg_flu_norisk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_anxiety <- results_preg_flu_norisk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_bipolar <- results_preg_flu_norisk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_PTSD <- results_preg_flu_norisk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_OCD <- results_preg_flu_norisk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_ADHD <- results_preg_flu_norisk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_ADHD_season2 <- pooled_results_preg_flu_norisk_ADHD
pooled_results_preg_flu_norisk_any_mh_season2 <- pooled_results_preg_flu_norisk_any_mh
pooled_results_preg_flu_norisk_mixed_season2 <- pooled_results_preg_flu_norisk_mixed
pooled_results_preg_flu_norisk_depression_season2 <- pooled_results_preg_flu_norisk_depression
pooled_results_preg_flu_norisk_anxiety_season2 <- pooled_results_preg_flu_norisk_anxiety
pooled_results_preg_flu_norisk_bipolar_season2 <- pooled_results_preg_flu_norisk_bipolar
pooled_results_preg_flu_norisk_OCD_season2 <- pooled_results_preg_flu_norisk_OCD
pooled_results_preg_flu_norisk_PTSD_season2 <- pooled_results_preg_flu_norisk_PTSD

save(pooled_results_preg_flu_norisk_ADHD_season2, file = "Pooled results/Pregnant_Influenza_NoRisk_ADHD_season2_SA.rdata")
save(pooled_results_preg_flu_norisk_any_mh_season2, file = "Pooled results/Pregnant_Influenza_NoRisk_AnyMentalHealth_season2_SA.rdata")
save(pooled_results_preg_flu_norisk_mixed_season2, file = "Pooled results/Pregnant_Influenza_NoRisk_Mixed_season2_SA.rdata")
save(pooled_results_preg_flu_norisk_depression_season2, file = "Pooled results/Pregnant_Influenza_NoRisk_Depression_season2_SA.rdata")
save(pooled_results_preg_flu_norisk_anxiety_season2, file = "Pooled results/Pregnant_Influenza_NoRisk_Anxiety_season2_SA.rdata")
save(pooled_results_preg_flu_norisk_bipolar_season2, file = "Pooled results/Pregnant_Influenza_NoRisk_Bipolar_season2_SA.rdata")
save(pooled_results_preg_flu_norisk_OCD_season2, file = "Pooled results/Pregnant_Influenza_NoRisk_OCD_season2_SA.rdata")
save(pooled_results_preg_flu_norisk_PTSD_season2, file = "Pooled results/Pregnant_Influenza_NoRisk_PTSD_season2_SA.rdata")
###################################

# Step 2: Run modified Poisson with weights on each imputation
model_list_preg_flu_norisk_any_mh <- list()
model_list_preg_flu_norisk_mixed <- list()
model_list_preg_flu_norisk_depression <- list()
model_list_preg_flu_norisk_anxiety <- list()
model_list_preg_flu_norisk_bipolar <- list()
model_list_preg_flu_norisk_PTSD <- list()
model_list_preg_flu_norisk_OCD <- list()
model_list_preg_flu_norisk_ADHD <- list()

for(i in 1:45) {
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_any_mh[[i]] <- glm(as.numeric(season3) ~ mental + county_2017 +
                                                country_of_birth + age_at_enrollment_categorized
                                              + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                              family = poisson(link = "log"),
                                              data = preg_flu_norisk_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_mixed[[i]] <- glm(as.numeric(season3) ~ mixed + county_2017 +
                                               country_of_birth + age_at_enrollment_categorized
                                             + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                             family = poisson(link = "log"),
                                             data = preg_flu_norisk_imp_iptw[[i]],
                                             weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_depression[[i]] <- glm(as.numeric(season3) ~ depression_pure + county_2017 +
                                                    country_of_birth + age_at_enrollment_categorized
                                                  + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                                  family = poisson(link = "log"),
                                                  data = preg_flu_norisk_imp_iptw[[i]],
                                                  weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_anxiety[[i]] <- glm(as.numeric(season3) ~ anxiety_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_bipolar[[i]] <- glm(as.numeric(season3) ~ bipolar_pure + county_2017 +
                                                 country_of_birth + age_at_enrollment_categorized
                                               + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                               family = poisson(link = "log"),
                                               data = preg_flu_norisk_imp_iptw[[i]],
                                               weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_PTSD[[i]] <- glm(as.numeric(season3) ~ PTSD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_norisk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_OCD[[i]] <- glm(as.numeric(season3) ~ OCD_pure + county_2017 +
                                             country_of_birth + age_at_enrollment_categorized
                                           + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                           family = poisson(link = "log"),
                                           data = preg_flu_norisk_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_preg_flu_norisk_ADHD[[i]] <- glm(as.numeric(season3) ~ ADHD_pure + county_2017 +
                                              country_of_birth + age_at_enrollment_categorized
                                            + smoking + parity + Education + Marital_status + BMI + profession + income_2017_cat, 
                                            family = poisson(link = "log"),
                                            data = preg_flu_norisk_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_preg_flu_norisk_any_mh <- lapply(model_list_preg_flu_norisk_any_mh, extract_robust)
results_preg_flu_norisk_depression <- lapply(model_list_preg_flu_norisk_depression, extract_robust)
results_preg_flu_norisk_anxiety <- lapply(model_list_preg_flu_norisk_anxiety, extract_robust)
results_preg_flu_norisk_bipolar <- lapply(model_list_preg_flu_norisk_bipolar, extract_robust)
results_preg_flu_norisk_mixed <- lapply(model_list_preg_flu_norisk_mixed, extract_robust)
results_preg_flu_norisk_PTSD <- lapply(model_list_preg_flu_norisk_PTSD, extract_robust)
results_preg_flu_norisk_OCD <- lapply(model_list_preg_flu_norisk_OCD, extract_robust)
results_preg_flu_norisk_ADHD <- lapply(model_list_preg_flu_norisk_ADHD, extract_robust)

# Combine into one dataframe
results_preg_flu_norisk_any_mh_df <- bind_rows(results_preg_flu_norisk_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_mixed_df <- bind_rows(results_preg_flu_norisk_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_depression_df <- bind_rows(results_preg_flu_norisk_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_anxiety_df <- bind_rows(results_preg_flu_norisk_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_bipolar_df <- bind_rows(results_preg_flu_norisk_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_PTSD_df <- bind_rows(results_preg_flu_norisk_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_OCD_df <- bind_rows(results_preg_flu_norisk_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_preg_flu_norisk_ADHD_df <- bind_rows(results_preg_flu_norisk_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_preg_flu_norisk_any_mh <- results_preg_flu_norisk_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_mixed <- results_preg_flu_norisk_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_depression <- results_preg_flu_norisk_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_anxiety <- results_preg_flu_norisk_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_bipolar <- results_preg_flu_norisk_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_PTSD <- results_preg_flu_norisk_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_OCD <- results_preg_flu_norisk_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_ADHD <- results_preg_flu_norisk_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_preg_flu_norisk_ADHD_season3 <- pooled_results_preg_flu_norisk_ADHD
pooled_results_preg_flu_norisk_any_mh_season3 <- pooled_results_preg_flu_norisk_any_mh
pooled_results_preg_flu_norisk_mixed_season3 <- pooled_results_preg_flu_norisk_mixed
pooled_results_preg_flu_norisk_depression_season3 <- pooled_results_preg_flu_norisk_depression
pooled_results_preg_flu_norisk_anxiety_season3 <- pooled_results_preg_flu_norisk_anxiety
pooled_results_preg_flu_norisk_bipolar_season3 <- pooled_results_preg_flu_norisk_bipolar
pooled_results_preg_flu_norisk_OCD_season3 <- pooled_results_preg_flu_norisk_OCD
pooled_results_preg_flu_norisk_PTSD_season3 <- pooled_results_preg_flu_norisk_PTSD

save(pooled_results_preg_flu_norisk_ADHD_season3, file = "Pooled results/Pregnant_Influenza_NoRisk_ADHD_season3_SA.rdata")
save(pooled_results_preg_flu_norisk_any_mh_season3, file = "Pooled results/Pregnant_Influenza_NoRisk_AnyMentalHealth_season3_SA.rdata")
save(pooled_results_preg_flu_norisk_mixed_season3, file = "Pooled results/Pregnant_Influenza_NoRisk_Mixed_season3_SA.rdata")
save(pooled_results_preg_flu_norisk_depression_season3, file = "Pooled results/Pregnant_Influenza_NoRisk_Depression_season3_SA.rdata")
save(pooled_results_preg_flu_norisk_anxiety_season3, file = "Pooled results/Pregnant_Influenza_NoRisk_Anxiety_season3_SA.rdata")
save(pooled_results_preg_flu_norisk_bipolar_season3, file = "Pooled results/Pregnant_Influenza_NoRisk_Bipolar_season3_SA.rdata")
save(pooled_results_preg_flu_norisk_OCD_season3, file = "Pooled results/Pregnant_Influenza_NoRisk_OCD_season3_SA.rdata")
save(pooled_results_preg_flu_norisk_PTSD_season3, file = "Pooled results/Pregnant_Influenza_NoRisk_PTSD_season3_SA.rdata")
############################################
to_add_old_covid <- old_covid[, c("person_id", "mental", "mixed", "depression_pure","anxiety_pure",
                      "bipolar_pure","PTSD_pure", "OCD_pure","ADHD_pure",
                      "eligible_dose2_date","eligible_booster1_date",
                      "eligible_booster2_date","received_valid_dose2",
                      "received_valid_booster1","received_valid_booster2",
                      "is_age_eligible_booster1", "is_age_eligible_primary",
                      "is_age_eligible_booster2")]
# to_add_primary <- primary_vacc[, c("person_id", "time_to_event","status")]
# to_add_old_covid <- to_add_old_covid[to_add_old_covid$person_id %in% to_add_primary$person_id,] 
# to_add <- merge(to_add_old_covid, to_add_primary, all = T)
to_add <- to_add_old_covid[to_add_old_covid$is_age_eligible_primary == 1,]
for(i in 1:30) {
  old_covid_imp_iptw[[i]]$mental <- NULL
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][old_covid_imp_iptw[[i]]$person_id %in% 
                                                       to_add$person_id,]
  old_covid_imp_iptw[[i]] <- merge(old_covid_imp_iptw[[i]], to_add, all = T)
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]] %>% group_by(old_covid_imp_iptw[[i]]) %>%
    mutate(anxiety_pure = max(anxiety_pure))
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][!duplicated(old_covid_imp_iptw[[i]]$person_id),]
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_old_covid_any_mh <- list()
model_list_old_covid_mixed <- list()
model_list_old_covid_depression <- list()
model_list_old_covid_anxiety <- list()
model_list_old_covid_bipolar <- list()
model_list_old_covid_PTSD <- list()
model_list_old_covid_OCD <- list()
model_list_old_covid_ADHD <- list()

for(i in 1:30) {
  # Run weighted modified Poisson
  model_list_old_covid_any_mh[[i]] <- glm(as.numeric(received_valid_dose2) ~ mental + county_2021 +
                                          country_of_birth + risk_factor 
                                        +  income_2021_cat + age_at_enrollment_categorized, 
                                        family = poisson(link = "log"),
                                        data = old_covid_imp_iptw[[i]],
                                        weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_mixed[[i]] <- glm(as.numeric(received_valid_dose2) ~ mixed + county_2021 +
                                         country_of_birth + risk_factor 
                                       +  income_2021_cat + age_at_enrollment_categorized, 
                                       family = poisson(link = "log"),
                                       data = old_covid_imp_iptw[[i]],
                                       weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_depression[[i]] <- glm(as.numeric(received_valid_dose2) ~ depression_pure + county_2021 +
                                              country_of_birth + risk_factor 
                                            +  income_2021_cat + age_at_enrollment_categorized, 
                                            family = poisson(link = "log"),
                                            data = old_covid_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_anxiety[[i]] <- glm(as.numeric(received_valid_dose2) ~ anxiety_pure + county_2021 +
                                           country_of_birth + risk_factor 
                                         +  income_2021_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_covid_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_bipolar[[i]] <- glm(as.numeric(received_valid_dose2) ~ bipolar_pure + county_2021 +
                                           country_of_birth + risk_factor 
                                         +  income_2021_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_covid_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_PTSD[[i]] <- glm(as.numeric(received_valid_dose2) ~ PTSD_pure + county_2021 +
                                        country_of_birth + risk_factor 
                                      +  income_2021_cat + age_at_enrollment_categorized, 
                                      family = poisson(link = "log"),
                                      data = old_covid_imp_iptw[[i]],
                                      weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_OCD[[i]] <- glm(as.numeric(received_valid_dose2) ~ OCD_pure + + county_2021 +
                                       country_of_birth + risk_factor 
                                     +  income_2021_cat + age_at_enrollment_categorized, 
                                     family = poisson(link = "log"),
                                     data = old_covid_imp_iptw[[i]],
                                     weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_ADHD[[i]] <- glm(as.numeric(received_valid_dose2) ~ ADHD_pure + county_2021 +
                                        country_of_birth + risk_factor 
                                      +  income_2021_cat + age_at_enrollment_categorized, 
                                      family = poisson(link = "log"),
                                      data = old_covid_imp_iptw[[i]],
                                      weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_old_covid_any_mh <- lapply(model_list_old_covid_any_mh, extract_robust)
results_old_covid_depression <- lapply(model_list_old_covid_depression, extract_robust)
results_old_covid_anxiety <- lapply(model_list_old_covid_anxiety, extract_robust)
results_old_covid_bipolar <- lapply(model_list_old_covid_bipolar, extract_robust)
results_old_covid_mixed <- lapply(model_list_old_covid_mixed, extract_robust)
results_old_covid_PTSD <- lapply(model_list_old_covid_PTSD, extract_robust)
results_old_covid_OCD <- lapply(model_list_old_covid_OCD, extract_robust)
results_old_covid_ADHD <- lapply(model_list_old_covid_ADHD, extract_robust)

# Combine into one dataframe
results_old_covid_any_mh_df <- bind_rows(results_old_covid_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_mixed_df <- bind_rows(results_old_covid_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_depression_df <- bind_rows(results_old_covid_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_anxiety_df <- bind_rows(results_old_covid_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_bipolar_df <- bind_rows(results_old_covid_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_PTSD_df <- bind_rows(results_old_covid_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_OCD_df <- bind_rows(results_old_covid_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_ADHD_df <- bind_rows(results_old_covid_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_old_covid_any_mh <- results_old_covid_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_mixed <- results_old_covid_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_depression <- results_old_covid_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_anxiety <- results_old_covid_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_bipolar <- results_old_covid_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_PTSD <- results_old_covid_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_OCD <- results_old_covid_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_ADHD <- results_old_covid_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_old_covid_ADHD, file = "Pooled results/Old_COVID19_ADHD_SA.rdata")
save(pooled_results_old_covid_any_mh, file = "Pooled results/Old_COVID19_AnyMentalHealth_SA.rdata")
save(pooled_results_old_covid_mixed, file = "Pooled results/Old_COVID19_Mixed_SA.rdata")
save(pooled_results_old_covid_depression, file = "Pooled results/Old_COVID19_Depression_SA.rdata")
save(pooled_results_old_covid_anxiety, file = "Pooled results/Old_COVID19_Anxiety_SA.rdata")
save(pooled_results_old_covid_bipolar, file = "Pooled results/Old_COVID19_Bipolar_SA.rdata")
save(pooled_results_old_covid_OCD, file = "Pooled results/Old_COVID19_OCD_SA.rdata")
save(pooled_results_old_covid_PTSD, file = "Pooled results/Old_COVID19_PTSD_SA.rdata")
###########################################################
# old_covid <- old_covid[old_covid$is_age_eligible_booster1 == 1,]
# to_add_old_covid <- old_covid[, c("person_id", "mental", "mixed", "depression_pure","anxiety_pure",
#                                   "bipolar_pure","PTSD_pure", "OCD_pure","ADHD_pure",
#                                   "eligible_dose2_date","eligible_booster1_date",
#                                   "eligible_booster2_date","received_valid_dose2",
#                                   "received_valid_booster1","received_valid_booster2")]
# to_add_primary <- primary_vacc[, c("person_id", "time_to_event_booster1","status_booster1")]
# to_add_primary <- to_add_primary[to_add_primary$person_id %in% to_add_old_covid$person_id,] 
to_add <- to_add_old_covid[to_add_old_covid$is_age_eligible_booster1 == 1,]
to_add$person_id <- as.character(to_add$person_id)
old_covid_imp_iptw <- old_covid_imp
for(i in 1:30) {
  old_covid_imp_iptw[[i]]$mental <- NULL
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][old_covid_imp_iptw[[i]]$person_id %in% 
                                                       to_add$person_id,]
  old_covid_imp_iptw[[i]] <- merge(old_covid_imp_iptw[[i]], to_add, all = T)
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][!duplicated(old_covid_imp_iptw[[i]]$person_id),]
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_old_covid_any_mh <- list()
model_list_old_covid_mixed <- list()
model_list_old_covid_depression <- list()
model_list_old_covid_anxiety <- list()
model_list_old_covid_bipolar <- list()
model_list_old_covid_PTSD <- list()
model_list_old_covid_OCD <- list()
model_list_old_covid_ADHD <- list()

for(i in 1:30) {
  # Run weighted modified Poisson
  model_list_old_covid_any_mh[[i]] <- glm(as.numeric(received_valid_booster1) ~ mental + county_2021 +
                                            country_of_birth + risk_factor 
                                          +  income_2021_cat + age_at_enrollment_categorized, 
                                          family = poisson(link = "log"),
                                          data = old_covid_imp_iptw[[i]],
                                          weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_mixed[[i]] <- glm(as.numeric(received_valid_booster1) ~ mixed + county_2021 +
                                           country_of_birth + risk_factor 
                                         +  income_2021_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_covid_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_depression[[i]] <- glm(as.numeric(received_valid_booster1) ~ depression_pure + county_2021 +
                                                country_of_birth + risk_factor 
                                              +  income_2021_cat + age_at_enrollment_categorized, 
                                              family = poisson(link = "log"),
                                              data = old_covid_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_anxiety[[i]] <- glm(as.numeric(received_valid_booster1) ~ anxiety_pure + county_2021 +
                                             country_of_birth + risk_factor 
                                           +  income_2021_cat + age_at_enrollment_categorized, 
                                           family = poisson(link = "log"),
                                           data = old_covid_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_bipolar[[i]] <- glm(as.numeric(received_valid_booster1) ~ bipolar_pure + county_2021 +
                                             country_of_birth + risk_factor 
                                           +  income_2021_cat + age_at_enrollment_categorized, 
                                           family = poisson(link = "log"),
                                           data = old_covid_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_PTSD[[i]] <- glm(as.numeric(received_valid_booster1) ~ PTSD_pure + county_2021 +
                                          country_of_birth + risk_factor 
                                        +  income_2021_cat + age_at_enrollment_categorized, 
                                        family = poisson(link = "log"),
                                        data = old_covid_imp_iptw[[i]],
                                        weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_OCD[[i]] <- glm(as.numeric(received_valid_booster1) ~ OCD_pure + + county_2021 +
                                         country_of_birth + risk_factor 
                                       +  income_2021_cat + age_at_enrollment_categorized, 
                                       family = poisson(link = "log"),
                                       data = old_covid_imp_iptw[[i]],
                                       weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_ADHD[[i]] <- glm(as.numeric(received_valid_booster1) ~ ADHD_pure + county_2021 +
                                          country_of_birth + risk_factor 
                                        +  income_2021_cat + age_at_enrollment_categorized, 
                                        family = poisson(link = "log"),
                                        data = old_covid_imp_iptw[[i]],
                                        weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_old_covid_any_mh <- lapply(model_list_old_covid_any_mh, extract_robust)
results_old_covid_depression <- lapply(model_list_old_covid_depression, extract_robust)
results_old_covid_anxiety <- lapply(model_list_old_covid_anxiety, extract_robust)
results_old_covid_bipolar <- lapply(model_list_old_covid_bipolar, extract_robust)
results_old_covid_mixed <- lapply(model_list_old_covid_mixed, extract_robust)
results_old_covid_PTSD <- lapply(model_list_old_covid_PTSD, extract_robust)
results_old_covid_OCD <- lapply(model_list_old_covid_OCD, extract_robust)
results_old_covid_ADHD <- lapply(model_list_old_covid_ADHD, extract_robust)

# Combine into one dataframe
results_old_covid_any_mh_df <- bind_rows(results_old_covid_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_mixed_df <- bind_rows(results_old_covid_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_depression_df <- bind_rows(results_old_covid_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_anxiety_df <- bind_rows(results_old_covid_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_bipolar_df <- bind_rows(results_old_covid_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_PTSD_df <- bind_rows(results_old_covid_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_OCD_df <- bind_rows(results_old_covid_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_ADHD_df <- bind_rows(results_old_covid_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_old_covid_any_mh <- results_old_covid_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_mixed <- results_old_covid_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_depression <- results_old_covid_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_anxiety <- results_old_covid_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_bipolar <- results_old_covid_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_PTSD <- results_old_covid_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_OCD <- results_old_covid_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_ADHD <- results_old_covid_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_old_covid_ADHD, file = "Pooled results/Old_COVID19_ADHD_Booster1_SA.rdata")
save(pooled_results_old_covid_any_mh, file = "Pooled results/Old_COVID19_AnyMentalHealth_Booster1_SA.rdata")
save(pooled_results_old_covid_mixed, file = "Pooled results/Old_COVID19_Mixed_Booster1_SA.rdata")
save(pooled_results_old_covid_depression, file = "Pooled results/Old_COVID19_Depression_Booster1_SA.rdata")
save(pooled_results_old_covid_anxiety, file = "Pooled results/Old_COVID19_Anxiety_Booster1_SA.rdata")
save(pooled_results_old_covid_bipolar, file = "Pooled results/Old_COVID19_Bipolar_Booster1_SA.rdata")
save(pooled_results_old_covid_OCD, file = "Pooled results/Old_COVID19_OCD_Booster1_SA.rdata")
save(pooled_results_old_covid_PTSD, file = "Pooled results/Old_COVID19_PTSD_Booster1_SA.rdata")
##########################################################
# old_covid <- old_covid[old_covid$is_age_eligible_booster2 == 1,]
# to_add_old_covid <- old_covid[, c("person_id", "mental", "mixed", "depression_pure","anxiety_pure",
#                                   "bipolar_pure","PTSD_pure", "OCD_pure","ADHD_pure",
#                                   "eligible_dose2_date","eligible_booster1_date",
#                                   "eligible_booster2_date","received_valid_dose2",
#                                   "received_valid_booster1","received_valid_booster2")]
# to_add_primary <- primary_vacc[, c("person_id", "time_to_event_booster2","status_booster2")]
# to_add_primary <- to_add_primary[to_add_primary$person_id %in% to_add_old_covid$person_id,] 
to_add <- to_add_old_covid[to_add_old_covid$is_age_eligible_booster2 == 1,]
to_add$person_id <- as.character(to_add$person_id)
old_covid_imp_iptw <- old_covid_imp
for(i in 1:30) {
  old_covid_imp_iptw[[i]]$mental <- NULL
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][old_covid_imp_iptw[[i]]$person_id %in% 
                                                       to_add$person_id,]
  old_covid_imp_iptw[[i]] <- merge(old_covid_imp_iptw[[i]], to_add, all = T)
  old_covid_imp_iptw[[i]] <- old_covid_imp_iptw[[i]][!duplicated(old_covid_imp_iptw[[i]]$person_id),]
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_old_covid_any_mh <- list()
model_list_old_covid_mixed <- list()
model_list_old_covid_depression <- list()
model_list_old_covid_anxiety <- list()
model_list_old_covid_bipolar <- list()
model_list_old_covid_PTSD <- list()
model_list_old_covid_OCD <- list()
model_list_old_covid_ADHD <- list()

for(i in 1:30) {
  # Run weighted modified Poisson
  model_list_old_covid_any_mh[[i]] <- glm(as.numeric(received_valid_booster2) ~ mental + county_2021 +
                                            country_of_birth + risk_factor 
                                          +  income_2021_cat + age_at_enrollment_categorized, 
                                          family = poisson(link = "log"),
                                          data = old_covid_imp_iptw[[i]],
                                          weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_mixed[[i]] <- glm(as.numeric(received_valid_booster2) ~ mixed + county_2021 +
                                           country_of_birth + risk_factor 
                                         +  income_2021_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_covid_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_depression[[i]] <- glm(as.numeric(received_valid_booster2) ~ depression_pure + county_2021 +
                                                country_of_birth + risk_factor 
                                              +  income_2021_cat + age_at_enrollment_categorized, 
                                              family = poisson(link = "log"),
                                              data = old_covid_imp_iptw[[i]],
                                              weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_anxiety[[i]] <- glm(as.numeric(received_valid_booster2) ~ anxiety_pure + county_2021 +
                                             country_of_birth + risk_factor 
                                           +  income_2021_cat + age_at_enrollment_categorized, 
                                           family = poisson(link = "log"),
                                           data = old_covid_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_bipolar[[i]] <- glm(as.numeric(received_valid_booster2) ~ bipolar_pure + county_2021 +
                                             country_of_birth + risk_factor 
                                           +  income_2021_cat + age_at_enrollment_categorized, 
                                           family = poisson(link = "log"),
                                           data = old_covid_imp_iptw[[i]],
                                           weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_PTSD[[i]] <- glm(as.numeric(received_valid_booster2) ~ PTSD_pure + county_2021 +
                                          country_of_birth + risk_factor 
                                        +  income_2021_cat + age_at_enrollment_categorized, 
                                        family = poisson(link = "log"),
                                        data = old_covid_imp_iptw[[i]],
                                        weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_OCD[[i]] <- glm(as.numeric(received_valid_booster2) ~ OCD_pure + + county_2021 +
                                         country_of_birth + risk_factor 
                                       +  income_2021_cat + age_at_enrollment_categorized, 
                                       family = poisson(link = "log"),
                                       data = old_covid_imp_iptw[[i]],
                                       weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_covid_ADHD[[i]] <- glm(as.numeric(received_valid_booster2) ~ ADHD_pure + county_2021 +
                                          country_of_birth + risk_factor 
                                        +  income_2021_cat + age_at_enrollment_categorized, 
                                        family = poisson(link = "log"),
                                        data = old_covid_imp_iptw[[i]],
                                        weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_old_covid_any_mh <- lapply(model_list_old_covid_any_mh, extract_robust)
results_old_covid_depression <- lapply(model_list_old_covid_depression, extract_robust)
results_old_covid_anxiety <- lapply(model_list_old_covid_anxiety, extract_robust)
results_old_covid_bipolar <- lapply(model_list_old_covid_bipolar, extract_robust)
results_old_covid_mixed <- lapply(model_list_old_covid_mixed, extract_robust)
results_old_covid_PTSD <- lapply(model_list_old_covid_PTSD, extract_robust)
results_old_covid_OCD <- lapply(model_list_old_covid_OCD, extract_robust)
results_old_covid_ADHD <- lapply(model_list_old_covid_ADHD, extract_robust)

# Combine into one dataframe
results_old_covid_any_mh_df <- bind_rows(results_old_covid_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_mixed_df <- bind_rows(results_old_covid_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_depression_df <- bind_rows(results_old_covid_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_anxiety_df <- bind_rows(results_old_covid_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_bipolar_df <- bind_rows(results_old_covid_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_PTSD_df <- bind_rows(results_old_covid_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_OCD_df <- bind_rows(results_old_covid_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_covid_ADHD_df <- bind_rows(results_old_covid_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_old_covid_any_mh <- results_old_covid_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_mixed <- results_old_covid_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_depression <- results_old_covid_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_anxiety <- results_old_covid_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_bipolar <- results_old_covid_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_PTSD <- results_old_covid_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_OCD <- results_old_covid_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_covid_ADHD <- results_old_covid_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_old_covid_ADHD, file = "Pooled results/Old_COVID19_ADHD_Booster2_SA.rdata")
save(pooled_results_old_covid_any_mh, file = "Pooled results/Old_COVID19_AnyMentalHealth_Booster2_SA.rdata")
save(pooled_results_old_covid_mixed, file = "Pooled results/Old_COVID19_Mixed_Booster2_SA.rdata")
save(pooled_results_old_covid_depression, file = "Pooled results/Old_COVID19_Depression_Booster2_SA.rdata")
save(pooled_results_old_covid_anxiety, file = "Pooled results/Old_COVID19_Anxiety_Booster2_SA.rdata")
save(pooled_results_old_covid_bipolar, file = "Pooled results/Old_COVID19_Bipolar_Booster2_SA.rdata")
save(pooled_results_old_covid_OCD, file = "Pooled results/Old_COVID19_OCD_Booster2_SA.rdata")
save(pooled_results_old_covid_PTSD, file = "Pooled results/Old_COVID19_PTSD_Booster2_SA.rdata")
##########################################################
to_add <- old_flu[, c("person_id", "mental", "mixed", "depression_pure",
                      "anxiety_pure", "bipolar_pure",
                      "PTSD_pure", "OCD_pure","ADHD_pure", "season1",
                      "season2", "season3")]
for(i in 1:30) {
  old_flu_imp_iptw[[i]] <- merge(old_flu_imp_iptw[[i]], to_add, all = T)
}

# Step 2: Run modified Poisson with weights on each imputation
model_list_old_flu_any_mh <- list()
model_list_old_flu_mixed <- list()
model_list_old_flu_depression <- list()
model_list_old_flu_anxiety <- list()
model_list_old_flu_bipolar <- list()
model_list_old_flu_PTSD <- list()
model_list_old_flu_OCD <- list()
model_list_old_flu_ADHD <- list()

for(i in 1:30) {
  # Run weighted modified Poisson
  model_list_old_flu_any_mh[[i]] <- glm(as.numeric(season1) ~ mental + county_2017 +
                                          country_of_birth + risk_factor 
                                        +  income_2017_cat + age_at_enrollment_categorized, 
                                        family = poisson(link = "log"),
                                        data = old_flu_imp_iptw[[i]],
                                        weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_mixed[[i]] <- glm(as.numeric(season1) ~ mixed + county_2017 +
                                         country_of_birth + risk_factor 
                                       +  income_2017_cat + age_at_enrollment_categorized, 
                                       family = poisson(link = "log"),
                                       data = old_flu_imp_iptw[[i]],
                                       weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_depression[[i]] <- glm(as.numeric(season1) ~ depression_pure + county_2017 +
                                              country_of_birth + risk_factor 
                                            +  income_2017_cat + age_at_enrollment_categorized, 
                                            family = poisson(link = "log"),
                                            data = old_flu_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_anxiety[[i]] <- glm(as.numeric(season1) ~ anxiety_pure + county_2017 +
                                           country_of_birth + risk_factor 
                                         +  income_2017_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_flu_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_bipolar[[i]] <- glm(as.numeric(season1) ~ bipolar_pure + county_2017 +
                                           country_of_birth + risk_factor 
                                         +  income_2017_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_flu_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_PTSD[[i]] <- glm(as.numeric(season1) ~ PTSD_pure + county_2017 +
                                        country_of_birth + risk_factor 
                                      +  income_2017_cat + age_at_enrollment_categorized, 
                                      family = poisson(link = "log"),
                                      data = old_flu_imp_iptw[[i]],
                                      weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_OCD[[i]] <- glm(as.numeric(season1) ~ OCD_pure + + county_2017 +
                                       country_of_birth + risk_factor 
                                     +  income_2017_cat + age_at_enrollment_categorized, 
                                     family = poisson(link = "log"),
                                     data = old_flu_imp_iptw[[i]],
                                     weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_ADHD[[i]] <- glm(as.numeric(season1) ~ ADHD_pure + county_2017 +
                                        country_of_birth + risk_factor 
                                      +  income_2017_cat + age_at_enrollment_categorized, 
                                      family = poisson(link = "log"),
                                      data = old_flu_imp_iptw[[i]],
                                      weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_old_flu_any_mh <- lapply(model_list_old_flu_any_mh, extract_robust)
results_old_flu_depression <- lapply(model_list_old_flu_depression, extract_robust)
results_old_flu_anxiety <- lapply(model_list_old_flu_anxiety, extract_robust)
results_old_flu_bipolar <- lapply(model_list_old_flu_bipolar, extract_robust)
results_old_flu_mixed <- lapply(model_list_old_flu_mixed, extract_robust)
results_old_flu_PTSD <- lapply(model_list_old_flu_PTSD, extract_robust)
results_old_flu_OCD <- lapply(model_list_old_flu_OCD, extract_robust)
results_old_flu_ADHD <- lapply(model_list_old_flu_ADHD, extract_robust)

# Combine into one dataframe
results_old_flu_any_mh_df <- bind_rows(results_old_flu_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_mixed_df <- bind_rows(results_old_flu_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_depression_df <- bind_rows(results_old_flu_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_anxiety_df <- bind_rows(results_old_flu_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_bipolar_df <- bind_rows(results_old_flu_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_PTSD_df <- bind_rows(results_old_flu_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_OCD_df <- bind_rows(results_old_flu_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_ADHD_df <- bind_rows(results_old_flu_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_old_flu_any_mh <- results_old_flu_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_mixed <- results_old_flu_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_depression <- results_old_flu_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_anxiety <- results_old_flu_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_bipolar <- results_old_flu_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_PTSD <- results_old_flu_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_OCD <- results_old_flu_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_ADHD <- results_old_flu_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_old_flu_ADHD, file = "Pooled results/Old_Influenza_ADHD_season1_SA.rdata")
save(pooled_results_old_flu_any_mh, file = "Pooled results/Old_Influenza_AnyMentalHealth_season1_SA.rdata")
save(pooled_results_old_flu_mixed, file = "Pooled results/Old_Influenza_Mixed_season1_SA.rdata")
save(pooled_results_old_flu_depression, file = "Pooled results/Old_Influenza_Depression_season1_SA.rdata")
save(pooled_results_old_flu_anxiety, file = "Pooled results/Old_Influenza_Anxiety_season1_SA.rdata")
save(pooled_results_old_flu_bipolar, file = "Pooled results/Old_Influenza_Bipolar_season1_SA.rdata")
save(pooled_results_old_flu_OCD, file = "Pooled results/Old_Influenza_OCD_season1_SA.rdata")
save(pooled_results_old_flu_PTSD, file = "Pooled results/Old_Influenza_PTSD_season1_SA.rdata")
###############################################################################

# Step 2: Run modified Poisson with weights on each imputation
model_list_old_flu_any_mh <- list()
model_list_old_flu_mixed <- list()
model_list_old_flu_depression <- list()
model_list_old_flu_anxiety <- list()
model_list_old_flu_bipolar <- list()
model_list_old_flu_PTSD <- list()
model_list_old_flu_OCD <- list()
model_list_old_flu_ADHD <- list()

for(i in 1:30) {
  # Run weighted modified Poisson
  model_list_old_flu_any_mh[[i]] <- glm(as.numeric(season2) ~ mental + county_2017 +
                                          country_of_birth + risk_factor 
                                        +  income_2017_cat + age_at_enrollment_categorized, 
                                        family = poisson(link = "log"),
                                        data = old_flu_imp_iptw[[i]],
                                        weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_mixed[[i]] <- glm(as.numeric(season2) ~ mixed + county_2017 +
                                         country_of_birth + risk_factor 
                                       +  income_2017_cat + age_at_enrollment_categorized, 
                                       family = poisson(link = "log"),
                                       data = old_flu_imp_iptw[[i]],
                                       weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_depression[[i]] <- glm(as.numeric(season2) ~ depression_pure + county_2017 +
                                              country_of_birth + risk_factor 
                                            +  income_2017_cat + age_at_enrollment_categorized, 
                                            family = poisson(link = "log"),
                                            data = old_flu_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_anxiety[[i]] <- glm(as.numeric(season2) ~ anxiety_pure + county_2017 +
                                           country_of_birth + risk_factor 
                                         +  income_2017_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_flu_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_bipolar[[i]] <- glm(as.numeric(season2) ~ bipolar_pure + county_2017 +
                                           country_of_birth + risk_factor 
                                         +  income_2017_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_flu_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_PTSD[[i]] <- glm(as.numeric(season2) ~ PTSD_pure + county_2017 +
                                        country_of_birth + risk_factor 
                                      +  income_2017_cat + age_at_enrollment_categorized, 
                                      family = poisson(link = "log"),
                                      data = old_flu_imp_iptw[[i]],
                                      weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_OCD[[i]] <- glm(as.numeric(season2) ~ OCD_pure + + county_2017 +
                                       country_of_birth + risk_factor 
                                     +  income_2017_cat + age_at_enrollment_categorized, 
                                     family = poisson(link = "log"),
                                     data = old_flu_imp_iptw[[i]],
                                     weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_ADHD[[i]] <- glm(as.numeric(season2) ~ ADHD_pure + county_2017 +
                                        country_of_birth + risk_factor 
                                      +  income_2017_cat + age_at_enrollment_categorized, 
                                      family = poisson(link = "log"),
                                      data = old_flu_imp_iptw[[i]],
                                      weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_old_flu_any_mh <- lapply(model_list_old_flu_any_mh, extract_robust)
results_old_flu_depression <- lapply(model_list_old_flu_depression, extract_robust)
results_old_flu_anxiety <- lapply(model_list_old_flu_anxiety, extract_robust)
results_old_flu_bipolar <- lapply(model_list_old_flu_bipolar, extract_robust)
results_old_flu_mixed <- lapply(model_list_old_flu_mixed, extract_robust)
results_old_flu_PTSD <- lapply(model_list_old_flu_PTSD, extract_robust)
results_old_flu_OCD <- lapply(model_list_old_flu_OCD, extract_robust)
results_old_flu_ADHD <- lapply(model_list_old_flu_ADHD, extract_robust)

# Combine into one dataframe
results_old_flu_any_mh_df <- bind_rows(results_old_flu_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_mixed_df <- bind_rows(results_old_flu_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_depression_df <- bind_rows(results_old_flu_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_anxiety_df <- bind_rows(results_old_flu_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_bipolar_df <- bind_rows(results_old_flu_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_PTSD_df <- bind_rows(results_old_flu_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_OCD_df <- bind_rows(results_old_flu_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_ADHD_df <- bind_rows(results_old_flu_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_old_flu_any_mh <- results_old_flu_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_mixed <- results_old_flu_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_depression <- results_old_flu_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_anxiety <- results_old_flu_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_bipolar <- results_old_flu_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_PTSD <- results_old_flu_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_OCD <- results_old_flu_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_ADHD <- results_old_flu_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_old_flu_ADHD, file = "Pooled results/Old_Influenza_ADHD_season2_SA.rdata")
save(pooled_results_old_flu_any_mh, file = "Pooled results/Old_Influenza_AnyMentalHealth_season2_SA.rdata")
save(pooled_results_old_flu_mixed, file = "Pooled results/Old_Influenza_Mixed_season2_SA.rdata")
save(pooled_results_old_flu_depression, file = "Pooled results/Old_Influenza_Depression_season2_SA.rdata")
save(pooled_results_old_flu_anxiety, file = "Pooled results/Old_Influenza_Anxiety_season2_SA.rdata")
save(pooled_results_old_flu_bipolar, file = "Pooled results/Old_Influenza_Bipolar_season2_SA.rdata")
save(pooled_results_old_flu_OCD, file = "Pooled results/Old_Influenza_OCD_season2_SA.rdata")
save(pooled_results_old_flu_PTSD, file = "Pooled results/Old_Influenza_PTSD_season2_SA.rdata")
##############################################################################

# Step 2: Run modified Poisson with weights on each imputation
model_list_old_flu_any_mh <- list()
model_list_old_flu_mixed <- list()
model_list_old_flu_depression <- list()
model_list_old_flu_anxiety <- list()
model_list_old_flu_bipolar <- list()
model_list_old_flu_PTSD <- list()
model_list_old_flu_OCD <- list()
model_list_old_flu_ADHD <- list()

for(i in 1:30) {
  # Run weighted modified Poisson
  model_list_old_flu_any_mh[[i]] <- glm(as.numeric(season2) ~ mental + county_2017 +
                                          country_of_birth + risk_factor 
                                        +  income_2017_cat + age_at_enrollment_categorized, 
                                        family = poisson(link = "log"),
                                        data = old_flu_imp_iptw[[i]],
                                        weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_mixed[[i]] <- glm(as.numeric(season2) ~ mixed + county_2017 +
                                         country_of_birth + risk_factor 
                                       +  income_2017_cat + age_at_enrollment_categorized, 
                                       family = poisson(link = "log"),
                                       data = old_flu_imp_iptw[[i]],
                                       weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_depression[[i]] <- glm(as.numeric(season2) ~ depression_pure + county_2017 +
                                              country_of_birth + risk_factor 
                                            +  income_2017_cat + age_at_enrollment_categorized, 
                                            family = poisson(link = "log"),
                                            data = old_flu_imp_iptw[[i]],
                                            weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_anxiety[[i]] <- glm(as.numeric(season2) ~ anxiety_pure + county_2017 +
                                           country_of_birth + risk_factor 
                                         +  income_2017_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_flu_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_bipolar[[i]] <- glm(as.numeric(season2) ~ bipolar_pure + county_2017 +
                                           country_of_birth + risk_factor 
                                         +  income_2017_cat + age_at_enrollment_categorized, 
                                         family = poisson(link = "log"),
                                         data = old_flu_imp_iptw[[i]],
                                         weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_PTSD[[i]] <- glm(as.numeric(season2) ~ PTSD_pure + county_2017 +
                                        country_of_birth + risk_factor 
                                      +  income_2017_cat + age_at_enrollment_categorized, 
                                      family = poisson(link = "log"),
                                      data = old_flu_imp_iptw[[i]],
                                      weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_OCD[[i]] <- glm(as.numeric(season2) ~ OCD_pure + + county_2017 +
                                       country_of_birth + risk_factor 
                                     +  income_2017_cat + age_at_enrollment_categorized, 
                                     family = poisson(link = "log"),
                                     data = old_flu_imp_iptw[[i]],
                                     weights = weights)  # Use IPTW weights
  
  # Run weighted modified Poisson
  model_list_old_flu_ADHD[[i]] <- glm(as.numeric(season2) ~ ADHD_pure + county_2017 +
                                        country_of_birth + risk_factor 
                                      +  income_2017_cat + age_at_enrollment_categorized, 
                                      family = poisson(link = "log"),
                                      data = old_flu_imp_iptw[[i]],
                                      weights = weights)  # Use IPTW weights
}

# Step 3: Extract robust coefficients and SEs from each model
extract_robust <- function(model) {
  # Get robust covariance matrix (accounts for both Poisson and weighting)
  robust_vcov <- vcovHC(model, type = "HC0")
  
  # Extract coefficients and robust SEs
  coefs <- coef(model)
  ses <- sqrt(diag(robust_vcov))
  
  # Return as data frame
  data.frame(
    term = names(coefs),
    estimate = coefs,
    se_robust = ses,
    var_robust = diag(robust_vcov)  # Store variance for pooling
  )
}

# Apply to all models
results_old_flu_any_mh <- lapply(model_list_old_flu_any_mh, extract_robust)
results_old_flu_depression <- lapply(model_list_old_flu_depression, extract_robust)
results_old_flu_anxiety <- lapply(model_list_old_flu_anxiety, extract_robust)
results_old_flu_bipolar <- lapply(model_list_old_flu_bipolar, extract_robust)
results_old_flu_mixed <- lapply(model_list_old_flu_mixed, extract_robust)
results_old_flu_PTSD <- lapply(model_list_old_flu_PTSD, extract_robust)
results_old_flu_OCD <- lapply(model_list_old_flu_OCD, extract_robust)
results_old_flu_ADHD <- lapply(model_list_old_flu_ADHD, extract_robust)

# Combine into one dataframe
results_old_flu_any_mh_df <- bind_rows(results_old_flu_any_mh, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_mixed_df <- bind_rows(results_old_flu_mixed, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_depression_df <- bind_rows(results_old_flu_depression, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_anxiety_df <- bind_rows(results_old_flu_anxiety, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_bipolar_df <- bind_rows(results_old_flu_bipolar, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_PTSD_df <- bind_rows(results_old_flu_PTSD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_OCD_df <- bind_rows(results_old_flu_OCD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))
results_old_flu_ADHD_df <- bind_rows(results_old_flu_ADHD, .id = "imputation") %>%
  mutate(imputation = as.numeric(imputation))

# Step 4: Pool results using Rubin's rules
pooled_results_old_flu_any_mh <- results_old_flu_any_mh_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_mixed <- results_old_flu_mixed_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_depression <- results_old_flu_depression_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_anxiety <- results_old_flu_anxiety_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_bipolar <- results_old_flu_bipolar_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_PTSD <- results_old_flu_PTSD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_OCD <- results_old_flu_OCD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

pooled_results_old_flu_ADHD <- results_old_flu_ADHD_df %>%
  group_by(term) %>%
  summarise(
    # Mean estimate across imputations
    Q_bar = mean(estimate),
    
    # Within-imputation variance (mean of robust variances)
    U_bar = mean(var_robust),
    
    # Between-imputation variance
    B = var(estimate),
    
    # Number of imputations
    m = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Total variance (Rubin's rule)
    T_var = U_bar + (1 + 1/m) * B,
    
    # Pooled SE
    pooled_se = sqrt(T_var),
    
    # Degrees of freedom
    df = (m - 1) * (1 + (U_bar / ((1 + 1/m) * B)))^2,
    
    # Confidence intervals (on log scale)
    lower_ci_log = Q_bar - qt(0.975, df) * pooled_se,
    upper_ci_log = Q_bar + qt(0.975, df) * pooled_se,
    
    # P-value
    p_value = 2 * (1 - pt(abs(Q_bar / pooled_se), df)),
    
    # Exponentiate to get Relative Risks
    RR = exp(Q_bar),
    RR_lower = exp(lower_ci_log),
    RR_upper = exp(upper_ci_log)
  )

save(pooled_results_old_flu_ADHD, file = "Pooled results/Old_Influenza_ADHD_season2_SA.rdata")
save(pooled_results_old_flu_any_mh, file = "Pooled results/Old_Influenza_AnyMentalHealth_season2_SA.rdata")
save(pooled_results_old_flu_mixed, file = "Pooled results/Old_Influenza_Mixed_season2_SA.rdata")
save(pooled_results_old_flu_depression, file = "Pooled results/Old_Influenza_Depression_season2_SA.rdata")
save(pooled_results_old_flu_anxiety, file = "Pooled results/Old_Influenza_Anxiety_season2_SA.rdata")
save(pooled_results_old_flu_bipolar, file = "Pooled results/Old_Influenza_Bipolar_season2_SA.rdata")
save(pooled_results_old_flu_OCD, file = "Pooled results/Old_Influenza_OCD_season2_SA.rdata")
save(pooled_results_old_flu_PTSD, file = "Pooled results/Old_Influenza_PTSD_season2_SA.rdata")
##############################################################################
old_flu_all_RR <- c(1.03,1.04,1.04,1.03,1.01,1.09,0.99,1.02)
old_flu_all_UCI <- c(1.05,1.06,1.05,1.05,1.03,1.12,1.02,1.10)
old_flu_all_LCI <- c(1.02,1.02,1.02,1.02,0.96,1.06,0.96,0.95)

old_flu_season1_RR <- c(1.09,1.09,1.09,1.14,0.90,1.40,0.97,1.46)
old_flu_season1_LCI <- c(1.04,0.97,1.03,1.05,0.80,1.15,0.79,0.97)
old_flu_season1_UCI <- c(1.15,1.22,1.15,1.24,1.02,1.61,1.19,2.19)

old_flu_season2_RR <- c(1.09,1.11,1.09,1.13,0.97,1.25,0.95,1.01)
old_flu_season2_LCI <- c(1.04,1.01,1.04,1.05,0.87,1.08,0.8,0.66)
old_flu_season2_UCI <- c(1.15,1.22,1.74,1.21,1.08,1.45,1.28,1.53)

old_flu_season3_RR <- c(1.07,1.08,1.07,1.08,0.95,1.29,0.97,1.12)
old_flu_season3_LCI <- c(1.03,1,1.03,1.02,0.87,1.15,0.84,0.84)
old_flu_season3_UCI <- c(1.12,1.17,1.12,1.15,1.04,1.45,1.11,1.5)

preg_covid_risk_all_RR <- c(1.03,1.05,1,1.13,1.08,1,0.97,0.92)
preg_covid_risk_all_LCI <- c(0.97,0.95,0.9,0.98,0.81,0.86,0.71,0.79)
preg_covid_risk_all_UCI <- c(1.09,1.16,1.1,1.3,1.44,1.18,1.33,1.07)

preg_covid_norisk_all_RR <- c(0.96,0.98,0.99,0.98,1.02,0.98,1,0.94)
preg_covid_norisk_all_LCI <- c(0.95,0.96,0.96,0.95,0.96,0.95,0.93,0.91)
preg_covid_norisk_all_UCI <- c(0.97,0.1,1.01,1.02,1.08,1.01,1.07,0.97)

preg_flu_risk_all_RR <- c(1.04,1.1,0.97,0.99,1.05,1.12,0.85,1.21)
preg_flu_risk_all_UCI <- c(0.99,1.01,0.9,0.83,0.87,0.94,0.81,0.95)
preg_flu_risk_all_LCI <- c(1.1,1.21,1.04,1.17,1.28,1.33,0.9,1.55)

preg_flu_risk_season1_RR <- c(1.8,2.53,1.31,1.2,0,2.08,0,3.08)
preg_flu_risk_season1_UCI <- c(1.15,1,45,0.57,0.29,0,0.64,0,0.66)
preg_flu_risk_season1_LCI <- c(2.81,4.43,3.05,5.03,0,6.7,0,14.4)

preg_flu_risk_season2_RR <- c(1.04,1.58,0.1,1.11,0.63,1.97,0,3.83)
preg_flu_risk_season2_UCI <- c(0.61,0.74,0.01,0.31,0.08,0.67,0,1.36)
preg_flu_risk_season2_LCI <- c(1.76,3.35,0.71,4.05,4.8,5.79,0,10.76)

preg_flu_risk_season3_RR <- c(0.91,0.95,0.2,1.24,4.08,2.36,0,0)
preg_flu_risk_season3_UCI <- c(0.47,0.28,0.03,0.27,1.71,0.67,0,0)
preg_flu_risk_season3_LCI <- c(1.78,3.14,1.47,5.61,13.51,8.29,0,0)

preg_flu_norisk_all_RR <- c(1.02,1,1.02,1.02,1,1.03,1.08,0.99)
preg_flu_norisk_all_UCI <- c(1,0.99,1.01,0.99,0.96,1,1.03,0.96)
preg_flu_norisk_all_LCI <- c(1.03,1.02,1.04,1.05,1.04,1.06,1.14,1.02)

preg_flu_norisk_season1_RR <- c(1.15,1.15,1.21,0.96,1.16,1.38,1.51,0.64)
preg_flu_norisk_season1_UCI <- c(1.02,0.9,0.99,0.65,0.65,0.98,0.86,0.35)
preg_flu_norisk_season1_LCI <- c(1.31,1.47,1.47,1.42,2.04,1.96,2.65,1.19)

preg_flu_norisk_season2_RR <- c(1.15,1.04,1.19,1.27,1.11,1.05,1.7,0.96)
preg_flu_norisk_season2_UCI <- c(1.03,0.83,0.99,0.95,0.67,0.74,1.08,0.6)
preg_flu_norisk_season2_LCI <- c(1.29,1.32,1.42,1.7,1.85,1.49,2.67,1.52)

preg_flu_norisk_season3_RR <- c(1.11,1.02,1.09,1.15,0.71,1.3,1.6,1.07)
preg_flu_norisk_season3_UCI <- c(0.99,0.81,0.9,0.85,0.35,0.94,0.99,0.72)
preg_flu_norisk_season3_LCI <- c(1.25,1.29,1.32,1.57,1.42,1.8,2.62,1.61)

old_covid_Primary_RR <- c(1.12,1.12,1.12,1.08,1.12,1.18,1.09,1.1)
old_covid_Primary_UCI <- c(1.11,1.09,1.11,1.07,1.10,1.13,1.04,1.04)
old_covid_Primary_LCI <- c(1.12,1.15,1.13,1.1,1.14,1.24,1.13,1.18)

old_covid_booster1_RR <- c(1.1,1.09,1.11,1.06,1.1,1.17,1.05,1.1)
old_covid_booster1_UCI <- c(1.09,1.06,1.1,1.04,1.07,1.11,0.99,0.98)
old_covid_booster1_LCI <- c(1.11,1.13,1.12,1.08,1.13,1.23,1.11,1.16)

old_covid_booster2_RR <- c(1.05,1.02,1.07,1,1.08,1.11,0.98,1.03)
old_covid_booster2_UCI <- c(1.04,0.96,1.05,0.96,1.03,1.03,0.9,0.89)
old_covid_booster2_LCI <- c(1.07,1.08,1.09,1.03,1.13,1.2,1.07,1.19)



















