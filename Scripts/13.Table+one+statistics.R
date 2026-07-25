################################################################################
# Script: 13.Table_one_statistics.R
#
# Purpose:
#   Make table ones for all subcohorts.
################################################################################
library(cobalt)
library(MatchThem)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

load("M:/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Pregnant_COVID_NoRisk_Weight_Corrected.rdata")
load("M:/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Pregnant_COVID_Risk_Weight_Corrected.rdata")
load("M:/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Old_Covid_Weights_Corrected.rdata")
load("M:/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Pregnant_Influenza_Risk_Weight_Corrected.rdata")
load("M:/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Pregnant_Influenza_NoRisk_Weight_Corrected.rdata")
load("M:/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Old_Influenza_Weight_Corrected.rdata")

data <- preg_covid_risk_imp_iptw

get_smd <- function(data) {
  bal.tab(
    mental ~ age_at_enrollment_categorized + county_2021 + country_of_birth + smoking 
    + parity + profession + income_2021_cat,  # <- your covariates
    data = data,
    weights = data$weights,
    method = "weighting",
    estimand = "ATE",
    un = TRUE# gives unweighted + weighted
  )$Balance
}

all_smd <- map(preg_covid_risk_imp_iptw, get_smd)

smd_df <- bind_rows(all_smd, .id = "imp") %>%
  mutate(imp = as.integer(imp),
         Diff.Un = abs(Diff.Un),
         Diff.Adj = abs(Diff.Adj))
smd_df$Variable <- rownames(smd_df)


smd_df <- smd_df %>%
  mutate(
    Variable = str_remove(Variable, "\\.\\.\\..*$")
  )

smd_summary <- smd_df %>%
  group_by(Variable) %>%
  summarise(
    mean_un = mean(Diff.Un, na.rm = TRUE),
    min_un  = min(Diff.Un, na.rm = TRUE),
    max_un  = max(Diff.Un, na.rm = TRUE),
    
    mean_w  = mean(Diff.Adj, na.rm = TRUE),
    min_w   = min(Diff.Adj, na.rm = TRUE),
    max_w   = max(Diff.Adj, na.rm = TRUE)
  )

var_labels <- c(
  "age_at_enrollment_categorized_18-24" = "Age (18-24)",
  "age_at_enrollment_categorized_25-30" = "Age (25-30)",
  "age_at_enrollment_categorized_31-35" = "Age (31-35)",
  "age_at_enrollment_categorized_36-40" = "Age (36-40)",
  "age_at_enrollment_categorized_>41" = "Age (> 41)",
  "country_of_birth_Norwegian" = "Country of birth (Norway)",
  "country_of_birth_Eastern European" = "Country of birth (Eastern Europe)",
  "country_of_birth_Western/ Northern/ Central European" = "Country of birth (Western, Northern or Central Europe)",
  "country_of_birth_Southern European" = "Country of birth (Southern Europe)",
  "country_of_birth_African" = "Country of birth (Africa)",
  "country_of_birth_Central/ East Asian" = "Country of birth (Central or East Asia)",
  "country_of_birth_Middle Eastern & North African" = "Country of birth (Middle East or North Africa)",
  "country_of_birth_South Asian" = "Country of birth (South Asia)",
  "country_of_birth_Latin American" = "Country of birth (Latin America)",
  "country_of_birth_North American" = "Country of birth (North America)",
  "county_2021_Central" = "Region of residence (Central Norway)",
  "county_2021_Eastern" = "Region of residence (Eastern Norway)",
  "county_2021_Northern" = "Region of residence (Northern Norway)",
  "county_2021_Southern" = "Region of residence (Southern Norway)",
  "county_2021_Western" = "Region of residence (Western Norway)",
  "income_2021_cat_High" = "Personal annual income (High)",
  "income_2021_cat_Medium" = "Personal annual income (Medium)",
  "income_2021_cat_Low" = "Personal annual income (Low)",
  "parity_0" = "Parity (0)",
  "parity_1" = "Parity (1)",
  "parity_2" = "Parity (2)",
  "parity_3" = "Parity (3)",
  "parity_4" = "Parity (>=4)",
  "profession_Academia" = "Profession (Academia)",
  "profession_Farmer/ Crafts/ Cleaning/ Transport workers" = "Profession (Farmer/ Crafts/ Cleaning/ Transport workers)",
  "profession_Military/ Leaders" = "Profession (Military/ Leaders)",
  "profession_Office/ Sales" = "Profession (Office/ Sales)",
  "smoking_No" = "Smoking (No)",
  "smoking_Occasionally" = "Smoking (Occasionally)",
  "smoking_Daily" = "Smoking (Daily)"
)

smd_summary <- smd_summary %>%
  mutate(
    Variable = recode(Variable, !!!var_labels)
  )

desired_order <- c(
  "Age (18-24)",
  "Age (25-30)",
  "Age (31-35)",
  "Age (36-40)",
  "Age (> 41)",
  "Country of birth (Norway)",
  "Country of birth (Eastern Europe)",
  "Country of birth (Western, Northern or Central Europe)",
  "Country of birth (Southern Europe)",
  "Country of birth (Africa)",
  "Country of birth (Central or East Asia)",
  "Country of birth (Middle East or North Africa)",
  "Country of birth (South Asia)",
  "Country of birth (Latin America)",
  "Country of birth (North America)",
  "Region of residence (Central Norway)",
  "Region of residence (Eastern Norway)",
  "Region of residence (Northern Norway)",
  "Region of residence (Southern Norway)",
  "Region of residence (Western Norway)",
  "Personal annual income (High)",
  "Personal annual income (Medium)",
  "Personal annual income (Low)",
  "Parity (0)",
  "Parity (1)",
  "Parity (2)",
  "Parity (3)",
  "Parity (>=4)",
  "Profession (Academia)",
  "Profession (Farmer/ Crafts/ Cleaning/ Transport workers)",
  "Profession (Military/ Leaders)",
  "Profession (Office/ Sales)",
  "Smoking (No)",
  "Smoking (Occasionally)",
  "Smoking (Daily)"
)
desired_order <- rev(desired_order)
smd_summary <- smd_summary %>%
  mutate(
    Variable = factor(Variable, levels = desired_order)
  )

plot_df <- smd_summary %>%
  pivot_longer(
    cols = -Variable,
    names_to = c("stat", "type"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

plot_df <- plot_df %>%
  mutate(
    type = recode(type,
                  un = "Unweighted",
                  w  = "Weighted"
    )
  )

p <- ggplot(plot_df, aes(x = mean, y = Variable, color = type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    height = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  labs(
    x = "Standardized Mean Difference",
    y = "Covariates",
    color = "Type"
  )
save(smd_summary, file = "SMD_TablePregnant_Risk_COVID-19.rdata")
ggsave("SMD_Love_Plot_Pregnant_Risk_COVID-19.pdf", plot = p, width = 12, height = 7, dpi = 300)
##############################################################
data <- preg_covid_norisk_imp_iptw

get_smd <- function(data) {
  bal.tab(
    mental ~ age_at_enrollment_categorized + county_2021 + country_of_birth + smoking 
    + parity + profession + income_2021_cat,  # <- your covariates
    data = data,
    weights = data$weights,
    method = "weighting",
    estimand = "ATE",
    un = TRUE# gives unweighted + weighted
  )$Balance
}

all_smd <- map(preg_covid_norisk_imp_iptw, get_smd)

smd_df <- bind_rows(all_smd, .id = "imp") %>%
  mutate(imp = as.integer(imp),
         Diff.Un = abs(Diff.Un),
         Diff.Adj = abs(Diff.Adj))
smd_df$Variable <- rownames(smd_df)


smd_df <- smd_df %>%
  mutate(
    Variable = str_remove(Variable, "\\.\\.\\..*$")
  )

smd_summary <- smd_df %>%
  group_by(Variable) %>%
  summarise(
    mean_un = mean(Diff.Un, na.rm = TRUE),
    min_un  = min(Diff.Un, na.rm = TRUE),
    max_un  = max(Diff.Un, na.rm = TRUE),
    
    mean_w  = mean(Diff.Adj, na.rm = TRUE),
    min_w   = min(Diff.Adj, na.rm = TRUE),
    max_w   = max(Diff.Adj, na.rm = TRUE)
  )

var_labels <- c(
  "age_at_enrollment_categorized_18-24" = "Age (18-24)",
  "age_at_enrollment_categorized_25-30" = "Age (25-30)",
  "age_at_enrollment_categorized_31-35" = "Age (31-35)",
  "age_at_enrollment_categorized_36-40" = "Age (36-40)",
  "age_at_enrollment_categorized_>41" = "Age (> 41)",
  "country_of_birth_Norwegian" = "Country of birth (Norway)",
  "country_of_birth_Eastern European" = "Country of birth (Eastern Europe)",
  "country_of_birth_Western/ Northern/ Central European" = "Country of birth (Western, Northern or Central Europe)",
  "country_of_birth_Southern European" = "Country of birth (Southern Europe)",
  "country_of_birth_African" = "Country of birth (Africa)",
  "country_of_birth_Central/ East Asian" = "Country of birth (Central or East Asia)",
  "country_of_birth_Middle Eastern & North African" = "Country of birth (Middle East or North Africa)",
  "country_of_birth_South Asian" = "Country of birth (South Asia)",
  "country_of_birth_Latin American" = "Country of birth (Latin America)",
  "country_of_birth_North American" = "Country of birth (North America)",
  "country_of_birth_Oceanian" = "Country of birth (Oceania)",
  "county_2021_Central" = "Region of residence (Central Norway)",
  "county_2021_Eastern" = "Region of residence (Eastern Norway)",
  "county_2021_Northern" = "Region of residence (Northern Norway)",
  "county_2021_Southern" = "Region of residence (Southern Norway)",
  "county_2021_Western" = "Region of residence (Western Norway)",
  "income_2021_cat_High" = "Personal annual income (High)",
  "income_2021_cat_Medium" = "Personal annual income (Medium)",
  "income_2021_cat_Low" = "Personal annual income (Low)",
  "parity_0" = "Parity (0)",
  "parity_1" = "Parity (1)",
  "parity_2" = "Parity (2)",
  "parity_3" = "Parity (3)",
  "parity_4" = "Parity (>=4)",
  "profession_Academia" = "Profession (Academia)",
  "profession_Farmer/ Crafts/ Cleaning/ Transport workers" = "Profession (Farmer/ Crafts/ Cleaning/ Transport workers)",
  "profession_Military/ Leaders" = "Profession (Military/ Leaders)",
  "profession_Office/ Sales" = "Profession (Office/ Sales)",
  "smoking_No" = "Smoking (No)",
  "smoking_Occasionally" = "Smoking (Occasionally)",
  "smoking_Daily" = "Smoking (Daily)"
)

smd_summary <- smd_summary %>%
  mutate(
    Variable = recode(Variable, !!!var_labels)
  )

desired_order <- c(
  "Age (18-24)",
  "Age (25-30)",
  "Age (31-35)",
  "Age (36-40)",
  "Age (> 41)",
  "Country of birth (Norway)",
  "Country of birth (Eastern Europe)",
  "Country of birth (Western, Northern or Central Europe)",
  "Country of birth (Southern Europe)",
  "Country of birth (Africa)",
  "Country of birth (Central or East Asia)",
  "Country of birth (Middle East or North Africa)",
  "Country of birth (South Asia)",
  "Country of birth (Latin America)",
  "Country of birth (North America)",
  "Country of birth (Oceania)",
  "Region of residence (Central Norway)",
  "Region of residence (Eastern Norway)",
  "Region of residence (Northern Norway)",
  "Region of residence (Southern Norway)",
  "Region of residence (Western Norway)",
  "Personal annual income (High)",
  "Personal annual income (Medium)",
  "Personal annual income (Low)",
  "Parity (0)",
  "Parity (1)",
  "Parity (2)",
  "Parity (3)",
  "Parity (>=4)",
  "Profession (Academia)",
  "Profession (Farmer/ Crafts/ Cleaning/ Transport workers)",
  "Profession (Military/ Leaders)",
  "Profession (Office/ Sales)",
  "Smoking (No)",
  "Smoking (Occasionally)",
  "Smoking (Daily)"
)
desired_order <- rev(desired_order)
smd_summary <- smd_summary %>%
  mutate(
    Variable = factor(Variable, levels = desired_order)
  )

plot_df <- smd_summary %>%
  pivot_longer(
    cols = -Variable,
    names_to = c("stat", "type"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

plot_df <- plot_df %>%
  mutate(
    type = recode(type,
                  un = "Unweighted",
                  w  = "Weighted"
    )
  )

p <- ggplot(plot_df, aes(x = mean, y = Variable, color = type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    height = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  labs(
    x = "Standardized Mean Difference",
    y = "Covariates",
    color = "Type"
  )
save(smd_summary, file = "SMD_TablePregnant_NoRisk_COVID-19.rdata")
ggsave("SMD_Love_Plot_Pregnant_NoRisk_COVID-19.pdf", plot = p, width = 12, height = 7, dpi = 300)
###############################################################################
data <- preg_flu_risk_imp_iptw

get_smd <- function(data) {
  bal.tab(
    mental ~ age_at_enrollment_categorized + county_2017 + country_of_birth + smoking 
    + parity + profession + income_2017_cat,  # <- your covariates
    data = data,
    weights = data$weights,
    method = "weighting",
    estimand = "ATE",
    un = TRUE# gives unweighted + weighted
  )$Balance
}

all_smd <- map(preg_flu_risk_imp_iptw, get_smd)

smd_df <- bind_rows(all_smd, .id = "imp") %>%
  mutate(imp = as.integer(imp),
         Diff.Un = abs(Diff.Un),
         Diff.Adj = abs(Diff.Adj))
smd_df$Variable <- rownames(smd_df)


smd_df <- smd_df %>%
  mutate(
    Variable = str_remove(Variable, "\\.\\.\\..*$")
  )

smd_summary <- smd_df %>%
  group_by(Variable) %>%
  summarise(
    mean_un = mean(Diff.Un, na.rm = TRUE),
    min_un  = min(Diff.Un, na.rm = TRUE),
    max_un  = max(Diff.Un, na.rm = TRUE),
    
    mean_w  = mean(Diff.Adj, na.rm = TRUE),
    min_w   = min(Diff.Adj, na.rm = TRUE),
    max_w   = max(Diff.Adj, na.rm = TRUE)
  )

var_labels <- c(
  "age_at_enrollment_categorized_18-24" = "Age (18-24)",
  "age_at_enrollment_categorized_25-30" = "Age (25-30)",
  "age_at_enrollment_categorized_31-35" = "Age (31-35)",
  "age_at_enrollment_categorized_36-40" = "Age (36-40)",
  "age_at_enrollment_categorized_>41" = "Age (> 41)",
  "country_of_birth_Norwegian" = "Country of birth (Norway)",
  "country_of_birth_Eastern European" = "Country of birth (Eastern Europe)",
  "country_of_birth_Western/ Northern/ Central European" = "Country of birth (Western, Northern or Central Europe)",
  "country_of_birth_Southern European" = "Country of birth (Southern Europe)",
  "country_of_birth_African" = "Country of birth (Africa)",
  "country_of_birth_Central/ East Asian" = "Country of birth (Central or East Asia)",
  "country_of_birth_Middle Eastern & North African" = "Country of birth (Middle East or North Africa)",
  "country_of_birth_South Asian" = "Country of birth (South Asia)",
  "country_of_birth_Latin American" = "Country of birth (Latin America)",
  "country_of_birth_North American" = "Country of birth (North America)",
  "country_of_birth_Oceanian" = "Country of birth (Oceania)",
  "county_2017_Central" = "Region of residence (Central Norway)",
  "county_2017_Eastern" = "Region of residence (Eastern Norway)",
  "county_2017_Northern" = "Region of residence (Northern Norway)",
  "county_2017_Southern" = "Region of residence (Southern Norway)",
  "county_2017_Western" = "Region of residence (Western Norway)",
  "income_2017_cat_High" = "Personal annual income (High)",
  "income_2017_cat_Medium" = "Personal annual income (Medium)",
  "income_2017_cat_Low" = "Personal annual income (Low)",
  "parity_0" = "Parity (0)",
  "parity_1" = "Parity (1)",
  "parity_2" = "Parity (2)",
  "parity_3" = "Parity (3)",
  "parity_4" = "Parity (>=4)",
  "profession_Academia" = "Profession (Academia)",
  "profession_Farmer/ Crafts/ Cleaning/ Transport workers" = "Profession (Farmer/ Crafts/ Cleaning/ Transport workers)",
  "profession_Military/ Leaders" = "Profession (Military/ Leaders)",
  "profession_Office/ Sales" = "Profession (Office/ Sales)",
  "smoking_No" = "Smoking (No)",
  "smoking_Occasionally" = "Smoking (Occasionally)",
  "smoking_Daily" = "Smoking (Daily)"
)

smd_summary <- smd_summary %>%
  mutate(
    Variable = recode(Variable, !!!var_labels)
  )

desired_order <- c(
  "Age (18-24)",
  "Age (25-30)",
  "Age (31-35)",
  "Age (36-40)",
  "Age (> 41)",
  "Country of birth (Norway)",
  "Country of birth (Eastern Europe)",
  "Country of birth (Western, Northern or Central Europe)",
  "Country of birth (Southern Europe)",
  "Country of birth (Africa)",
  "Country of birth (Central or East Asia)",
  "Country of birth (Middle East or North Africa)",
  "Country of birth (South Asia)",
  "Country of birth (Latin America)",
  "Country of birth (North America)",
  "Country of birth (Oceania)",
  "Region of residence (Central Norway)",
  "Region of residence (Eastern Norway)",
  "Region of residence (Northern Norway)",
  "Region of residence (Southern Norway)",
  "Region of residence (Western Norway)",
  "Personal annual income (High)",
  "Personal annual income (Medium)",
  "Personal annual income (Low)",
  "Parity (0)",
  "Parity (1)",
  "Parity (2)",
  "Parity (3)",
  "Parity (>=4)",
  "Profession (Academia)",
  "Profession (Farmer/ Crafts/ Cleaning/ Transport workers)",
  "Profession (Military/ Leaders)",
  "Profession (Office/ Sales)",
  "Smoking (No)",
  "Smoking (Occasionally)",
  "Smoking (Daily)"
)
desired_order <- rev(desired_order)
smd_summary <- smd_summary %>%
  mutate(
    Variable = factor(Variable, levels = desired_order)
  )

plot_df <- smd_summary %>%
  pivot_longer(
    cols = -Variable,
    names_to = c("stat", "type"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

plot_df <- plot_df %>%
  mutate(
    type = recode(type,
                  un = "Unweighted",
                  w  = "Weighted"
    )
  )

p <- ggplot(plot_df, aes(x = mean, y = Variable, color = type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    height = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  labs(
    x = "Standardized Mean Difference",
    y = "Covariates",
    color = "Type"
  )
save(smd_summary, file = "SMD_Table_Pregnant_Risk_Influenza.rdata")
ggsave("SMD_Love_Plot_Pregnant_Risk_Influenza.pdf", plot = p, width = 12, height = 7, dpi = 300)
###############################################################################
data <- preg_flu_norisk_imp_iptw

get_smd <- function(data) {
  bal.tab(
    mental ~ age_at_enrollment_categorized + county_2017 + country_of_birth + smoking 
    + parity + profession + income_2017_cat,  # <- your covariates
    data = data,
    weights = data$weights,
    method = "weighting",
    estimand = "ATE",
    un = TRUE# gives unweighted + weighted
  )$Balance
}

all_smd <- map(preg_flu_norisk_imp_iptw, get_smd)

smd_df <- bind_rows(all_smd, .id = "imp") %>%
  mutate(imp = as.integer(imp),
         Diff.Un = abs(Diff.Un),
         Diff.Adj = abs(Diff.Adj))
smd_df$Variable <- rownames(smd_df)


smd_df <- smd_df %>%
  mutate(
    Variable = str_remove(Variable, "\\.\\.\\..*$")
  )

smd_summary <- smd_df %>%
  group_by(Variable) %>%
  summarise(
    mean_un = mean(Diff.Un, na.rm = TRUE),
    min_un  = min(Diff.Un, na.rm = TRUE),
    max_un  = max(Diff.Un, na.rm = TRUE),
    
    mean_w  = mean(Diff.Adj, na.rm = TRUE),
    min_w   = min(Diff.Adj, na.rm = TRUE),
    max_w   = max(Diff.Adj, na.rm = TRUE)
  )

var_labels <- c(
  "age_at_enrollment_categorized_18-24" = "Age (18-24)",
  "age_at_enrollment_categorized_25-30" = "Age (25-30)",
  "age_at_enrollment_categorized_31-35" = "Age (31-35)",
  "age_at_enrollment_categorized_36-40" = "Age (36-40)",
  "age_at_enrollment_categorized_>41" = "Age (> 41)",
  "country_of_birth_Norwegian" = "Country of birth (Norway)",
  "country_of_birth_Eastern European" = "Country of birth (Eastern Europe)",
  "country_of_birth_Western/ Northern/ Central European" = "Country of birth (Western, Northern or Central Europe)",
  "country_of_birth_Southern European" = "Country of birth (Southern Europe)",
  "country_of_birth_African" = "Country of birth (Africa)",
  "country_of_birth_Central/ East Asian" = "Country of birth (Central or East Asia)",
  "country_of_birth_Middle Eastern & North African" = "Country of birth (Middle East or North Africa)",
  "country_of_birth_South Asian" = "Country of birth (South Asia)",
  "country_of_birth_Latin American" = "Country of birth (Latin America)",
  "country_of_birth_North American" = "Country of birth (North America)",
  "country_of_birth_Oceanian" = "Country of birth (Oceania)",
  "county_2017_Central" = "Region of residence (Central Norway)",
  "county_2017_Eastern" = "Region of residence (Eastern Norway)",
  "county_2017_Northern" = "Region of residence (Northern Norway)",
  "county_2017_Southern" = "Region of residence (Southern Norway)",
  "county_2017_Western" = "Region of residence (Western Norway)",
  "income_2017_cat_High" = "Personal annual income (High)",
  "income_2017_cat_Medium" = "Personal annual income (Medium)",
  "income_2017_cat_Low" = "Personal annual income (Low)",
  "parity_0" = "Parity (0)",
  "parity_1" = "Parity (1)",
  "parity_2" = "Parity (2)",
  "parity_3" = "Parity (3)",
  "parity_4" = "Parity (>=4)",
  "profession_Academia" = "Profession (Academia)",
  "profession_Farmer/ Crafts/ Cleaning/ Transport workers" = "Profession (Farmer/ Crafts/ Cleaning/ Transport workers)",
  "profession_Military/ Leaders" = "Profession (Military/ Leaders)",
  "profession_Office/ Sales" = "Profession (Office/ Sales)",
  "smoking_No" = "Smoking (No)",
  "smoking_Occasionally" = "Smoking (Occasionally)",
  "smoking_Daily" = "Smoking (Daily)"
)

smd_summary <- smd_summary %>%
  mutate(
    Variable = recode(Variable, !!!var_labels)
  )

desired_order <- c(
  "Age (18-24)",
  "Age (25-30)",
  "Age (31-35)",
  "Age (36-40)",
  "Age (> 41)",
  "Country of birth (Norway)",
  "Country of birth (Eastern Europe)",
  "Country of birth (Western, Northern or Central Europe)",
  "Country of birth (Southern Europe)",
  "Country of birth (Africa)",
  "Country of birth (Central or East Asia)",
  "Country of birth (Middle East or North Africa)",
  "Country of birth (South Asia)",
  "Country of birth (Latin America)",
  "Country of birth (North America)",
  "Country of birth (Oceania)",
  "Region of residence (Central Norway)",
  "Region of residence (Eastern Norway)",
  "Region of residence (Northern Norway)",
  "Region of residence (Southern Norway)",
  "Region of residence (Western Norway)",
  "Personal annual income (High)",
  "Personal annual income (Medium)",
  "Personal annual income (Low)",
  "Parity (0)",
  "Parity (1)",
  "Parity (2)",
  "Parity (3)",
  "Parity (>=4)",
  "Profession (Academia)",
  "Profession (Farmer/ Crafts/ Cleaning/ Transport workers)",
  "Profession (Military/ Leaders)",
  "Profession (Office/ Sales)",
  "Smoking (No)",
  "Smoking (Occasionally)",
  "Smoking (Daily)"
)
desired_order <- rev(desired_order)
smd_summary <- smd_summary %>%
  mutate(
    Variable = factor(Variable, levels = desired_order)
  )

plot_df <- smd_summary %>%
  pivot_longer(
    cols = -Variable,
    names_to = c("stat", "type"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

plot_df <- plot_df %>%
  mutate(
    type = recode(type,
                  un = "Unweighted",
                  w  = "Weighted"
    )
  )

p <- ggplot(plot_df, aes(x = mean, y = Variable, color = type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    height = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  labs(
    x = "Standardized Mean Difference",
    y = "Covariates",
    color = "Type"
  )
save(smd_summary, file = "SMD_Table_Pregnant_NoRisk_Influenza.rdata")
ggsave("SMD_Love_Plot_Pregnant_NoRisk_Influenza.pdf", plot = p, width = 12, height = 7, dpi = 300)
###############################################################################
data <- old_flu_imp_iptw

get_smd <- function(data) {
  bal.tab(
    Any_MH ~ age_at_enrollment_categorized + county_2017 + country_of_birth + risk_factor 
    + income_2017_cat,  # <- your covariates
    data = data,
    weights = data$weights,
    method = "weighting",
    estimand = "ATE",
    un = TRUE# gives unweighted + weighted
  )$Balance
}

all_smd <- map(old_flu_imp_iptw, get_smd)

smd_df <- bind_rows(all_smd, .id = "imp") %>%
  mutate(imp = as.integer(imp),
         Diff.Un = abs(Diff.Un),
         Diff.Adj = abs(Diff.Adj))
smd_df$Variable <- rownames(smd_df)


smd_df <- smd_df %>%
  mutate(
    Variable = str_remove(Variable, "\\.\\.\\..*$")
  )

smd_summary <- smd_df %>%
  group_by(Variable) %>%
  summarise(
    mean_un = mean(Diff.Un, na.rm = TRUE),
    min_un  = min(Diff.Un, na.rm = TRUE),
    max_un  = max(Diff.Un, na.rm = TRUE),
    
    mean_w  = mean(Diff.Adj, na.rm = TRUE),
    min_w   = min(Diff.Adj, na.rm = TRUE),
    max_w   = max(Diff.Adj, na.rm = TRUE)
  )

var_labels <- c(
  "age_at_enrollment_categorized_65-70" = "Age (65-70)",
  "age_at_enrollment_categorized_71-75" = "Age (71-75)",
  "age_at_enrollment_categorized_76-80" = "Age (76-80)",
  "age_at_enrollment_categorized_>81" = "Age (>=81)",
  "country_of_birth_Norwegian" = "Country of birth (Norway)",
  "country_of_birth_Eastern European" = "Country of birth (Eastern Europe)",
  "country_of_birth_Western/ Northern/ Central European" = "Country of birth (Western, Northern or Central Europe)",
  "country_of_birth_Southern European" = "Country of birth (Southern Europe)",
  "country_of_birth_African" = "Country of birth (Africa)",
  "country_of_birth_Central/ East Asian" = "Country of birth (Central or East Asia)",
  "country_of_birth_Middle Eastern & North African" = "Country of birth (Middle East or North Africa)",
  "country_of_birth_South Asian" = "Country of birth (South Asia)",
  "country_of_birth_Latin American" = "Country of birth (Latin America)",
  "country_of_birth_North American" = "Country of birth (North America)",
  "country_of_birth_Oceanian" = "Country of birth (Oceania)",  
  "county_2017_Central" = "Region of residence (Central Norway)",
  "county_2017_Eastern" = "Region of residence (Eastern Norway)",
  "county_2017_Northern" = "Region of residence (Northern Norway)",
  "county_2017_Southern" = "Region of residence (Southern Norway)",
  "county_2017_Western" = "Region of residence (Western Norway)",
  "income_2017_cat_High" = "Personal annual income (High)",
  "income_2017_cat_Medium" = "Personal annual income (Medium)",
  "income_2017_cat_Low" = "Personal annual income (Low)",
  "risk_factor" = "Risk factors for infection (Yes)")

smd_summary <- smd_summary %>%
  mutate(
    Variable = recode(Variable, !!!var_labels)
  )

desired_order <- c(
  "Age (65-70)",
  "Age (71-75)",
  "Age (76-80)",
  "Age (>=81)",
  "Country of birth (Norway)",
  "Country of birth (Eastern Europe)",
  "Country of birth (Western, Northern or Central Europe)",
  "Country of birth (Southern Europe)",
  "Country of birth (Africa)",
  "Country of birth (Central or East Asia)",
  "Country of birth (Middle East or North Africa)",
  "Country of birth (South Asia)",
  "Country of birth (Latin America)",
  "Country of birth (North America)",
  "Country of birth (Oceania)",  
  "Region of residence (Central Norway)",
  "Region of residence (Eastern Norway)",
  "Region of residence (Northern Norway)",
  "Region of residence (Southern Norway)",
  "Region of residence (Western Norway)",
  "Personal annual income (High)",
  "Personal annual income (Medium)",
  "Personal annual income (Low)",
  "Risk factors for infection (Yes)" 
)
desired_order <- rev(desired_order)
smd_summary <- smd_summary %>%
  mutate(
    Variable = factor(Variable, levels = desired_order)
  )

plot_df <- smd_summary %>%
  pivot_longer(
    cols = -Variable,
    names_to = c("stat", "type"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

plot_df <- plot_df %>%
  mutate(
    type = recode(type,
                  un = "Unweighted",
                  w  = "Weighted"
    )
  )

p <- ggplot(plot_df, aes(x = mean, y = Variable, color = type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    height = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  labs(
    x = "Standardized Mean Difference",
    y = "Covariates",
    color = "Type"
  )
save(smd_summary, file = "SMD_Table_Older_Adult_Influenza.rdata")
ggsave("SMD_Love_Plot_older_Adult_Influenza.pdf", plot = p, width = 12, height = 7, dpi = 300)
###############################################################################
data <- old_covid_imp

get_smd <- function(data) {
  bal.tab(
    mental ~ age_at_enrollment_categorized + county_2021 + country_of_birth + risk_factor 
    + income_2021_cat,  # <- your covariates
    data = data,
    weights = data$iptw_weights,
    method = "weighting",
    estimand = "ATE",
    un = TRUE# gives unweighted + weighted
  )$Balance
}

all_smd <- map(old_covid_imp, get_smd)

smd_df <- bind_rows(all_smd, .id = "imp") %>%
  mutate(imp = as.integer(imp),
         Diff.Un = abs(Diff.Un),
         Diff.Adj = abs(Diff.Adj))
smd_df$Variable <- rownames(smd_df)


smd_df <- smd_df %>%
  mutate(
    Variable = str_remove(Variable, "\\.\\.\\..*$")
  )

smd_summary <- smd_df %>%
  group_by(Variable) %>%
  summarise(
    mean_un = mean(Diff.Un, na.rm = TRUE),
    min_un  = min(Diff.Un, na.rm = TRUE),
    max_un  = max(Diff.Un, na.rm = TRUE),
    
    mean_w  = mean(Diff.Adj, na.rm = TRUE),
    min_w   = min(Diff.Adj, na.rm = TRUE),
    max_w   = max(Diff.Adj, na.rm = TRUE)
  )

var_labels <- c(
  "age_at_enrollment_categorized_65-70" = "Age (65-70)",
  "age_at_enrollment_categorized_71-75" = "Age (71-75)",
  "age_at_enrollment_categorized_76-80" = "Age (76-80)",
  "age_at_enrollment_categorized_>81" = "Age (>=81)",
  "country_of_birth_Norwegian" = "Country of birth (Norway)",
  "country_of_birth_Eastern European" = "Country of birth (Eastern Europe)",
  "country_of_birth_Western/ Northern/ Central European" = "Country of birth (Western, Northern or Central Europe)",
  "country_of_birth_Southern European" = "Country of birth (Southern Europe)",
  "country_of_birth_African" = "Country of birth (Africa)",
  "country_of_birth_Central/ East Asian" = "Country of birth (Central or East Asia)",
  "country_of_birth_Middle Eastern & North African" = "Country of birth (Middle East or North Africa)",
  "country_of_birth_South Asian" = "Country of birth (South Asia)",
  "country_of_birth_Latin American" = "Country of birth (Latin America)",
  "country_of_birth_North American" = "Country of birth (North America)",
  "country_of_birth_Oceanian" = "Country of birth (Oceania)",  
  "county_2021_Central" = "Region of residence (Central Norway)",
  "county_2021_Eastern" = "Region of residence (Eastern Norway)",
  "county_2021_Northern" = "Region of residence (Northern Norway)",
  "county_2021_Southern" = "Region of residence (Southern Norway)",
  "county_2021_Western" = "Region of residence (Western Norway)",
  "income_2021_cat_High" = "Personal annual income (High)",
  "income_2021_cat_Medium" = "Personal annual income (Medium)",
  "income_2021_cat_Low" = "Personal annual income (Low)",
  "risk_factor" = "Risk factors for infection (Yes)")

smd_summary <- smd_summary %>%
  mutate(
    Variable = recode(Variable, !!!var_labels)
  )

desired_order <- c(
  "Age (65-70)",
  "Age (71-75)",
  "Age (76-80)",
  "Age (>=81)",
  "Country of birth (Norway)",
  "Country of birth (Eastern Europe)",
  "Country of birth (Western, Northern or Central Europe)",
  "Country of birth (Southern Europe)",
  "Country of birth (Africa)",
  "Country of birth (Central or East Asia)",
  "Country of birth (Middle East or North Africa)",
  "Country of birth (South Asia)",
  "Country of birth (Latin America)",
  "Country of birth (North America)",
  "Country of birth (Oceania)",  
  "Region of residence (Central Norway)",
  "Region of residence (Eastern Norway)",
  "Region of residence (Northern Norway)",
  "Region of residence (Southern Norway)",
  "Region of residence (Western Norway)",
  "Personal annual income (High)",
  "Personal annual income (Medium)",
  "Personal annual income (Low)",
  "Risk factors for infection (Yes)" 
)
desired_order <- rev(desired_order)
smd_summary <- smd_summary %>%
  mutate(
    Variable = factor(Variable, levels = desired_order)
  )

plot_df <- smd_summary %>%
  pivot_longer(
    cols = -Variable,
    names_to = c("stat", "type"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

plot_df <- plot_df %>%
  mutate(
    type = recode(type,
                  un = "Unweighted",
                  w  = "Weighted"
    )
  )

p <- ggplot(plot_df, aes(x = mean, y = Variable, color = type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    height = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  labs(
    x = "Standardized Mean Difference",
    y = "Covariates",
    color = "Type"
  )
save(smd_summary, file = "SMD_Table_Older_Adult_COVID-19.rdata")
ggsave("SMD_Love_Plot_older_Adult_COVID-19.pdf", plot = p, width = 12, height = 7, dpi = 300)
###############################################################################
library(survey)
library(tableone)

# define weighted survey design
design.w <- svydesign(
  ids = ~1,
  weights = ~weights,
  data = covid_preg_risk
)

# variables to summarize
vars <- c("age_at_enrollment_categorized", 
          "county_2021", "country_of_birth",
          "smoking", "parity", "profession", 
          "income_2021_cat")

library(gtsummary)
library(flextable)

# Save directly to Word
tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1.docx")

# Or with customized formatting
tbl %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_formatted.docx")
# Create Table 1
tbl <- tbl_svysummary(
  design.w,
  by = mental,
  include = vars,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 1
) %>%
  add_overall() %>%
  add_p() %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels()

tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_covid_preg_risk.docx")
######################################
# define weighted survey design
design.w <- svydesign(
  ids = ~1,
  weights = ~weights,
  data = covid_preg_norisk
)

# variables to summarize
vars <- c("age_at_enrollment_categorized", 
          "county_2021", "country_of_birth",
          "smoking", "parity", "profession", 
          "income_2021_cat")

library(gtsummary)
library(flextable)

# Save directly to Word
tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1.docx")

# Or with customized formatting
tbl %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_formatted.docx")
# Create Table 1
tbl <- tbl_svysummary(
  design.w,
  by = mental,
  include = vars,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 1
) %>%
  add_overall() %>%
  add_p() %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels()

tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_covid_preg_norisk.docx")
#########################################
# define weighted survey design
design.w <- svydesign(
  ids = ~1,
  weights = ~weights,
  data = flu_norisk_preg
)

# variables to summarize
vars <- c("age_at_enrollment_categorized", 
          "county_2017", "country_of_birth",
          "smoking", "parity", "profession", 
          "income_2017_cat")

library(gtsummary)
library(flextable)

# Save directly to Word
tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1.docx")

# Or with customized formatting
tbl %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_formatted.docx")
# Create Table 1
tbl <- tbl_svysummary(
  design.w,
  by = mental,
  include = vars,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 1
) %>%
  add_overall() %>%
  add_p() %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels()

tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_flu_preg_norisk.docx")
#######################################
# define weighted survey design
design.w <- svydesign(
  ids = ~1,
  weights = ~weights,
  data = flu_risk_preg
)

# variables to summarize
vars <- c("age_at_enrollment_categorized", 
          "county_2017", "country_of_birth",
          "smoking", "parity", "profession", 
          "income_2017_cat")

library(gtsummary)
library(flextable)

# Save directly to Word
tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1.docx")

# Or with customized formatting
tbl %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_formatted.docx")
# Create Table 1
tbl <- tbl_svysummary(
  design.w,
  by = mental,
  include = vars,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 1
) %>%
  add_overall() %>%
  add_p() %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels()

tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_flu_preg_risk.docx")
###########################################
# define weighted survey design
design.w <- svydesign(
  ids = ~1,
  weights = ~weights,
  data = flu_old
)

# variables to summarize
vars <- c("age_at_enrollment_categorized", 
          "county_2017", "country_of_birth",
          "risk_factor", 
          "income_2017_cat")

library(gtsummary)
library(flextable)

# Create Table 1
tbl <- tbl_svysummary(
  design.w,
  by = Any_MH,
  include = vars,
  statistic = list(
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 1
) %>%
  add_overall() %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels()%>%
  add_n()

tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_flu_old.docx")
#####################################
# define weighted survey design
design.w <- svydesign(
  ids = ~1,
  weights = ~iptw_weights,
  data = covid_old
)

# variables to summarize
vars <- c("age_at_enrollment_categorized", 
          "county_2021", "country_of_birth",
          "risk_factor", 
          "income_2021_cat")

library(gtsummary)
library(flextable)

# Create Table 1
tbl <- tbl_svysummary(
  design.w,
  by = mental,
  include = vars,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 1
) %>%
  add_overall() %>%
  add_p() %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels()

tbl %>%
  as_flex_table() %>%
  save_as_docx(path = "table1_covid_old.docx")



# Check how many observations are in your survey design
nrow(design.w)  # Should equal your dataset N

# Check for missingness in each variable
covid_old %>%
  summarise(
    age_missing = sum(is.na(age)),
    sex_missing = sum(is.na(sex)),
    race_missing = sum(is.na(race)),
    comorbidity_missing = sum(is.na(comorbidity_score)),
    treatment_missing = sum(is.na(treatment)),
    weights_missing = sum(is.na(iptw_weights))
  )

# Check if weights are zero or negative (can cause exclusion)
summary(df$iptw_weights)
sum(df$iptw_weights == 0)  # These might be dropped
sum(df$iptw_weights < 0)   # Negative weights cause issues
