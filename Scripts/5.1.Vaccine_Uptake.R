################################################################################
# Script: 5.1.Vaccine_Uptake.R
#
# Purpose:
#   Calculate vaccine uptake for older adult and pregnant populations included
#   in the influenza and COVID-19 vaccine cohorts. Plot a heatmap of uptake of both vaccines 
#   in both populations.
################################################################################
library(lubridate)
library(readr)
library(dplyr)

load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/Scripts/old_COVID_MentalHealth_Vaccine.rdata")
old_covid_old <- old_covid
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Populations/Older_Adult_COVID19_Population.rdata")
old_covid <- old_covid_old[old_covid_old$person_id %in% old_covid$person_id,]

load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/Scripts/Old_flu_MentalHealth_Vaccine.rdata")

load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/Scripts/Pregnant_COVID_Risk_MentalHealth_Vaccine.rdata")

load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/Scripts/Pregnant_COVID_NoRisk_MentalHealth_Vaccine.rdata")
pregnant_covid_norisk_old <- pregnant_covid_norisk
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/Scripts/Pregnant_FLU_Risk_MentalHealth_Vaccine.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/Scripts/Pregnant_FLU_NoRisk_MentalHealth_Vaccine.rdata")


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

old_covid$anxiety_pure[old_covid$danxiety_mh == 1 & old_covid$mixed == 0] <- 1
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

old_flu <- old_flu %>% group_by(person_id, eligible_2016_17) %>%
  mutate(season1 = max(season1))

old_flu <- old_flu %>% group_by(person_id, eligible_2017_18) %>%
  mutate(season2 = max(season2))

old_flu <- old_flu %>% group_by(person_id, eligible_2018_19) %>%
  mutate(season3 = max(season3))

old_flu <- old_flu %>% group_by(person_id) %>%
  mutate(all.seasons = max(all.seasons))

Uptake_Old_Flu_NoMental_season1 <- table(old_flu$season1[old_flu$mental == 0 & !duplicated(old_flu$person_id) & old_flu$eligible_2016_17 == 1])

Uptake_Old_Flu_season1 <- c(19.1,28.7,28.3,27.2,27.6,22.5,30.4,18,-5,21.5)
Uptake_Old_Flu_season2 <- c(24.2,35.5,35,32.7,34,27.5,38.1,22.9,0, 31.6)
Uptake_Old_Flu_season3 <- c(31.2,42.9,41.8,40.8,41.3,34.4,45.8,31.4,-5,37.1)

old_flu$eligible <- 0
old_flu$eligible[old_flu$eligible_2016_17 == 1 | old_flu$eligible_2017_18 == 1 | old_flu$eligible_2018_19 == 1] <- 1
old_flu$ vaccinated <- 0
old_flu$vaccinated[old_flu$season1 == 1 | old_flu$season2 == 1 | old_flu$season3 == 1] <- 1
Uptake_Old_Flu_allseasons <- c(35.7,49.7,48.6,47.5,47.8,40.3,53,34.5,0, 41.8)
# conditions <- c("No Mental Health Conditions", "Any Mental Health Condition","Mixed", "Depression", "Anxiety", "Bipolar", "PTSD", "OCD", "ED", "ADHD")
# Uptake_old_flu <- data.frame(Conditions = conditions, Season1 = Uptake_Old_Flu_season1, Season2 = Uptake_Old_Flu_season2, 
#                              Season3 = Uptake_Old_Flu_season3, AllSeasons = Uptake_Old_Flu_allseasons)
# 
# library(pheatmap)
# 
# # 1. Calculate percentage difference from reference
# ref_value_season1 <- Uptake_old_flu$Season1[Uptake_old_flu$Conditions == "No Mental Health Conditions"]
# Uptake_old_flu$percent_diff_season1 <- ((Uptake_old_flu$Season1 - ref_value_season1) / ref_value_season1) * 100
# Uptake_old_flu$percent_diff_season1[Uptake_old_flu$Conditions == "No Mental Health Conditions"] <- 0
# 
# ref_value_season2 <- Uptake_old_flu$Season2[Uptake_old_flu$Conditions == "No Mental Health Conditions"]
# Uptake_old_flu$percent_diff_season2 <- ((Uptake_old_flu$Season2 - ref_value_season2) / ref_value_season2) * 100
# Uptake_old_flu$percent_diff_season2[Uptake_old_flu$Conditions == "No Mental Health Conditions"] <- 0
# 
# ref_value_season3 <- Uptake_old_flu$Season3[Uptake_old_flu$Conditions == "No Mental Health Conditions"]
# Uptake_old_flu$percent_diff_season3 <- ((Uptake_old_flu$Season3 - ref_value_season3) / ref_value_season3) * 100
# Uptake_old_flu$percent_diff_season3[Uptake_old_flu$Conditions == "No Mental Health Conditions"] <- 0
# 
# ref_value_all.season <- Uptake_old_flu$AllSeasons[Uptake_old_flu$Conditions == "No Mental Health Conditions"]
# Uptake_old_flu$percent_diff_allseasons <- ((Uptake_old_flu$AllSeasons - ref_value_all.season) / ref_value_all.season) * 100
# Uptake_old_flu$percent_diff_allseasons[Uptake_old_flu$Conditions == "No Mental Health Conditions"] <- 0
# 
# # Set conditions as row names
# df_heat <- Uptake_old_flu
# rownames(df_heat) <- df_heat[,1]  # Set first column as row names
# df_heat <- df_heat[,-c(1:5)]  # Remove the condition column (now in row names)
# colnames(df_heat) <- c("2016/2017", "2017/2018", "2018/2019", "2016/2019")
# # Create heatmap with conditions on y-axis
# Old_flu_plot <- pheatmap(df_heat,
#          cluster_rows = F,      # Group similar conditions
#          cluster_cols = F,      # Group similar antidepressants
#          # scale = "column",         # Normalize by antidepressant (recommended)
#          color = colorRampPalette(c("#fee8c8", "#e34a33"))(100),
#          main = "Influenza Vaccine Uptake by Mental Health Condition in Older Adults",
#          ylab = "Presence of Mental Health Conditions",  # y-axis label
#          xlab = "Vaccination seasons",     # x-axis label
#          show_rownames = TRUE,
#          show_colnames = TRUE,
#          fontsize_row = 10,
#          fontsize_col = 12,
#          display_numbers = T,
#          angle_col = 45)           # Angle antidepressant names

#########################################################################################
pregnant_flu_risk <- pregnant_flu_risk %>% group_by(Preg_id) %>%
  mutate(vaccinated = max(vaccinated))

pregnant_flu_risk$season1 <- 0
pregnant_flu_risk$season2 <- 0
pregnant_flu_risk$season3 <- 0
pregnant_flu_risk$vaccinated[ymd(pregnant_flu_risk$vx_admin_date) < "2016-10-01"] <- 0
pregnant_flu_risk$vaccinated[ymd(pregnant_flu_risk$vx_admin_date) > "2019-03-31"] <- 0
pregnant_flu_risk$all.seasons <- pregnant_flu_risk$vaccinated

season1.start <- as.Date("2016-09-01")
season1.end <- as.Date("2017-03-31")
season2.start <- as.Date("2017-09-01")
season2.end <- as.Date("2018-03-31")
season3.start <- as.Date("2018-09-01")
season3.end <- as.Date("2019-03-31")

pregnant_flu_risk$eligible_risk <- 0
pregnant_flu_risk$eligible_risk[pregnant_flu_risk$eligible_2016_2017_risk == 1 |
                                  pregnant_flu_risk$eligible_2017_2018_risk == 1|
                                  pregnant_flu_risk$eligible_2018_2019_risk == 1] <- 1

pregnant_flu_risk$season1[pregnant_flu_risk$vaccinated == 1 & pregnant_flu_risk$eligible_2016_2017_risk] <- 1
pregnant_flu_risk$season2[pregnant_flu_risk$vaccinated == 1 & pregnant_flu_risk$eligible_2017_2018_risk] <- 1
pregnant_flu_risk$season3[pregnant_flu_risk$vaccinated == 1 & pregnant_flu_risk$eligible_2018_2019_risk] <- 1

pregnant_flu_risk_Uptake_season1 <- c(11.2,14.5,15.5,10.5,21.4,16.7,17.1,NA,25)
pregnant_flu_risk_Uptake_season2 <- c(11.9,15.6,13.9,10.3,8.3,16.7,26,0,44.4)
pregnant_flu_risk_Uptake_season3 <- c(21.4,20.5,27,8.3,25,100,10,NA,33.3)
pregnant_flu_risk_Uptake_all_seasons <- c(12.8,16,18.1,10.1,16.1,18.8,18.8,0,35.3)

Uptake_pregnant_flu_risk <- data.frame(Conditions = conditions, Season1 = pregnant_flu_risk_Uptake_season1, 
                                       Season2 = pregnant_flu_risk_Uptake_season2, 
                             Season3 = pregnant_flu_risk_Uptake_season3, AllSeasons = pregnant_flu_risk_Uptake_all_seasons)

library(pheatmap)

# 1. Calculate percentage difference from reference
ref_value_season1 <- Uptake_pregnant_flu_risk$Season1[Uptake_pregnant_flu_risk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_flu_risk$percent_diff_season1 <- ((Uptake_pregnant_flu_risk$Season1 - ref_value_season1) / ref_value_season1) * 100
Uptake_pregnant_flu_risk$percent_diff_season1[Uptake_pregnant_flu_risk$Conditions == "No Mental Health Conditions"] <- 0

ref_value_season2 <- Uptake_pregnant_flu_risk$Season2[Uptake_pregnant_flu_risk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_flu_risk$percent_diff_season2 <- ((Uptake_pregnant_flu_risk$Season2 - ref_value_season2) / ref_value_season2) * 100
Uptake_pregnant_flu_risk$percent_diff_season2[Uptake_pregnant_flu_risk$Conditions == "No Mental Health Conditions"] <- 0

ref_value_season3 <- Uptake_pregnant_flu_risk$Season3[Uptake_pregnant_flu_risk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_flu_risk$percent_diff_season3 <- ((Uptake_pregnant_flu_risk$Season3 - ref_value_season3) / ref_value_season3) * 100
Uptake_pregnant_flu_risk$percent_diff_season3[Uptake_pregnant_flu_risk$Conditions == "No Mental Health Conditions"] <- 0

ref_value_all.season <- Uptake_pregnant_flu_risk$AllSeasons[Uptake_pregnant_flu_risk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_flu_risk$percent_diff_allseasons <- ((Uptake_pregnant_flu_risk$AllSeasons - ref_value_all.season) / ref_value_all.season) * 100
Uptake_pregnant_flu_risk$percent_diff_allseasons[Uptake_pregnant_flu_risk$Conditions == "No Mental Health Conditions"] <- 0

# Set conditions as row names
df_heat <- Uptake_pregnant_flu_risk
rownames(df_heat) <- df_heat[,1]  # Set first column as row names
df_heat <- df_heat[,-c(1:5)]  # Remove the condition column (now in row names)
colnames(df_heat) <- c("2016/2017", "2017/2018", "2018/2019", "2016/2019")
# Create heatmap with conditions on y-axis
Pregnant_flu_risk_plot <- pheatmap(df_heat,
                         cluster_rows = F,      # Group similar conditions
                         cluster_cols = F,      # Group similar antidepressants
                         # scale = "column",         # Normalize by antidepressant (recommended)
                         color = colorRampPalette(c("#fee8c8", "#e34a33"))(100),
                         main = "Influenza Vaccine Uptake by Mental Health Condition in Pregnant Individuals with Risk Factors",
                         ylab = "Mental Health Conditions",  # y-axis label
                         xlab = "Vaccination seasons",     # x-axis label
                         show_rownames = TRUE,
                         show_colnames = TRUE,
                         fontsize_row = 10,
                         fontsize_col = 12,
                         display_numbers = T,
                         angle_col = 0)           # Angle antidepressant names
#####################################################################################
pregnant_flu_norisk <- pregnant_flu_norisk %>% group_by(Preg_id) %>%
  mutate(vaccinated = max(vaccinated))

pregnant_flu_norisk$season1 <- 0
pregnant_flu_norisk$season2 <- 0
pregnant_flu_norisk$season3 <- 0
pregnant_flu_norisk$vaccinated[ymd(pregnant_flu_norisk$vx_admin_date) < "2016-10-01"] <- 0
pregnant_flu_norisk$vaccinated[ymd(pregnant_flu_norisk$vx_admin_date) > "2019-03-31"] <- 0
pregnant_flu_norisk$all.seasons <- pregnant_flu_norisk$vaccinated

season1.start <- as.Date("2016-09-01")
season1.end <- as.Date("2017-03-31")
season2.start <- as.Date("2017-09-01")
season2.end <- as.Date("2018-03-31")
season3.start <- as.Date("2018-09-01")
season3.end <- as.Date("2019-03-31")

pregnant_flu_norisk$eligible_norisk <- 0
pregnant_flu_norisk$eligible_norisk[pregnant_flu_norisk$eligible_2016_2017_norisk == 1 |
                                  pregnant_flu_norisk$eligible_2017_2018_norisk == 1|
                                  pregnant_flu_norisk$eligible_2018_2019_norisk == 1] <- 1

pregnant_flu_norisk$season1[pregnant_flu_norisk$vaccinated == 1 & pregnant_flu_norisk$eligible_2016_2017_norisk] <- 1
pregnant_flu_norisk$season2[pregnant_flu_norisk$vaccinated == 1 & pregnant_flu_norisk$eligible_2017_2018_norisk] <- 1
pregnant_flu_norisk$season3[pregnant_flu_norisk$vaccinated == 1 & pregnant_flu_norisk$eligible_2018_2019_norisk] <- 1

pregnant_flu_norisk_Uptake_season1 <- c(9.6,10,10.3,9.4,9.1,11.5,11.5,12.6,5.3)
pregnant_flu_norisk_Uptake_season2 <- c(12.9,13.7,13.6,13.1,15,10.9,15.8,19.6,8.2)
pregnant_flu_norisk_Uptake_season3 <- c(27.3,27.6,25.2,28.1,32.3,17.5,31.2,36.4,22.5)
pregnant_flu_norisk_Uptake_all_seasons <- c(14.4,15.3,15,14.9,16.3,12.4,17.7,19,10.5)

Uptake_pregnant_flu_norisk <- data.frame(Conditions = conditions, Season1 = pregnant_flu_norisk_Uptake_season1, 
                                       Season2 = pregnant_flu_norisk_Uptake_season2, 
                                       Season3 = pregnant_flu_norisk_Uptake_season3, AllSeasons = pregnant_flu_norisk_Uptake_all_seasons)

library(pheatmap)

# 1. Calculate percentage difference from reference
ref_value_season1 <- Uptake_pregnant_flu_norisk$Season1[Uptake_pregnant_flu_norisk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_flu_norisk$percent_diff_season1 <- ((Uptake_pregnant_flu_norisk$Season1 - ref_value_season1) / ref_value_season1) * 100
Uptake_pregnant_flu_norisk$percent_diff_season1[Uptake_pregnant_flu_norisk$Conditions == "No Mental Health Conditions"] <- 0

ref_value_season2 <- Uptake_pregnant_flu_norisk$Season2[Uptake_pregnant_flu_norisk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_flu_norisk$percent_diff_season2 <- ((Uptake_pregnant_flu_norisk$Season2 - ref_value_season2) / ref_value_season2) * 100
Uptake_pregnant_flu_norisk$percent_diff_season2[Uptake_pregnant_flu_norisk$Conditions == "No Mental Health Conditions"] <- 0

ref_value_season3 <- Uptake_pregnant_flu_norisk$Season3[Uptake_pregnant_flu_norisk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_flu_norisk$percent_diff_season3 <- ((Uptake_pregnant_flu_norisk$Season3 - ref_value_season3) / ref_value_season3) * 100
Uptake_pregnant_flu_norisk$percent_diff_season3[Uptake_pregnant_flu_norisk$Conditions == "No Mental Health Conditions"] <- 0

ref_value_all.season <- Uptake_pregnant_flu_norisk$AllSeasons[Uptake_pregnant_flu_norisk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_flu_norisk$percent_diff_allseasons <- ((Uptake_pregnant_flu_norisk$AllSeasons - ref_value_all.season) / ref_value_all.season) * 100
Uptake_pregnant_flu_norisk$percent_diff_allseasons[Uptake_pregnant_flu_norisk$Conditions == "No Mental Health Conditions"] <- 0

# Set conditions as row names
df_heat <- Uptake_pregnant_flu_norisk
rownames(df_heat) <- df_heat[,1]  # Set first column as row names
df_heat <- df_heat[,-c(1:5)]  # Remove the condition column (now in row names)
colnames(df_heat) <- c("2016/2017", "2017/2018", "2018/2019", "2016/2019")

# Create heatmap with conditions on y-axis
Pregnant_flu_norisk_plot <- pheatmap(df_heat,
                                   cluster_rows = F,      # Group similar conditions
                                   cluster_cols = F,      # Group similar antidepressants
                                   # scale = "column",         # Normalize by antidepressant (recommended)
                                   color = colorRampPalette(c("#fee8c8", "#e34a33"))(100),
                                   main = "Influenza Vaccine Uptake by Mental Health Condition in Pregnant Individuals without Risk Factors",
                                   ylab = "Presence of Mental Health Conditions",  # y-axis label
                                   xlab = "Vaccination seasons",     # x-axis label
                                   show_rownames = TRUE,
                                   show_colnames = TRUE,
                                   fontsize_row = 10,
                                   fontsize_col = 12,
                                   display_numbers = T,
                                   angle_col = 0)           # Angle antidepressant names

#########################################################################################
# Pregnant COVID risk
pregnant_covid_norisk <- pregnant_covid_norisk %>% group_by(Preg_id) %>%
  mutate(vaccinated = max(vaccinated))

pregnant_covid_norisk$vaccinated[ymd(pregnant_covid_norisk$vx_admin_date) < "2021-08-01"] <- 0

############## New addition ##############
pregnant_covid_norisk <- pregnant_covid_norisk_last[pregnant_covid_norisk_last$Preg_id %in% pregnant_covid_norisk$Preg_id,]
##########################################
pregnant_covid_norisk_Uptake_all_seasons <- c(28.1,27.5,25.9,28.8,25.7,30.6,29.1,29,2.8,24.2)

Uptake_pregnant_covid_norisk <- data.frame(Conditions = conditions, AllSeasons = pregnant_covid_norisk_Uptake_all_seasons)

ref_value_all.season <- Uptake_pregnant_covid_norisk$AllSeasons[Uptake_pregnant_covid_norisk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_covid_norisk$percent_diff_allseasons <- ((Uptake_pregnant_covid_norisk$AllSeasons - ref_value_all.season) / ref_value_all.season) * 100
Uptake_pregnant_covid_norisk$percent_diff_allseasons[Uptake_pregnant_covid_norisk$Conditions == "No Mental Health Conditions"] <- 0

# Set conditions as row names
df_heat <- Uptake_pregnant_covid_norisk
rownames(df_heat) <- df_heat[,1]  # Set first column as row names
df_heat <- as.data.frame(df_heat[,-c(1,2)])  # Remove the condition column (now in row names)
colnames(df_heat)[1] <- "Uptake during pregnancy"
rownames(df_heat) <- conditions

# Create heatmap with conditions on y-axis
Pregnant_covid_norisk_plot <- pheatmap(df_heat,
                                     cluster_rows = F,      # Group similar conditions
                                     cluster_cols = F,      # Group similar antidepressants
                                     # scale = "column",         # Normalize by antidepressant (recommended)
                                     color = colorRampPalette(c("#fee8c8", "#e34a33"))(100),
                                     main = "COVID-19 Vaccine Uptake by Mental Health Condition in Pregnant Individuals without Risk Factors",
                                     ylab = "Presence of Mental Health Conditions",  # y-axis label
                                     #xlab = "Vaccination seasons",     # x-axis label
                                     show_rownames = TRUE,
                                     show_colnames = TRUE,
                                     fontsize_row = 10,
                                     fontsize_col = 12,
                                     display_numbers = T,
                                     angle_col = 0)           # Angle antidepressant names

#########################################################################################
# Pregnant COVID risk
pregnant_covid_risk <- pregnant_covid_risk %>% group_by(Preg_id) %>%
  mutate(vaccinated = max(vaccinated))

pregnant_covid_risk$vaccinated[ymd(pregnant_covid_risk$vx_admin_date) < "2021-08-01"] <- 0

pregnant_covid_risk_Uptake_all_seasons <- c(32.8,32.8,40.5,31.4,29,22.2,29.6,0,21.1)

Uptake_pregnant_covid_risk <- data.frame(Conditions = conditions, AllSeasons = pregnant_covid_risk_Uptake_all_seasons)

ref_value_all.season <- Uptake_pregnant_covid_risk$AllSeasons[Uptake_pregnant_covid_risk$Conditions == "No Mental Health Conditions"]
Uptake_pregnant_covid_risk$percent_diff_allseasons <- ((Uptake_pregnant_covid_risk$AllSeasons - ref_value_all.season) / ref_value_all.season) * 100
Uptake_pregnant_covid_risk$percent_diff_allseasons[Uptake_pregnant_covid_risk$Conditions == "No Mental Health Conditions"] <- 0

# Set conditions as row names
df_heat <- Uptake_pregnant_covid_risk
rownames(df_heat) <- df_heat[,1]  # Set first column as row names
df_heat <- as.data.frame(df_heat[,-c(1,2)])  # Remove the condition column (now in row names)
colnames(df_heat)[1] <- "Uptake during pregnancy"
rownames(df_heat) <- conditions

# Create heatmap with conditions on y-axis
Pregnant_covid_risk_plot <- pheatmap(df_heat,
                                       cluster_rows = F,      # Group similar conditions
                                       cluster_cols = F,      # Group similar antidepressants
                                       # scale = "column",         # Normalize by antidepressant (recommended)
                                       color = colorRampPalette(c("#fee8c8", "#e34a33"))(100),
                                       main = "COVID-19 Vaccine Uptake by Mental Health Condition in Pregnant Individuals without Risk Factors",
                                       ylab = "Presence of Mental Health Conditions",  # y-axis label    # x-axis label
                                       show_rownames = TRUE,
                                       show_colnames = TRUE,
                                       fontsize_row = 10,
                                       fontsize_col = 12,
                                       display_numbers = T,
                                       angle_col = 0)           # Angle antidepressant names

#########################################################################################
# Load the required package
library(dplyr)

# STEP 1: Prepare the Data
# Ensure vaccination_date is a Date object
old_flu$vx_admin_date <- ymd(old_flu$vx_admin_date)

# STEP 2: Create the Main Analysis Dataframe
# This adds BOTH total dose count AND dose sequence
old_flu1 <- old_flu %>%
  # Group all rows belonging to the same person
  group_by(person_id) %>%
  # Sort by date within each group (oldest dose first)
  arrange(person_id, vx_admin_date) %>%
  # Create new columns within each group
  mutate(
    total_doses = n(),                    # Counts all rows for this person
    dose_sequence = row_number(),          # Numbers doses 1, 2, 3... by date
    days_since_previous = as.numeric(vx_admin_date - lag(vx_admin_date))
  ) %>%
  filter(!is.na(days_since_previous)) %>% # Removes the first dose row (no interval)
  # Ungroup to finish the operation
  ungroup()

library(dplyr)
library(lubridate)

# Official Danish recommendation start dates
rec_dates <- list(
  primary_start = as.Date("2021-01-01"),   # Primary series campaign start
  booster1_start = as.Date("2021-12-31"),  # First booster recommendation for 65+
  booster2_start = as.Date("2022-08-30")   # Second booster recommendation
)

# Minimum intervals between doses (adjust based on Danish guidelines)
min_intervals <- list(
  dose2_after_dose1 = 21,        # Standard Pfizer/Moderna primary interval
  booster1_after_primary = 5 * 30,  # ~6 months after primary completion
  booster2_after_booster1 = 4 * 30  # ~5 months after first booster
)

# Assuming 'df' has columns: person_id, birth_date, vaccination_date, dose_sequence
person_timeline <- old_covid %>%
  arrange(person_id, vx_admin_date) %>%
  group_by(person_id) %>%
  summarise(
    birth_date = first(birth_date),
    dose1_date = nth(vx_admin_date, 1),
    dose2_date = nth(vx_admin_date, 2),
    dose3_date = nth(vx_admin_date, 3),
    dose4_date = nth(vx_admin_date, 4)
  ) %>%
  mutate(
    # 1. Check AGE eligibility at each recommendation start date
    age_at_primary_rec = as.numeric(floor((rec_dates$primary_start - birth_date) / 365.25)),
    age_at_booster1_rec = as.numeric(floor((rec_dates$booster1_start - birth_date) / 365.25)),
    age_at_booster2_rec = as.numeric(floor((rec_dates$booster2_start - birth_date) / 365.25)),
    
    is_age_eligible_primary = age_at_primary_rec >= 65,
    is_age_eligible_booster1 = age_at_booster1_rec >= 65,
    is_age_eligible_booster2 = age_at_booster2_rec >= 65,
    
    # 2. Calculate PERSONAL eligibility dates (TWO-PART RULE)
    # Primary series eligibility (dose 2): Later of Jan 1, 2021 OR 21 days after dose 1
    eligible_dose2_date = if_else(
      condition = (is_age_eligible_primary == TRUE) & (!is.na(dose1_date)),
      true = pmax(rec_dates$primary_start, 
                  ymd(dose1_date) + days(min_intervals$dose2_after_dose1), 
                  na.rm = TRUE),  # CRITICAL FIX
      false = as.Date(NA)
    ),
    
    # First booster eligibility: Later of Jan 1, 2022 OR 6 months after primary completion
    eligible_booster1_date = if_else(
      is_age_eligible_booster1 & !is.na(ymd(dose2_date)),
      pmax(rec_dates$booster1_start, (ymd(dose2_date) + days(150))),
      as.Date(NA)
    ),
    # Second booster eligibility: Later of Sep 1, 2022 OR 5 months after first booster
    eligible_booster2_date = if_else(
      is_age_eligible_booster2 & !is.na(ymd(dose3_date)),
      pmax(rec_dates$booster2_start, (ymd(dose3_date) + days(120))),
      as.Date(NA)
    ),
    
    # 4. Check adherence within valid windows
    received_valid_dose2 = case_when(
      is.na(ymd(eligible_dose2_date)) ~ FALSE,
      !is.na(ymd(dose2_date)) & ymd(dose2_date) >= ymd(eligible_dose2_date) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    received_valid_booster1 = case_when(
      is.na(ymd(dose3_date)) ~ FALSE,                     # No 3rd dose at all
      dose3_date < as.Date("2021-12-31") ~ FALSE,   # 3rd dose was too early (before rec)
      TRUE ~ TRUE                                    # It's a valid 1st booster
    ),
    
    # 2. A dose is a VALID 2nd booster if:
    #    - It is the person's 4th dose (dose4_date is not NA)
    #    - It was given ON or AFTER the official 2nd booster start date (2022-09-01)
    #    - AND the person has a VALID 1st booster (enforcing sequence)
    received_valid_booster2 = case_when(
      is.na(ymd(dose4_date)) ~ FALSE,                     # No 4th dose at all
      dose4_date < as.Date("2022-08-31") ~ FALSE,   # 4th dose was too early
      received_valid_booster1 != TRUE ~ FALSE,       # KEY: No valid 1st booster
      TRUE ~ TRUE   
    )
  )
################ New addition #################
old_covid1 <- merge(old_covid, person_timeline, all = T)
old_covid2 <- old_covid1 %>% group_by(person_id) %>%
  mutate(received_valid_dose2 = max(received_valid_dose2),
  received_valid_booster1 = max(received_valid_booster1),
  received_valid_booster2 = max(received_valid_booster2))
  
##############################################

# person_timeline <- merge(person_timeline, old_cov, all = T)
person_timeline <- old_covid2[!duplicated(old_covid2$person_id),]
# Primary series completion (dose 2) among age-eligible cohort
primary_uptake <- old_covid2 %>%
  filter(is_age_eligible_primary == T & mental == 0 & !duplicated(person_id)) %>%
  ungroup %>%
    dplyr::summarise(
      eligible_cohort_n = n(),          # Got at least dose 1
      completed_primary_valid = sum(received_valid_dose2 == 1),   # Got dose 2 in valid window
      primary_completion_rate = completed_primary_valid / eligible_cohort_n)
# First booster uptake among those eligible
booster1_uptake <- old_covid2[!is.na(ymd(eligible_booster1_date))& mental ==  0 & !duplicated(person_id), 
           .(eligible = .N,
             count_ones = sum(received_valid_booster1 == 1, na.rm = TRUE),
             percent_ones = sum(received_valid_booster1 == 1, na.rm = TRUE)/.N * 100)]

# Second booster uptake among those eligible  
library(data.table)
setDT(old_covid2)
booster2_uptake <- old_covid2[!is.na(ymd(eligible_booster2_date))& mental ==  0 & !duplicated(person_id), 
                         .(eligible = .N,
                           count_ones = sum(received_valid_booster2 == 1, na.rm = TRUE),
                           percent_ones = sum(received_valid_booster2 == 1, na.rm = TRUE)/.N * 100)]
print(primary_uptake)
print(booster1_uptake)
print(booster2_uptake)
Old_covid_primary_uptake <- c(94.5,98,97.9,98.4,97.7,97.1,96.8,94.8,100,95.4)
Old_covid_booster1_uptake <- c(97.7,96.7,96.7,97,96.4,95.8,96.3,96.3,100,95.3)
Old_covid_booster2_uptake <- c(86.9,86.1,86.6,86.1,84.8,82.5,78,84.4,100,85.2)
# Calculate the uptake per mental health condition using the uptake calculations above 
# but adjusted to different mental health conditions


old_covid_primary_1dose <- c(primary_uptake$primary_initiation_rate, primary_uptake_nomental$primary_initiation_rate, 
                             primary_uptake_mixed$primary_initiation_rate, primary_uptake_depression$primary_initiation_rate,
                             primary_uptake_anxiety$primary_initiation_rate, primary_uptake_bipolar$primary_initiation_rate,
                             primary_uptake_PTSD$primary_initiation_rate, primary_uptake_OCD$primary_initiation_rate,
                             primary_uptake_ED$primary_initiation_rate, primary_uptake_ADHD$primary_initiation_rate)
old_covid_primary_2dose <- c(primary_uptake$primary_completion_rate, primary_uptake_nomental$primary_completion_rate, 
                             primary_uptake_mixed$primary_completion_rate, primary_uptake_depression$primary_completion_rate,
                             primary_uptake_anxiety$primary_completion_rate, primary_uptake_bipolar$primary_completion_rate,
                             primary_uptake_PTSD$primary_completion_rate, primary_uptake_OCD$primary_completion_rate,
                             primary_uptake_ED$primary_completion_rate, primary_uptake_ADHD$primary_completion_rate)

old_covid_booster1 <- c(booster1_uptake$uptake_rate, booster1_uptake_nomental$uptake_rate, 
                        booster1_uptake_mixed$uptake_rate, booster1_uptake_depression$uptake_rate,
                        booster1_uptake_anxiety$uptake_rate, booster1_uptake_bipolar$uptake_rate,
                        booster1_uptake_PTSD$uptake_rate, booster1_uptake_OCD$uptake_rate,
                        booster1_uptake_ED$uptake_rate, booster1_uptake_ADHD$uptake_rate)

old_covid_booster2 <- c(booster2_uptake$uptake_rate, booster2_uptake_nomental$uptake_rate, 
                        booster2_uptake_mixed$uptake_rate, booster2_uptake_depression$uptake_rate,
                        booster2_uptake_anxiety$uptake_rate, booster2_uptake_bipolar$uptake_rate,
                        booster2_uptake_PTSD$uptake_rate, booster2_uptake_OCD$uptake_rate,
                        booster2_uptake_ED$uptake_rate, booster2_uptake_ADHD$uptake_rate)

conditions <- c("General Eligible population","No Mental Health Conditions","Mixed", "Depression", "Anxiety", "Bipolar", "PTSD", "OCD", "ED", "ADHD")


Uptake_old_covid <- data.frame(Conditions = conditions, Primary_Dose1 = old_covid_primary_1dose,
                                         Primary_Dose2 = old_covid_primary_2dose, Booster1 = old_covid_booster1,
                                         Booster2 = old_covid_booster2)

ref_value_primary_dose1 <- Uptake_old_covid$Primary_Dose1[Uptake_old_covid$Conditions == "General Eligible population"]
Uptake_old_covid$percent_diff_primary_dose1 <- ((Uptake_old_covid$Primary_Dose1 - ref_value_primary_dose1) / ref_value_primary_dose1) * 100
Uptake_old_covid$percent_diff_primary_dose1[Uptake_old_covid$Conditions == "General Eligible population"] <- 0

ref_value_primary_dose2 <- Uptake_old_covid$Primary_Dose2[Uptake_old_covid$Conditions == "General Eligible population"]
Uptake_old_covid$percent_diff_primary_dose2 <- ((Uptake_old_covid$Primary_Dose2 - ref_value_primary_dose2) / ref_value_primary_dose2) * 100
Uptake_old_covid$percent_diff_primary_dose2[Uptake_old_covid$Conditions == "General Eligible population"] <- 0

ref_value_booster1 <- Uptake_old_covid$Booster1[Uptake_old_covid$Conditions == "General Eligible population"]
Uptake_old_covid$percent_diff_booster1 <- ((Uptake_old_covid$Booster1 - ref_value_booster1) / ref_value_booster1) * 100
Uptake_old_covid$percent_diff_booster1[Uptake_old_covid$Conditions == "General Eligible population"] <- 0

ref_value_booster2 <- Uptake_old_covid$Booster2[Uptake_old_covid$Conditions == "General Eligible population"]
Uptake_old_covid$percent_diff_booster2 <- ((Uptake_old_covid$Booster2 - ref_value_booster2) / ref_value_booster2) * 100
Uptake_old_covid$percent_diff_booster2[Uptake_old_covid$Conditions == "General Eligible population"] <- 0

# Set conditions as row names
df_heat <- Uptake_old_covid
rownames(df_heat) <- df_heat[,1]  # Set first column as row names
df_heat <- as.data.frame(df_heat[,-c(1:5)])  # Remove the condition column (now in row names)
colnames(df_heat) <- c("Primary - Dose 1", "Primary - Dose 2", "Booster - Dose 1", "Booster - Dose 2")

# Create heatmap with conditions on y-axis
Old_covid_plot <- pheatmap(df_heat,
                                     cluster_rows = F,      # Group similar conditions
                                     cluster_cols = F,      # Group similar antidepressants
                                     # scale = "column",         # Normalize by antidepressant (recommended)
                                     color = colorRampPalette(c("dodgerblue", "grey95", "mediumpurple1"))(100),
                                     main = "COVID-19 Vaccine Uptake by Mental Health Condition in Older Adults",
                                     ylab = "Presence of Mental Health Conditions",  # y-axis label
                                     xlab = "Vaccination Doses",     # x-axis label
                                     show_rownames = TRUE,
                                     show_colnames = TRUE,
                                     fontsize_row = 10,
                                     fontsize_col = 12,
                                     display_numbers = T,
                                     angle_col = 0)           # Angle antidepressant names


###################################################################################

library(gridExtra)
plot <- grid.arrange(Old_flu_plot$gtable, Pregnant_flu_norisk_plot$gtable, Pregnant_flu_risk_plot$gtable,
             Old_covid_plot$gtable,Pregnant_covid_norisk_plot$gtable, Pregnant_covid_risk_plot$gtable,
             nrow = 2, ncol = 3)

library(dplyr)
library(tidyr)
library(ggplot2)
Uptake_Old_Flu_season1 <- c(20.1,23.2,22.6,23.7,24.1,22.5,25.2,19.3,31.6)
# Uptake_Old_Flu_season1 <- (Uptake_Old_Flu_season1 - Uptake_Old_Flu_season1[1])/ Uptake_Old_Flu_season1[1]*100
Uptake_Old_Flu_season2 <- c(26.4,29.2,28.3,29.8,30.1,25.5,28.9,23.6,25.4)
# Uptake_Old_Flu_season2 <- (Uptake_Old_Flu_season2 - Uptake_Old_Flu_season2[1])/ Uptake_Old_Flu_season2[1]*100
Uptake_Old_Flu_season3 <- c(33.3,36.7,35.6,37.4,37.4,33.0,38.2,31.1,33.0)
# Uptake_Old_Flu_season3 <- (Uptake_Old_Flu_season3 - Uptake_Old_Flu_season3[1])/ Uptake_Old_Flu_season3[1]*100
# old_flu$eligible <- 0
# old_flu$eligible[ old_flu$eligible_2016_17 == 1 | old_flu$eligible_2017_18 == 1 | old_flu$eligible_2018_19 == 1] <- 1

Uptake_Old_Flu_allseasons <- c(38.4,42.9,41.6,44.0,43.3,38.7,44.8,35.3,36.5)
# Uptake_Old_Flu_allseasons <- (Uptake_Old_Flu_allseasons - Uptake_Old_Flu_allseasons[1])/ Uptake_Old_Flu_allseasons[1]*100

pregnant_flu_risk_Uptake_season1 <- c(12,18.4,20.8,16.7,12.5,NA,37.5,NA,40)
pregnant_flu_risk_Uptake_season2 <- c(17.1,18.1,23.5,2.7,33.3,33.3,33.3,NA,50)
pregnant_flu_risk_Uptake_season3 <- c(30.2,25,20,7.1,50,75,50,NA,NA)
pregnant_flu_risk_Uptake_all_seasons <- c(12,15.9,19.3,10.8,17.6,14.3,26.3,NA,27.3)


pregnant_flu_norisk_Uptake_season1 <- c(9.6,11,10.5,12.5,9.4,11.2,12.5,14.8,5.6)
pregnant_flu_norisk_Uptake_season2 <- c(12.9,14,12.3,14.4,16.8,16.8,13.3,24.1,9.9)
pregnant_flu_norisk_Uptake_season3 <- c(27.3,27.5,23.2,27.6,36.4,20,29.8,42.5,23.6)
pregnant_flu_norisk_Uptake_all_seasons <- c(14.5,15.9,14,16.5,17.2,15.1,16.4,24.6,11.3)

pregnant_covid_norisk_Uptake <- c(33,37.5,36.4,36.5,52.2,33.3,44.4,40,20)
pregnant_covid_risk_Uptake <- c(28.1,25.5,24.7,27.2,26.7,32.3,24.5,28.6,19.8)

Old_covid_primary_uptake <- c(83.2,93.3,94.5,93.5,91.6,93.2,91.4,92,90.4)
Old_covid_booster1_uptake <- c(94.5,92.6,92.8,92.7,91.3,92.5,92.9,90.3,90.4)
Old_covid_booster2_uptake <- c(80.3,76.7,74.1,76.9,74.8,77.9,74.1,74.8,76.0)

df <- data.frame(
  MentalHealth = factor(
    c(
      "No mental health conditions",
      "Any mental health condition",
      "Psychiatric comorbidity",
      "Depression",
      "Anxiety",
      "Bipolar disorder",
      "PTSD",
      "OCD",
      "ADHD"
    ),
    levels = c(
      "No mental health conditions",
      "Any mental health condition",
      "Psychiatric comorbidity",
      "Depression",
      "Anxiety",
      "Bipolar disorder",
      "PTSD",
      "OCD",
      "ADHD"
    )
  ),
  Influenza_Pregnancy.Risk_Season.1 = pregnant_flu_risk_Uptake_season1,
  Influenza_Pregnancy.Risk_Season.2 = pregnant_flu_risk_Uptake_season2,
  Influenza_Pregnancy.Risk_Season.3 = pregnant_flu_risk_Uptake_season3,
  Influenza_Pregnancy.Risk_All.seasons = pregnant_flu_risk_Uptake_all_seasons,
  Influenza_Pregnancy.No.Risk_Season.1 = pregnant_flu_norisk_Uptake_season1,
  Influenza_Pregnancy.No.Risk_Season.2 = pregnant_flu_norisk_Uptake_season2,
  Influenza_Pregnancy.No.Risk_Season.3 = pregnant_flu_norisk_Uptake_season3,
  Influenza_Pregnancy.No.Risk_All.seasons = pregnant_flu_norisk_Uptake_all_seasons,
  Influenza_Older_Season.1     = Uptake_Old_Flu_season1,
  Influenza_Older_Season.2     = Uptake_Old_Flu_season2,
  Influenza_Older_Season.3     = Uptake_Old_Flu_season3,
  Influenza_Older_All.seasons     = Uptake_Old_Flu_allseasons,
  COVID_Pregnancy.No.Risk     = pregnant_covid_norisk_Uptake,
  COVID_Pregnancy.Risk     = pregnant_covid_risk_Uptake, 
  COVID_Older_Primary         = Old_covid_primary_uptake,
  COVID_Older_Booster1         = Old_covid_booster1_uptake,
  COVID_Older_Booster2         = Old_covid_booster2_uptake
)


df_long <- df %>%
  pivot_longer(
    cols = -MentalHealth,
    names_to = c("Vaccine", "AgeGroup", "Season/Dose"),
    names_sep = "_",
    values_to = "Uptake"
  )

df_long$AgeGroup.Dose <- paste0(df_long$AgeGroup, ".", df_long$`Season/Dose`)
df_long$AgeGroup.Dose <- factor(df_long$AgeGroup.Dose, levels = c("Older.Primary", "Older.Booster1", "Older.Booster2",
                                                          "Pregnancy.Risk.NA", "Pregnancy.No.Risk.NA", 
                                                          "Older.Season.1", "Older.Season.2", "Older.Season.3",
                                                          "Older.All.seasons","Pregnancy.Risk.Season.1",
                                                          "Pregnancy.Risk.Season.2","Pregnancy.Risk.Season.3",
                                                          "Pregnancy.Risk.All.seasons","Pregnancy.No.Risk.Season.1",
                                                          "Pregnancy.No.Risk.Season.2","Pregnancy.No.Risk.Season.3",
                                                          "Pregnancy.No.Risk.All.seasons"))

df_long$MentalHealth <- factor(df_long$MentalHealth, levels = c("ADHD","OCD","PTSD","Bipolar disorder",
                                                                "Anxiety", "Depression" , 
                                                                "Psychiatric comorbidity",
                                                                "Any mental health condition",
                                                                "No mental health conditions"))

ggplot(df_long, aes(x = AgeGroup.Dose, y = MentalHealth, fill = Uptake)) +
  geom_tile(color = "white") +
  facet_wrap(~ Vaccine, scales = "free_x") +
  scale_fill_gradient(
    low = "#fee8c8",
    high = "#e34a33",
    name = "Uptake (%)"
  ) +
  labs(
    title = "",
    x = "Age Group",
    y = "Mental Health Condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#EAE1DA"),
    panel.background = element_rect(fill = "#EAE1DA"),
    legend.background = element_rect(fill = "white"),
    legend.box.background = element_rect(fill = "#EAE1DA")
  )+ geom_text(
    aes(
      label = paste0(round(Uptake), "%"),
      color = "black"
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("black", "white")) + 
  scale_x_discrete(labels = c(Older.Primary = "Older Adults - Primary",
                              Older.Booster1 = "Older Adults - Booster 1",
                              Older.Booster2 = "Older Adults - Booster 2",
                              Pregnancy.No.Risk.NA = "Pregnancy - Without Risk Factors",
                              Pregnancy.Risk.NA = "Pregnancy - With Risk Factors",
                              Older.All.seasons = "Older Adults - All Seasons",
                              Older.Season.1 = "Older Adults - Season 1",
                              Older.Season.2 = "Older Adults - Season 2",
                              Older.Season.3 = "Older Adults - Season 3",
                              Pregnancy.No.Risk.All.seasons = "Pregnancy - Without Risk Factors - All Seasons",
                              Pregnancy.No.Risk.Season.1 = "Pregnancy - Without Risk Factors - Season 1",
                              Pregnancy.No.Risk.Season.2 = "Pregnancy - Without Risk Factors - Season 2",
                              Pregnancy.No.Risk.Season.3 = "Pregnancy - Without Risk Factors - Season 3",
                              Pregnancy.Risk.All.seasons = "Pregnancy - With Risk Factors - All Seasons",
                              Pregnancy.Risk.Season.1 = "Pregnancy - With Risk Factors - Season 1",
                              Pregnancy.Risk.Season.2 = "Pregnancy - With Risk Factors - Season 2",
                              Pregnancy.Risk.Season.3 = "Pregnancy - With Risk Factors - Season 3"))


# Or plot difference from no mental condition in each group
df_long_diff <- df_long %>%
  group_by(AgeGroup.Dose) %>%
  mutate(diff = ((Uptake - first(Uptake)) / first(Uptake)) * 100) %>%
  ungroup()

ggplot(df_long_diff, aes(x = AgeGroup.Dose, y = MentalHealth, fill = diff)) +
  geom_tile(color = "white") +
  facet_wrap(~ Vaccine, scales = "free_x") +
  scale_fill_gradient(
    low = "#6A89A7",
    high = "#C59B4A",
    name = "Difference in Uptake (%)"
  ) +
  labs(
    title = "",
    x = "Age Group",
    y = "Mental Health Condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#EAE1DA"),
    panel.background = element_rect(fill = "#EAE1DA"),
    legend.background = element_rect(fill = "white"),
    legend.box.background = element_rect(fill = "#EAE1DA")
  )+ geom_text(
    aes(
      label = paste0(round(diff), "%"),
      color = "black"
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("black", "white")) + 
  scale_x_discrete(labels = c(Older.Primary = "Older Adults - Primary",
                              Older.Booster1 = "Older Adults - Booster 1",
                              Older.Booster2 = "Older Adults - Booster 2",
                              Pregnancy.No.Risk.NA = "Pregnancy - Without Risk Factors",
                              Pregnancy.Risk.NA = "Pregnancy - With Risk Factors",
                              Older.All.seasons = "Older Adults - All Seasons",
                              Older.Season.1 = "Older Adults - Season 1",
                              Older.Season.2 = "Older Adults - Season 2",
                              Older.Season.3 = "Older Adults - Season 3",
                              Pregnancy.No.Risk.All.seasons = "Pregnancy - Without Risk Factors - All Seasons",
                              Pregnancy.No.Risk.Season.1 = "Pregnancy - Without Risk Factors - Season 1",
                              Pregnancy.No.Risk.Season.2 = "Pregnancy - Without Risk Factors - Season 2",
                              Pregnancy.No.Risk.Season.3 = "Pregnancy - Without Risk Factors - Season 3",
                              Pregnancy.Risk.All.seasons = "Pregnancy - With Risk Factors - All Seasons",
                              Pregnancy.Risk.Season.1 = "Pregnancy - With Risk Factors - Season 1",
                              Pregnancy.Risk.Season.2 = "Pregnancy - With Risk Factors - Season 2",
                              Pregnancy.Risk.Season.3 = "Pregnancy - With Risk Factors - Season 3"))

##################################################################
library(scatterplot3d)
library(plotly)
library(dplyr)
df <- df_long
# Convert categories to numeric
df$mh_num <- as.numeric(factor(df$MentalHealth))
df$group_num <- as.numeric(factor(df$AgeGroup.Dose))


df$AgeGroup.Dose <- recode(
  df$AgeGroup.Dose,
'Older.Primary' = "Older Adults - Primary",
'Older.Booster1' = "Older Adults - Booster 1",
'Older.Booster2' = "Older Adults - Booster 2",
'Pregnancy.No.Risk.NA' = "Pregnancy - Without Risk Factors",
'Pregnancy.Risk.NA' = "Pregnancy - With Risk Factors",
'Older.All.seasons' = "Older Adults - All Seasons",
'Older.Season.1' = "Older Adults - Season 1",
'Older.Season.2' = "Older Adults - Season 2",
'Older.Season.3' = "Older Adults - Season 3",
'Pregnancy.No.Risk.All.seasons' = "Pregnancy - Without Risk Factors - All Seasons",
'Pregnancy.No.Risk.Season.1' = "Pregnancy - Without Risk Factors - Season 1",
'Pregnancy.No.Risk.Season.2' = "Pregnancy - Without Risk Factors - Season 2",
'Pregnancy.No.Risk.Season.3' = "Pregnancy - Without Risk Factors - Season 3",
'Pregnancy.Risk.All.seasons' = "Pregnancy - With Risk Factors - All Seasons",
'Pregnancy.Risk.Season.1' = "Pregnancy - With Risk Factors - Season 1",
'Pregnancy.Risk.Season.2' = "Pregnancy - With Risk Factors - Season 2",
'Pregnancy.Risk.Season.3' = "Pregnancy - With Risk Factors - Season 3"
)
# Split data
flu <- subset(df, grepl("influenza", df$Vaccine, ignore.case = T))
covid <- subset(df, grepl("covid", df$Vaccine, ignore.case = T))

fig <- plot_ly()

for(i in 1:nrow(flu)) {
  
  fig <- fig %>%
    add_trace(
      type = "scatter3d",
      mode = "lines",
      
      x = c(flu$group_num[i], flu$group_num[i]),
      y = c(flu$mh_num[i], flu$mh_num[i]),
      z = c(0, flu$Uptake[i]),
      
      line = list(
        width = 15
      ),
      
      showlegend = FALSE
    )
}

fig <- fig %>%
  layout(
    scene = list(
      camera = list(
        eye = list(x = 1.8, y = 1.8, z = 1.2)
      ),
      xaxis = list(
        title = "Life Stage - Season",
        tickvals = unique(flu$group_num),
        ticktext = unique(flu$AgeGroup.Dose),
        standoff = 20
      ),
      
      yaxis = list(
        title = list(text = "Mental Health Condition",standoff = 20),
        tickvals = unique(flu$mh_num),
        ticktext = unique(flu$MentalHealth)
        
      ),
      
      zaxis = list(
        title = "Vaccine Uptake (%)"
      )
    )
  )

fig

library(htmlwidgets)


table()







