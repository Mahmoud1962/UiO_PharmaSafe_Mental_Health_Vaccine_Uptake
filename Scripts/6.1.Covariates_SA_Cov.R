################################################################################
# Script: 6.1.Covariates_SA_Cov.R
#
# Purpose:
#   Compile the needed covariates (including missing values) for older adult and
#   pregnant populations included in the influenza and COVID-19 vaccine cohorts.
#   This is done for the sensitivity analysis reclassifying BMI, education,
#   marital status, and demnetia status as confounders and not mediators.
################################################################################library(dplyr)
library(lubridate)
library(haven)
library(readr)
###########################################################################
# Dementia risk factors
# Let's start by defining the risk factors
load("Path_to_Pregnant_COVID-19_NoRisk_Covariates_Corrected_NoDuplicates.rdata")
load("Path_to_Pregnant_Influenza_Risk_Covariates_Corrected_NoDuplicates.rdata")
load("Path_to_Pregnant_COVID-19_Risk_Covariates_Corrected_NoDuplicates.rdata")
load("Path_to_Pregnant_Influenza_NoRisk_Covariates_Corrected_NoDuplicates.rdata")
load("Path_to_Old_flu_Covariates.rdata")
load("Path_to_Old_covid_Covariates.rdata")

Dementia_ICD10 <- c("F00", "F01", "F02","F03", "G30", "G31")
Dementia_ICPC <- c("P70")

NPR <- read_delim("Path_to_EVENTS_NPR_SOM.csv", 
                  delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
load("Path_to_Old_COVID_MH_Corrected_Final.rdata")
load("Path_to_Old_Influenza_NoDuplicates.rdata")

NPR_flu <- NPR[NPR$person_id %in% cov_old_flu$person_id, ]
NPR_flu1 <- NPR_flu[NPR_flu$event_code %in% Dementia_ICD10,]

merged <- merge(old_flu, NPR_flu1, all = T)
merged$Dementia <- 0
merged$Dementia[merged$event_code %in% Dementia_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged <- merged[, c("person_id", "Dementia")]
cov_old_flu <- merge(cov_old_flu, merged, all = T)
cov_old_flu <- cov_old_flu[!duplicated(cov_old_flu$person_id),]
cov_old_flu <- cov_old_flu[cov_old_flu$person_id %in% old_flu$person_id,]
cov_old_flu$Dementia[is.na(cov_old_flu$Dementia)] <- 0

NPR_covid <- NPR[NPR$person_id %in% old_covid$person_id, ]
NPR_covid1 <- NPR_covid[NPR_covid$event_code %in% Dementia_ICD10,]

merged <- merge(old_covid, NPR_covid1, all = T)
merged$Dementia <- 0
merged$Dementia[merged$event_code %in% Dementia_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged <- merged[, c("person_id", "Dementia")]
cov_old_covid <- cov_old_covid[cov_old_covid$person_id %in% old_covid$person_id,]
merged <- merged[merged$Dementia == 1,]
cov_old_covid$Dementia <- 0
cov_old_covid$Dementia[cov_old_covid$person_id %in% merged$person_id] <- 1
save(cov_old_covid, file = "Old_COVID-19_Covariates_SA_Cov.rdata")
save(cov_old_flu, file = "Old_InfluenzaCovariates_SA_Cov.rdata")

# Now we need to check KUHR for risk factors and procedures from NPR
KUHR_2015 <- read_delim("Path_to_EVENTS_KUHR_2015.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2015 <- KUHR_2015[KUHR_2015$person_id %in% old_flu$person_id,]
KUHR_2015 <- KUHR_2015[grepl("P70", KUHR_2015$event_code, ignore.case = T), ]
gc()
KUHR_2016 <- read_delim("Path_to_EVENTS_KUHR_2016.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2016 <- KUHR_2016[KUHR_2016$person_id %in% old_flu$person_id,]
KUHR_2016 <- KUHR_2016[grepl("P70", KUHR_2016$event_code, ignore.case = T), ]
gc()
KUHR_2017 <- read_delim("Path_to_EVENTS_KUHR_2017.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2017 <- KUHR_2017[KUHR_2017$person_id %in% old_flu$person_id,]
KUHR_2017 <- KUHR_2017[grepl("P70", KUHR_2017$event_code, ignore.case = T), ]
gc()
KUHR_2018 <- read_delim("Path_to_EVENTS_KUHR_2018.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2018 <- KUHR_2018[KUHR_2018$person_id %in% old_flu$person_id,]
KUHR_2018 <- KUHR_2018[grepl("P70", KUHR_2018$event_code, ignore.case = T), ]
gc()
KUHR_2019 <- read_delim("Path_to_EVENTS_KUHR_2019.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2019 <- KUHR_2019[KUHR_2019$person_id %in% old_flu$person_id,]
KUHR_2019 <- KUHR_2019[grepl("P70", KUHR_2019$event_code, ignore.case = T), ]
gc()
KUHR <- merge(KUHR_2015, KUHR_2016, all = T)
KUHR <- merge(KUHR, KUHR_2017, all = T)
KUHR <- merge(KUHR, KUHR_2018, all = T)
KUHR <- merge(KUHR, KUHR_2019, all = T)

merged <- merge(old_flu, KUHR, all = T)
merged$Dementia <- 0
merged$Dementia[grepl("P70", merged$event_code, ignore.case = T) & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1)) & merged$event_record_vocabulary == "ICPC"] <- 1
merged <- merged %>% group_by(person_id) %>%
  mutate(Dementia = max(Dementia))
merged <- merged[merged$Dementia == 1,]
cov_old_flu$Dementia[cov_old_flu$person_id %in% merged$person_id] <- 1
save(cov_old_flu, file = "Old_Influenza_Covariates_SA_Cov.rdata")

KUHR_2020 <- read_delim("Path_to_EVENTS_KUHR_2020.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2020 <- KUHR_2020[KUHR_2020$person_id %in% old_covid$person_id,]
KUHR_2020 <- KUHR_2020[grepl("P70", KUHR_2020$event_code, ignore.case = T), ]
gc()
KUHR_2021 <- read_delim("Path_to_EVENTS_KUHR_2021.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2021 <- KUHR_2021[KUHR_2021$person_id %in% old_covid$person_id,]
KUHR_2021 <- KUHR_2021[grepl("P70", KUHR_2021$event_code, ignore.case = T), ]
gc()
KUHR_2022 <- read_delim("Path_to_EVENTS_KUHR_2022.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2022 <- KUHR_2022[KUHR_2022$person_id %in% old_covid$person_id,]
KUHR_2022 <- KUHR_2022[grepl("P70", KUHR_2022$event_code, ignore.case = T), ]
gc()
KUHR_2023 <- read_delim("Path_to_EVENTS_KUHR_2023.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2023 <- KUHR_2023[KUHR_2023$person_id %in% old_covid$person_id,]
KUHR_2023 <- KUHR_2023[grepl("P70", KUHR_2023$event_code, ignore.case = T), ]
gc()
KUHR <- merge(KUHR_2020, KUHR_2021, all = T)
KUHR <- merge(KUHR, KUHR_2022, all = T)
KUHR <- merge(KUHR, KUHR_2023, all = T)

merged <- merge(old_covid, KUHR, all = T)
merged$Dementia <- 0
merged$Dementia[grepl("P70", merged$event_code, ignore.case = T) & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1)) & merged$event_record_vocabulary == "ICPC"] <- 1
merged <- merged %>% group_by(person_id) %>%
  mutate(Dementia = max(Dementia))
merged <- merged[merged$Dementia == 1,]
cov_old_covid$Dementia[cov_old_covid$person_id %in% merged$person_id] <- 1
save(cov_old_covid, file = "Old_COVID-19_Covariates_SA_Cov.rdata")
###################################################################################
# Education
SSB <- read_sas("Path_to_w22_0605_bu_utd.sas7bdat")
SSB_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% cov_old_covid$person_id,]
SSB_covid$Education <- ifelse(SSB_covid$bu_gruppe_2021 %in% c("IU", "GS", "VGS"), "Upper secondary or lower", "Post-secondary or higher") 
SSB_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% cov_old_flu$person_id,]
SSB_flu$Education <- ifelse(SSB_flu$bu_gruppe_2017 %in% c("IU", "GS", "VGS"), "Upper secondary or lower", "Post-secondary or higher") 
SSB_covid <- SSB_covid[, c(1,14)]
SSB_flu <- SSB_flu[, c(1,14)]
colnames(SSB_covid) [1] <- "person_id"
colnames(SSB_flu) [1] <- "person_id"
cov_old_covid <- merge(cov_old_covid, SSB_covid, all = T)
cov_old_flu <- merge(cov_old_flu, SSB_flu, all = T)
save(cov_old_covid, file = "Old_COVID-19_Covariates_SA_Cov.rdata")
save(cov_old_flu, file = "Old_InfluenzaCovariates_SA_Cov.rdata")
###################################################################################
SSB <- read_sas("Path_to_w22_0605_sivilstand.sas7bdat")
SSB_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% cov_old_covid$person_id,]
SSB_covid <- SSB_covid %>%
  mutate(Marital_status = case_when(
    # Married / Cohabiting
    SSB_covid$sivilstand_2021 %in% c(2, 6, 9) ~ "Married/Cohabiting",
    SSB_covid$sivilstand_2021 %in% c(1, 4) ~ "Single/Unmarried",
    SSB_covid$sivilstand_2021 %in% c(3, 5, 7, 8) ~ "Other",
    # Default for any unexpected values
    TRUE ~ NA_character_
  ))

SB_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% cov_old_flu$person_id,]
SSB_flu <- SSB_flu %>%
  mutate(Marital_status = case_when(
    # Married / Cohabiting
    SSB_flu$sivilstand_2017 %in% c(2, 6, 9) ~ "Married/Cohabiting",
    SSB_flu$sivilstand_2017 %in% c(1, 4) ~ "Single/Unmarried",
    SSB_flu$sivilstand_2017 %in% c(3, 5, 7, 8) ~ "Other",
    # Default for any unexpected values
    TRUE ~ NA_character_
  ))
SSB_covid <- SSB_covid[, c(1,10)]
SSB_flu <- SSB_flu[, c(1,10)]
colnames(SSB_covid) [1] <- "person_id"
colnames(SSB_flu) [1] <- "person_id"
cov_old_covid <- merge(cov_old_covid, SSB_covid, all = T)
cov_old_flu <- merge(cov_old_flu, SSB_flu, all = T)
cov_old_covid <- cov_old_covid[!duplicated(cov_old_covid$person_id),]
cov_old_flu <- cov_old_flu[!duplicated(cov_old_flu$person_id),]
save(cov_old_covid, file = "Old_COVID-19_Covariates_SA_Cov.rdata")
save(cov_old_flu, file = "Old_Influenza_Covariates_SA_Cov.rdata")
#################################################################################################
# Education
SSB <- read_sas("Path_to_w22_0605_bu_utd.sas7bdat")
SSB_preg_covid_risk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_risk$person_id,]
SSB_preg_covid_norisk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_norisk$person_id,]
SSB_preg_flu_risk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_risk$person_id,]
SSB_preg_flu_norisk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_norisk$person_id,]
SSB_preg_covid_norisk$Education <- ifelse(SSB_preg_covid_norisk$bu_gruppe_2021 %in% c("IU", "GS", "VGS"), "Upper secondary or lower", "Post-secondary or higher") 
SSB_preg_covid_risk$Education <- ifelse(SSB_preg_covid_risk$bu_gruppe_2021 %in% c("IU", "GS", "VGS"), "Upper secondary or lower", "Post-secondary or higher") 
SSB_preg_flu_norisk$Education <- ifelse(SSB_preg_flu_norisk$bu_gruppe_2017 %in% c("IU", "GS", "VGS"), "Upper secondary or lower", "Post-secondary or higher") 
SSB_preg_flu_risk$Education <- ifelse(SSB_preg_flu_risk$bu_gruppe_2017 %in% c("IU", "GS", "VGS"), "Upper secondary or lower", "Post-secondary or higher") 
SSB_preg_covid_norisk <- SSB_preg_covid_norisk[, c(1,14)]
SSB_preg_covid_risk <- SSB_preg_covid_risk[, c(1,14)]
SSB_preg_flu_norisk <- SSB_preg_flu_norisk[, c(1,14)]
SSB_preg_flu_risk <- SSB_preg_flu_risk[, c(1,14)]

colnames(SSB_preg_flu_risk) [1] <- "person_id"
colnames(SSB_preg_flu_norisk) [1] <- "person_id"
colnames(SSB_preg_covid_norisk) [1] <- "person_id"
colnames(SSB_preg_covid_risk) [1] <- "person_id"

cov_preg_covid_norisk$person_id <- pregnant_covid_norisk$person_id
cov_preg_covid_risk$person_id <- pregnant_covid_risk$person_id
cov_preg_flu_norisk$person_id <- pregnant_flu_norisk$person_id
cov_preg_flu_risk$person_id <- pregnant_flu_risk$person_id

cov_preg_covid_norisk <- merge(cov_preg_covid_norisk, SSB_preg_covid_norisk, all = T)
cov_preg_covid_risk <- merge(cov_preg_covid_risk, SSB_preg_covid_risk, all = T)
cov_preg_flu_norisk <- merge(cov_preg_flu_norisk, SSB_preg_flu_norisk, all = T)
cov_preg_flu_risk <- merge(cov_preg_flu_risk, SSB_preg_flu_risk, all = T)

save(cov_preg_covid_norisk, file = "Pregnant_COVID-19_NoRisk_Covariates_SA_Cov.rdata")
save(cov_preg_covid_risk, file = "Pregnant_COVID-19_Risk_Covariates_SA_Cov.rdata")
save(cov_preg_flu_risk, file = "Pregnant_Influenza_Risk_Covariates_SA_Cov.rdata")
save(cov_preg_flu_norisk, file = "Pregnant_Influenza_NoRisk_Covariates_SA_Cov.rdata")
###################################################################################
# Marital status
SSB <- read_sas("Path_to_w22_0605_sivilstand.sas7bdat")
SSB_preg_covid_risk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_risk$person_id,]
SSB_preg_covid_norisk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_norisk$person_id,]
SSB_preg_flu_risk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_risk$person_id,]
SSB_preg_flu_norisk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_norisk$person_id,]
SSB_preg_covid_risk <- SSB_preg_covid_risk %>%
  mutate(Marital_status = case_when(
    # Married / Cohabiting
    SSB_preg_covid_risk$sivilstand_2021 %in% c(2, 6, 9) ~ "Married/Cohabiting",
    SSB_preg_covid_risk$sivilstand_2021 %in% c(1, 4) ~ "Single/Unmarried",
    SSB_preg_covid_risk$sivilstand_2021 %in% c(3, 5, 7, 8) ~ "Other",
    # Default for any unexpected values
    TRUE ~ NA_character_
  ))
SSB_preg_covid_norisk <- SSB_preg_covid_norisk %>%
  mutate(Marital_status = case_when(
    # Married / Cohabiting
    SSB_preg_covid_norisk$sivilstand_2021 %in% c(2, 6, 9) ~ "Married/Cohabiting",
    SSB_preg_covid_norisk$sivilstand_2021 %in% c(1, 4) ~ "Single/Unmarried",
    SSB_preg_covid_norisk$sivilstand_2021 %in% c(3, 5, 7, 8) ~ "Other",
    # Default for any unexpected values
    TRUE ~ NA_character_
  ))
SSB_preg_flu_norisk <- SSB_preg_flu_norisk %>%
  mutate(Marital_status = case_when(
    # Married / Cohabiting
    SSB_preg_flu_norisk$sivilstand_2017 %in% c(2, 6, 9) ~ "Married/Cohabiting",
    SSB_preg_flu_norisk$sivilstand_2017 %in% c(1, 4) ~ "Single/Unmarried",
    SSB_preg_flu_norisk$sivilstand_2017 %in% c(3, 5, 7, 8) ~ "Other",
    # Default for any unexpected values
    TRUE ~ NA_character_
  ))
SSB_preg_flu_risk <- SSB_preg_flu_risk %>%
  mutate(Marital_status = case_when(
    # Married / Cohabiting
    SSB_preg_flu_risk$sivilstand_2017 %in% c(2, 6, 9) ~ "Married/Cohabiting",
    SSB_preg_flu_risk$sivilstand_2017 %in% c(1, 4) ~ "Single/Unmarried",
    SSB_preg_flu_risk$sivilstand_2017 %in% c(3, 5, 7, 8) ~ "Other",
    # Default for any unexpected values
    TRUE ~ NA_character_
  ))
SSB_preg_covid_norisk <- SSB_preg_covid_norisk[, c(1,10)]
SSB_preg_covid_risk <- SSB_preg_covid_risk[, c(1,10)]
SSB_preg_flu_norisk <- SSB_preg_flu_norisk[, c(1,10)]
SSB_preg_flu_risk <- SSB_preg_flu_risk[, c(1,10)]
colnames(SSB_preg_covid_norisk) [1] <- "person_id"
colnames(SSB_preg_covid_risk) [1] <- "person_id"
colnames(SSB_preg_flu_risk) [1] <- "person_id"
colnames(SSB_preg_flu_norisk) [1] <- "person_id"
cov_preg_covid_norisk <- merge(cov_preg_covid_norisk, SSB_preg_covid_norisk, all = T)
cov_preg_covid_risk <- merge(cov_preg_covid_risk, SSB_preg_covid_risk, all = T)
cov_preg_flu_norisk <- merge(cov_preg_flu_norisk, SSB_preg_flu_norisk, all = T)
cov_preg_flu_risk <- merge(cov_preg_flu_risk, SSB_preg_flu_risk, all = T)

save(cov_preg_covid_norisk, file = "Pregnant_COVID-19_NoRisk_Covariates_SA_Cov.rdata")
save(cov_preg_covid_risk, file = "Pregnant_COVID-19_Risk_Covariates_SA_Cov.rdata")
save(cov_preg_flu_risk, file = "Pregnant_Influenza_Risk_Covariates_SA_Cov.rdata")
save(cov_preg_flu_norisk, file = "Pregnant_Influenza_NoRisk_Covariates_SA_Cov.rdata")
#################################################################################################
# BMI
pregnant_covid_risk <- pregnant_covid_risk %>%
  mutate(BMI = case_when(
    KMI_FOER < 18.5 ~ "< 18.5",
    KMI_FOER >= 18.5 & KMI_FOER < 25 ~ "18.5 – 24.9",
    KMI_FOER >= 25 & KMI_FOER < 30 ~ "25 – 29.9",
    KMI_FOER >= 30 ~ "≥ 30",
    TRUE ~ NA_character_  # Missing values
  ))

pregnant_covid_norisk <- pregnant_covid_norisk %>%
  mutate(BMI = case_when(
    KMI_FOER < 18.5 ~ "< 18.5",
    KMI_FOER >= 18.5 & KMI_FOER < 25 ~ "18.5 – 24.9",
    KMI_FOER >= 25 & KMI_FOER < 30 ~ "25 – 29.9",
    KMI_FOER >= 30 ~ "≥ 30",
    TRUE ~ NA_character_  # Missing values
  ))

pregnant_flu_norisk <- pregnant_flu_norisk %>%
  mutate(BMI = case_when(
    KMI_FOER < 18.5 ~ "< 18.5",
    KMI_FOER >= 18.5 & KMI_FOER < 25 ~ "18.5 – 24.9",
    KMI_FOER >= 25 & KMI_FOER < 30 ~ "25 – 29.9",
    KMI_FOER >= 30 ~ "≥ 30",
    TRUE ~ NA_character_  # Missing values
  ))

pregnant_flu_risk <- pregnant_flu_risk %>%
  mutate(BMI = case_when(
    KMI_FOER < 18.5 ~ "< 18.5",
    KMI_FOER >= 18.5 & KMI_FOER < 25 ~ "18.5 – 24.9",
    KMI_FOER >= 25 & KMI_FOER < 30 ~ "25 – 29.9",
    KMI_FOER >= 30 ~ "≥ 30",
    TRUE ~ NA_character_  # Missing values
  ))

cov_preg_flu_norisk <- cov_preg_flu_norisk[!duplicated(cov_preg_flu_norisk$Preg_id),]
covid_risk <- pregnant_covid_risk[, c(1,156)]
covid_norisk <- pregnant_covid_norisk[, c(1,157)]
flu_risk <- pregnant_flu_risk[, c(1,167)]
flu_norisk <- pregnant_flu_norisk[, c(1,168)]

cov_preg_covid_norisk <- merge(cov_preg_covid_norisk, covid_norisk, all = T)
cov_preg_covid_risk <- merge(cov_preg_covid_risk, covid_risk, all = T)
cov_preg_flu_norisk <- merge(cov_preg_flu_norisk, flu_norisk, all = T)
cov_preg_flu_risk <- merge(cov_preg_flu_risk, flu_risk, all = T)

save(cov_preg_covid_norisk, file = "Pregnant_COVID-19_NoRisk_Covariates_SA_Cov.rdata")
save(cov_preg_covid_risk, file = "Pregnant_COVID-19_Risk_Covariates_SA_Cov.rdata")
save(cov_preg_flu_risk, file = "Pregnant_Influenza_Risk_Covariates_SA_Cov.rdata")
save(cov_preg_flu_norisk, file = "Pregnant_Influenza_NoRisk_Covariates_SA_Cov.rdata")
#################################################################################################











