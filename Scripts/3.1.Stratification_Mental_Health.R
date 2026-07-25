################################################################################
# Script: 3.1.Stratification_Mental_Health.R
#
# Purpose:
#   Identify the exposure to mental health conditions for older adult and
#   pregnant populations included in the influenza and COVID-19 vaccine cohorts.
################################################################################

# Load in all cohorts
load("Path_to_Older_Adult_Influenza_Population.rdata")
load("Path_to_Older_Adult_COVID19_Population.rdata")
load("Path_to_Pregnant_Influenza_Risk_Population.rdata")
load("Path_to_Pregnant_Influenza_NoRisk_Population.rdata")
load("Path_to_Pregnant_COVID19_Risk_Population.rdata")
load("Path_to_Pregnant_COVID19_NoRisk_Population.rdata")

# Load libraries in
library(lubridate)
library(data.table)
library(readr)
library(haven)
library(dplyr)
library(broom)
# NPR does not have the mental health diagnoses used in this project and will thus be skipped
# Check for each mental health separately in each population
# Make and Save a KUHR file for influenza
KUHR_2015 <- read.csv("Path_to_/2015 Data fra KUHR 22-261.dsv", sep = ";")
KUHR_2015 <- KUHR_2015[(grepl("P", KUHR_2015$DIAGNOSER, ignore.case = T) & KUHR_2015$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2015$DIAGNOSER, ignore.case = T) & KUHR_2015$DIAGNOSEKODEVERK == "ICD-10"),]
gc()
KUHR_2016 <- read.csv("Path_to_2016 Data fra KUHR 22-261.dsv", sep = ";")
KUHR_2016 <- KUHR_2016[(grepl("P", KUHR_2016$DIAGNOSER, ignore.case = T) & KUHR_2016$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2016$DIAGNOSER, ignore.case = T) & KUHR_2016$DIAGNOSEKODEVERK == "ICD-10"),]
gc()
KUHR_2017 <- read.csv("Path_to_2017 Data fra KUHR 22-261.dsv", sep = ";")
KUHR_2017 <- KUHR_2017[(grepl("P", KUHR_2017$DIAGNOSER, ignore.case = T) & KUHR_2017$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2017$DIAGNOSER, ignore.case = T) & KUHR_2017$DIAGNOSEKODEVERK == "ICD-10"),]
gc()
KUHR_2018 <- read.csv("Path_to_2018 Data fra KUHR 22-261.dsv", sep = ";")
KUHR_2018 <- KUHR_2018[(grepl("P", KUHR_2018$DIAGNOSER, ignore.case = T) & KUHR_2018$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2018$DIAGNOSER, ignore.case = T) & KUHR_2018$DIAGNOSEKODEVERK == "ICD-10"),]
gc()
KUHR_2019 <- fread("Path_to_2019 Data fra KUHR 22-261.dsv", sep = ';')
KUHR_2019 <- KUHR_2019[(grepl("P", KUHR_2019$DIAGNOSER, ignore.case = T) & KUHR_2019$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2019$DIAGNOSER, ignore.case = T) & KUHR_2019$DIAGNOSEKODEVERK == "ICD-10"),]
gc()

KUHR <- merge(KUHR_2015, KUHR_2016, all = T)
KUHR <- merge(KUHR, KUHR_2017, all = T)
KUHR <- merge(KUHR, KUHR_2018, all = T)
KUHR <- merge(KUHR, KUHR_2019, all = T)

save(KUHR, file = "KUHR_FLU_MH.rdata")

# Make and Save a KUHR file for COVID
KUHR_2020 <- fread("Path_to_2020 Data fra KUHR 22-261.dsv", sep = ';')
KUHR_2020 <- KUHR_2020[(grepl("P", KUHR_2020$DIAGNOSER, ignore.case = T) & KUHR_2020$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2020$DIAGNOSER, ignore.case = T) & KUHR_2020$DIAGNOSEKODEVERK == "ICD-10"),]
gc()
KUHR_2021 <- fread("Path_to_2021 Data fra KUHR 22-261.dsv", sep = ';')
KUHR_2021 <- KUHR_2021[(grepl("P", KUHR_2021$DIAGNOSER, ignore.case = T) & KUHR_2021$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2021$DIAGNOSER, ignore.case = T) & KUHR_2021$DIAGNOSEKODEVERK == "ICD-10"),]
gc()
KUHR_2022 <- fread("Path_to_2022 Data fra KUHR 22-261.dsv", sep = ';')
KUHR_2022 <- KUHR_2022[(grepl("P", KUHR_2022$DIAGNOSER, ignore.case = T) & KUHR_2022$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2022$DIAGNOSER, ignore.case = T) & KUHR_2022$DIAGNOSEKODEVERK == "ICD-10"),]
gc()
KUHR_2023 <- fread("Path_to_2023 Data fra KUHR 22-261.dsv", sep = ';')
KUHR_2023 <- KUHR_2023[(grepl("P", KUHR_2023$DIAGNOSER, ignore.case = T) & KUHR_2023$DIAGNOSEKODEVERK == "ICPC-2")
                       |(grepl("F", KUHR_2023$DIAGNOSER, ignore.case = T) & KUHR_2023$DIAGNOSEKODEVERK == "ICD-10"),]
gc()

KUHR <- merge(KUHR_2020, KUHR_2021, all = T)
KUHR <- merge(KUHR, KUHR_2022, all = T)
KUHR <- merge(KUHR, KUHR_2023, all = T)

ref_kuhr <- read.csv("Path_to_Reference_dates_KUHR.csv", sep = ",")
colnames(ref_kuhr) [1] <- "PASIENTLOPENUMMER"
KUHR<- merge(KUHR, ref_kuhr, all = T)
KUHR$date <- as.Date(KUHR$ref_date) + KUHR$DIFFERANSEDAGER
save(KUHR, file = "KUHR_COVID_MH.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
old_flu$enrollment_date <- if_else((old_flu$birth_date + years(65)) <= ymd(20161001), ymd(20161001), (old_flu$birth_date + years(65)))
old_flu$lookback <- old_flu$enrollment_date - years(1)

# Depression P76
KUHR_depression <- KUHR[(grepl("P76", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F32|F33", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$PASIENTLOPENUMMER %in% old_flu$KOBLINGSNOEKKEL,]
KUHR_depression <- KUHR_depression[, c("PASIENTLOPENUMMER", "DIAGNOSER", "DIAGNOSEKODEVERK", "date")]
colnames(KUHR_depression)[1] <- "person_id"
colnames(old_flu)[1] <- "person_id"
merged <- merge(old_flu, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= as.Date(merged$enrollment_date) & as.Date(merged$date) >= as.Date(merged$lookback)] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
depression_IDs <- depression_IDs[!duplicated(depression_IDs) & !is.na(depression_IDs)]
save(depression_IDs, file = "Depression_Old_Flu_KUHR.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# Bipolar P73
KUHR_depression <- KUHR[(grepl("P73", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F31", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$PASIENTLOPENUMMER %in% old_flu$person_id,]
KUHR_depression <- KUHR_depression[, c("PASIENTLOPENUMMER", "DIAGNOSER", "DIAGNOSEKODEVERK", "date")]
colnames(KUHR_depression)[1] <- "person_id"
merged <- merge(old_flu, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= as.Date(merged$enrollment_date) & as.Date(merged$date) >= as.Date(merged$lookback)] <- 1
merged <- merged[merged$depression == 1,]
bipolar_IDs <- merged$person_id
bipolar_IDs <- bipolar_IDs[!duplicated(bipolar_IDs) & !is.na(bipolar_IDs)]
save(bipolar_IDs, file = "Bipolar_Old_Flu_KUHR.rdata")# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# GAD and Panic P74
KUHR_depression <- KUHR[(grepl("P74", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F41", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$PASIENTLOPENUMMER %in% old_flu$person_id,]
KUHR_depression <- KUHR_depression[, c("PASIENTLOPENUMMER", "DIAGNOSER", "DIAGNOSEKODEVERK", "date")]
colnames(KUHR_depression)[1] <- "person_id"
merged <- merge(old_flu, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= as.Date(merged$enrollment_date) & as.Date(merged$date) >= as.Date(merged$lookback)] <- 1
merged <- merged[merged$depression == 1,]
anxiety_IDs <- merged$person_id
anxiety_IDs <- anxiety_IDs[!duplicated(anxiety_IDs) & !is.na(anxiety_IDs)]
save(anxiety_IDs, file = "Anxiety_Old_Flu_KUHR.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# PTSD P82
KUHR_depression <- KUHR[(grepl("P82", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F43", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$PASIENTLOPENUMMER %in% old_flu$person_id,]
KUHR_depression <- KUHR_depression[, c("PASIENTLOPENUMMER", "DIAGNOSER", "DIAGNOSEKODEVERK", "date")]
colnames(KUHR_depression)[1] <- "person_id"
merged <- merge(old_flu, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= as.Date(merged$enrollment_date) & as.Date(merged$date) >= as.Date(merged$lookback)] <- 1
merged <- merged[merged$depression == 1,]
PTSD_IDs <- merged$person_id
PTSD_IDs <- PTSD_IDs[!duplicated(PTSD_IDs) & !is.na(PTSD_IDs)]
save(PTSD_IDs, file = "PTSD_Old_Flu_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# OCD P79
KUHR_depression <- KUHR[(grepl("P79", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F42", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$PASIENTLOPENUMMER %in% old_flu$person_id,]
KUHR_depression <- KUHR_depression[, c("PASIENTLOPENUMMER", "DIAGNOSER", "DIAGNOSEKODEVERK", "date")]
colnames(KUHR_depression)[1] <- "person_id"
merged <- merge(old_flu, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= as.Date(merged$enrollment_date) & as.Date(merged$date) >= as.Date(merged$lookback)] <- 1
merged <- merged[merged$depression == 1,]
OCD_IDs <- merged$person_id
OCD_IDs <- OCD_IDs[!duplicated(OCD_IDs) & !is.na(OCD_IDs)]
save(OCD_IDs, file = "OCD_Old_Flu_KUHR.rdata")
# ED P79
KUHR_depression <- KUHR[(grepl("P86", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F50", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$PASIENTLOPENUMMER %in% old_flu$person_id,]
KUHR_depression <- KUHR_depression[, c("PASIENTLOPENUMMER", "DIAGNOSER", "DIAGNOSEKODEVERK", "date")]
colnames(KUHR_depression)[1] <- "person_id"
merged <- merge(old_flu, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= as.Date(merged$enrollment_date) & as.Date(merged$date) >= as.Date(merged$lookback)] <- 1
merged <- merged[merged$depression == 1,]
ED_IDs <- merged$person_id
ED_IDs <- ED_IDs[!duplicated(ED_IDs) & !is.na(ED_IDs)]
save(ED_IDs, file = "ED_Old_Flu_KUHR.rdata")
# ADHD P81
KUHR_depression <- KUHR[(grepl("P81", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F90", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$PASIENTLOPENUMMER %in% old_flu$person_id,]
KUHR_depression <- KUHR_depression[, c("PASIENTLOPENUMMER", "DIAGNOSER", "DIAGNOSEKODEVERK", "date")]
colnames(KUHR_depression)[1] <- "person_id"
merged <- merge(old_flu, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= as.Date(merged$enrollment_date) & as.Date(merged$date) >= as.Date(merged$lookback)] <- 1
merged <- merged[merged$depression == 1,]
ADHD_IDs <- merged$person_id
ADHD_IDs <- ADHD_IDs[!duplicated(ADHD_IDs) & !is.na(ADHD_IDs)]
save(ADHD_IDs, file = "ADHD_Old_Flu_KUHR.rdata")
####################################################################################################################
# Now let's do it for the pregnant risk flu
# Depression P76
colnames(KUHR)[1] <- "person_id"
KUHR_depression <- KUHR[(grepl("P76", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F32|F33", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_risk$person_id,]
merged <- merge(pregnant_flu_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
depression_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(depression_IDs, file = "Depression_Pregnant_Flu_Risk_KUHR.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# Bipolar P73
KUHR_depression <- KUHR[(grepl("P73", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F31", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_risk$person_id,]
merged <- merge(pregnant_flu_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
bipolar_IDs <- merged$person_id
bipolar_IDs <- bipolar_IDs[!duplicated(bipolar_IDs)]
save(bipolar_IDs, file = "Bipolar_Pregnant_Flu_Risk_KUHR.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# GAD and Panic P74
KUHR_depression <- KUHR[(grepl("P74", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F41", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_risk$person_id,]
merged <- merge(pregnant_flu_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
anxiety_IDs <- merged$person_id
anxiety_IDs <- anxiety_IDs[!duplicated(anxiety_IDs)]
save(anxiety_IDs, file = "Anxiety_Pregnant_Flu_Risk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# PTSD P82
KUHR_depression <- KUHR[(grepl("P82", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F43", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_risk$person_id,]
merged <- merge(pregnant_flu_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
PTSD_IDs <- merged$person_id
PTSD_IDs <- PTSD_IDs[!duplicated(PTSD_IDs)]
save(PTSD_IDs, file = "PTSD_Pregnant_Flu_Risk_KUHR.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# OCD P79
KUHR_depression <- KUHR[(grepl("P79", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F42", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_risk$person_id,]
merged <- merge(pregnant_flu_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
OCD_IDs <- merged$person_id
OCD_IDs <- OCD_IDs[!duplicated(OCD_IDs)]
save(OCD_IDs, file = "OCD_Pregnant_Flu_Risk_KUHR.rdata")
# ED P86
KUHR_depression <- KUHR[(grepl("P86", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F50", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_risk$person_id,]
merged <- merge(pregnant_flu_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
ED_IDs <- merged$person_id
ED_IDs <- ED_IDs[!duplicated(ED_IDs)]
save(ED_IDs, file = "ED_Pregnant_Flu_Risk_KUHR.rdata")

# ADHD P81
KUHR_depression <- KUHR[(grepl("P81", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F90", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_risk$person_id,]
merged <- merge(pregnant_flu_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
ADHD_IDs <- merged$person_id
ADHD_IDs <- ADHD_IDs[!duplicated(ADHD_IDs)]
save(ADHD_IDs, file = "ADHD_Pregnant_Flu_Risk_KUHR.rdata")

#########################################################################################################################################
# Now let's do it for the pregnant risk covid
# Depression P76
KUHR_depression <- KUHR[(grepl("P76", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F32|F33", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_norisk$person_id,]
merged <- merge(pregnant_flu_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
Depression_IDs <- merged$person_id
Depression_IDs <- Depression_IDs[!duplicated(Depression_IDs)]
save(Depression_IDs, file = "Depression_Pregnant_Flu_Risk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# Bipolar P73
KUHR_depression <- KUHR[(grepl("P73", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F31", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_norisk$person_id,]
merged <- merge(pregnant_flu_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
Bipolar_IDs <- merged$person_id
Bipolar_IDs <- Bipolar_IDs[!duplicated(Bipolar_IDs)]
save(Bipolar_IDs, file = "Bipolar_Pregnant_Flu_Risk_KUHR.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# GAD and Panic P74
KUHR_depression <- KUHR[(grepl("P74", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F41", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_norisk$person_id,]
merged <- merge(pregnant_flu_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
Anxiety_IDs <- merged$person_id
Anxiety_IDs <- Anxiety_IDs[!duplicated(Anxiety_IDs)]
save(Anxiety_IDs, file = "Anxiety_Pregnant_Flu_Risk_KUHR.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# PTSD P82
KUHR_depression <- KUHR[(grepl("P82", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F43", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_norisk$person_id,]
merged <- merge(pregnant_flu_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
Anxiety_IDs <- merged$person_id
PTSD_IDs <- Anxiety_IDs[!duplicated(Anxiety_IDs)]
save(PTSD_IDs, file = "PTSD_Pregnant_Flu_Risk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# OCD P79
KUHR_depression <- KUHR[(grepl("P79", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F42", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_norisk$person_id,]
merged <- merge(pregnant_flu_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
Anxiety_IDs <- merged$person_id
OCD_IDs <- Anxiety_IDs[!duplicated(Anxiety_IDs)]
save(OCD_IDs, file = "OCD_Pregnant_Flu_Risk_KUHR.rdata")
# ED P86
KUHR_depression <- KUHR[(grepl("P86", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F50", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_norisk$person_id,]
merged <- merge(pregnant_flu_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
Anxiety_IDs <- merged$person_id
ED_IDs <- Anxiety_IDs[!duplicated(Anxiety_IDs)]
save(ED_IDs, file = "ED_Pregnant_Flu_Risk_KUHR.rdata")
# ADHD P81
KUHR_depression <- KUHR[(grepl("P81", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F90", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_flu_norisk$person_id,]
merged <- merge(pregnant_flu_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
Anxiety_IDs <- merged$person_id
ADHD_IDs <- Anxiety_IDs[!duplicated(Anxiety_IDs)]
save(ADHD_IDs, file = "ADHD_Pregnant_Flu_Risk_KUHR.rdata")

#########################################################################################################################################
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# Depression P76
colnames(KUHR)[1] <- "person_id"
KUHR_depression <- KUHR[(grepl("P76", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F32|F33", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_norisk$person_id,]
merged <- merge(pregnant_covid_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
depression_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(depression_IDs, file = "Depression_Pregnant_COVID_NoRisk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# Bipolar P73
KUHR_depression <- KUHR[(grepl("P73", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F31", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_norisk$person_id,]
merged <- merge(pregnant_covid_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
bipolar_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(bipolar_IDs, file = "Bipolar_Pregnant_COVID_NoRisk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# GAD and Panic P74
KUHR_depression <- KUHR[(grepl("P74", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F41", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_norisk$person_id,]
merged <- merge(pregnant_covid_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
anxiety_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(anxiety_IDs, file = "Anxiety_Pregnant_COVID_NoRisk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# PTSD P82
KUHR_depression <- KUHR[(grepl("P82", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F43", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_norisk$person_id,]
merged <- merge(pregnant_covid_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
PTSD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(PTSD_IDs, file = "PTSD_Pregnant_COVID_NoRisk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# OCD P79
KUHR_depression <- KUHR[(grepl("P79", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F42", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_norisk$person_id,]
merged <- merge(pregnant_covid_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
OCD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(OCD_IDs, file = "OCD_Pregnant_COVID_NoRisk_KUHR.rdata")

# ED P86
KUHR_depression <- KUHR[(grepl("P86", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F50", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_norisk$person_id,]
merged <- merge(pregnant_covid_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
ED_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(ED_IDs, file = "ED_Pregnant_COVID_NoRisk_KUHR.rdata")
# ADHD P81
KUHR_depression <- KUHR[(grepl("P81", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F90", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_norisk$person_id,]
merged <- merge(pregnant_covid_norisk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_norisk & as.Date(merged$date) >= (merged$enrollment_norisk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
ADHD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(ADHD_IDs, file = "ADHD_Pregnant_COVID_NoRisk_KUHR.rdata")
###########################################################################################################################################
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# Depression P76
KUHR_depression <- KUHR[(grepl("P76", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F32|F33", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_risk$person_id,]
merged <- merge(pregnant_covid_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
depression_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(depression_IDs, file = "Depression_Pregnant_COVID_Risk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# Bipolar P73
KUHR_depression <- KUHR[(grepl("P73", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F31", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_risk$person_id,]
merged <- merge(pregnant_covid_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
bipolar_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(bipolar_IDs, file = "Bipolar_Pregnant_COVID_Risk_KUHR.rdata")

# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# GAD and Panic P74
KUHR_depression <- KUHR[(grepl("P74", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F41", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_risk$person_id,]
merged <- merge(pregnant_covid_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
anxiety_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(anxiety_IDs, file = "Anxiety_Pregnant_COVID_Risk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# PTSD P82
KUHR_depression <- KUHR[(grepl("P82", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F43", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_risk$person_id,]
merged <- merge(pregnant_covid_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
PTSD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(PTSD_IDs, file = "PTSD_Pregnant_COVID_Risk_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# OCD P79
KUHR_depression <- KUHR[(grepl("P79", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F42", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_risk$person_id,]
merged <- merge(pregnant_covid_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
OCD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(OCD_IDs, file = "OCD_Pregnant_COVID_Risk_KUHR.rdata")
# ED P86
KUHR_depression <- KUHR[(grepl("P86", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F50", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_risk$person_id,]
merged <- merge(pregnant_covid_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
ED_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(ED_IDs, file = "ED_Pregnant_COVID_Risk_KUHR.rdata")

# ADHD P81
KUHR_depression <- KUHR[(grepl("P81", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F90", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% pregnant_covid_risk$person_id,]
merged <- merge(pregnant_covid_risk, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_risk & as.Date(merged$date) >= (merged$enrollment_risk - years(1))] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
ADHD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(ADHD_IDs, file = "ADHD_Pregnant_COVID_Risk_KUHR.rdata")

###########################################################################################################################################
# Now let's do it for the older adult population
# Depression P76
old_covid$person_id <- as.integer(old_covid$person_id)
old_covid <- old_covid[, -16]
KUHR_depression <- KUHR[(grepl("P76", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F32|F33", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% old_covid$person_id,]
merged <- merge(old_covid, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_date & as.Date(merged$date) >= merged$lookback] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
depression_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(depression_IDs, file = "Depression_Old_COVID_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# Bipolar P73
KUHR_depression <- KUHR[(grepl("P73", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F31", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% old_covid$person_id,]
merged <- merge(old_covid, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_date & as.Date(merged$date) >= merged$lookback] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
bipolar_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(bipolar_IDs, file = "Bipolar_Old_COVID_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# GAD and Panic P74
KUHR_depression <- KUHR[(grepl("P74", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F41", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% old_covid$person_id,]
merged <- merge(old_covid, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_date & as.Date(merged$date) >= merged$lookback] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
anxiety_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(anxiety_IDs, file = "Anxiety_Old_COVID_KUHR.rdata")
# PTSD P82
KUHR_depression <- KUHR[(grepl("P82", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F43", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression$PASIENTLOPENUMMER <- as.character(KUHR_depression$PASIENTLOPENUMMER)
KUHR_depression <- KUHR_depression[KUHR_depression$PASIENTLOPENUMMER %in% old_covid$person_id,]
colnames(KUHR_depression)[1] <- "person_id"
KUHR_depression <- KUHR_depression[, c(1,5,10,11,18)]
merged <- merge(old_covid, KUHR_depression, all = T , by= "person_id")
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_date & as.Date(merged$date) >= merged$lookback] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
PTSD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(PTSD_IDs, file = "PTSD_Old_COVID_KUHR.rdata")
# Let's start by looking for diagnoses in the Flu cohort for pregnancy and the old 
# OCD P79
KUHR_depression <- KUHR[(grepl("P79", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F42", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% old_covid$person_id,]
merged <- merge(old_covid, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_date & as.Date(merged$date) >= merged$lookback] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
OCD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(OCD_IDs, file = "OCD_Old_COVID_KUHR.rdata")
# ED P86
KUHR_depression <- KUHR[(grepl("P86", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F50", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% old_covid$person_id,]
merged <- merge(old_covid, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_date & as.Date(merged$date) >= merged$lookback] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
ED_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(ED_IDs, file = "ED_Old_COVID_KUHR.rdata")
# ADHD P81
KUHR_depression <- KUHR[(grepl("P81", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICPC-2")
                        |(grepl("F90", KUHR$DIAGNOSER, ignore.case = T) & KUHR$DIAGNOSEKODEVERK == "ICD-10"),]
KUHR_depression <- KUHR_depression[KUHR_depression$person_id %in% old_covid$person_id,]
merged <- merge(old_covid, KUHR_depression, all = T)
merged$depression <- 0
merged$depression[as.Date(merged$date) <= merged$enrollment_date & as.Date(merged$date) >= merged$lookback] <- 1
merged <- merged[merged$depression == 1,]
depression_IDs <- merged$person_id
ADHD_IDs <- depression_IDs[!duplicated(depression_IDs)]
save(ADHD_IDs, file = "ADHD_Old_COVID_KUHR.rdata")
#######################################################################################################################################
# Now we need to compile everything together to have the subgroups
# We do it for the old flu first
load("M:/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/Populations/Older_Adult_COVID19_Population.rdata")
# Depression
old_covid$depression_mh <- 0
old_covid$depression_mh[old_covid$person_id %in% depression_IDs] <- 1
# Anxiety
old_covid$anxiety_mh <- 0
old_covid$danxiety_mh[old_covid$person_id %in% anxiety_IDs] <- 1
# Bipolar
old_covid$bipolar_mh <- 0
old_covid$bipolar_mh[old_covid$person_id %in% bipolar_IDs] <- 1
# PTSD
old_covid$PTSD_mh <- 0
old_covid$PTSD_mh[old_covid$person_id %in% PTSD_IDs] <- 1
# OCD
old_covid$OCD_mh <- 0
old_covid$OCD_mh[old_covid$person_id %in% OCD_IDs] <- 1
# ED
old_covid$ED_mh <- 0
old_covid$ED_mh[old_covid$person_id %in% ED_IDs] <- 1
# ADHD
old_covid$ADHD_mh <- 0
old_covid$ADHD_mh[old_covid$person_id %in% ADHD_IDs] <- 1
save(old_covid, file = "Old_COVID_Population_MH_corrected.rdata")
#######################################################################################################################################
# We do it for the old covid

# Depression
pregnant_covid_risk$depression_mh <- 0
pregnant_covid_risk$depression_mh[pregnant_covid_risk$person_id %in% depression_IDs] <- 1
# Anxiety
pregnant_covid_risk$anxiety_mh <- 0
pregnant_covid_risk$anxiety_mh[pregnant_covid_risk$person_id %in% anxiety_IDs] <- 1
# Bipolar
pregnant_covid_risk$bipolar_mh <- 0
pregnant_covid_risk$bipolar_mh[pregnant_covid_risk$person_id %in% bipolar_IDs] <- 1
# PTSD
pregnant_covid_risk$PTSD_mh <- 0
pregnant_covid_risk$PTSD_mh[pregnant_covid_risk$person_id %in% PTSD_IDs] <- 1
# OCD
pregnant_covid_risk$OCD_mh <- 0
pregnant_covid_risk$OCD_mh[pregnant_covid_risk$person_id %in% OCD_IDs] <- 1
# ED
pregnant_covid_risk$ED_mh <- 0
pregnant_covid_risk$ED_mh[pregnant_covid_risk$person_id %in% ED_IDs] <- 1
# ADHD
pregnant_covid_risk$ADHD_mh <- 0
pregnant_covid_risk$ADHD_mh[pregnant_covid_risk$person_id %in% ADHD_IDs] <- 1
save(pregnant_covid_risk, file = "Pregnant_Risk_COVID_Population_MH_corrected.rdata")
########################################################################################################################
# Depression
pregnant_covid_norisk$depression_mh <- 0
pregnant_covid_norisk$depression_mh[pregnant_covid_norisk$person_id %in% depression_IDs] <- 1
# Anxiety
pregnant_covid_norisk$anxiety_mh <- 0
pregnant_covid_norisk$anxiety_mh[pregnant_covid_norisk$person_id %in% anxiety_IDs] <- 1
# Bipolar
pregnant_covid_norisk$bipolar_mh <- 0
pregnant_covid_norisk$bipolar_mh[pregnant_covid_norisk$person_id %in% bipolar_IDs] <- 1
# PTSD
pregnant_covid_norisk$PTSD_mh <- 0
pregnant_covid_norisk$PTSD_mh[pregnant_covid_norisk$person_id %in% PTSD_IDs] <- 1
# OCD
pregnant_covid_norisk$OCD_mh <- 0
pregnant_covid_norisk$OCD_mh[pregnant_covid_norisk$person_id %in% OCD_IDs] <- 1
# ED
pregnant_covid_norisk$ED_mh <- 0
pregnant_covid_norisk$ED_mh[pregnant_covid_norisk$person_id %in% ED_IDs] <- 1
# ADHD
pregnant_covid_norisk$ADHD_mh <- 0
pregnant_covid_norisk$ADHD_mh[pregnant_covid_norisk$person_id %in% ADHD_IDs] <- 1
save(pregnant_covid_norisk, file = "Pregnant_NoRisk_COVID_Population_MH_corrected.rdata")
#########################################################################################################
pregnant_covid_norisk$depression_mh <- 0
pregnant_covid_norisk$depression_mh[pregnant_covid_norisk$person_id %in% depression_IDs] <- 1
# Anxiety
pregnant_covid_norisk$anxiety_mh <- 0
pregnant_covid_norisk$anxiety_mh[pregnant_covid_norisk$person_id %in% anxiety_IDs] <- 1
# Bipolar
pregnant_covid_norisk$bipolar_mh <- 0
pregnant_covid_norisk$bipolar_mh[pregnant_covid_norisk$person_id %in% bipolar_IDs] <- 1
# PTSD
pregnant_covid_norisk$PTSD_mh <- 0
pregnant_covid_norisk$PTSD_mh[pregnant_covid_norisk$person_id %in% PTSD_IDs] <- 1
# OCD
pregnant_covid_norisk$OCD_mh <- 0
pregnant_covid_norisk$OCD_mh[pregnant_covid_norisk$person_id %in% OCD_IDs] <- 1
# ED
pregnant_covid_norisk$ED_mh <- 0
pregnant_covid_norisk$ED_mh[pregnant_covid_norisk$person_id %in% ED_IDs] <- 1
# ADHD
pregnant_covid_norisk$ADHD_mh <- 0
pregnant_covid_norisk$ADHD_mh[pregnant_covid_norisk$person_id %in% ADHD_IDs] <- 1
save(pregnant_covid_norisk, file = "Pregnant_NoRisk_COVID_Population_MH_corrected.rdata")
########################################################################################
pregnant_flu_norisk$depression_mh <- 0
pregnant_flu_norisk$depression_mh[pregnant_flu_norisk$person_id %in% Depression_IDs] <- 1
# Anxiety
pregnant_flu_norisk$anxiety_mh <- 0
pregnant_flu_norisk$anxiety_mh[pregnant_flu_norisk$person_id %in% Anxiety_IDs] <- 1
# Bipolar
pregnant_flu_norisk$bipolar_mh <- 0
pregnant_flu_norisk$bipolar_mh[pregnant_flu_norisk$person_id %in% Bipolar_IDs] <- 1
# PTSD
pregnant_flu_norisk$PTSD_mh <- 0
pregnant_flu_norisk$PTSD_mh[pregnant_flu_norisk$person_id %in% PTSD_IDs] <- 1
# OCD
pregnant_flu_norisk$OCD_mh <- 0
pregnant_flu_norisk$OCD_mh[pregnant_flu_norisk$person_id %in% OCD_IDs] <- 1
# ED
pregnant_flu_norisk$ED_mh <- 0
pregnant_flu_norisk$ED_mh[pregnant_flu_norisk$person_id %in% ED_IDs] <- 1
# ADHD
pregnant_flu_norisk$ADHD_mh <- 0
pregnant_flu_norisk$ADHD_mh[pregnant_flu_norisk$person_id %in% ADHD_IDs] <- 1
save(pregnant_flu_norisk, file = "Pregnant_NoRisk_Influenza_Population_MH_corrected.rdata")
############################################################################################
pregnant_flu_risk$depression_mh <- 0
pregnant_flu_risk$depression_mh[pregnant_flu_risk$person_id %in% depression_IDs] <- 1
# Anxiety
pregnant_flu_risk$anxiety_mh <- 0
pregnant_flu_risk$anxiety_mh[pregnant_flu_risk$person_id %in% anxiety_IDs] <- 1
# Bipolar
pregnant_flu_risk$bipolar_mh <- 0
pregnant_flu_risk$bipolar_mh[pregnant_flu_risk$person_id %in% bipolar_IDs] <- 1
# PTSD
pregnant_flu_risk$PTSD_mh <- 0
pregnant_flu_risk$PTSD_mh[pregnant_flu_risk$person_id %in% PTSD_IDs] <- 1
# OCD
pregnant_flu_risk$OCD_mh <- 0
pregnant_flu_risk$OCD_mh[pregnant_flu_risk$person_id %in% OCD_IDs] <- 1
# ED
pregnant_flu_risk$ED_mh <- 0
pregnant_flu_risk$ED_mh[pregnant_flu_risk$person_id %in% ED_IDs] <- 1
# ADHD
pregnant_flu_risk$ADHD_mh <- 0
pregnant_flu_risk$ADHD_mh[pregnant_flu_risk$person_id %in% ADHD_IDs] <- 1
save(pregnant_flu_risk, file = "Pregnant_Risk_Influenza_Population_MH_corrected.rdata")
#######################################################################################
old_flu$depression_mh <- 0
old_flu$depression_mh[old_flu$person_id %in% depression_IDs] <- 1
# Anxiety
old_flu$anxiety_mh <- 0
old_flu$anxiety_mh[old_flu$person_id %in% anxiety_IDs] <- 1
# Bipolar
old_flu$bipolar_mh <- 0
old_flu$bipolar_mh[old_flu$person_id %in% bipolar_IDs] <- 1
# PTSD
old_flu$PTSD_mh <- 0
old_flu$PTSD_mh[old_flu$person_id %in% PTSD_IDs] <- 1
# OCD
old_flu$OCD_mh <- 0
old_flu$OCD_mh[old_flu$person_id %in% OCD_IDs] <- 1
# ED
old_flu$ED_mh <- 0
old_flu$ED_mh[old_flu$person_id %in% ED_IDs] <- 1
# ADHD
old_flu$ADHD_mh <- 0
old_flu$ADHD_mh[old_flu$person_id %in% ADHD_IDs] <- 1
save(old_flu, file = "Old_Influenza_Population_MH_corrected.rdata")
