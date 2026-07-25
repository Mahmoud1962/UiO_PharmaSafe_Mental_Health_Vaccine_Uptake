
###############################################################################
# Script: Outcome_Definition.R
# Purpose: Define influenza and COVID-19 vaccination outcomes for study cohorts.
###############################################################################

library(lubridate)
library(readr)
library(dplyr)

load("Path_to_Old_Covid_Population_MH.rdata")
load("Path_to_Pregnant_Influenza_Risk_Population_MH.rdata")
load("Path_to_Pregnant_Influenza_NoRisk_Population_MH.rdata")
load("Path_to_Pregnant_Covid_NoRisk_Population_MH.rdata")
load("Path_to_Pregnant_Covid_Risk_Population_MH.rdata")
load("Path_to_Old_Influenza_Population_MH.rdata")

flu.codes <- c("J07BB01", "J07BB02", "J07BB03")
covid.codes <- c("J07BN02", 'J07BN01', 'J07BX03', 'J07BN04', 'J07BN03')

SYSVAK <- read_csv("Path_to_VACCINES.csv")
SYSVAK <- SYSVAK[SYSVAK$person_id %in% old_covid$person_id | SYSVAK$person_id %in% old_flu$person_id |
                   SYSVAK$person_id %in% pregnant_covid_risk$person_id | SYSVAK$person_id %in% pregnant_covid_norisk$person_id |
                   SYSVAK$person_id %in% pregnant_flu_risk$person_id |SYSVAK$person_id %in% pregnant_flu_norisk$person_id ,]

###############################################################################
# We start with the Old Adult Flu population
SYSVAK_OLD_FLU <- SYSVAK[SYSVAK$person_id %in% old_flu$person_id,]
SYSVAK_OLD_FLU <- SYSVAK_OLD_FLU[SYSVAK_OLD_FLU$vx_atc %in% flu.codes,]
merged <- merge(old_flu, SYSVAK_OLD_FLU, all = T)

# Now we split the outcome per vaccination season and keep the date for censoring
merged$vac.season.1 <- dmy('1-09-2016')
merged$vac.season.1.end <- dmy('30-04-2017')

merged$vac.season.2 <- dmy('1-09-2017')
merged$vac.season.2.end <- dmy('30-04-2018')

merged$vac.season.3 <- dmy('1-09-2018')
merged$vac.season.3.end <- dmy('30-04-2019')

# Now we check if the vaccination date is within the study period which is the enrollment column until the vac.season.3.end date
merged <- merged[ymd(merged$vx_admin_date) >= merged$enrollment_date & ymd(merged$vx_admin_date) <= merged$vac.season.3.end, ]

# We stratify the outcome per season
merged$season1 <- 0
merged$season2 <- 0
merged$season3 <- 0
merged <- merged[!is.na(merged$person_id),]

merged$season1[ymd(merged$vx_admin_date) >= merged$vac.season.1 & ymd(merged$vx_admin_date) <= merged$vac.season.1.end] <- 1
merged$season2[ymd(merged$vx_admin_date) >= merged$vac.season.2 & ymd(merged$vx_admin_date) <= merged$vac.season.2.end] <- 1
merged$season3[ymd(merged$vx_admin_date) >= merged$vac.season.3 & ymd(merged$vx_admin_date) <= merged$vac.season.3.end] <- 1

merged <- merged[, c(1,36,37,47:55)]

old_flu <- merge(old_flu, merged, all = T)

old_flu$season1[is.na(old_flu$season1)] <- 0
old_flu$season2[is.na(old_flu$season2)] <- 0
old_flu$season3[is.na(old_flu$season3)] <- 0
old_flu$all.seasons <- 0
old_flu$all.seasons[ old_flu$season1 == 1 | old_flu$season2 == 1 |old_flu$season3 == 1] <- 1

save(old_flu, file = "Old_flu_MentalHealth_Vaccine.rdata")
###############################################################################
# We do the same for the Old Adult covid population but yearly instead of seasons
SYSVAK_OLD_COVID <- SYSVAK[SYSVAK$person_id %in% old_covid$person_id,]
SYSVAK_OLD_COVID <- SYSVAK_OLD_COVID[SYSVAK_OLD_COVID$vx_atc %in% covid.codes,]
merged <- merge(old_covid, SYSVAK_OLD_COVID, all = T)

# Now we split the outcome per vaccination season and keep the date for censoring
merged$vac.season.1 <- dmy('01-01-2021')
merged$vac.season.1.end <- dmy('31-12-2021')

merged$vac.season.2 <- dmy('01-01-2022')
merged$vac.season.2.end <- dmy('31-12-2022')

merged$vac.season.3 <- dmy('01-01-2023')
merged$vac.season.3.end <- dmy('31-12-2023')

# Now we check if the vaccination date is within the study period which is the enrollment column until the vac.season.3.end date
merged <- merged[ymd(merged$vx_admin_date) >= merged$enrollment_date & ymd(merged$vx_admin_date) <= merged$vac.season.3.end, ]

# We stratify the outcome per season
merged$season1 <- 0
merged$season2 <- 0
merged$season3 <- 0
merged <- merged[!is.na(merged$person_id),]

merged$season1[ymd(merged$vx_admin_date) >= merged$vac.season.1 & ymd(merged$vx_admin_date) <= merged$vac.season.1.end] <- 1
merged$season2[ymd(merged$vx_admin_date) >= merged$vac.season.2 & ymd(merged$vx_admin_date) <= merged$vac.season.2.end] <- 1
merged$season3[ymd(merged$vx_admin_date) >= merged$vac.season.3 & ymd(merged$vx_admin_date) <= merged$vac.season.3.end] <- 1

merged <- merged[, c(1,56)]

old_covid <- merge(old_covid, merged, all = T)

old_covid$season1[is.na(old_covid$season1)] <- 0
old_covid$season2[is.na(old_covid$season2)] <- 0
old_covid$season3[is.na(old_covid$season3)] <- 0
old_covid$all.seasons <- 0
old_covid$all.seasons[ old_covid$season1 == 1 | old_covid$season2 == 1 |old_covid$season3 == 1] <- 1

save(old_covid, file = "old_COVID_MentalHealth_Vaccine.rdata")
###############################################################################
# We do the same for the pregnant covid population but yearly instead of seasons
SYSVAK_PREGNANT_COVID_risk <- SYSVAK[SYSVAK$person_id %in% pregnant_covid_risk$person_id,]
SYSVAK_PREGNANT_COVID_risk <- SYSVAK_PREGNANT_COVID_risk[SYSVAK_PREGNANT_COVID_risk$vx_atc %in% covid.codes,]
pregnant_covid_risk$person_id <- as.numeric(pregnant_covid_risk$person_id)
merged <- full_join(pregnant_covid_risk, SYSVAK_PREGNANT_COVID_risk, by = "person_id", relationship = "many-to-many")
# Now we split the outcome per vaccination season and keep the date for censoring
merged <- merged[!is.na(merged$vx_admin_date),]

# Now we check if the vaccination date is within the study period which is the enrollment column until the vac.season.3.end date
merged1 <- merged[ymd(merged$vx_admin_date) >= merged$enrollment_risk & ymd(merged$vx_admin_date) <= merged$delivery, ]

merged$vaccinated <- 0
merged$vaccinated[ymd(merged$vx_admin_date) >= merged$enrollment_risk & ymd(merged$vx_admin_date) <= merged$delivery] <- 1
merged <- merged[, c(122,145,146,156)]

pregnant_covid_risk <- merge(pregnant_covid_risk, merged, all = T)

pregnant_covid_risk$vaccinated[is.na(pregnant_covid_risk$vaccinated)] <- 0
save(pregnant_covid_risk, file = "Pregnant_COVID__Risk_MentalHealth_Vaccine.rdata")
###############################################################################################################
# We do the same for the pregnant covid population but yearly instead of seasons
SYSVAK_PREGNANT_COVID_norisk <- SYSVAK[SYSVAK$person_id %in% pregnant_covid_norisk$person_id,]
SYSVAK_PREGNANT_COVID_norisk <- SYSVAK_PREGNANT_COVID_norisk[SYSVAK_PREGNANT_COVID_norisk$vx_atc %in% covid.codes,]
pregnant_covid_norisk$person_id <- as.numeric(pregnant_covid_norisk$person_id)
merged <- full_join(pregnant_covid_norisk, SYSVAK_PREGNANT_COVID_norisk, by = "person_id", relationship = "many-to-many")
# Now we split the outcome per vaccination season and keep the date for censoring
merged <- merged[!is.na(merged$vx_admin_date),]

# Now we check if the vaccination date is within the study period which is the enrollment column until the vac.season.3.end date
merged1 <- merged[ymd(merged$vx_admin_date) >= merged$enrollment_norisk & ymd(merged$vx_admin_date) <= merged$delivery, ]

merged$vaccinated <- 0
merged$vaccinated[ymd(merged$vx_admin_date) >= merged$enrollment_norisk & ymd(merged$vx_admin_date) <= merged$delivery] <- 1
merged <- merged[, c(1,155,156,166)]

pregnant_covid_norisk <- merge(pregnant_covid_norisk, merged, all = T)

pregnant_covid_norisk$vaccinated[is.na(pregnant_covid_norisk$vaccinated)] <- 0
save(pregnant_covid_norisk, file = "Pregnant_COVID_NoRisk_MentalHealth_Vaccine.rdata")
###############################################################################
# We do the same for the pregnant covid population but yearly instead of seasons
SYSVAK_PREGNANT_flu_norisk <- SYSVAK[SYSVAK$person_id %in% pregnant_flu_norisk$person_id,]
SYSVAK_PREGNANT_flu_norisk <- SYSVAK_PREGNANT_flu_norisk[SYSVAK_PREGNANT_flu_norisk$vx_atc %in% flu.codes,]
pregnant_flu_norisk$person_id <- as.numeric(pregnant_flu_norisk$person_id)
merged <- full_join(pregnant_flu_norisk, SYSVAK_PREGNANT_flu_norisk, by = "person_id", relationship = "many-to-many")
# Now we split the outcome per vaccination season and keep the date for censoring
merged <- merged[!is.na(merged$vx_admin_date),]

# Now we check if the vaccination date is within the study period which is the enrollment column until the vac.season.3.end date
merged1 <- merged[ymd(merged$vx_admin_date) >= merged$enrollment_norisk & ymd(merged$vx_admin_date) <= merged$delivery, ]

merged$vaccinated <- 0
merged$vaccinated[ymd(merged$vx_admin_date) >= merged$enrollment_norisk & ymd(merged$vx_admin_date) <= merged$delivery] <- 1
merged <- merged[ymd(merged$vx_admin_date) >= as.Date("2016-09-01") & ymd(merged$vx_admin_date) <= as.Date("2019-03-31"),]
merged <- merged[, c(128,151,152,162)]

pregnant_flu_norisk <- merge(pregnant_flu_norisk, merged, all = T)

pregnant_flu_norisk$vaccinated[is.na(pregnant_flu_norisk$vaccinated)] <- 0
save(pregnant_flu_norisk, file = "Pregnant_FLU_NoRisk_MentalHealth_Vaccine.rdata")
###############################################################################
# We do the same for the pregnant covid population but yearly instead of seasons
SYSVAK_PREGNANT_flu_risk <- SYSVAK[SYSVAK$person_id %in% pregnant_flu_risk$person_id,]
SYSVAK_PREGNANT_flu_risk <- SYSVAK_PREGNANT_flu_risk[SYSVAK_PREGNANT_flu_risk$vx_atc %in% flu.codes,]
pregnant_flu_risk$person_id <- as.numeric(pregnant_flu_risk$person_id)
merged <- full_join(pregnant_flu_risk, SYSVAK_PREGNANT_flu_risk, by = "person_id", relationship = "many-to-many")
# Now we split the outcome per vaccination season and keep the date for censoring
merged <- merged[!is.na(merged$vx_admin_date),]

# Now we check if the vaccination date is within the study period which is the enrollment column until the vac.season.3.end date
merged1 <- merged[ymd(merged$vx_admin_date) >= merged$enrollment_risk & ymd(merged$vx_admin_date) <= merged$delivery, ]

merged$vaccinated <- 0
merged$vaccinated[ymd(merged$vx_admin_date) >= merged$enrollment_risk & ymd(merged$vx_admin_date) <= merged$delivery] <- 1
merged <- merged[ymd(merged$vx_admin_date) >= as.Date("2016-09-01") & ymd(merged$vx_admin_date) <= as.Date("2019-03-31"),]
merged <- merged[, c(128,151,152,162)]

pregnant_flu_risk <- merge(pregnant_flu_risk, merged, all = T)

pregnant_flu_risk$vaccinated[is.na(pregnant_flu_risk$vaccinated)] <- 0
save(pregnant_flu_risk, file = "Pregnant_FLU_Risk_MentalHealth_Vaccine.rdata")

























