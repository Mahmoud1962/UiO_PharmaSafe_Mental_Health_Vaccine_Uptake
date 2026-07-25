library(lubridate)
library(readr)
library(dplyr)
library(haven)
SSB <- read_sas("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_faste_opplysninger.sas7bdat")
reference <- read.csv("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/Reference_dates_SSB.csv")
SSB$birth_date <- as.Date(reference$ref_date) + SSB$foedselsdato_delta
SSB <- merge(SSB, reference, all = T)
SSB <- SSB[SSB$kjoenn == 1,]
SSB$death_date <- as.Date(SSB$ref_date) + SSB$doeds_dato_delta
SSB <- SSB %>%
  mutate(
    # Season 2016-17
    eligible_2016_17 = (birth_date <= as.Date("2016-10-01") - years(65) |
                          (birth_date + years(65) >= as.Date("2016-10-01") & 
                             birth_date + years(65) <= as.Date("2017-03-31"))) &
      (is.na(death_date) | death_date >= as.Date("2016-10-01")),
    
    # Season 2017-18
    eligible_2017_18 = (birth_date <= as.Date("2017-10-01") - years(65) |
                          (birth_date + years(65) >= as.Date("2017-10-01") & 
                             birth_date + years(65) <= as.Date("2018-03-31"))) &
      (is.na(death_date) | death_date >= as.Date("2017-10-01")),
    
    # Season 2018-19
    eligible_2018_19 = (birth_date <= as.Date("2018-10-01") - years(65) |
                          (birth_date + years(65) >= as.Date("2018-10-01") & 
                             birth_date + years(65) <= as.Date("2019-03-31"))) &
      (is.na(death_date) | death_date >= as.Date("2018-10-01"))
  )

old_flu <- SSB[SSB$eligible_2016_17 | SSB$eligible_2017_18 | SSB$eligible_2018_19 ,]
old_flu <- old_flu[!is.na(old_flu$birth_date),]


# Check the percentage dead in the older adult cohort for influenza vaccine
old_flu <- old_flu %>%
  mutate(
    died_during_2016_17 = eligible_2016_17 & 
      !is.na(death_date) & 
      death_date >= as.Date("2016-10-01") & 
      death_date <= as.Date("2017-03-31"),
    
    died_during_2017_18 = eligible_2017_18 & 
      !is.na(death_date) & 
      death_date >= as.Date("2017-10-01") & 
      death_date <= as.Date("2018-03-31"),
    
    died_during_2018_19 = eligible_2018_19 & 
      !is.na(death_date) & 
      death_date >= as.Date("2018-10-01") & 
      death_date <= as.Date("2019-03-31")
  )
# See how many died during each season among the eligible
death_summary <- SSB %>%
  summarise(
    across(starts_with("eligible_"), sum),
    across(starts_with("died_during_"), sum)
  )

print(death_summary)
# 1% died in the 2016-17 season, 1.9% for the 2017-18 season, and 1.7% for the 2018-19 season
save(old_flu, file = "Older_Adult_Influenza_Population.rdata")
#########################################################################
# check the percentage of death in the covid19 older adult population
# Study period: January 1st 2021 to December 31st 2023
study_start <- as.Date("2021-01-01")
study_end <- as.Date("2023-12-31")

# Calculate eligibility and outcomes
old_covid <- SSB %>%
  mutate(
    # Turn 65 before or during study period
    turns_65_during_study = (birth_date + years(65)) <= study_end &
      (birth_date + years(65)) >= study_start,
    
    turns_65_before_study = (birth_date + years(65)) < study_start,
    # Eligible if they turn 65 before or during study AND alive at study start
    eligible = (turns_65_during_study | turns_65_before_study) &  # Turns 65 before/during study
      (is.na(death_date) | death_date >= study_start),  # Alive at study start
    
    # Died during study period (among eligible)
    died_during_study = eligible & 
      !is.na(death_date) & 
      death_date >= study_start & 
      death_date <= study_end,
    
    # Additional useful info
    date_turned_65 = birth_date + years(65),
    enrolled_at_study_start = date_turned_65 <= study_start
  )

old_covid <- old_covid[old_covid$eligible, ]
old_covid <- old_covid[!is.na(old_covid$eligible),]
old_covid <- old_covid[old_covid$kjoenn == 1,]
old_covid$enrollment_date <- if_else(old_covid$date_turned_65 <= study_start, 
                                    study_start, 
                                    old_covid$date_turned_65)

old_covid$lookback <- old_covid$enrollment_date - months(6)
old_covid$washout <- as.Date("2021-01-01")
colnames(old_covid)[1] <- "person_id"
old_covid <- old_covid[!is.na(old_covid$lookback),] # 631,817
# now check for covid 19 diagnosis in the look back period
npr_som <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_NPR_SOM.csv", 
                       delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))

npr_som <- npr_som[grepl("u071", npr_som$event_code, ignore.case = T),]
npr_som <- npr_som[npr_som$person_id %in% old_covid$person_id,]
npr_covid <- merge(npr_som, old_covid, all = T)
npr_covid <- npr_covid[!is.na(npr_covid$event_code),]

covid_before <- npr_covid[ymd(npr_covid$start_date_record) <= npr_covid$enrollment_date & ymd(npr_covid$start_date_record) >= npr_covid$lookback,]

old_covid <- old_covid[!old_covid$person_id %in% covid_before$person_id,] # 631,316
save(old_covid, file = "Older_Adult_COVID19_Population.rdata")

# now check KUHR for diagnoses in the look back period
kuhr_2020 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2020.csv", delim = ",")
kuhr_2020 <- kuhr_2020[grepl("R992", kuhr_2020$event_code, ignore.case = T),]
kuhr_2020 <- kuhr_2020[kuhr_2020$person_id %in% old_covid$person_id,]
save(kuhr_2020, file = "KUHR_2020.rdata")
kuhr_2021 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2021.csv", delim = ",")
kuhr_2021 <- kuhr_2021[grepl("R992", kuhr_2021$event_code, ignore.case = T),]
kuhr_2021 <- kuhr_2021[kuhr_2021$person_id %in% old_covid$person_id,]
save(kuhr_2021, file = "KUHR_2021.rdata")
kuhr_2022 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2022.csv", delim = ",")
kuhr_2022 <- kuhr_2022[grepl("R992", kuhr_2022$event_code, ignore.case = T),]
kuhr_2022 <- kuhr_2022[kuhr_2022$person_id %in% old_covid$person_id,]
save(kuhr_2022, file = "KUHR_2022.rdata")
kuhr_2023 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2023.csv", delim = ",")
kuhr_2023 <- kuhr_2023[grepl("R992", kuhr_2023$event_code, ignore.case = T),]
kuhr_2023 <- kuhr_2023[kuhr_2023$person_id %in% old_covid$person_id,]
save(kuhr_2023, file = "KUHR_2023.rdata")

Kuhr_old_covid <- merge(kuhr_2020, kuhr_2021, all = T)
Kuhr_old_covid <- merge(Kuhr_old_covid, kuhr_2022, all = T)
Kuhr_old_covid <- merge(Kuhr_old_covid, kuhr_2023, all = T)
save(Kuhr_old_covid, file = "KUHR_OLD_COVID.rdata")


merged <- merge(Kuhr_old_covid, old_covid, all = T)
merged <- merged[!is.na(merged$event_code),]
merged <- merged[!is.na(merged$enrollment_date),]
covid_before <- merged[ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= merged$lookback,]
old_covid <- old_covid[!old_covid$person_id %in% covid_before$person_id,] # 628,748

# now check MSIS
msis <- read_delim("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/FHI/MSIS/H-602-E_MSIS-data_2024-09/H-602-E_MSIS-data_2024-09.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE,locale = locale(encoding = "Latin1"))

msis <- msis[msis$KOBLINGSNOEKKEL %in% old_covid$person_id,]
colnames(msis) [1] <- "person_id"
merged <- merge(msis, old_covid, all = T)
merged <- merged[!is.na(merged$person_id),]
referece_msis <- read_delim("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/Reference_dates_FHI.csv", delim = ",")
colnames(referece_msis) [1] <- "person_id"
referece_msis <- referece_msis[referece_msis$person_id %in% merged$person_id,]
merged <- merge(merged, referece_msis, all = T)

merged$test <- as.Date(merged$ref_date) + merged$PrøvedatoDiffDager
merged <- merged[!is.na(merged$test),]
covid_before <- merged[merged$test <= merged$enrollment_date & merged$test >= merged$lookback,]
old_covid <- old_covid[!old_covid$person_id %in% covid_before$person_id,] # 627,553
# Now check vaccination during the washout period
covid.codes <- c("J07BN02", 'J07BN01', 'J07BX03', 'J07BN04', 'J07BN03')
SYSVAK <- read_csv("N:/durable/vac4eu/CDMInstances/vac4eu_1052/VACCINES.csv")
SYSVAK <- SYSVAK[SYSVAK$person_id %in% old_covid$person_id,]
SYSVAK <- SYSVAK[SYSVAK$vx_atc %in% covid.codes, ]
merged <- merge(SYSVAK, old_covid, all = T)
vaccinated_before <- merged[ymd(merged$vx_admin_date) < merged$enrollment_date & ymd(merged$vx_admin_date) > study_start,]
vaccinated_before <- vaccinated_before[!is.na(vaccinated_before$vx_admin_date),]
old_covid <- old_covid[!old_covid$person_id %in% vaccinated_before$person_id,] # 552,756
save(old_covid, file = "Older_Adult_COVID19_Population.rdata")
# 55,173 women died within the study period out of 552,756
# 10 % died within the study period, makes sense since this is the COVID era
###################################################################################################################

# Define the pregnant population for influenza
library(haven)
mbrn <- read_sav("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/FHI/MBRN/MBRN correct data file to use/p222359_mfr_2_2023_2/p222359_mfr_2_2023_2.sav")
mbrn <- mbrn[!is.na(mbrn$SVLEN_DG),]  # removes 2,372
reference_FHI <- read.csv("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/Reference_dates_FHI.csv")
reference_mbrn <- reference_FHI[reference_FHI$lopenr %in% mbrn$ID_MOR,]
colnames(mbrn)[4] <- "lopenr"
mbrn <- merge(mbrn, reference_mbrn, all = T)
mbrn <- mbrn[!is.na(mbrn$ref_date),] # 3,415

mbrn$delivery <- mbrn$FDATO_DIFFERANSEDAGER_MOR + as.Date(mbrn$ref_date)
mbrn$delivery <- ymd(mbrn$delivery)
mbrn$LMP <- mbrn$delivery - days(mbrn$SVLEN_DG)

SSB <- read_sas("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_faste_opplysninger.sas7bdat")
reference <- read.csv("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/Reference_dates_SSB.csv")
SSB$birth_date <- as.Date(reference$ref_date) + SSB$foedselsdato_delta
SSB <- SSB[SSB$KOBLINGSNOEKKEL %in% mbrn$lopenr, ]
SSB <- SSB[, c(1, 15)]
colnames(SSB)[1] <- "lopenr"
mbrn <- merge(mbrn, SSB, all = T)
mbrn_eligible <- mbrn[mbrn$LMP - mbrn$birth_date >= years(18), ]

# Eligible pregnancies are those whose delivery date is after October 1st 2016
# but earlier than March 31st 2019
# Define key dates
start_date <- as.Date("2016-10-01")
study_end <- as.Date("2019-03-31")

# Calculate gestational week 13 (LMP + 12 weeks + 1 day = 85 days)
pregnant_influenza <- mbrn_eligible %>%
  mutate(
    # Calculate gestational week 13 (LMP + 12 weeks + 1 day = 85 days)
    gest_week_13 = LMP + days(85)
  ) %>%
  # FILTER FIRST - only include women who meet study criteria
  filter(
    # Group 1: Pregnant on Oct 1, 2016 and delivered after
    (LMP <= start_date & delivery > start_date) |
      # Group 2: Got pregnant after Oct 1, 2016 and delivered before Mar 31, 2019
      (LMP > start_date & delivery <= study_end)
  ) %>%
  # THEN calculate enrollment dates only for included women
  mutate(
    # Enrollment Date Variable 1: Based on LMP
    enrollment_risk = if_else(LMP <= start_date, start_date, LMP),
    
    # Enrollment Date Variable 2: Based on gestational week 13
    enrollment_norisk = if_else(gest_week_13 <= start_date, start_date, gest_week_13)
  )

pregnant_influenza <- pregnant_influenza %>%
  mutate(
    # Eligibility for 2016-2017 season: Gestational week 13+ during Oct 1, 2016 - Mar 31, 2017
    eligible_2016_2017_norisk = gest_week_13 <= as.Date("2017-03-31") & delivery >= as.Date("2016-09-01"),
    
    # Eligibility for 2017-2018 season: Gestational week 13+ during Oct 1, 2017 - Mar 31, 2018
    eligible_2017_2018_norisk = gest_week_13 <= as.Date("2018-03-31") & delivery >= as.Date("2017-09-01"),
    
    # Eligibility for 2018-2019 season: Gestational week 13+ during Oct 1, 2018 - Mar 31, 2019
    eligible_2018_2019_norisk = gest_week_13 <= as.Date("2019-03-31") & delivery >= as.Date("2018-09-01"),
    # Eligibility for 2016-2017 season: Gestational week 13+ during Oct 1, 2016 - Mar 31, 2017
    eligible_2016_2017_risk = LMP <= as.Date("2017-03-31") & delivery >= as.Date("2016-09-01"),
    
    # Eligibility for 2017-2018 season: Gestational week 13+ during Oct 1, 2017 - Mar 31, 2018
    eligible_2017_2018_risk = LMP <= as.Date("2018-03-31") & delivery >= as.Date("2017-09-01"),
    
    # Eligibility for 2018-2019 season: Gestational week 13+ during Oct 1, 2018 - Mar 31, 2019
    eligible_2018_2019_risk = LMP <= as.Date("2019-03-31") & delivery >= as.Date("2018-09-01") 
  )
save(pregnant_influenza, file = "Pregnant_Influenza_Population_RiskFactors_YetToBeVerified.rdata")
#######################################################################

# eligible pregnancies for COVID19 are those with an LMP date after January
# 1st 2021 and a delivery date before December 31st 2023
start_date <- as.Date("2021-08-01")
study_end <- as.Date("2023-12-31")

# Calculate gestational week 13 (LMP + 12 weeks + 1 day = 85 days)
pregnant_covid <- mbrn_eligible %>%
  mutate(
    # Calculate gestational week 13 (LMP + 12 weeks + 1 day = 85 days)
    gest_week_13 = LMP + days(85)
  ) %>%
  # FILTER FIRST - only include women who meet study criteria
  filter(
    # Group 1: Pregnant on Oct 1, 2016 and delivered after
    (LMP <= start_date & delivery > start_date) |
      # Group 2: Got pregnant after Oct 1, 2016 and delivered before Mar 31, 2019
      (LMP > start_date & delivery <= study_end)
  ) %>%
  # THEN calculate enrollment dates only for included women
  mutate(
    # Enrollment Date Variable 1: Based on LMP
    enrollment_risk = if_else(LMP <= start_date, start_date, LMP),
    
    # Enrollment Date Variable 2: Based on gestational week 13
    enrollment_norisk = if_else(gest_week_13 <= start_date, start_date, gest_week_13)
  )
save(pregnant_covid, file = "Pregnant_COVID_ELigibility_Risk_ToBeVerified.rdata")
# We need to check the previous COVID-19 diagnoses and tests as well as the vaccinations after assessing risk factors so that we can choose an enrollment date


