################################################################################
# Script: 7.1.Mice_SA.R
#
# Purpose:
#   Impute missing values in covariates needed for the primary analysis
#   of older adult and pregnant populations included in the influenza and
#   COVID-19 vaccine cohorts. This is done in the sensitivity analysis 
#   redefine the exposure assessment window to all avaialable time.
################################################################################
library(dplyr)
library(lubridate)
library(haven)
library(mice)
library(data.table)

# Since age was not properly captured, we need to fix it
load("Path_to_Pregnant_flu_risk_covariates.rdata")

# First we calculate the percent missingness
cov_preg_flu_risk <- pregnant_flu_risk[, c("Preg_id","age_at_enrollment_categorized", "county_2017", "country_of_birth", "smoking", "parity", "profession", "income_2017_cat","all.seasons", "mental" )]
cov_preg_flu_risk <- cov_preg_flu_risk %>% group_by(Preg_id) %>%
  mutate(age_at_enrollment_categorized = coalesce(age_at_enrollment_categorized, na.omit(age_at_enrollment_categorized)[1]),
         county_2017 = coalesce(county_2017, na.omit(county_2017)[1]),
         country_of_birth = coalesce(country_of_birth, na.omit(country_of_birth)[1]),
         smoking = coalesce(smoking, na.omit(smoking)[1]),
         income_2017_cat = coalesce(income_2017_cat, na.omit(income_2017_cat)[1]),
         all.seasons = max(all.seasons),
         mental = max(mental),
         parity = max(parity),
         profession = coalesce(profession, na.omit(profession)[1]))

cov_preg_flu_risk <- cov_preg_flu_risk[!duplicated(cov_preg_flu_risk$Preg_id),]
# fix age and MH
load("Path_to_Pregnant_Influenza_Risk_NoDuplicates.rdata")
cov_preg_flu_risk <- cov_preg_flu_risk[cov_preg_flu_risk$Preg_id %in% pregnant_flu_risk$Preg_id,]
cov_preg_flu_risk$mental <- pregnant_flu_risk$mental
rows_missing <- which(rowSums(is.na(cov_preg_flu_risk)) >= 1)
(length(rows_missing)/1878) * 100

colMeans(is.na(cov_preg_flu_risk))
### We need 45 imputations

load("Path_to_Pregnant_flu_norisk_covariates.rdata")

# First we calculate the percent missingness
cov_preg_flu_norisk <- pregnant_flu_norisk[, c("Preg_id","age_at_enrollment_categorized", "county_2017", "country_of_birth", "smoking", "parity", "profession", "income_2017_cat","all.seasons", "mental" )]
cov_preg_flu_norisk <- cov_preg_flu_norisk %>% group_by(Preg_id) %>%
  mutate(age_at_enrollment_categorized = coalesce(age_at_enrollment_categorized, na.omit(age_at_enrollment_categorized)[1]),
         county_2017 = coalesce(county_2017, na.omit(county_2017)[1]),
         country_of_birth = coalesce(country_of_birth, na.omit(country_of_birth)[1]),
         smoking = coalesce(smoking, na.omit(smoking)[1]),
         income_2017_cat = coalesce(income_2017_cat, na.omit(income_2017_cat)[1]),
         all.seasons = max(all.seasons),
         mental = max(mental),
         parity = max(parity),
         profession = coalesce(profession, na.omit(profession)[1]))

cov_preg_flu_norisk <- cov_preg_flu_norisk[!duplicated(cov_preg_flu_norisk$Preg_id),]
# Fix age and MH
load("Path_to_Pregnant_Influenza_NoRisk_NoDuplicates.rdata")
cov_preg_flu_norisk <- cov_preg_flu_norisk[cov_preg_flu_norisk$Preg_id %in% pregnant_flu_norisk$Preg_id,]
cov_preg_flu_norisk$mental <- pregnant_flu_norisk$mental

rows_missing <- which(rowSums(is.na(cov_preg_flu_norisk)) >= 1)
(length(rows_missing)/136060) * 100

colMeans(is.na(cov_preg_flu_norisk))
### We need 45 imputations


load("Path_to_Pregnant_covid_norisk_covariates.rdata")

# First we calculate the percent missingness
cov_preg_covid_norisk <- pregnant_covid_norisk[, c("Preg_id","age_at_enrollment_categorized", "county_2021", "country_of_birth", "smoking", "parity", "profession", "income_2021_cat","vaccinated", "mental" )]
cov_preg_covid_norisk <- cov_preg_covid_norisk %>% group_by(Preg_id) %>%
  mutate(age_at_enrollment_categorized = coalesce(age_at_enrollment_categorized, na.omit(age_at_enrollment_categorized)[1]),
         county_2021 = coalesce(county_2021, na.omit(county_2021)[1]),
         country_of_birth = coalesce(country_of_birth, na.omit(country_of_birth)[1]),
         smoking = coalesce(smoking, na.omit(smoking)[1]),
         income_2021_cat = coalesce(income_2021_cat, na.omit(income_2021_cat)[1]),
         vaccinated = max(vaccinated),
         mental = max(mental),
         parity = max(parity),
         profession = coalesce(profession, na.omit(profession)[1]))

cov_preg_covid_norisk <- cov_preg_covid_norisk[!duplicated(cov_preg_covid_norisk$Preg_id),]
cov_preg_covid_norisk <- cov_preg_covid_norisk[!is.na(cov_preg_covid_norisk$Preg_id),]
# Fix age and MH
load("Path_to_Pregnant_COVID_NoRisk_NoDuplicates.rdata")
cov_preg_covid_norisk <- cov_preg_covid_norisk[cov_preg_covid_norisk$Preg_id %in% pregnant_covid_norisk$Preg_id,]
cov_preg_covid_norisk$mental <- pregnant_covid_norisk$mental

rows_missing <- which(rowSums(is.na(cov_preg_covid_norisk)) >= 1)
(length(rows_missing)/99418) * 100

colMeans(is.na(cov_preg_covid_norisk))
### We need 45 imputations

load("Path_to_Pregnant_covid_risk_covariates.rdata")

# First we calculate the percent missingness
cov_preg_covid_risk <- pregnant_covid_risk[, c("Preg_id","age_at_enrollment_categorized", "county_2021", "country_of_birth", "smoking", "parity", "profession", "income_2021_cat","vaccinated", "mental" )]
cov_preg_covid_risk <- cov_preg_covid_risk %>% group_by(Preg_id) %>%
  mutate(age_at_enrollment_categorized = coalesce(age_at_enrollment_categorized, na.omit(age_at_enrollment_categorized)[1]),
         county_2021 = coalesce(county_2021, na.omit(county_2021)[1]),
         country_of_birth = coalesce(country_of_birth, na.omit(country_of_birth)[1]),
         smoking = coalesce(smoking, na.omit(smoking)[1]),
         income_2021_cat = coalesce(income_2021_cat, na.omit(income_2021_cat)[1]),
         vaccinated = max(vaccinated),
         mental = max(mental),
         parity = max(parity),
         profession = coalesce(profession, na.omit(profession)[1]))

cov_preg_covid_risk <- cov_preg_covid_risk[!duplicated(cov_preg_covid_risk$Preg_id),]
cov_preg_covid_risk <- cov_preg_covid_risk[!is.na(cov_preg_covid_risk$Preg_id),]

# Fix age and MH
load("Path_to_Pregnant_COVID_Risk_NoDuplicates.rdata")
cov_preg_covid_risk <- cov_preg_covid_risk[cov_preg_covid_risk$Preg_id %in% pregnant_covid_risk$Preg_id,]
cov_preg_covid_risk$mental <- pregnant_covid_risk$mental
rows_missing <- which(rowSums(is.na(cov_preg_covid_risk)) >= 1)
(length(rows_missing)/1342) * 100

colMeans(is.na(cov_preg_covid_risk))
### We need 45 imputations
##########################################################################################
# Now we make the matrix and impute
predictor_matrix <- matrix(0, nrow = ncol(cov_preg_covid_risk), ncol = ncol(cov_preg_covid_risk), dimnames = list(names(cov_preg_covid_risk), names(cov_preg_covid_risk)))

predictor_matrix[,"age_at_enrollment_categorized"]<- 1
predictor_matrix[,"parity"]<- 1
predictor_matrix[,"county_2021"]<- 1
predictor_matrix[,"country_of_birth"]<- 1
predictor_matrix[,"vaccinated"]<- 1
predictor_matrix[,"mental"]<- 1

cov_preg_covid_risk$parity <- as.factor(cov_preg_covid_risk$parity)
cov_preg_covid_risk$age_at_enrollment_categorized <- as.factor(cov_preg_covid_risk$age_at_enrollment_categorized)
cov_preg_covid_risk$county_2021 <- as.factor(cov_preg_covid_risk$county_2021)
cov_preg_covid_risk$country_of_birth <- as.factor(cov_preg_covid_risk$country_of_birth)
cov_preg_covid_risk$vaccinated <- as.factor(cov_preg_covid_risk$vaccinated)
cov_preg_covid_risk$mental <- as.factor(cov_preg_covid_risk$mental)
cov_preg_covid_risk$smoking <- as.factor(cov_preg_covid_risk$smoking)
cov_preg_covid_risk$profession <- as.factor(cov_preg_covid_risk$profession)
cov_preg_covid_risk$income_2021_cat <- as.factor(cov_preg_covid_risk$income_2021_cat)

method <- make.method(cov_preg_covid_risk) 


imp_preg_covid_risk <- mice(cov_preg_covid_risk, m= 45, maxit = 1, method = method, predictorMatrix = predictor_matrix, seed = 123)

saveRDS(imp_preg_covid_risk, file = "Cov_Imputed_Pregnancy_Covid_Risk_SA.rds")

predictor_matrix <- matrix(0, nrow = ncol(cov_preg_covid_norisk), ncol = ncol(cov_preg_covid_norisk), dimnames = list(names(cov_preg_covid_norisk), names(cov_preg_covid_norisk)))

predictor_matrix[,"age_at_enrollment_categorized"]<- 1
predictor_matrix[,"parity"]<- 1
predictor_matrix[,"vaccinated"]<- 1
predictor_matrix[,"mental"]<- 1

cov_preg_covid_norisk$parity <- as.factor(cov_preg_covid_norisk$parity)
cov_preg_covid_norisk$age_at_enrollment_categorized <- as.factor(cov_preg_covid_norisk$age_at_enrollment_categorized)
cov_preg_covid_norisk$county_2021 <- as.factor(cov_preg_covid_norisk$county_2021)
cov_preg_covid_norisk$country_of_birth <- as.factor(cov_preg_covid_norisk$country_of_birth)
cov_preg_covid_norisk$vaccinated <- as.factor(cov_preg_covid_norisk$vaccinated)
cov_preg_covid_norisk$mental <- as.factor(cov_preg_covid_norisk$mental)
cov_preg_covid_norisk$smoking <- as.factor(cov_preg_covid_norisk$smoking)
cov_preg_covid_norisk$profession <- as.factor(cov_preg_covid_norisk$profession)
cov_preg_covid_norisk$income_2021_cat <- as.factor(cov_preg_covid_norisk$income_2021_cat)

method <- make.method(cov_preg_covid_norisk) 


imp_preg_covid_norisk <- mice(cov_preg_covid_norisk, m= 45, maxit = 1, method = method, predictorMatrix = predictor_matrix, seed = 123)

saveRDS(imp_preg_covid_norisk, file = "Cov_Imputed_Pregnancy_Covid_Norisk_SA.rds")


predictor_matrix <- matrix(0, nrow = ncol(cov_preg_flu_norisk), ncol = ncol(cov_preg_flu_norisk), dimnames = list(names(cov_preg_flu_norisk), names(cov_preg_flu_norisk)))

predictor_matrix[,"age_at_enrollment_categorized"]<- 1
predictor_matrix[,"parity"]<- 1
predictor_matrix[,"all.seasons"]<- 1
predictor_matrix[,"mental"]<- 1

cov_preg_flu_norisk$parity <- as.factor(cov_preg_flu_norisk$parity)
cov_preg_flu_norisk$age_at_enrollment_categorized <- as.factor(cov_preg_flu_norisk$age_at_enrollment_categorized)
cov_preg_flu_norisk$county_2017 <- as.factor(cov_preg_flu_norisk$county_2017)
cov_preg_flu_norisk$country_of_birth <- as.factor(cov_preg_flu_norisk$country_of_birth)
cov_preg_flu_norisk$all.seasons <- as.factor(cov_preg_flu_norisk$all.seasons)
cov_preg_flu_norisk$mental <- as.factor(cov_preg_flu_norisk$mental)
cov_preg_flu_norisk$smoking <- as.factor(cov_preg_flu_norisk$smoking)
cov_preg_flu_norisk$profession <- as.factor(cov_preg_flu_norisk$profession)
cov_preg_flu_norisk$income_2017_cat <- as.factor(cov_preg_flu_norisk$income_2017_cat)

method <- make.method(cov_preg_flu_norisk) 


imp_preg_flu_norisk <- mice(cov_preg_flu_norisk, m= 45, maxit = 1, method = method, predictorMatrix = predictor_matrix, seed = 123)

saveRDS(imp_preg_flu_norisk, file = "Cov_Imputed_Pregnancy_Influenza_Norisk_SA.rds")


predictor_matrix <- matrix(0, nrow = ncol(cov_preg_flu_risk), ncol = ncol(cov_preg_flu_risk), dimnames = list(names(cov_preg_flu_risk), names(cov_preg_flu_risk)))

predictor_matrix[,"age_at_enrollment_categorized"]<- 1
predictor_matrix[,"parity"]<- 1
predictor_matrix[,"all.seasons"]<- 1
predictor_matrix[,"mental"]<- 1

cov_preg_flu_risk$parity <- as.factor(cov_preg_flu_risk$parity)
cov_preg_flu_risk$age_at_enrollment_categorized <- as.factor(cov_preg_flu_risk$age_at_enrollment_categorized)
cov_preg_flu_risk$county_2017 <- as.factor(cov_preg_flu_risk$county_2017)
cov_preg_flu_risk$country_of_birth <- as.factor(cov_preg_flu_risk$country_of_birth)
cov_preg_flu_risk$all.seasons <- as.factor(cov_preg_flu_risk$all.seasons)
cov_preg_flu_risk$mental <- as.factor(cov_preg_flu_risk$mental)
cov_preg_flu_risk$smoking <- as.factor(cov_preg_flu_risk$smoking)
cov_preg_flu_risk$profession <- as.factor(cov_preg_flu_risk$profession)
cov_preg_flu_risk$income_2017_cat <- as.factor(cov_preg_flu_risk$income_2017_cat)

method <- make.method(cov_preg_flu_risk) 


imp_preg_flu_risk <- mice(cov_preg_flu_risk, m= 45, maxit = 1, method = method, predictorMatrix = predictor_matrix, seed = 123)

saveRDS(imp_preg_flu_risk, file = "Cov_Imputed_Pregnancy_Influenza_risk_SA.rds")

load("Path_to_Scripts/Old_covid_Covariates.rdata")
old_covid$any_mh <- 0
old_covid$any_mh[old_covid$depression_mh == 1 | old_covid$aniety_mh == 1 | old_covid$bipolar_mh == 1 | old_covid$PTSD_mh == 1 | old_covid$OCD_mh == 1 | old_covid$ADHD_mh == 1] <- 1

# First we calculate the percent missingness
cov_old_covid <- old_covid[, c("person_id","age_at_enrollment_categorized", "county_2021", "country_of_birth", "risk_factor", "income_2021_cat","all.seasons")]

old_covid <- old_covid %>% group_by(person_id) %>%
  mutate(age_at_enrollment_categorized = coalesce(age_at_enrollment_categorized, na.omit(age_at_enrollment_categorized)[1]),
         county_2021 = coalesce(county_2021, na.omit(county_2021)[1]),
         country_of_birth = coalesce(country_of_birth, na.omit(country_of_birth)[1]),
         risk_factor = max(risk_factor),
         income_2021_cat = coalesce(income_2021_cat, na.omit(income_2021_cat)[1]),
         all.seasons = max(all.seasons))

old_covid <- old_covid[!duplicated(old_covid$person_id),]
cov_old_covid <- old_covid[!is.na(old_covid$person_id),]
cov_old_covid$risk_factor[is.na(cov_old_covid$risk_factor)] <- 0

rows_missing <- which(rowSums(is.na(cov_old_covid)) >= 1)
(length(rows_missing)/552756) * 100

colMeans(is.na(cov_old_covid))

predictor_matrix <- matrix(0, nrow = ncol(cov_old_covid), ncol = ncol(cov_old_covid), dimnames = list(names(cov_old_covid), names(cov_old_covid)))

predictor_matrix[,"age_at_enrollment_categorized"]<- 1
predictor_matrix[,"risk_factor"]<- 1
predictor_matrix[,"all.seasons"]<- 1
predictor_matrix[,"mental"]<- 1

cov_old_covid$age_at_enrollment_categorized <- as.factor(cov_old_covid$age_at_enrollment_categorized)
cov_old_covid$county_2021 <- as.factor(cov_old_covid$county_2021)
cov_old_covid$country_of_birth <- as.factor(cov_old_covid$country_of_birth)
cov_old_covid$all.seasons <- as.factor(cov_old_covid$all.seasons)
cov_old_covid$mental <- as.factor(cov_old_covid$mental)
cov_old_covid$risk_factor <- as.factor(cov_old_covid$risk_factor)
cov_old_covid$income_2021_cat <- as.factor(cov_old_covid$income_2021_cat)

method <- make.method(cov_old_covid) 


imp_old_covid <- mice(cov_old_covid, m= 30, maxit = 1, method = method, predictorMatrix = predictor_matrix, seed = 123)

saveRDS(imp_old_covid, file = "Cov_Imputed_Old_Covid_SA.rds")

load("Path_to_Old_flu_Covariates.rdata")
old_flu$any_mh <- 0
old_flu$any_mh[old_flu$depression_mh == 1 | old_flu$aniety_mh == 1 | old_flu$bipolar_mh == 1 | old_flu$PTSD_mh == 1 | old_flu$OCD_mh == 1 | old_flu$ADHD_mh == 1] <- 1

# First we calculate the percent missingness
cov_old_flu <- old_flu[, c("person_id","age_at_enrollment_categorized", "county_2017", "country_of_birth", "risk_factor", "income_2017_cat","all.seasons", "any_mh")]

cov_old_flu <- cov_old_flu %>% group_by(person_id) %>%
  mutate(age_at_enrollment_categorized = coalesce(age_at_enrollment_categorized, na.omit(age_at_enrollment_categorized)[1]),
         county_2017 = coalesce(county_2017, na.omit(county_2017)[1]),
         country_of_birth = coalesce(country_of_birth, na.omit(country_of_birth)[1]),
         risk_factor = max(risk_factor),
         income_2017_cat = coalesce(income_2017_cat, na.omit(income_2017_cat)[1]),
         all.seasons = max(all.seasons),
         any_mh = max(any_mh))

cov_old_flu <- cov_old_flu[!duplicated(cov_old_flu$person_id),]
cov_old_flu <- cov_old_flu[!is.na(cov_old_flu$person_id),]
cov_old_flu$risk_factor[is.na(cov_old_flu$risk_factor)] <- 0

rows_missing <- which(rowSums(is.na(cov_old_flu)) >= 1)
(length(rows_missing)/514029) * 100

colMeans(is.na(cov_old_flu))

predictor_matrix <- matrix(0, nrow = ncol(cov_old_flu), ncol = ncol(cov_old_flu), dimnames = list(names(cov_old_flu), names(cov_old_flu)))

predictor_matrix[,"age_at_enrollment_categorized"]<- 1
predictor_matrix[,"risk_factor"]<- 1
predictor_matrix[,"all.seasons"]<- 1
predictor_matrix[,"Any_MH"]<- 1

cov_old_flu$age_at_enrollment_categorized <- as.factor(cov_old_flu$age_at_enrollment_categorized)
cov_old_flu$county_2017 <- as.factor(cov_old_flu$county_2017)
cov_old_flu$country_of_birth <- as.factor(cov_old_flu$country_of_birth)
cov_old_flu$all.seasons <- as.factor(cov_old_flu$all.seasons)
cov_old_flu$Any_MH <- as.factor(cov_old_flu$Any_MH)
cov_old_flu$risk_factor <- as.factor(cov_old_flu$risk_factor)
cov_old_flu$income_2017_cat <- as.factor(cov_old_flu$income_2017_cat)

method <- make.method(cov_old_flu) 


imp_old_flu <- mice(cov_old_flu, m= 30, maxit = 1, method = method, predictorMatrix = predictor_matrix, seed = 123)

saveRDS(imp_old_flu, file = "Cov_Imputed_Old_Influenza_SA.rds")

