library(dplyr)
library(lubridate)
library(haven)
library(readr)

################################################################
###################Covariates for Old Adults####################
################################################################
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Old_COVID_Population_MH_corrected.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Old_Influenza_Population_MH_corrected.rdata")
old_covid$Any_MH <- ifelse(old_covid$depression_mh == 1| old_covid$anxiety_mh == 1| old_covid$bipolar_mh ==1|
                             old_covid$PTSD_mh == 1| old_covid$OCD_mh == 1| old_covid$ADHD_mh == 1, 1, 0)
old_flu$Any_MH <- ifelse(old_flu$depression_mh == 1| old_flu$anxiety_mh == 1| old_flu$bipolar_mh ==1|
                             old_flu$PTSD_mh == 1| old_flu$OCD_mh == 1| old_flu$ADHD_mh == 1, 1, 0)

# Age at enrollment
old_covid$age_at_enrollment <- interval(old_covid$birth_date, old_covid$enrollment_date) %>%
  as.period()
old_covid$age_at_enrollment <- year(old_covid$age_at_enrollment)
old_covid$age_at_enrollment_categorized <- NA
old_covid$age_at_enrollment_categorized[old_covid$age_at_enrollment >= 65 & old_covid$age_at_enrollment <= 70] <- "65-70"
old_covid$age_at_enrollment_categorized[old_covid$age_at_enrollment >= 71 & old_covid$age_at_enrollment <= 75] <- "71-75"
old_covid$age_at_enrollment_categorized[old_covid$age_at_enrollment >= 76 & old_covid$age_at_enrollment <= 80] <- "76-80"
old_covid$age_at_enrollment_categorized[old_covid$age_at_enrollment >= 81] <- ">81"
############################
old_flu$age_at_enrollment <- interval(old_flu$birth_date, old_flu$enrollment_date) %>%
  as.period()
old_flu$age_at_enrollment <- year(old_flu$age_at_enrollment)
old_flu$age_at_enrollment_categorized <- NA
old_flu$age_at_enrollment_categorized[old_flu$age_at_enrollment >= 65 & old_flu$age_at_enrollment <= 70] <- "65-70"
old_flu$age_at_enrollment_categorized[old_flu$age_at_enrollment >= 71 & old_flu$age_at_enrollment <= 75] <- "71-75"
old_flu$age_at_enrollment_categorized[old_flu$age_at_enrollment >= 76 & old_flu$age_at_enrollment <= 80] <- "76-80"
old_flu$age_at_enrollment_categorized[old_flu$age_at_enrollment >= 81] <- ">81"

################################################################
# County 
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_bostedsfylke.sas7bdat")
SSB$county_2017 <- NA
SSB$county_2021 <- NA
SSB$county_2017[SSB$bostedsfylke_2017 == "01"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "02"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "03"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "04"] <- "Central"
SSB$county_2017[SSB$bostedsfylke_2017 == "05"] <- "Central"
SSB$county_2017[SSB$bostedsfylke_2017 == "06"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "07"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "08"] <- "Southern"
SSB$county_2017[SSB$bostedsfylke_2017 == "09"] <- "Southern"
SSB$county_2017[SSB$bostedsfylke_2017 == "10"] <- "Southern"
SSB$county_2017[SSB$bostedsfylke_2017 == "11"] <- "Southern"
SSB$county_2017[SSB$bostedsfylke_2017 == "12"] <- "Western"
SSB$county_2017[SSB$bostedsfylke_2017 == "14"] <- "Western"
SSB$county_2017[SSB$bostedsfylke_2017 == "15"] <- "Western"
SSB$county_2017[SSB$bostedsfylke_2017 == "18"] <- "Central"
SSB$county_2017[SSB$bostedsfylke_2017 == "50"] <- "Central"
SSB$county_2017[SSB$bostedsfylke_2017 == "19"] <- "Northern"
SSB$county_2017[SSB$bostedsfylke_2017 == "20"] <- "Northern"

SSB$county_2021[SSB$bostedsfylke_2021 == "03"] <- "Eastern"
SSB$county_2021[SSB$bostedsfylke_2021 == "11"] <- "Southern"
SSB$county_2021[SSB$bostedsfylke_2021 == "15"] <- "Western"
SSB$county_2021[SSB$bostedsfylke_2021 == "18"] <- "Central"
SSB$county_2021[SSB$bostedsfylke_2021 == "30"] <- "Eastern"
SSB$county_2021[SSB$bostedsfylke_2021 == "34"] <- "Central"
SSB$county_2021[SSB$bostedsfylke_2021 == "38"] <- "Eastern"
SSB$county_2021[SSB$bostedsfylke_2021 == "42"] <- "Southern"
SSB$county_2021[SSB$bostedsfylke_2021 == "46"] <- "Western"
SSB$county_2021[SSB$bostedsfylke_2021 == "50"] <- "Central"
SSB$county_2021[SSB$bostedsfylke_2021 == "54"] <- "Northern"

SSB_old_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% old_flu$person_id,]
SSB_old_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% old_covid$person_id,]
SSB_old_flu <- SSB_old_flu[, c("KOBLINGSNOEKKEL", "county_2017")]
SSB_old_covid <- SSB_old_covid[, c("KOBLINGSNOEKKEL", "county_2021")]
colnames(SSB_old_covid)[1] <- "person_id"
colnames(SSB_old_flu)[1] <- "person_id"
old_covid <- merge(old_covid, SSB_old_covid, all = T)
old_flu <- merge(old_flu, SSB_old_flu, all = T)
################################################################
# Country of birth
Norwegian <- "000"
West_North_Central <- c("101", "102", "103", "104", "105", "106", "112","114",'117','121','127','128','129','130','139','141','144',
                        '153', '162','163', '164')
Southern <- c('118','119','123','126','132','134','137','154')
Eastern <- c("111","113", '115','120','122','124','130','133','136','138','140','146','148','152','155','156','157','158','159','160',
             '161')
MENA <- c('143','203','249','286','303','304','306','356','379','409','426','452','456','460','476','496','500','508','520','524','540'
          ,'544','564','578')
Central_East_Asia <- c('404','406','407','412','430','432','436','464','480','484','488','492','502','510','516','550','552','554')
South_Asian <- c('213','410','416','420','424','428','444','448','478','504','512','513','528','534','537','548','568','575','808')
Latin_America <- c('601','602','603','604','606','608','613','616','620','622','624','629', '631','632','636','644','648','650','652',
                   '654','657','658','659','660','661','664','668','672','676','677','678','679','680','681','685','686','687','705',
                   '710','715','720','725','730','735','740','745','755','760','765','770','775')
North_America <- c('612','684')
Australian <- c('802','805','806','807','808','809','811','812','813','814','815','816','817','818','819','820','821','826','827','833')
African <- c('204', '205','209','216','220','229','235','239','241','246','250','254','256','260','264','266','270','273','278',
             '279','281','283','289','296','299','307','308','309','313','319','322','323','326','329','333','336','337',
             '338','339','346','357','359','369','373','376','386','389','393')

old_covid$country_of_birth <- NA
old_covid$country_of_birth[old_covid$fodeland == "000" ] <- "Norwegian"
old_covid$country_of_birth[old_covid$fodeland %in%  West_North_Central] <- "Western/ Northern/ Central European"
old_covid$country_of_birth[old_covid$fodeland %in%  Southern] <- "Southern European"
old_covid$country_of_birth[old_covid$fodeland %in%  Eastern] <- "Eastern European"
old_covid$country_of_birth[old_covid$fodeland %in%  MENA] <- "Middle Eastern & North African"
old_covid$country_of_birth[old_covid$fodeland %in%  Central_East_Asia] <- "Central/ East Asian"
old_covid$country_of_birth[old_covid$fodeland %in%  South_Asian] <- "South Asian"
old_covid$country_of_birth[old_covid$fodeland %in%  African] <- "African"
old_covid$country_of_birth[old_covid$fodeland %in%  Australian] <- "Oceanian"
old_covid$country_of_birth[old_covid$fodeland %in%  North_America] <- "North American"
old_covid$country_of_birth[old_covid$fodeland %in%  Latin_America] <- "Latin American"
########################
old_flu$country_of_birth <- NA
old_flu$country_of_birth[old_flu$fodeland == "000" ] <- "Norwegian"
old_flu$country_of_birth[old_flu$fodeland %in%  West_North_Central] <- "Western/ Northern/ Central European"
old_flu$country_of_birth[old_flu$fodeland %in%  Southern] <- "Southern European"
old_flu$country_of_birth[old_flu$fodeland %in%  Eastern] <- "Eastern European"
old_flu$country_of_birth[old_flu$fodeland %in%  MENA] <- "Middle Eastern & North African"
old_flu$country_of_birth[old_flu$fodeland %in%  Central_East_Asia] <- "Central/ East Asian"
old_flu$country_of_birth[old_flu$fodeland %in%  South_Asian] <- "South Asian"
old_flu$country_of_birth[old_flu$fodeland %in%  African] <- "African"
old_flu$country_of_birth[old_flu$fodeland %in%  Australian] <- "Oceanian"
old_flu$country_of_birth[old_flu$fodeland %in%  North_America] <- "North American"
old_flu$country_of_birth[old_flu$fodeland %in%  Latin_America] <- "Latin American"
###################################################################################

# COVID-19 risk factors
# Let's start by defining the risk factors

transplantation_ICD10 <- c("Z940", "Z941", "Z942", "Z943", "Z944", "Z948")
transplantation_NCMP <- c("KAS10", "KAS11", "KAS20", "KAS21", "JLE00", "JLE03", "JLE10", "JLE16", "JJC00", "JJC10", "JJC20")

neurology_ICD10 <- c("G1" ,"G20", "G21", "G23", "G24", "G405", "G610", "G70", "G71", "G800", "G802", "G803", "F72", "F73", "F840", "F841", "Q050", "Q051", "Q052", "Q053", "Q054", "Q055", "Q056", "Q90")

kidney_failure_ICD10 <- c("N183", "N184", "N185")
kidney_failure_NCMP <- c("JAGD30", "JAGD31")

liver_failure_ICD10 <- c("K704", "K72")

Diabetes_ICD10 <- c("E10,E11,E12,E13,E14")
Diabetes_ICPC <- c("T89", "T90")

Chronic_lung_disease_ICD10 <- c("J41", "J42", "J43", "J44", "J45", "J46", "J47", "J84", "J98", "E84")
Chronic_lung_disease_ICPC <- c("R95", "R96")

Obesity_ICD10 <- "E66"
Obesity_ICPC <- "T82"

Hematological_cancer_ICD10 <- c("C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90", "C91", "C92", "C93", "C94", "C95", "C96", "D45", "D45", "D47")
Hematological_cancer_NCMP <- "AAG"

Immunodeficiency_ICD10 <- c("D80", "D81", "D82", "D83", "D84")

Heart_disease_ICD10 <- c("I05", "I06", "I07", "I08", "I09", "I2", "I31", "I32", "I34", "I35", "I36", "I37", "I39", "I40", "I41", "I42", "I43", "I46", "I48", "I49", "I50")
Heart_disease_ICPC <- c("K74", "K75", "K76", "K77", "K78", "K82", "K83", "K87")


Stroke_ICD10 <- c("I60", "I61", "I62", "I63", "I64", "I69.1", "I69.2", "I69.3", "I69.4", "I69.8", "I69.0")
Stroke_ICPC <- c("K90", "K91")

Dementia_ICD10 <- c("F0", "G30", "G31")
Dementia_ICPC <- "P70"


Other_Active_Cancer_ICD10 <- c("C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C80")
Other_Active_Cancer_NCMP <- c("WEOA", "WEOB", "WEOC", "WBOC", "WBGM", "RAGG")

Impaired_immunity_ICD10 <- c("G35", "M05", "M08", "M06", "M07", "M09", "M13", "M14", "K50", "K51")

NPR <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_NPR_SOM.csv", 
                  delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))

NPR_flu <- NPR[NPR$person_id %in% old_flu$person_id, ]
NPR_flu1 <- NPR_flu[NPR_flu$event_code %in% transplantation_ICD10| NPR_flu$event_code %in% Chronic_lung_disease_ICD10| NPR_flu$event_code %in% Dementia_ICD10|
                      NPR_flu$event_code %in% Diabetes_ICD10 | NPR_flu$event_code %in% Heart_disease_ICD10 | NPR_flu$event_code %in% Hematological_cancer_ICD10|
                      NPR_flu$event_code %in% Immunodeficiency_ICD10| NPR_flu$event_code %in% Impaired_immunity_ICD10 | NPR_flu$event_code %in% kidney_failure_ICD10|
                      NPR_flu$event_code %in% liver_failure_ICD10| NPR_flu$event_code %in% neurology_ICD10| NPR_flu$event_code %in% Obesity_ICD10|
                      NPR_flu$event_code %in% Other_Active_Cancer_ICD10 | NPR_flu$event_code %in% Stroke_ICD10,]

merged <- merge(old_flu, NPR_flu1, all = T)
merged$risk_factor <- 0
merged$lung_disease <- 0
merged$Dementia <- 0
merged$diabetes <- 0
merged$heart_disease <- 0
merged$hematological_cancer <- 0
merged$Immunodeficiency <- 0
merged$Impaired_immunity <- 0
merged$kidney_failure <- 0
merged$liver_failure <- 0
merged$neurology <- 0
merged$obesity <- 0
merged$cancer <- 0 
merged$stroke <- 0
merged$transplantation <- 0

merged$lung_disease[merged$event_code %in% Chronic_lung_disease_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$Dementia[merged$event_code %in% Dementia_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$diabetes[merged$event_code %in% Diabetes_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$heart_disease[merged$event_code %in% Heart_disease_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$hematological_cancer[merged$event_code %in% Hematological_cancer_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$Immunodeficiency[merged$event_code %in% Immunodeficiency_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$Impaired_immunity[merged$event_code %in% Impaired_immunity_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$kidney_failure[merged$event_code %in% kidney_failure_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$liver_failure[merged$event_code %in% liver_failure_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$neurology[merged$event_code %in% neurology_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$obesity[merged$event_code %in% Obesity_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$cancer[merged$event_code %in% Other_Active_Cancer_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$stroke[merged$event_code %in% Stroke_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$transplantation[merged$event_code %in% transplantation_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1

merged <- merged %>% group_by(person_id) %>%
  mutate(lung_disease_final = max(lung_disease),
         Dementia_final = max(Dementia),
         diabetes_final = max(diabetes),
         heart_disease_final = max(heart_disease), 
         hematological_cancer_final = max(hematological_cancer),
         Immunodeficiency_final = max(Immunodeficiency),
         Impaired_immunity_final = max(Impaired_immunity),
         kidney_failure_final = max(kidney_failure),
         liver_failure_final = max(liver_failure),
         neurology_final = max(neurology),
         obesity_final = max(obesity),
         cancer_final = max(cancer),
         stroke_final = max(stroke),
         transplantation_final = max(transplantation))
merged <- merged[!duplicated(merged$person_id),]
merged$risk_factor[merged$lung_disease_final ==1 | merged$Dementia_final ==1 |merged$diabetes_final ==1 |merged$heart_disease_final ==1 |merged$hematological_cancer_final ==1 |
                     merged$Immunodeficiency_final ==1 |merged$Impaired_immunity_final ==1 |merged$kidney_failure_final ==1 |merged$liver_failure_final ==1 |merged$neurology_final ==1 |
                     merged$obesity_final ==1 |merged$cancer_final ==1 |merged$stroke_final ==1 |merged$transplantation_final ==1 ] <- 1

# Now we need to check KUHR for risk factors and procedures from NPR
KUHR_2015 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2015.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2015 <- KUHR_2015[KUHR_2015$person_id %in% old_flu$person_id,]
KUHR_2015 <- KUHR_2015[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2015$event_code, ignore.case = T), ]
gc()
KUHR_2016 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2016.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2016 <- KUHR_2016[KUHR_2016$person_id %in% old_flu$person_id,]
KUHR_2016 <- KUHR_2016[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2016$event_code, ignore.case = T), ]
gc()
KUHR_2017 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2017.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2017 <- KUHR_2017[KUHR_2017$person_id %in% old_flu$person_id,]
KUHR_2017 <- KUHR_2017[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2017$event_code, ignore.case = T), ]
gc()
KUHR_2018 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2018.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2018 <- KUHR_2018[KUHR_2018$person_id %in% old_flu$person_id,]
KUHR_2018 <- KUHR_2018[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2018$event_code, ignore.case = T), ]
gc()
KUHR_2019 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2019.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2019 <- KUHR_2019[KUHR_2019$person_id %in% old_flu$person_id,]
KUHR_2019 <- KUHR_2019[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2019$event_code, ignore.case = T), ]
gc()
KUHR <- merge(KUHR_2015, KUHR_2016, all = T)
KUHR <- merge(KUHR, KUHR_2017, all = T)
KUHR <- merge(KUHR, KUHR_2018, all = T)
KUHR <- merge(KUHR, KUHR_2019, all = T)

merged1 <- merge(merged, KUHR, all = T)
merged1$lung_disease[grepl("R95|R96", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$Dementia[grepl("P70", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$diabetes[grepl("T89|T90", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$heart_disease[grepl("K74|K75|K76|K77|K78|K82|K83|K87", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$obesity[grepl("T82", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$stroke[grepl("K91|K90", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged2 <- merged1[,c("person_id", "risk_factor")]
old_flu <- merge(old_flu, merged2, all = T)
save(old_covid, file = "Old_covid_Covariates.rdata")

NPR <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_NPR_SOM.csv", 
                  delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))

NPR_covid <- NPR[NPR$person_id %in% old_covid$person_id, ]
NPR_covid1 <- NPR_covid[NPR_covid$event_code %in% transplantation_ICD10| NPR_covid$event_code %in% Chronic_lung_disease_ICD10| NPR_covid$event_code %in% Dementia_ICD10|
                      NPR_covid$event_code %in% Diabetes_ICD10 | NPR_covid$event_code %in% Heart_disease_ICD10 | NPR_covid$event_code %in% Hematological_cancer_ICD10|
                      NPR_covid$event_code %in% Immunodeficiency_ICD10| NPR_covid$event_code %in% Impaired_immunity_ICD10 | NPR_covid$event_code %in% kidney_failure_ICD10|
                      NPR_covid$event_code %in% liver_failure_ICD10| NPR_covid$event_code %in% neurology_ICD10| NPR_covid$event_code %in% Obesity_ICD10|
                      NPR_covid$event_code %in% Other_Active_Cancer_ICD10 | NPR_covid$event_code %in% Stroke_ICD10,]

merged <- merge(old_covid, NPR_covid1, all = T)
merged$risk_factor <- 0
merged$lung_disease <- 0
merged$Dementia <- 0
merged$diabetes <- 0
merged$heart_disease <- 0
merged$hematological_cancer <- 0
merged$Immunodeficiency <- 0
merged$Impaired_immunity <- 0
merged$kidney_failure <- 0
merged$liver_failure <- 0
merged$neurology <- 0
merged$obesity <- 0
merged$cancer <- 0 
merged$stroke <- 0
merged$transplantation <- 0

merged$lung_disease[merged$event_code %in% Chronic_lung_disease_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$Dementia[merged$event_code %in% Dementia_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$diabetes[merged$event_code %in% Diabetes_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$heart_disease[merged$event_code %in% Heart_disease_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$hematological_cancer[merged$event_code %in% Hematological_cancer_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$Immunodeficiency[merged$event_code %in% Immunodeficiency_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$Impaired_immunity[merged$event_code %in% Impaired_immunity_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$kidney_failure[merged$event_code %in% kidney_failure_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$liver_failure[merged$event_code %in% liver_failure_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$neurology[merged$event_code %in% neurology_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$obesity[merged$event_code %in% Obesity_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$cancer[merged$event_code %in% Other_Active_Cancer_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$stroke[merged$event_code %in% Stroke_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1
merged$transplantation[merged$event_code %in% transplantation_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_date & ymd(merged$start_date_record) >= (merged$enrollment_date - years(1))] <- 1

merged <- merged %>% group_by(person_id) %>%
  mutate(lung_disease_final = max(lung_disease),
         Dementia_final = max(Dementia),
         diabetes_final = max(diabetes),
         heart_disease_final = max(heart_disease), 
         hematological_cancer_final = max(hematological_cancer),
         Immunodeficiency_final = max(Immunodeficiency),
         Impaired_immunity_final = max(Impaired_immunity),
         kidney_failure_final = max(kidney_failure),
         liver_failure_final = max(liver_failure),
         neurology_final = max(neurology),
         obesity_final = max(obesity),
         cancer_final = max(cancer),
         stroke_final = max(stroke),
         transplantation_final = max(transplantation))
merged <- merged[!duplicated(merged$person_id),]
merged$risk_factor[merged$lung_disease_final ==1 | merged$Dementia_final ==1 |merged$diabetes_final ==1 |merged$heart_disease_final ==1 |merged$hematological_cancer_final ==1 |
                     merged$Immunodeficiency_final ==1 |merged$Impaired_immunity_final ==1 |merged$kidney_failure_final ==1 |merged$liver_failure_final ==1 |merged$neurology_final ==1 |
                     merged$obesity_final ==1 |merged$cancer_final ==1 |merged$stroke_final ==1 |merged$transplantation_final ==1 ] <- 1

# Now we need to check KUHR for risk factors and procedures from NPR
KUHR_2020 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2020.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2020 <- KUHR_2020[KUHR_2020$person_id %in% old_flu$person_id,]
KUHR_2020 <- KUHR_2020[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2020$event_code, ignore.case = T), ]
gc()
KUHR_2021 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2021.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2021 <- KUHR_2021[KUHR_2021$person_id %in% old_flu$person_id,]
KUHR_2021 <- KUHR_2021[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2021$event_code, ignore.case = T), ]
gc()
KUHR_2022 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2022.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2022 <- KUHR_2022[KUHR_2022$person_id %in% old_flu$person_id,]
KUHR_2022 <- KUHR_2022[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2022$event_code, ignore.case = T), ]
gc()
KUHR_2023 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2023.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2023 <- KUHR_2023[KUHR_2023$person_id %in% old_flu$person_id,]
KUHR_2023 <- KUHR_2023[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2023$event_code, ignore.case = T), ]
gc()
KUHR <- merge(KUHR_2020, KUHR_2021, all = T)
KUHR <- merge(KUHR, KUHR_2022, all = T)
KUHR <- merge(KUHR, KUHR_2023, all = T)

merged1 <- merge(merged, KUHR, all = T)
merged1$lung_disease[grepl("R95|R96", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$Dementia[grepl("P70", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$diabetes[grepl("T89|T90", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$heart_disease[grepl("K74|K75|K76|K77|K78|K82|K83|K87", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$obesity[grepl("T82", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$stroke[grepl("K91|K90", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_date & ymd(merged1$start_date_record) >= (merged1$enrollment_date - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged2 <- merged1[,c("person_id", "risk_factor")]
old_covid <- merge(old_covid, merged2, all = T)
save(old_covid, file = "Old_covid_Covariates.rdata")
###################################################################################
# Income
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_inntekt.sas7bdat")
SSB$income_2017 <- NA
SSB$income_2021 <- NA
SSB$income_2017[SSB$aargang == "2017"] <- SSB$wlonn[SSB$aargang == "2017"]
SSB$income_2021[SSB$aargang == "2021"] <- SSB$wlonn[SSB$aargang == "2021"]
SSB$income_2017_cat <- NA
SSB$income_2021_cat <- NA
SSB$income_2017_cat[SSB$income_2017 <= 399999] <- "Low"
SSB$income_2017_cat[SSB$income_2017 > 399999 & SSB$income_2017 <= 899998] <- "Medium"
SSB$income_2017_cat[SSB$income_2017 >= 899999] <- "High"
SSB$income_2021_cat[SSB$income_2021 <= 399999] <- "Low"
SSB$income_2021_cat[SSB$income_2021 > 399999 & SSB$income_2017 <= 899998] <- "Medium"
SSB$income_2021_cat[SSB$income_2021 >= 899999] <- "High"
SSB_old_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% old_flu$person_id,]
SSB_old_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% old_covid$person_id,]
SSB_old_flu <- SSB_old_flu[, c("KOBLINGSNOEKKEL", "income_2017_cat")]
SSB_old_covid <- SSB_old_covid[, c("KOBLINGSNOEKKEL", "income_2021_cat")]
colnames(SSB_old_covid)[1] <- "person_id"
colnames(SSB_old_flu)[1] <- "person_id"
old_covid <- merge(old_covid, SSB_old_covid, all = T)
old_flu <- merge(old_flu, SSB_old_flu, all = T)
save(old_flu, file = "Old_flu_Covariates.rdata")
######################################################################################
#############################Covariates for pregnancy#################################
######################################################################################
# Income
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_inntekt.sas7bdat")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Pregnant_COVID_Risk_Uptake.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Pregnant_COVID_NoRisk_Uptake.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Pregnant_Influenza_Risk_Uptake.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Pregnant_Influenza_NoRisk_Uptake.rdata")

SSB$income_2017 <- NA
SSB$income_2021 <- NA
SSB$income_2017[SSB$aargang == "2017"] <- SSB$wlonn[SSB$aargang == "2017"]
SSB$income_2021[SSB$aargang == "2021"] <- SSB$wlonn[SSB$aargang == "2021"]
SSB$income_2017_cat <- NA
SSB$income_2021_cat <- NA
SSB$income_2017_cat[SSB$income_2017 <= 399999] <- "Low"
SSB$income_2017_cat[SSB$income_2017 > 399999 & SSB$income_2017 <= 899998] <- "Medium"
SSB$income_2017_cat[SSB$income_2017 >= 899999] <- "High"
SSB$income_2021_cat[SSB$income_2021 <= 399999] <- "Low"
SSB$income_2021_cat[SSB$income_2021 > 399999 & SSB$income_2021 <= 899998] <- "Medium"
SSB$income_2021_cat[SSB$income_2021 >= 899999] <- "High"
SSB_preg_risk_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_risk$person_id,]
SSB_preg_norisk_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_norisk$person_id,]
SSB_preg_risk_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_risk$person_id,]
SSB_preg_norisk_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_norisk$person_id,]
SSB_preg_risk_flu <- SSB_preg_risk_flu[, c("KOBLINGSNOEKKEL", "income_2017_cat")]
SSB_preg_norisk_flu <- SSB_preg_norisk_flu[, c("KOBLINGSNOEKKEL", "income_2017_cat")]
SSB_preg_risk_covid <- SSB_preg_risk_covid[, c("KOBLINGSNOEKKEL", "income_2021_cat")]
SSB_preg_norisk_covid <- SSB_preg_norisk_covid[, c("KOBLINGSNOEKKEL", "income_2021_cat")]
colnames(SSB_preg_risk_flu)[1] <- "person_id"
colnames(SSB_preg_norisk_flu)[1] <- "person_id"
colnames(SSB_preg_risk_covid)[1] <- "person_id"
colnames(SSB_preg_norisk_covid)[1] <- "person_id"
pregnant_flu_risk <- merge(pregnant_flu_risk, SSB_preg_risk_flu, all = T)
pregnant_flu_norisk <- merge(pregnant_flu_norisk, SSB_preg_norisk_flu, all = T)
pregnant_covid_risk <- merge(pregnant_covid_risk, SSB_preg_risk_covid, all = T)
pregnant_covid_norisk <- merge(pregnant_covid_norisk, SSB_preg_norisk_covid, all = T)
######################################################################################
# County
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_bostedsfylke.sas7bdat")
SSB$county_2017 <- NA
SSB$county_2021 <- NA
SSB$county_2017[SSB$bostedsfylke_2017 == "01"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "02"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "03"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "04"] <- "Central"
SSB$county_2017[SSB$bostedsfylke_2017 == "05"] <- "Central"
SSB$county_2017[SSB$bostedsfylke_2017 == "06"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "07"] <- "Eastern"
SSB$county_2017[SSB$bostedsfylke_2017 == "08"] <- "Southern"
SSB$county_2017[SSB$bostedsfylke_2017 == "09"] <- "Southern"
SSB$county_2017[SSB$bostedsfylke_2017 == "10"] <- "Southern"
SSB$county_2017[SSB$bostedsfylke_2017 == "11"] <- "Southern"
SSB$county_2017[SSB$bostedsfylke_2017 == "12"] <- "Western"
SSB$county_2017[SSB$bostedsfylke_2017 == "14"] <- "Western"
SSB$county_2017[SSB$bostedsfylke_2017 == "15"] <- "Western"
SSB$county_2017[SSB$bostedsfylke_2017 == "18"] <- "Central"
SSB$county_2017[SSB$bostedsfylke_2017 == "50"] <- "Central"
SSB$county_2017[SSB$bostedsfylke_2017 == "19"] <- "Northern"
SSB$county_2017[SSB$bostedsfylke_2017 == "20"] <- "Northern"

SSB$county_2021[SSB$bostedsfylke_2021 == "03"] <- "Eastern"
SSB$county_2021[SSB$bostedsfylke_2021 == "11"] <- "Southern"
SSB$county_2021[SSB$bostedsfylke_2021 == "15"] <- "Western"
SSB$county_2021[SSB$bostedsfylke_2021 == "18"] <- "Central"
SSB$county_2021[SSB$bostedsfylke_2021 == "30"] <- "Eastern"
SSB$county_2021[SSB$bostedsfylke_2021 == "34"] <- "Central"
SSB$county_2021[SSB$bostedsfylke_2021 == "38"] <- "Eastern"
SSB$county_2021[SSB$bostedsfylke_2021 == "42"] <- "Southern"
SSB$county_2021[SSB$bostedsfylke_2021 == "46"] <- "Western"
SSB$county_2021[SSB$bostedsfylke_2021 == "50"] <- "Central"
SSB$county_2021[SSB$bostedsfylke_2021 == "54"] <- "Northern"

SSB_preg_risk_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_risk$person_id,]
SSB_preg_norisk_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_norisk$person_id,]
SSB_preg_risk_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_risk$person_id,]
SSB_preg_norisk_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_norisk$person_id,]
SSB_preg_risk_flu <- SSB_preg_risk_flu[, c("KOBLINGSNOEKKEL", "county_2017")]
SSB_preg_norisk_flu <- SSB_preg_norisk_flu[, c("KOBLINGSNOEKKEL", "county_2017")]
SSB_preg_risk_covid <- SSB_preg_risk_covid[, c("KOBLINGSNOEKKEL", "county_2021")]
SSB_preg_norisk_covid <- SSB_preg_norisk_covid[, c("KOBLINGSNOEKKEL", "county_2021")]
colnames(SSB_preg_risk_flu)[1] <- "person_id"
colnames(SSB_preg_norisk_flu)[1] <- "person_id"
colnames(SSB_preg_risk_covid)[1] <- "person_id"
colnames(SSB_preg_norisk_covid)[1] <- "person_id"
pregnant_flu_risk <- merge(pregnant_flu_risk, SSB_preg_risk_flu, all = T)
pregnant_flu_norisk <- merge(pregnant_flu_norisk, SSB_preg_norisk_flu, all = T)
pregnant_covid_risk <- merge(pregnant_covid_risk, SSB_preg_risk_covid, all = T)
pregnant_covid_norisk <- merge(pregnant_covid_norisk, SSB_preg_norisk_covid, all = T)
#############################################################################
# Age at enrollment
SSB <- read_sas("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_faste_opplysninger.sas7bdat")
reference <- read.csv("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/Reference_dates_SSB.csv")
SSB$birth_date <- as.Date(reference$ref_date) + SSB$foedselsdato_delta

SSB_flu_risk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_risk$person_id,]
SSB_flu_norisk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_norisk$person_id,]
SSB_covid_risk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_risk$person_id,]
SSB_covid_norisk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_norisk$person_id,]

SSB_flu_norisk <- SSB_flu_norisk[, c("KOBLINGSNOEKKEL", "birth_date")]
SSB_flu_risk <- SSB_flu_risk[, c("KOBLINGSNOEKKEL", "birth_date")]
SSB_covid_norisk <- SSB_covid_norisk[, c("KOBLINGSNOEKKEL", "birth_date")]
SSB_covid_risk <- SSB_covid_risk[, c("KOBLINGSNOEKKEL", "birth_date")]

colnames(SSB_flu_norisk)[1] <- "person_id"
colnames(SSB_flu_risk)[1] <- "person_id"
colnames(SSB_covid_norisk)[1] <- "person_id"
colnames(SSB_covid_risk)[1] <- "person_id"
pregnant_covid_risk <- merge(pregnant_covid_risk, SSB_covid_risk, all = T)
pregnant_covid_norisk <- merge(pregnant_covid_norisk, SSB_covid_norisk, all = T)
pregnant_flu_risk <- merge(pregnant_flu_risk, SSB_flu_risk, all = T)
pregnant_flu_norisk <- merge(pregnant_flu_norisk, SSB_flu_norisk, all = T)

pregnant_covid_norisk$age_at_enrollment <- interval(pregnant_covid_norisk$birth_date, pregnant_covid_norisk$enrollment_norisk) %>%
  as.period()
pregnant_covid_norisk$age_at_enrollment <- year(pregnant_covid_norisk$age_at_enrollment)
pregnant_covid_norisk$age_at_enrollment_categorized <- NA
pregnant_covid_norisk$age_at_enrollment_categorized[pregnant_covid_norisk$age_at_enrollment >= 18 & pregnant_covid_norisk$age_at_enrollment <= 24] <- "18-24"
pregnant_covid_norisk$age_at_enrollment_categorized[pregnant_covid_norisk$age_at_enrollment >= 25 & pregnant_covid_norisk$age_at_enrollment <= 30] <- "25-30"
pregnant_covid_norisk$age_at_enrollment_categorized[pregnant_covid_norisk$age_at_enrollment >= 31 & pregnant_covid_norisk$age_at_enrollment <= 35] <- "31-35"
pregnant_covid_norisk$age_at_enrollment_categorized[pregnant_covid_norisk$age_at_enrollment >= 36 & pregnant_covid_norisk$age_at_enrollment <= 40] <- "36-40"
pregnant_covid_norisk$age_at_enrollment_categorized[pregnant_covid_norisk$age_at_enrollment >= 41] <- ">41"

pregnant_covid_risk$age_at_enrollment <- interval(pregnant_covid_risk$birth_date, pregnant_covid_risk$enrollment_norisk) %>%
  as.period()
pregnant_covid_risk$age_at_enrollment <- year(pregnant_covid_risk$age_at_enrollment)
pregnant_covid_risk$age_at_enrollment_categorized <- NA
pregnant_covid_risk$age_at_enrollment_categorized[pregnant_covid_risk$age_at_enrollment >= 18 & pregnant_covid_risk$age_at_enrollment <= 24] <- "18-24"
pregnant_covid_risk$age_at_enrollment_categorized[pregnant_covid_risk$age_at_enrollment >= 25 & pregnant_covid_risk$age_at_enrollment <= 30] <- "25-30"
pregnant_covid_risk$age_at_enrollment_categorized[pregnant_covid_risk$age_at_enrollment >= 31 & pregnant_covid_risk$age_at_enrollment <= 35] <- "31-35"
pregnant_covid_risk$age_at_enrollment_categorized[pregnant_covid_risk$age_at_enrollment >= 36 & pregnant_covid_risk$age_at_enrollment <= 40] <- "36-40"
pregnant_covid_risk$age_at_enrollment_categorized[pregnant_covid_risk$age_at_enrollment >= 41] <- ">41"

pregnant_flu_risk$age_at_enrollment <- interval(pregnant_flu_risk$birth_date, pregnant_flu_risk$enrollment_norisk) %>%
  as.period()
pregnant_flu_risk$age_at_enrollment <- year(pregnant_flu_risk$age_at_enrollment)
pregnant_flu_risk$age_at_enrollment_categorized <- NA
pregnant_flu_risk$age_at_enrollment_categorized[pregnant_flu_risk$age_at_enrollment >= 18 & pregnant_flu_risk$age_at_enrollment <= 24] <- "18-24"
pregnant_flu_risk$age_at_enrollment_categorized[pregnant_flu_risk$age_at_enrollment >= 25 & pregnant_flu_risk$age_at_enrollment <= 30] <- "25-30"
pregnant_flu_risk$age_at_enrollment_categorized[pregnant_flu_risk$age_at_enrollment >= 31 & pregnant_flu_risk$age_at_enrollment <= 35] <- "31-35"
pregnant_flu_risk$age_at_enrollment_categorized[pregnant_flu_risk$age_at_enrollment >= 36 & pregnant_flu_risk$age_at_enrollment <= 40] <- "36-40"
pregnant_flu_risk$age_at_enrollment_categorized[pregnant_flu_risk$age_at_enrollment >= 41] <- ">41"

pregnant_flu_norisk$age_at_enrollment <- interval(pregnant_flu_norisk$birth_date, pregnant_flu_norisk$enrollment_norisk) %>%
  as.period()
pregnant_flu_norisk$age_at_enrollment <- year(pregnant_flu_norisk$age_at_enrollment)
pregnant_flu_norisk$age_at_enrollment_categorized <- NA
pregnant_flu_norisk$age_at_enrollment_categorized[pregnant_flu_norisk$age_at_enrollment >= 18 & pregnant_flu_norisk$age_at_enrollment <= 24] <- "18-24"
pregnant_flu_norisk$age_at_enrollment_categorized[pregnant_flu_norisk$age_at_enrollment >= 25 & pregnant_flu_norisk$age_at_enrollment <= 30] <- "25-30"
pregnant_flu_norisk$age_at_enrollment_categorized[pregnant_flu_norisk$age_at_enrollment >= 31 & pregnant_flu_norisk$age_at_enrollment <= 35] <- "31-35"
pregnant_flu_norisk$age_at_enrollment_categorized[pregnant_flu_norisk$age_at_enrollment >= 36 & pregnant_flu_norisk$age_at_enrollment <= 40] <- "36-40"
pregnant_flu_norisk$age_at_enrollment_categorized[pregnant_flu_norisk$age_at_enrollment >= 41] <- ">41"
#############################################################################
# Country of birth
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_faste_opplysninger.sas7bdat")
Norwegian <- "000"
West_North_Central <- c("101", "102", "103", "104", "105", "106", "112","114",'117','121','127','128','129','130','139','141','144',
                        '153', '162','163', '164')
Southern <- c('118','119','123','126','132','134','137','154')
Eastern <- c("111","113", '115','120','122','124','130','133','136','138','140','146','148','152','155','156','157','158','159','160',
             '161')
MENA <- c('143','203','249','286','303','304','306','356','379','409','426','452','456','460','476','496','500','508','520','524','540'
          ,'544','564','578')
Central_East_Asia <- c('404','406','407','412','430','432','436','464','480','484','488','492','502','510','516','550','552','554')
South_Asian <- c('213','410','416','420','424','428','444','448','478','504','512','513','528','534','537','548','568','575','808')
Latin_America <- c('601','602','603','604','606','608','613','616','620','622','624','629', '631','632','636','644','648','650','652',
                   '654','657','658','659','660','661','664','668','672','676','677','678','679','680','681','685','686','687','705',
                   '710','715','720','725','730','735','740','745','755','760','765','770','775')
North_America <- c('612','684')
Australian <- c('802','805','806','807','808','809','811','812','813','814','815','816','817','818','819','820','821','826','827','833')
African <- c('204', '205','209','216','220','229','235','239','241','246','250','254','256','260','264','266','270','273','278',
             '279','281','283','289','296','299','307','308','309','313','319','322','323','326','329','333','336','337',
             '338','339','346','357','359','369','373','376','386','389','393')

SSB$country_of_birth <- NA
SSB$country_of_birth[SSB$fodeland == "000" ] <- "Norwegian"
SSB$country_of_birth[SSB$fodeland %in%  West_North_Central] <- "Western/ Northern/ Central European"
SSB$country_of_birth[SSB$fodeland %in%  Southern] <- "Southern European"
SSB$country_of_birth[SSB$fodeland %in%  Eastern] <- "Eastern European"
SSB$country_of_birth[SSB$fodeland %in%  MENA] <- "Middle Eastern & North African"
SSB$country_of_birth[SSB$fodeland %in%  Central_East_Asia] <- "Central/ East Asian"
SSB$country_of_birth[SSB$fodeland %in%  South_Asian] <- "South Asian"
SSB$country_of_birth[SSB$fodeland %in%  African] <- "African"
SSB$country_of_birth[SSB$fodeland %in%  Australian] <- "Oceanian"
SSB$country_of_birth[SSB$fodeland %in%  North_America] <- "North American"
SSB$country_of_birth[SSB$fodeland %in%  Latin_America] <- "Latin American"

SSB_flu_risk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_risk$person_id,]
SSB_flu_norisk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_norisk$person_id,]
SSB_covid_risk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_risk$person_id,]
SSB_covid_norisk <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_norisk$person_id,]

SSB_flu_norisk <- SSB_flu_norisk[, c("KOBLINGSNOEKKEL", "country_of_birth")]
SSB_flu_risk <- SSB_flu_risk[, c("KOBLINGSNOEKKEL", "country_of_birth")]
SSB_covid_norisk <- SSB_covid_norisk[, c("KOBLINGSNOEKKEL", "country_of_birth")]
SSB_covid_risk <- SSB_covid_risk[, c("KOBLINGSNOEKKEL", "country_of_birth")]

colnames(SSB_flu_norisk)[1] <- "person_id"
colnames(SSB_flu_risk)[1] <- "person_id"
colnames(SSB_covid_norisk)[1] <- "person_id"
colnames(SSB_covid_risk)[1] <- "person_id"
pregnant_covid_risk <- merge(pregnant_covid_risk, SSB_covid_risk, all = T)
pregnant_covid_norisk <- merge(pregnant_covid_norisk, SSB_covid_norisk, all = T)
pregnant_flu_risk <- merge(pregnant_flu_risk, SSB_flu_risk, all = T)
pregnant_flu_norisk <- merge(pregnant_flu_norisk, SSB_flu_norisk, all = T)

#############################################################################
# Smoking 
pregnant_flu_risk$smoking <- NA
pregnant_flu_risk$smoking[pregnant_flu_risk$MOR_ROYKTE_1_TRIMESTER == 1] <- "No"
pregnant_flu_risk$smoking[pregnant_flu_risk$MOR_ROYKTE_1_TRIMESTER == 2] <- "Occasionally"
pregnant_flu_risk$smoking[pregnant_flu_risk$MOR_ROYKTE_1_TRIMESTER == 3] <- "Daily"

pregnant_flu_norisk$smoking <- NA
pregnant_flu_norisk$smoking[pregnant_flu_norisk$MOR_ROYKTE_1_TRIMESTER == 1] <- "No"
pregnant_flu_norisk$smoking[pregnant_flu_norisk$MOR_ROYKTE_1_TRIMESTER == 2] <- "Occasionally"
pregnant_flu_norisk$smoking[pregnant_flu_norisk$MOR_ROYKTE_1_TRIMESTER == 3] <- "Daily"

pregnant_covid_risk$smoking <- NA
pregnant_covid_risk$smoking[pregnant_covid_risk$MOR_ROYKTE_1_TRIMESTER == 1] <- "No"
pregnant_covid_risk$smoking[pregnant_covid_risk$MOR_ROYKTE_1_TRIMESTER == 2] <- "Occasionally"
pregnant_covid_risk$smoking[pregnant_covid_risk$MOR_ROYKTE_1_TRIMESTER == 3] <- "Daily"

pregnant_covid_norisk$smoking <- NA
pregnant_covid_norisk$smoking[pregnant_covid_norisk$MOR_ROYKTE_1_TRIMESTER == 1] <- "No"
pregnant_covid_norisk$smoking[pregnant_covid_norisk$MOR_ROYKTE_1_TRIMESTER == 2] <- "Occasionally"
pregnant_covid_norisk$smoking[pregnant_covid_norisk$MOR_ROYKTE_1_TRIMESTER == 3] <- "Daily"
#################################################################################
# Profession
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_regsys_2017.sas7bdat")

SSB <- SSB %>%
  mutate(
    profession = case_when(
      str_starts(arb_yrke_styrk08, "0") ~ "Military/ Leaders",
      str_starts(arb_yrke_styrk08, "1") ~ "Military/ Leaders",
      str_starts(arb_yrke_styrk08, "2") ~ "Academia",
      str_starts(arb_yrke_styrk08, "3") ~ "Academia",
      str_starts(arb_yrke_styrk08, "4") ~ "Office/ Sales",
      str_starts(arb_yrke_styrk08, "5") ~ "Office/ Sales",
      str_starts(arb_yrke_styrk08, "6") ~ "Farmer/ Crafts/ Cleaning/ Transport workers",
      str_starts(arb_yrke_styrk08, "7") ~ "Farmer/ Crafts/ Cleaning/ Transport workers",
      str_starts(arb_yrke_styrk08, "8") ~ "Farmer/ Crafts/ Cleaning/ Transport workers",
      str_starts(arb_yrke_styrk08, "9") ~ "Farmer/ Crafts/ Cleaning/ Transport workers",
      TRUE ~ "Other_Category"
    )
  )
SSB$profession[SSB$profession == "Other_Category"] <- NA

SSB_preg_risk_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_risk$person_id,]
SSB_preg_norisk_flu <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_flu_norisk$person_id,]
SSB_preg_risk_flu <- SSB_preg_risk_flu[, c("KOBLINGSNOEKKEL", "profession")]
SSB_preg_norisk_flu <- SSB_preg_norisk_flu[, c("KOBLINGSNOEKKEL", "profession")]
colnames(SSB_preg_risk_flu)[1] <- "person_id"
colnames(SSB_preg_norisk_flu)[1] <- "person_id"
pregnant_flu_risk <- merge(pregnant_flu_risk, SSB_preg_risk_flu, all = T)
pregnant_flu_norisk <- merge(pregnant_flu_norisk, SSB_preg_norisk_flu, all = T)

SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_regsys_2021.sas7bdat")

SSB <- SSB %>%
  mutate(
    profession = case_when(
      str_starts(arb_yrke_styrk08, "0") ~ "Military/ Leaders",
      str_starts(arb_yrke_styrk08, "1") ~ "Military/ Leaders",
      str_starts(arb_yrke_styrk08, "2") ~ "Academia",
      str_starts(arb_yrke_styrk08, "3") ~ "Academia",
      str_starts(arb_yrke_styrk08, "4") ~ "Office/ Sales",
      str_starts(arb_yrke_styrk08, "5") ~ "Office/ Sales",
      str_starts(arb_yrke_styrk08, "6") ~ "Farmer/ Crafts/ Cleaning/ Transport workers",
      str_starts(arb_yrke_styrk08, "7") ~ "Farmer/ Crafts/ Cleaning/ Transport workers",
      str_starts(arb_yrke_styrk08, "8") ~ "Farmer/ Crafts/ Cleaning/ Transport workers",
      str_starts(arb_yrke_styrk08, "9") ~ "Farmer/ Crafts/ Cleaning/ Transport workers",
      TRUE ~ "Other_Category"
    )
  )
SSB$profession[SSB$profession == "Other_Category"] <- NA

SSB_preg_risk_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_risk$person_id,]
SSB_preg_norisk_covid <- SSB[SSB$KOBLINGSNOEKKEL %in% pregnant_covid_norisk$person_id,]
SSB_preg_risk_covid <- SSB_preg_risk_covid[, c("KOBLINGSNOEKKEL", "profession")]
SSB_preg_norisk_covid <- SSB_preg_norisk_covid[, c("KOBLINGSNOEKKEL", "profession")]
colnames(SSB_preg_risk_covid)[1] <- "person_id"
colnames(SSB_preg_norisk_covid)[1] <- "person_id"
pregnant_covid_risk <- merge(pregnant_covid_risk, SSB_preg_risk_covid, all = T)
pregnant_covid_norisk <- merge(pregnant_covid_norisk, SSB_preg_norisk_covid, all = T)
#################################################################################
# Parity
pregnant_covid_norisk$parity <- NA
pregnant_covid_risk$parity <- NA
pregnant_flu_norisk$parity <- NA
pregnant_flu_risk$parity <- NA

pregnant_covid_norisk$parity[pregnant_covid_norisk$PARITET == 1] <- 1
pregnant_covid_norisk$parity[pregnant_covid_norisk$PARITET == 2] <- 2
pregnant_covid_norisk$parity[pregnant_covid_norisk$PARITET == 3] <- 3
pregnant_covid_norisk$parity[pregnant_covid_norisk$PARITET >= 4] <- 4
pregnant_covid_norisk$parity[pregnant_covid_norisk$PARITET == 0] <- 0

pregnant_covid_risk$parity[pregnant_covid_risk$PARITET == 1] <- 1
pregnant_covid_risk$parity[pregnant_covid_risk$PARITET == 2] <- 2
pregnant_covid_risk$parity[pregnant_covid_risk$PARITET == 3] <- 3
pregnant_covid_risk$parity[pregnant_covid_risk$PARITET >= 4] <- 4
pregnant_covid_risk$parity[pregnant_covid_risk$PARITET == 0] <- 0

pregnant_flu_risk$parity[pregnant_flu_risk$PARITET == 1] <- 1
pregnant_flu_risk$parity[pregnant_flu_risk$PARITET == 2] <- 2
pregnant_flu_risk$parity[pregnant_flu_risk$PARITET == 3] <- 3
pregnant_flu_risk$parity[pregnant_flu_risk$PARITET >= 4] <- 4
pregnant_flu_risk$parity[pregnant_flu_risk$PARITET == 0] <- 0

pregnant_flu_norisk$parity[pregnant_flu_norisk$PARITET == 1] <- 1
pregnant_flu_norisk$parity[pregnant_flu_norisk$PARITET == 2] <- 2
pregnant_flu_norisk$parity[pregnant_flu_norisk$PARITET == 3] <- 3
pregnant_flu_norisk$parity[pregnant_flu_norisk$PARITET >= 4] <- 4
pregnant_flu_norisk$parity[pregnant_flu_norisk$PARITET == 0] <- 0

save(pregnant_covid_norisk, file = "Pregnant_covid_norisk_covariates.rdata")
save(pregnant_covid_risk, file = "Pregnant_covid_risk_covariates.rdata")
save(pregnant_flu_norisk, file = "Pregnant_flu_norisk_covariates.rdata")
save(pregnant_flu_risk, file = "Pregnant_flu_risk_covariates.rdata")
###########################################################################
# Dementia risk factors
# Let's start by defining the risk factors
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Pregnant_COVID-19_NoRisk_Covariates_Corrected_NoDuplicates.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Pregnant_Influenza_Risk_Covariates_Corrected_NoDuplicates.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Pregnant_COVID-19_Risk_Covariates_Corrected_NoDuplicates.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Pregnant_Influenza_NoRisk_Covariates_Corrected_NoDuplicates.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Old_flu_Covariates.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Old_covid_Covariates.rdata")

Dementia_ICD10 <- c("F00", "F01", "F02","F03", "G30", "G31")
Dementia_ICPC <- c("P70")

NPR <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_NPR_SOM.csv", 
                  delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Old_COVID_MH_Corrected_Final.rdata")
load("/ess/p1921/home/p1921-mahmoudz/Mahmoud/Vaccine_Uptake/New_run_20260117/Scripts/Old_Influenza_NoDuplicates.rdata")

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
KUHR_2015 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2015.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2015 <- KUHR_2015[KUHR_2015$person_id %in% old_flu$person_id,]
KUHR_2015 <- KUHR_2015[grepl("P70", KUHR_2015$event_code, ignore.case = T), ]
gc()
KUHR_2016 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2016.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2016 <- KUHR_2016[KUHR_2016$person_id %in% old_flu$person_id,]
KUHR_2016 <- KUHR_2016[grepl("P70", KUHR_2016$event_code, ignore.case = T), ]
gc()
KUHR_2017 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2017.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2017 <- KUHR_2017[KUHR_2017$person_id %in% old_flu$person_id,]
KUHR_2017 <- KUHR_2017[grepl("P70", KUHR_2017$event_code, ignore.case = T), ]
gc()
KUHR_2018 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2018.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2018 <- KUHR_2018[KUHR_2018$person_id %in% old_flu$person_id,]
KUHR_2018 <- KUHR_2018[grepl("P70", KUHR_2018$event_code, ignore.case = T), ]
gc()
KUHR_2019 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2019.csv", 
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

KUHR_2020 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2020.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2020 <- KUHR_2020[KUHR_2020$person_id %in% old_covid$person_id,]
KUHR_2020 <- KUHR_2020[grepl("P70", KUHR_2020$event_code, ignore.case = T), ]
gc()
KUHR_2021 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2021.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2021 <- KUHR_2021[KUHR_2021$person_id %in% old_covid$person_id,]
KUHR_2021 <- KUHR_2021[grepl("P70", KUHR_2021$event_code, ignore.case = T), ]
gc()
KUHR_2022 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2022.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2022 <- KUHR_2022[KUHR_2022$person_id %in% old_covid$person_id,]
KUHR_2022 <- KUHR_2022[grepl("P70", KUHR_2022$event_code, ignore.case = T), ]
gc()
KUHR_2023 <- read_delim("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2023.csv", 
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
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_bu_utd.sas7bdat")
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
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_sivilstand.sas7bdat")
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
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_bu_utd.sas7bdat")
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
SSB <- read_sas("/ess/p1921/data/durable/VAC4EU datasets/Delivery June-Sep 2024/SSB/w22_0605_UiO_2024_juni/w22_0605_sivilstand.sas7bdat")
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











