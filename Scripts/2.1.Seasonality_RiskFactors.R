# This script aims to more properly define what seasons the patients are included in as well as clearly define risk groups. 
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

NPR <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_NPR_SOM.csv", 
                  delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))

NPR_flu <- NPR[NPR$person_id %in% pregnant_influenza$lopenr, ]
NPR_flu1 <- NPR_flu[NPR_flu$event_code %in% transplantation_ICD10| NPR_flu$event_code %in% Chronic_lung_disease_ICD10| NPR_flu$event_code %in% Dementia_ICD10|
                      NPR_flu$event_code %in% Diabetes_ICD10 | NPR_flu$event_code %in% Heart_disease_ICD10 | NPR_flu$event_code %in% Hematological_cancer_ICD10|
                      NPR_flu$event_code %in% Immunodeficiency_ICD10| NPR_flu$event_code %in% Impaired_immunity_ICD10 | NPR_flu$event_code %in% kidney_failure_ICD10|
                      NPR_flu$event_code %in% liver_failure_ICD10| NPR_flu$event_code %in% neurology_ICD10| NPR_flu$event_code %in% Obesity_ICD10|
                      NPR_flu$event_code %in% Other_Active_Cancer_ICD10 | NPR_flu$event_code %in% Stroke_ICD10,]

colnames(pregnant_influenza) [1] <- "person_id"
merged <- merge(pregnant_influenza, NPR_flu1, all = T)
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

merged$lung_disease[merged$event_code %in% Chronic_lung_disease_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$Dementia[merged$event_code %in% Dementia_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$diabetes[merged$event_code %in% Diabetes_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$heart_disease[merged$event_code %in% Heart_disease_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$hematological_cancer[merged$event_code %in% Hematological_cancer_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$Immunodeficiency[merged$event_code %in% Immunodeficiency_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$Impaired_immunity[merged$event_code %in% Impaired_immunity_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$kidney_failure[merged$event_code %in% kidney_failure_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$liver_failure[merged$event_code %in% liver_failure_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$neurology[merged$event_code %in% neurology_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$obesity[merged$event_code %in% Obesity_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$cancer[merged$event_code %in% Other_Active_Cancer_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$stroke[merged$event_code %in% Stroke_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$transplantation[merged$event_code %in% transplantation_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1

merged$Preg_id <- paste0(merged$person_id,"-", merged$LMP)
merged <- merged %>% group_by(Preg_id) %>%
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
merged <- merged[!duplicated(merged$Preg_id),]
merged$risk_factor[merged$lung_disease_final ==1 | merged$Dementia_final ==1 |merged$diabetes_final ==1 |merged$heart_disease_final ==1 |merged$hematological_cancer_final ==1 |
                     merged$Immunodeficiency_final ==1 |merged$Impaired_immunity_final ==1 |merged$kidney_failure_final ==1 |merged$liver_failure_final ==1 |merged$neurology_final ==1 |
                     merged$obesity_final ==1 |merged$cancer_final ==1 |merged$stroke_final ==1 |merged$transplantation_final ==1 ] <- 1

# Now we need to check KUHR for risk factors and procedures from NPR
KUHR_2015 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2015.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2015 <- KUHR_2015[KUHR_2015$person_id %in% pregnant_influenza$person_id,]
KUHR_2015 <- KUHR_2015[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2015$event_code, ignore.case = T), ]

KUHR_2016 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2016.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2016 <- KUHR_2016[KUHR_2016$person_id %in% pregnant_influenza$person_id,]
KUHR_2016 <- KUHR_2016[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2016$event_code, ignore.case = T), ]

KUHR_2017 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2017.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2017 <- KUHR_2017[KUHR_2017$person_id %in% pregnant_influenza$person_id,]
KUHR_2017 <- KUHR_2017[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2017$event_code, ignore.case = T), ]

KUHR_2018 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2018.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2018 <- KUHR_2018[KUHR_2018$person_id %in% pregnant_influenza$person_id,]
KUHR_2018 <- KUHR_2018[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2018$event_code, ignore.case = T), ]

KUHR_2019 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2019.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2019 <- KUHR_2019[KUHR_2019$person_id %in% pregnant_influenza$person_id,]
KUHR_2019 <- KUHR_2019[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2019$event_code, ignore.case = T), ]

KUHR <- merge(KUHR_2015, KUHR_2016, all = T)
KUHR <- merge(KUHR, KUHR_2017, all = T)
KUHR <- merge(KUHR, KUHR_2018, all = T)
KUHR <- merge(KUHR, KUHR_2019, all = T)



colnames(pregnant_influenza) [1] <- "person_id"
merged1 <- merge(merged, KUHR, all = T)
merged1$lung_disease[grepl("R95|R96", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$Dementia[grepl("P70", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$diabetes[grepl("T89|T90", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$heart_disease[grepl("K74|K75|K76|K77|K78|K82|K83|K87", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$obesity[grepl("T82", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$stroke[grepl("K91|K90", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
save(pregnant_flu_risk, file = "Pregnant_Influenza_Risk_Population.rdata")
pregnant_flu_norisk <- merged[merged$risk_factor == 0,]
save(pregnant_flu_norisk, file = "Pregnant_Influenza_NoRisk.rdata")

###################################################################################################################
# NOw we do the same for the COVID-19 pregnant population
NPR <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_NPR_SOM.csv", 
                  delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))

NPR_COVID <- NPR[NPR$person_id %in% pregnant_covid$person_id, ]
NPR_COVID <- NPR_COVID[NPR_COVID$event_code %in% transplantation_ICD10| NPR_COVID$event_code %in% Chronic_lung_disease_ICD10| NPR_COVID$event_code %in% Dementia_ICD10|
                      NPR_COVID$event_code %in% Diabetes_ICD10 | NPR_COVID$event_code %in% Heart_disease_ICD10 | NPR_COVID$event_code %in% Hematological_cancer_ICD10|
                      NPR_COVID$event_code %in% Immunodeficiency_ICD10| NPR_COVID$event_code %in% Impaired_immunity_ICD10 | NPR_COVID$event_code %in% kidney_failure_ICD10|
                      NPR_COVID$event_code %in% liver_failure_ICD10| NPR_COVID$event_code %in% neurology_ICD10| NPR_COVID$event_code %in% Obesity_ICD10|
                      NPR_COVID$event_code %in% Other_Active_Cancer_ICD10 | NPR_COVID$event_code %in% Stroke_ICD10,]

colnames(pregnant_covid) [1] <- "person_id"
merged <- merge(pregnant_covid, NPR_COVID, all = T)
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

merged$lung_disease[merged$event_code %in% Chronic_lung_disease_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$Dementia[merged$event_code %in% Dementia_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$diabetes[merged$event_code %in% Diabetes_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$heart_disease[merged$event_code %in% Heart_disease_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$hematological_cancer[merged$event_code %in% Hematological_cancer_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$Immunodeficiency[merged$event_code %in% Immunodeficiency_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$Impaired_immunity[merged$event_code %in% Impaired_immunity_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$kidney_failure[merged$event_code %in% kidney_failure_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$liver_failure[merged$event_code %in% liver_failure_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$neurology[merged$event_code %in% neurology_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$obesity[merged$event_code %in% Obesity_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$cancer[merged$event_code %in% Other_Active_Cancer_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$stroke[merged$event_code %in% Stroke_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1
merged$transplantation[merged$event_code %in% transplantation_ICD10 & ymd(merged$start_date_record) <= merged$enrollment_risk & ymd(merged$start_date_record) >= (merged$enrollment_risk - years(1))] <- 1

colnames(pregnant_covid) [1] <- "person_id"
# Now we need to check KUHR for risk factors and procedures from NPR
KUHR_2020 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2020.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2020 <- KUHR_2020[KUHR_2020$person_id %in% pregnant_covid$person_id,]
KUHR_2020 <- KUHR_2020[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2020$event_code, ignore.case = T), ]

KUHR_2021 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2021.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2021 <- KUHR_2021[KUHR_2021$person_id %in% pregnant_covid$person_id,]
KUHR_2021 <- KUHR_2021[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2021$event_code, ignore.case = T), ]

KUHR_2022 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2022.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2022 <- KUHR_2022[KUHR_2022$person_id %in% pregnant_covid$person_id,]
KUHR_2022 <- KUHR_2022[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2022$event_code, ignore.case = T), ]

KUHR_2023 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2023.csv", 
                        delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))
KUHR_2023 <- KUHR_2023[KUHR_2023$person_id %in% pregnant_covid$person_id,]
KUHR_2023 <- KUHR_2023[grepl("R95|R96|P70|T89|T90|K74|K75|K76|K77|K78|K82|K83|K87|T82|K90|K91", KUHR_2023$event_code, ignore.case = T), ]


KUHR <- merge(KUHR_2020, KUHR_2021, all = T)
KUHR <- merge(KUHR, KUHR_2022, all = T)
KUHR <- merge(KUHR, KUHR_2023, all = T)

merged1 <- merge(merged, KUHR, all = T)
merged1$lung_disease[grepl("R95|R96", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$Dementia[grepl("P70", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$diabetes[grepl("T89|T90", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$heart_disease[grepl("K74|K75|K76|K77|K78|K82|K83|K87", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$obesity[grepl("T82", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1
merged1$stroke[grepl("K91|K90", merged1$event_code, ignore.case = T) & ymd(merged1$start_date_record) <= merged1$enrollment_risk & ymd(merged1$start_date_record) >= (merged1$enrollment_risk - years(1)) & merged1$event_record_vocabulary == "ICPC"] <- 1


merged1$Preg_id <- paste0(merged1$person_id,"-", merged1$LMP)
merged1 <- merged1 %>% group_by(Preg_id) %>%
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
merged1 <- merged1[!duplicated(merged1$Preg_id),]
merged1$risk_factor[merged1$lung_disease_final ==1 | merged1$Dementia_final ==1 |merged1$diabetes_final ==1 |merged1$heart_disease_final ==1 |merged1$hematological_cancer_final ==1 |
                     merged1$Immunodeficiency_final ==1 |merged1$Impaired_immunity_final ==1 |merged1$kidney_failure_final ==1 |merged1$liver_failure_final ==1 |merged1$neurology_final ==1 |
                     merged1$obesity_final ==1 |merged1$cancer_final ==1 |merged1$stroke_final ==1 |merged1$transplantation_final ==1 ] <- 1
merged1 <- merged1[!duplicated(merged1$Preg_id),]
pregnant_covid_risk <- merged1[merged1$risk_factor == 1,]
pregnant_covid_norisk <- merged1[merged1$risk_factor == 0,]
pregnant_covid_norisk <- pregnant_covid_norisk[!is.na(pregnant_covid_norisk$Preg_id),]
pregnant_covid_risk <- pregnant_covid_risk[!is.na(pregnant_covid_risk$Preg_id),]

save(pregnant_covid_risk, file = "Pregnant_COVID_Risk_Population_YETExclusion.rdata")
save(pregnant_covid_norisk, file = "Pregnant_COVID_NoRisk_Population_YETEXCLUSION.rdata")

# Now check NPR

npr_som <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_NPR_SOM.csv", 
                      delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))

npr_som <- npr_som[grepl("u071", npr_som$event_code, ignore.case = T),]
npr_som_risk <- npr_som[npr_som$person_id %in% pregnant_covid_risk$person_id,]
npr_som_norisk <- npr_som[npr_som$person_id %in% pregnant_covid_norisk$person_id,]
npr_covid_risk <- merge(npr_som_risk, pregnant_covid_risk, all = T)
npr_covid_norisk <- merge(npr_som_norisk, pregnant_covid_norisk, all = T)
npr_covid_risk <- npr_covid_risk[!is.na(npr_covid_risk$event_code),]
npr_covid_norisk <- npr_covid_norisk[!is.na(npr_covid_norisk$event_code),]

covid_before_risk <- npr_covid_risk[ymd(npr_covid_risk$start_date_record) <= npr_covid_risk$enrollment_risk & ymd(npr_covid_risk$start_date_record) >= (npr_covid_risk$enrollment_risk - years(1)),]
covid_before_norisk <- npr_covid_norisk[ymd(npr_covid_norisk$start_date_record) <= npr_covid_norisk$enrollment_norisk & ymd(npr_covid_norisk$start_date_record) >= (npr_covid_risk$enrollment_norisk - years(1)),]


pregnant_covid_risk <- pregnant_covid_risk[!pregnant_covid_risk$person_id %in% covid_before_risk$person_id,] # 1571
pregnant_covid_norisk <- pregnant_covid_norisk[!pregnant_covid_norisk$person_id %in% covid_before_norisk$person_id,] # 118904

save(pregnant_covid_risk, file = "Pregnant_COVID19_Population_risk.rdata")
save(pregnant_covid_norisk, file = "Pregnant_COVID19_Population_norisk.rdata")

# now check KUHR for diagnoses in the look back period
kuhr_2020 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2020.csv", delim = ",")
kuhr_2020 <- kuhr_2020[grepl("R992", kuhr_2020$event_code, ignore.case = T),]
kuhr_2020_risk <- kuhr_2020[kuhr_2020$person_id %in% pregnant_covid_risk$person_id,]
kuhr_2020_norisk <- kuhr_2020[kuhr_2020$person_id %in% pregnant_covid_norisk$person_id,]
rm(kuhr_2020)
gc()
kuhr_2021 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2021.csv", delim = ",")
kuhr_2021 <- kuhr_2021[grepl("R992", kuhr_2021$event_code, ignore.case = T),]
kuhr_2021_risk <- kuhr_2021[kuhr_2021$person_id %in% pregnant_covid_risk$person_id,]
kuhr_2021_norisk <- kuhr_2021[kuhr_2021$person_id %in% pregnant_covid_norisk$person_id,]
rm(kuhr_2021)
gc()
kuhr_2022 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2022.csv", delim = ",")
kuhr_2022 <- kuhr_2022[grepl("R992", kuhr_2022$event_code, ignore.case = T),]
kuhr_2022_risk <- kuhr_2022[kuhr_2022$person_id %in% pregnant_covid_risk$person_id,]
kuhr_2022_norisk <- kuhr_2022[kuhr_2022$person_id %in% pregnant_covid_norisk$person_id,]
rm(kuhr_2022)
gc()
kuhr_2023 <- read_delim("N:/durable/vac4eu/CDMInstances/vac4eu_1052/EVENTS_KUHR_2023.csv", delim = ",")
kuhr_2023 <- kuhr_2023[grepl("R992", kuhr_2023$event_code, ignore.case = T),]
kuhr_2023_risk <- kuhr_2023[kuhr_2023$person_id %in% pregnant_covid_risk$person_id,]
kuhr_2023_norisk <- kuhr_2023[kuhr_2023$person_id %in% pregnant_covid_norisk$person_id,]
rm(kuhr_2023)
gc()

Kuhr_pregnant_covid_risk <- merge(kuhr_2020_risk, kuhr_2021_risk, all = T)
Kuhr_pregnant_covid_risk <- merge(Kuhr_pregnant_covid_risk, kuhr_2022_risk, all = T)
Kuhr_pregnant_covid_risk <- merge(Kuhr_pregnant_covid_risk, kuhr_2023_risk, all = T)
save(Kuhr_pregnant_covid_risk, file = "KUHR_PREGNANT_COVID_RISK.rdata")

Kuhr_pregnant_covid_norisk <- merge(kuhr_2020_norisk, kuhr_2021_norisk, all = T)
Kuhr_pregnant_covid_norisk <- merge(Kuhr_pregnant_covid_norisk, kuhr_2022_norisk, all = T)
Kuhr_pregnant_covid_norisk <- merge(Kuhr_pregnant_covid_norisk, kuhr_2023_norisk, all = T)
save(Kuhr_pregnant_covid_norisk, file = "KUHR_PREGNANT_COVID_NORISK.rdata")

merged_risk <- merge(Kuhr_pregnant_covid_risk, pregnant_covid_risk, all = T)
merged_norisk <- merge(Kuhr_pregnant_covid_norisk, pregnant_covid_norisk, all = T)
merged_risk <- merged_risk[!is.na(merged_risk$event_code),]
merged_norisk <- merged_norisk[!is.na(merged_norisk$event_code),]
merged_risk <- merged_risk[!is.na(merged_risk$enrollment_risk),]
merged_norisk <- merged_norisk[!is.na(merged_norisk$enrollment_norisk),]
covid_before_risk <- merged_risk[ymd(merged_risk$start_date_record) <= merged_risk$enrollment_risk & ymd(merged_risk$start_date_record) >= (merged_risk$enrollment_risk - years(1)),]
covid_before_norisk <- merged_norisk[ymd(merged_norisk$start_date_record) <= merged_norisk$enrollment_norisk & ymd(merged_norisk$start_date_record) >= (merged_norisk$enrollment_norisk - years(1)),]
pregnant_covid_risk <- pregnant_covid_risk[!pregnant_covid_risk$person_id %in% covid_before_risk$person_id,] # 628,748
pregnant_covid_norisk <- pregnant_covid_norisk[!pregnant_covid_norisk$person_id %in% covid_before_norisk$person_id,] # 628,748

# now check MSIS
msis <- read_delim("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/FHI/MSIS/H-602-E_MSIS-data_2024-09/H-602-E_MSIS-data_2024-09.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE,locale = locale(encoding = "Latin1"))

msis_risk <- msis[msis$KOBLINGSNOEKKEL %in% pregnant_covid_risk$person_id,]
msis_norisk <- msis[msis$KOBLINGSNOEKKEL %in% pregnant_covid_norisk$person_id,]
colnames(msis_risk) [1] <- "person_id"
colnames(msis_norisk) [1] <- "person_id"
merged_risk <- merge(msis_risk, pregnant_covid_risk, all = T)
merged_norisk <- merge(msis_norisk, pregnant_covid_norisk, all = T)
merged_risk <- merged_risk[!is.na(merged_risk$person_id),]
merged_norisk <- merged_norisk[!is.na(merged_norisk$person_id),]
referece_msis <- read_delim("N:/durable/VAC4EU datasets/Delivery June-Sep 2024/Reference_dates_FHI.csv", delim = ",")
colnames(referece_msis) [1] <- "person_id"
referece_msis_risk <- referece_msis[referece_msis$person_id %in% merged_risk$person_id,]
referece_msis_norisk <- referece_msis[referece_msis$person_id %in% merged_norisk$person_id,]

merged_risk <- merge(merged_risk, referece_msis_risk, all = T)
merged_norisk <- merge(merged_norisk, referece_msis_norisk, all = T)

merged_risk$test <- as.Date(merged_risk$ref_date, format = "%Y-%m-%d") + merged_risk$PrøvedatoDiffDager
merged_norisk$test <- as.Date(merged_norisk$ref_date, format = "%Y-%m-%d") + merged_norisk$PrøvedatoDiffDager
merged_risk <- merged_risk[!is.na(merged_risk$test),]
merged_norisk <- merged_norisk[!is.na(merged_norisk$test),]
covid_before_risk <- merged_risk[merged_risk$test <= merged_risk$enrollment_risk & merged_risk$test >= (merged_risk$enrollment_risk - years(1)),]
covid_before_norisk <- merged_norisk[merged_norisk$test <= merged_norisk$enrollment_norisk & merged_norisk$test >= (merged_norisk$enrollment_norisk - years(1)),]
pregnant_covid_risk <- pregnant_covid_risk[!pregnant_covid_risk$person_id %in% covid_before_risk$person_id,] # 627,553
pregnant_covid_norisk <- pregnant_covid_norisk[!pregnant_covid_norisk$person_id %in% covid_before_norisk$person_id,] # 627,553
# Now let's check for prior vaccination
covid.codes <- c("J07BN02", 'J07BN01', 'J07BX03', 'J07BN04', 'J07BN03')
SYSVAK <- read_csv("N:/durable/vac4eu/CDMInstances/vac4eu_1052/VACCINES.csv")
SYSVAK <- SYSVAK[SYSVAK$vx_atc %in% covid.codes, ]
SYSVAK1 <- SYSVAK[SYSVAK$person_id %in% pregnant_covid_norisk$person_id,]
merged <- merge(SYSVAK1, pregnant_covid_norisk, all = T)
vaccinated_before <- merged[ymd(merged$vx_admin_date) < merged$enrollment_norisk & ymd(merged$vx_admin_date) > merged$LMP,]
vaccinated_before <- vaccinated_before[!is.na(vaccinated_before$vx_admin_date),]
pregnant_covid_norisk <- pregnant_covid_norisk[!pregnant_covid_norisk$person_id %in% vaccinated_before$person_id,] # 552,756
save(pregnant_covid_norisk, file = "Pregnant_COVID19_NoRisk_Population.rdata")

