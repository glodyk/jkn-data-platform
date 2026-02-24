setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")


# Core packages
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(stringr)
library(lubridate)
library(randomForest)
library(styler)
library(splitstackshape)
library(sos)
library(readr)
library(anytime)
library(styler)
library(openxlsx)
library(data.table)

load("ur_all.rda")

ganglion <- data %>%
  subset(Tglstjkeu >= "2024-08-01") %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagnosa,"M674")) %>%
  select(Nokapst,Nosjp,Umur,Jkpst,Nmppkperujuk,
         Nmdati2Layan,Nmppklayan,Nmtkp,Tglpelayanan,
         Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
         Biayaverifikasi)
write.xlsx(ganglion, file = "ganglion rawat inap.xlsx")
ganglion <- ganglion[order(ganglion$Nokapst,ganglion$Tglplgsjp),]

sectio <- data %>%
  subset(Nmppklayan %in% c("RS Fatimah","RS Intan Medika","RSI Nashrul Ummah")) %>%
  subset(Tglstjkeu >= "2025-01-01") %>%
  subset(CBG == "O-6-10") %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"O15|O32|O34|O359|O365|O36|O41|O42|O44|O45|O48|O63|O68")) %>%
  select(-Nmtkp)
#========================================================================
sectio <- sectio %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
sectio <- sectio %>% mutate(Diagsekunder = na_if(Diagsekunder, ""))
sectio <- sectio %>% mutate(Procedure = na_if(Procedure, "-"))
sectio <- sectio %>% mutate(Procedure = na_if(Procedure, ""))

sectio <- sectio %>%
  mutate(
    Kddiagprimer1 = case_when(
      Kddiagprimer == "Z014" ~ NA,
      Kddiagprimer == "Z016" ~ NA,
      Kddiagprimer == "Z030" ~ NA,
      Kddiagprimer == "Z031" ~ NA,
      Kddiagprimer == "Z038" ~ NA,
      Kddiagprimer == "Z039" ~ NA,
      Kddiagprimer == "Z041" ~ NA,
      Kddiagprimer == "Z048" ~ NA,
      Kddiagprimer == "Z080" ~ NA,
      Kddiagprimer == "Z088" ~ NA,
      Kddiagprimer == "Z089" ~ NA,
      Kddiagprimer == "Z090" ~ NA,
      Kddiagprimer == "Z091" ~ NA,
      Kddiagprimer == "Z092" ~ NA,
      Kddiagprimer == "Z093" ~ NA,
      Kddiagprimer == "Z094" ~ NA,
      Kddiagprimer == "Z097" ~ NA,
      Kddiagprimer == "Z098" ~ NA,
      Kddiagprimer == "Z099" ~ NA,
      Kddiagprimer == "Z209" ~ NA,
      Kddiagprimer == "Z291" ~ NA,
      Kddiagprimer == "Z301" ~ NA,
      Kddiagprimer == "Z340" ~ NA,
      Kddiagprimer == "Z348" ~ NA,
      Kddiagprimer == "Z349" ~ NA,
      Kddiagprimer == "Z351" ~ NA,
      Kddiagprimer == "Z358" ~ NA,
      Kddiagprimer == "Z359" ~ NA,
      Kddiagprimer == "Z370" ~ NA,
      Kddiagprimer == "Z390" ~ NA,
      Kddiagprimer == "Z392" ~ NA,
      Kddiagprimer == "Z419" ~ NA,
      Kddiagprimer == "Z429" ~ NA,
      Kddiagprimer == "Z470" ~ NA,
      Kddiagprimer == "Z478" ~ NA,
      Kddiagprimer == "Z479" ~ NA,
      Kddiagprimer == "Z480" ~ NA,
      Kddiagprimer == "Z488" ~ NA,
      Kddiagprimer == "Z489" ~ NA,
      Kddiagprimer == "Z490" ~ NA,
      Kddiagprimer == "Z491" ~ NA,
      Kddiagprimer == "Z492" ~ NA,
      Kddiagprimer == "Z500" ~ NA,
      Kddiagprimer == "Z501" ~ NA,
      Kddiagprimer == "Z502" ~ NA,
      Kddiagprimer == "Z503" ~ NA,
      Kddiagprimer == "Z504" ~ NA,
      Kddiagprimer == "Z505" ~ NA,
      Kddiagprimer == "Z506" ~ NA,
      Kddiagprimer == "Z507" ~ NA,
      Kddiagprimer == "Z508" ~ NA,
      Kddiagprimer == "Z509" ~ NA,
      Kddiagprimer == "Z510" ~ NA,
      Kddiagprimer == "Z511" ~ NA,
      Kddiagprimer == "Z512" ~ NA,
      Kddiagprimer == "Z513" ~ NA,
      Kddiagprimer == "Z514" ~ NA,
      Kddiagprimer == "Z515" ~ NA,
      Kddiagprimer == "Z516" ~ NA,
      Kddiagprimer == "Z518" ~ NA,
      Kddiagprimer == "Z519" ~ NA,
      Kddiagprimer == "Z549" ~ NA,
      Kddiagprimer == "Z590" ~ NA,
      Kddiagprimer == "Z596" ~ NA,
      Kddiagprimer == "Z719" ~ NA,
      Kddiagprimer == "Z760" ~ NA,
      Kddiagprimer == "Z801" ~ NA,
      Kddiagprimer == "Z898" ~ NA,
      Kddiagprimer == "Z908" ~ NA,
      Kddiagprimer == "Z941" ~ NA,
      Kddiagprimer == "Z955" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z988" ~ NA,
      TRUE ~ Kddiagprimer))

sectio$Nmdiagprimer1 <- with(sectio,
                             ifelse(is.na(sectio$Kddiagprimer1),NA,Nmdiagprimer))
sectio <- sectio %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
sectio$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,sectio$Diagprimer)), "both")
sectio <- sectio %>%
  mutate(Diagnosis = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
sectio$Diagnosis <- as.character(trimws(gsub(";NA|NA;","",sectio$Diagnosis)), "both")
sectio <- sectio %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)

sectio <- sectio %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
sectio <- sectio %>% mutate(Procedure = na_if(Procedure, "-"))
sectio$Diagnosa <- as.character(trimws(sectio$Diagnosa), "both")
sectio$Diagnosis <- as.character(trimws(sectio$Diagnosis), "both")

library(splitstackshape)
sectio1 <- concat.split(sectio, "Diagnosis", ";")
colnames(sectio1)

sectio1$Diagnosis_1 <- as.character(trimws(sectio1$Diagnosis_1))
sectio1$Diagnosis_2 <- as.character(trimws(sectio1$Diagnosis_2))
sectio1$Diagnosis_3 <- as.character(trimws(sectio1$Diagnosis_3))
sectio1$Diagnosis_4 <- as.character(trimws(sectio1$Diagnosis_4))
sectio1$Diagnosis_5 <- as.character(trimws(sectio1$Diagnosis_5))
sectio1$Diagnosis_6 <- as.character(trimws(sectio1$Diagnosis_6))
sectio1$Diagnosis_7 <- as.character(trimws(sectio1$Diagnosis_7))

sectio1$Diagnosis_1 <- with(sectio1,ifelse(is.na(sectio1$Diagnosis_1),NA,Diagnosis_1))
sectio1$Diagnosis_2 <- with(sectio1,ifelse(is.na(sectio1$Diagnosis_2),NA,Diagnosis_2))
sectio1$Diagnosis_3 <- with(sectio1,ifelse(is.na(sectio1$Diagnosis_3),NA,Diagnosis_3))
sectio1$Diagnosis_4 <- with(sectio1,ifelse(is.na(sectio1$Diagnosis_4),NA,Diagnosis_4))
sectio1$Diagnosis_5 <- with(sectio1,ifelse(is.na(sectio1$Diagnosis_5),NA,Diagnosis_5))
sectio1$Diagnosis_6 <- with(sectio1,ifelse(is.na(sectio1$Diagnosis_6),NA,Diagnosis_6))
sectio1$Diagnosis_7 <- with(sectio1,ifelse(is.na(sectio1$Diagnosis_7),NA,Diagnosis_7))

sectio1 <- sectio1 %>%
  mutate(
    Diagnosis_01 = case_when(
      Diagnosis_1 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosis_1 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosis_1 == "O822 - Delivery by caesarean hysterectomy" ~ NA,
      Diagnosis_1 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosis_1 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosis_1 == "Z014 - Gynaecological examination (general)(routine)" ~ NA,
      Diagnosis_1 == "Z016 - Radiological examination, not elsewhere classified" ~ NA,
      Diagnosis_1 == "Z030 - Observation for suspected tuberculosis" ~ NA,
      Diagnosis_1 == "Z031 - Observation for suspected malignant neoplasm" ~ NA,
      Diagnosis_1 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosis_1 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosis_1 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosis_1 == "Z048 - Examination and observation for other specified reasons" ~ NA,
      Diagnosis_1 == "Z080 - Follow-up examination after surgery for malignant neoplasm" ~ NA,
      Diagnosis_1 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosis_1 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosis_1 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosis_1 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosis_1 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosis_1 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosis_1 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosis_1 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosis_1 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosis_1 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosis_1 == "Z209 - Contact with and exposure to unspecified communicable disease" ~ NA,
      Diagnosis_1 == "Z291 - Prophylactic immunotherapy" ~ NA,
      Diagnosis_1 == "Z301 - Insertion of (intrauterine) contraceptive device" ~ NA,
      Diagnosis_1 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosis_1 == "Z348 - Supervision of other normal pregnancy" ~ NA,
      Diagnosis_1 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosis_1 == "Z351 - Supervision of pregnancy with history of abortive outcome" ~ NA,
      Diagnosis_1 == "Z358 - Supervision of other high-risk pregnancies" ~ NA,
      Diagnosis_1 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosis_1 == "Z370 - Single live birth" ~ NA,
      Diagnosis_1 == "Z390 - Care and examination immediately after delivery" ~ NA,
      Diagnosis_1 == "Z392 - Routine postpartum follow-up" ~ NA,
      Diagnosis_1 == "Z419 - Procedure for purposes other than remedying health state, unspecified" ~ NA,
      Diagnosis_1 == "Z429 - Follow-up care involving plastic surgery, unspecified" ~ NA,
      Diagnosis_1 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosis_1 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosis_1 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosis_1 == "Z480 - Attention to surgical dressings and sutures" ~ NA,
      Diagnosis_1 == "Z488 - Other specified surgical follow-up care" ~ NA,
      Diagnosis_1 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosis_1 == "Z490 - Preparatory care for dialysis" ~ NA,
      Diagnosis_1 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosis_1 == "Z492 - Other dialysis" ~ NA,
      Diagnosis_1 == "Z500 - Cardiac rehabilitation" ~ NA,
      Diagnosis_1 == "Z501 - Other physical therapy" ~ NA,
      Diagnosis_1 == "Z502 - Alcohol rehabilitation" ~ NA,
      Diagnosis_1 == "Z503 - Drug rehabilitation" ~ NA,
      Diagnosis_1 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosis_1 == "Z505 - Speech therapy" ~ NA,
      Diagnosis_1 == "Z506 - Orthoptic training" ~ NA,
      Diagnosis_1 == "Z507 - Occupational therapy and vocational rehabilitation, not elsewhere classified" ~ NA,
      Diagnosis_1 == "Z508 - Care involving use of other rehabilitation procedures" ~ NA,
      Diagnosis_1 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosis_1 == "Z510 - Radiotherapy session" ~ NA,
      Diagnosis_1 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosis_1 == "Z512 - Other chemotherapy" ~ NA,
      Diagnosis_1 == "Z513 - Blood transfusion (without reported diagnosis)" ~ NA,
      Diagnosis_1 == "Z514 - Preparatory care for subsequent treatment, not elsewhere classified" ~ NA,
      Diagnosis_1 == "Z515 - Palliative care" ~ NA,
      Diagnosis_1 == "Z516 - Desensitization to allergens" ~ NA,
      Diagnosis_1 == "Z518 - Other specified medical care" ~ NA,
      Diagnosis_1 == "Z519 - Medical care, unspecified" ~ NA,
      Diagnosis_1 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosis_1 == "Z590 - Homelessness" ~ NA,
      Diagnosis_1 == "Z596 - Low income" ~ NA,
      Diagnosis_1 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosis_1 == "Z760 - Issue of repeat prescription" ~ NA,
      Diagnosis_1 == "Z801 - Family history of malignant neoplasm of trachea, bronchus and lung" ~ NA,
      Diagnosis_1 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosis_1 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosis_1 == "Z941 - Heart transplant status" ~ NA,
      Diagnosis_1 == "Z955 - Presence of coronary angioplasty implant and graft" ~ NA,
      Diagnosis_1 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosis_1 == "Z988 - Other specified postsurgical states" ~ NA,
      TRUE ~ Diagnosis_1)) %>%
  select(-Diagnosis_1)

sectio1 <- sectio1 %>%
  mutate(
    Diagnosis_02 = case_when(
      Diagnosis_2 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosis_2 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosis_2 == "O822 - Delivery by caesarean hysterectomy" ~ NA,
      Diagnosis_2 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosis_2 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosis_2 == "Z014 - Gynaecological examination (general)(routine)" ~ NA,
      Diagnosis_2 == "Z016 - Radiological examination, not elsewhere classified" ~ NA,
      Diagnosis_2 == "Z030 - Observation for suspected tuberculosis" ~ NA,
      Diagnosis_2 == "Z031 - Observation for suspected malignant neoplasm" ~ NA,
      Diagnosis_2 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosis_2 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosis_2 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosis_2 == "Z048 - Examination and observation for other specified reasons" ~ NA,
      Diagnosis_2 == "Z080 - Follow-up examination after surgery for malignant neoplasm" ~ NA,
      Diagnosis_2 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosis_2 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosis_2 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosis_2 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosis_2 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosis_2 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosis_2 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosis_2 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosis_2 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosis_2 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosis_2 == "Z209 - Contact with and exposure to unspecified communicable disease" ~ NA,
      Diagnosis_2 == "Z291 - Prophylactic immunotherapy" ~ NA,
      Diagnosis_2 == "Z301 - Insertion of (intrauterine) contraceptive device" ~ NA,
      Diagnosis_2 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosis_2 == "Z348 - Supervision of other normal pregnancy" ~ NA,
      Diagnosis_2 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosis_2 == "Z351 - Supervision of pregnancy with history of abortive outcome" ~ NA,
      Diagnosis_2 == "Z358 - Supervision of other high-risk pregnancies" ~ NA,
      Diagnosis_2 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosis_2 == "Z370 - Single live birth" ~ NA,
      Diagnosis_2 == "Z390 - Care and examination immediately after delivery" ~ NA,
      Diagnosis_2 == "Z392 - Routine postpartum follow-up" ~ NA,
      Diagnosis_2 == "Z419 - Procedure for purposes other than remedying health state, unspecified" ~ NA,
      Diagnosis_2 == "Z429 - Follow-up care involving plastic surgery, unspecified" ~ NA,
      Diagnosis_2 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosis_2 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosis_2 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosis_2 == "Z480 - Attention to surgical dressings and sutures" ~ NA,
      Diagnosis_2 == "Z488 - Other specified surgical follow-up care" ~ NA,
      Diagnosis_2 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosis_2 == "Z490 - Preparatory care for dialysis" ~ NA,
      Diagnosis_2 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosis_2 == "Z492 - Other dialysis" ~ NA,
      Diagnosis_2 == "Z500 - Cardiac rehabilitation" ~ NA,
      Diagnosis_2 == "Z501 - Other physical therapy" ~ NA,
      Diagnosis_2 == "Z502 - Alcohol rehabilitation" ~ NA,
      Diagnosis_2 == "Z503 - Drug rehabilitation" ~ NA,
      Diagnosis_2 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosis_2 == "Z505 - Speech therapy" ~ NA,
      Diagnosis_2 == "Z506 - Orthoptic training" ~ NA,
      Diagnosis_2 == "Z507 - Occupational therapy and vocational rehabilitation, not elsewhere classified" ~ NA,
      Diagnosis_2 == "Z508 - Care involving use of other rehabilitation procedures" ~ NA,
      Diagnosis_2 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosis_2 == "Z510 - Radiotherapy session" ~ NA,
      Diagnosis_2 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosis_2 == "Z512 - Other chemotherapy" ~ NA,
      Diagnosis_2 == "Z513 - Blood transfusion (without reported diagnosis)" ~ NA,
      Diagnosis_2 == "Z514 - Preparatory care for subsequent treatment, not elsewhere classified" ~ NA,
      Diagnosis_2 == "Z515 - Palliative care" ~ NA,
      Diagnosis_2 == "Z516 - Desensitization to allergens" ~ NA,
      Diagnosis_2 == "Z518 - Other specified medical care" ~ NA,
      Diagnosis_2 == "Z519 - Medical care, unspecified" ~ NA,
      Diagnosis_2 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosis_2 == "Z590 - Homelessness" ~ NA,
      Diagnosis_2 == "Z596 - Low income" ~ NA,
      Diagnosis_2 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosis_2 == "Z760 - Issue of repeat prescription" ~ NA,
      Diagnosis_2 == "Z801 - Family history of malignant neoplasm of trachea, bronchus and lung" ~ NA,
      Diagnosis_2 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosis_2 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosis_2 == "Z941 - Heart transplant status" ~ NA,
      Diagnosis_2 == "Z955 - Presence of coronary angioplasty implant and graft" ~ NA,
      Diagnosis_2 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosis_2 == "Z988 - Other specified postsurgical states" ~ NA,
      TRUE ~ Diagnosis_2)) %>%
  select(-Diagnosis_2)

sectio1 <- sectio1 %>%
  mutate(
    Diagnosis_03 = case_when(
      Diagnosis_3 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosis_3 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosis_3 == "O822 - Delivery by caesarean hysterectomy" ~ NA,
      Diagnosis_3 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosis_3 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosis_3 == "Z014 - Gynaecological examination (general)(routine)" ~ NA,
      Diagnosis_3 == "Z016 - Radiological examination, not elsewhere classified" ~ NA,
      Diagnosis_3 == "Z030 - Observation for suspected tuberculosis" ~ NA,
      Diagnosis_3 == "Z031 - Observation for suspected malignant neoplasm" ~ NA,
      Diagnosis_3 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosis_3 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosis_3 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosis_3 == "Z048 - Examination and observation for other specified reasons" ~ NA,
      Diagnosis_3 == "Z080 - Follow-up examination after surgery for malignant neoplasm" ~ NA,
      Diagnosis_3 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosis_3 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosis_3 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosis_3 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosis_3 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosis_3 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosis_3 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosis_3 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosis_3 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosis_3 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosis_3 == "Z209 - Contact with and exposure to unspecified communicable disease" ~ NA,
      Diagnosis_3 == "Z291 - Prophylactic immunotherapy" ~ NA,
      Diagnosis_3 == "Z301 - Insertion of (intrauterine) contraceptive device" ~ NA,
      Diagnosis_3 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosis_3 == "Z348 - Supervision of other normal pregnancy" ~ NA,
      Diagnosis_3 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosis_3 == "Z351 - Supervision of pregnancy with history of abortive outcome" ~ NA,
      Diagnosis_3 == "Z358 - Supervision of other high-risk pregnancies" ~ NA,
      Diagnosis_3 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosis_3 == "Z370 - Single live birth" ~ NA,
      Diagnosis_3 == "Z390 - Care and examination immediately after delivery" ~ NA,
      Diagnosis_3 == "Z392 - Routine postpartum follow-up" ~ NA,
      Diagnosis_3 == "Z419 - Procedure for purposes other than remedying health state, unspecified" ~ NA,
      Diagnosis_3 == "Z429 - Follow-up care involving plastic surgery, unspecified" ~ NA,
      Diagnosis_3 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosis_3 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosis_3 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosis_3 == "Z480 - Attention to surgical dressings and sutures" ~ NA,
      Diagnosis_3 == "Z488 - Other specified surgical follow-up care" ~ NA,
      Diagnosis_3 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosis_3 == "Z490 - Preparatory care for dialysis" ~ NA,
      Diagnosis_3 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosis_3 == "Z492 - Other dialysis" ~ NA,
      Diagnosis_3 == "Z500 - Cardiac rehabilitation" ~ NA,
      Diagnosis_3 == "Z501 - Other physical therapy" ~ NA,
      Diagnosis_3 == "Z502 - Alcohol rehabilitation" ~ NA,
      Diagnosis_3 == "Z503 - Drug rehabilitation" ~ NA,
      Diagnosis_3 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosis_3 == "Z505 - Speech therapy" ~ NA,
      Diagnosis_3 == "Z506 - Orthoptic training" ~ NA,
      Diagnosis_3 == "Z507 - Occupational therapy and vocational rehabilitation, not elsewhere classified" ~ NA,
      Diagnosis_3 == "Z508 - Care involving use of other rehabilitation procedures" ~ NA,
      Diagnosis_3 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosis_3 == "Z510 - Radiotherapy session" ~ NA,
      Diagnosis_3 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosis_3 == "Z512 - Other chemotherapy" ~ NA,
      Diagnosis_3 == "Z513 - Blood transfusion (without reported diagnosis)" ~ NA,
      Diagnosis_3 == "Z514 - Preparatory care for subsequent treatment, not elsewhere classified" ~ NA,
      Diagnosis_3 == "Z515 - Palliative care" ~ NA,
      Diagnosis_3 == "Z516 - Desensitization to allergens" ~ NA,
      Diagnosis_3 == "Z518 - Other specified medical care" ~ NA,
      Diagnosis_3 == "Z519 - Medical care, unspecified" ~ NA,
      Diagnosis_3 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosis_3 == "Z590 - Homelessness" ~ NA,
      Diagnosis_3 == "Z596 - Low income" ~ NA,
      Diagnosis_3 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosis_3 == "Z760 - Issue of repeat prescription" ~ NA,
      Diagnosis_3 == "Z801 - Family history of malignant neoplasm of trachea, bronchus and lung" ~ NA,
      Diagnosis_3 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosis_3 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosis_3 == "Z941 - Heart transplant status" ~ NA,
      Diagnosis_3 == "Z955 - Presence of coronary angioplasty implant and graft" ~ NA,
      Diagnosis_3 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosis_3 == "Z988 - Other specified postsurgical states" ~ NA,
      TRUE ~ Diagnosis_3)) %>%
  select(-Diagnosis_3)

sectio1 <- sectio1 %>%
  mutate(
    Diagnosis_04 = case_when(
      Diagnosis_4 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosis_4 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosis_4 == "O822 - Delivery by caesarean hysterectomy" ~ NA,
      Diagnosis_4 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosis_4 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosis_4 == "Z014 - Gynaecological examination (general)(routine)" ~ NA,
      Diagnosis_4 == "Z016 - Radiological examination, not elsewhere classified" ~ NA,
      Diagnosis_4 == "Z030 - Observation for suspected tuberculosis" ~ NA,
      Diagnosis_4 == "Z031 - Observation for suspected malignant neoplasm" ~ NA,
      Diagnosis_4 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosis_4 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosis_4 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosis_4 == "Z048 - Examination and observation for other specified reasons" ~ NA,
      Diagnosis_4 == "Z080 - Follow-up examination after surgery for malignant neoplasm" ~ NA,
      Diagnosis_4 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosis_4 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosis_4 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosis_4 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosis_4 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosis_4 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosis_4 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosis_4 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosis_4 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosis_4 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosis_4 == "Z209 - Contact with and exposure to unspecified communicable disease" ~ NA,
      Diagnosis_4 == "Z291 - Prophylactic immunotherapy" ~ NA,
      Diagnosis_4 == "Z301 - Insertion of (intrauterine) contraceptive device" ~ NA,
      Diagnosis_4 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosis_4 == "Z348 - Supervision of other normal pregnancy" ~ NA,
      Diagnosis_4 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosis_4 == "Z351 - Supervision of pregnancy with history of abortive outcome" ~ NA,
      Diagnosis_4 == "Z358 - Supervision of other high-risk pregnancies" ~ NA,
      Diagnosis_4 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosis_4 == "Z370 - Single live birth" ~ NA,
      Diagnosis_4 == "Z390 - Care and examination immediately after delivery" ~ NA,
      Diagnosis_4 == "Z392 - Routine postpartum follow-up" ~ NA,
      Diagnosis_4 == "Z419 - Procedure for purposes other than remedying health state, unspecified" ~ NA,
      Diagnosis_4 == "Z429 - Follow-up care involving plastic surgery, unspecified" ~ NA,
      Diagnosis_4 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosis_4 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosis_4 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosis_4 == "Z480 - Attention to surgical dressings and sutures" ~ NA,
      Diagnosis_4 == "Z488 - Other specified surgical follow-up care" ~ NA,
      Diagnosis_4 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosis_4 == "Z490 - Preparatory care for dialysis" ~ NA,
      Diagnosis_4 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosis_4 == "Z492 - Other dialysis" ~ NA,
      Diagnosis_4 == "Z500 - Cardiac rehabilitation" ~ NA,
      Diagnosis_4 == "Z501 - Other physical therapy" ~ NA,
      Diagnosis_4 == "Z502 - Alcohol rehabilitation" ~ NA,
      Diagnosis_4 == "Z503 - Drug rehabilitation" ~ NA,
      Diagnosis_4 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosis_4 == "Z505 - Speech therapy" ~ NA,
      Diagnosis_4 == "Z506 - Orthoptic training" ~ NA,
      Diagnosis_4 == "Z507 - Occupational therapy and vocational rehabilitation, not elsewhere classified" ~ NA,
      Diagnosis_4 == "Z508 - Care involving use of other rehabilitation procedures" ~ NA,
      Diagnosis_4 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosis_4 == "Z510 - Radiotherapy session" ~ NA,
      Diagnosis_4 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosis_4 == "Z512 - Other chemotherapy" ~ NA,
      Diagnosis_4 == "Z513 - Blood transfusion (without reported diagnosis)" ~ NA,
      Diagnosis_4 == "Z514 - Preparatory care for subsequent treatment, not elsewhere classified" ~ NA,
      Diagnosis_4 == "Z515 - Palliative care" ~ NA,
      Diagnosis_4 == "Z516 - Desensitization to allergens" ~ NA,
      Diagnosis_4 == "Z518 - Other specified medical care" ~ NA,
      Diagnosis_4 == "Z519 - Medical care, unspecified" ~ NA,
      Diagnosis_4 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosis_4 == "Z590 - Homelessness" ~ NA,
      Diagnosis_4 == "Z596 - Low income" ~ NA,
      Diagnosis_4 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosis_4 == "Z760 - Issue of repeat prescription" ~ NA,
      Diagnosis_4 == "Z801 - Family history of malignant neoplasm of trachea, bronchus and lung" ~ NA,
      Diagnosis_4 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosis_4 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosis_4 == "Z941 - Heart transplant status" ~ NA,
      Diagnosis_4 == "Z955 - Presence of coronary angioplasty implant and graft" ~ NA,
      Diagnosis_4 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosis_4 == "Z988 - Other specified postsurgical states" ~ NA,
      TRUE ~ Diagnosis_4)) %>%
  select(-Diagnosis_4)

sectio1 <- sectio1 %>%
  mutate(
    Diagnosis_05 = case_when(
      Diagnosis_5 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosis_5 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosis_5 == "O822 - Delivery by caesarean hysterectomy" ~ NA,
      Diagnosis_5 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosis_5 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosis_5 == "Z014 - Gynaecological examination (general)(routine)" ~ NA,
      Diagnosis_5 == "Z016 - Radiological examination, not elsewhere classified" ~ NA,
      Diagnosis_5 == "Z030 - Observation for suspected tuberculosis" ~ NA,
      Diagnosis_5 == "Z031 - Observation for suspected malignant neoplasm" ~ NA,
      Diagnosis_5 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosis_5 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosis_5 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosis_5 == "Z048 - Examination and observation for other specified reasons" ~ NA,
      Diagnosis_5 == "Z080 - Follow-up examination after surgery for malignant neoplasm" ~ NA,
      Diagnosis_5 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosis_5 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosis_5 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosis_5 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosis_5 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosis_5 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosis_5 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosis_5 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosis_5 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosis_5 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosis_5 == "Z209 - Contact with and exposure to unspecified communicable disease" ~ NA,
      Diagnosis_5 == "Z291 - Prophylactic immunotherapy" ~ NA,
      Diagnosis_5 == "Z301 - Insertion of (intrauterine) contraceptive device" ~ NA,
      Diagnosis_5 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosis_5 == "Z348 - Supervision of other normal pregnancy" ~ NA,
      Diagnosis_5 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosis_5 == "Z351 - Supervision of pregnancy with history of abortive outcome" ~ NA,
      Diagnosis_5 == "Z358 - Supervision of other high-risk pregnancies" ~ NA,
      Diagnosis_5 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosis_5 == "Z370 - Single live birth" ~ NA,
      Diagnosis_5 == "Z390 - Care and examination immediately after delivery" ~ NA,
      Diagnosis_5 == "Z392 - Routine postpartum follow-up" ~ NA,
      Diagnosis_5 == "Z419 - Procedure for purposes other than remedying health state, unspecified" ~ NA,
      Diagnosis_5 == "Z429 - Follow-up care involving plastic surgery, unspecified" ~ NA,
      Diagnosis_5 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosis_5 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosis_5 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosis_5 == "Z480 - Attention to surgical dressings and sutures" ~ NA,
      Diagnosis_5 == "Z488 - Other specified surgical follow-up care" ~ NA,
      Diagnosis_5 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosis_5 == "Z490 - Preparatory care for dialysis" ~ NA,
      Diagnosis_5 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosis_5 == "Z492 - Other dialysis" ~ NA,
      Diagnosis_5 == "Z500 - Cardiac rehabilitation" ~ NA,
      Diagnosis_5 == "Z501 - Other physical therapy" ~ NA,
      Diagnosis_5 == "Z502 - Alcohol rehabilitation" ~ NA,
      Diagnosis_5 == "Z503 - Drug rehabilitation" ~ NA,
      Diagnosis_5 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosis_5 == "Z505 - Speech therapy" ~ NA,
      Diagnosis_5 == "Z506 - Orthoptic training" ~ NA,
      Diagnosis_5 == "Z507 - Occupational therapy and vocational rehabilitation, not elsewhere classified" ~ NA,
      Diagnosis_5 == "Z508 - Care involving use of other rehabilitation procedures" ~ NA,
      Diagnosis_5 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosis_5 == "Z510 - Radiotherapy session" ~ NA,
      Diagnosis_5 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosis_5 == "Z512 - Other chemotherapy" ~ NA,
      Diagnosis_5 == "Z513 - Blood transfusion (without reported diagnosis)" ~ NA,
      Diagnosis_5 == "Z514 - Preparatory care for subsequent treatment, not elsewhere classified" ~ NA,
      Diagnosis_5 == "Z515 - Palliative care" ~ NA,
      Diagnosis_5 == "Z516 - Desensitization to allergens" ~ NA,
      Diagnosis_5 == "Z518 - Other specified medical care" ~ NA,
      Diagnosis_5 == "Z519 - Medical care, unspecified" ~ NA,
      Diagnosis_5 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosis_5 == "Z590 - Homelessness" ~ NA,
      Diagnosis_5 == "Z596 - Low income" ~ NA,
      Diagnosis_5 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosis_5 == "Z760 - Issue of repeat prescription" ~ NA,
      Diagnosis_5 == "Z801 - Family history of malignant neoplasm of trachea, bronchus and lung" ~ NA,
      Diagnosis_5 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosis_5 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosis_5 == "Z941 - Heart transplant status" ~ NA,
      Diagnosis_5 == "Z955 - Presence of coronary angioplasty implant and graft" ~ NA,
      Diagnosis_5 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosis_5 == "Z988 - Other specified postsurgical states" ~ NA,
      TRUE ~ Diagnosis_5)) %>%
  select(-Diagnosis_5)

sectio1 <- sectio1 %>%
  mutate(
    Diagnosis_06 = case_when(
      Diagnosis_6 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosis_6 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosis_6 == "O822 - Delivery by caesarean hysterectomy" ~ NA,
      Diagnosis_6 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosis_6 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosis_6 == "Z014 - Gynaecological examination (general)(routine)" ~ NA,
      Diagnosis_6 == "Z016 - Radiological examination, not elsewhere classified" ~ NA,
      Diagnosis_6 == "Z030 - Observation for suspected tuberculosis" ~ NA,
      Diagnosis_6 == "Z031 - Observation for suspected malignant neoplasm" ~ NA,
      Diagnosis_6 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosis_6 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosis_6 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosis_6 == "Z048 - Examination and observation for other specified reasons" ~ NA,
      Diagnosis_6 == "Z080 - Follow-up examination after surgery for malignant neoplasm" ~ NA,
      Diagnosis_6 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosis_6 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosis_6 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosis_6 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosis_6 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosis_6 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosis_6 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosis_6 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosis_6 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosis_6 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosis_6 == "Z209 - Contact with and exposure to unspecified communicable disease" ~ NA,
      Diagnosis_6 == "Z291 - Prophylactic immunotherapy" ~ NA,
      Diagnosis_6 == "Z301 - Insertion of (intrauterine) contraceptive device" ~ NA,
      Diagnosis_6 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosis_6 == "Z348 - Supervision of other normal pregnancy" ~ NA,
      Diagnosis_6 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosis_6 == "Z351 - Supervision of pregnancy with history of abortive outcome" ~ NA,
      Diagnosis_6 == "Z358 - Supervision of other high-risk pregnancies" ~ NA,
      Diagnosis_6 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosis_6 == "Z370 - Single live birth" ~ NA,
      Diagnosis_6 == "Z390 - Care and examination immediately after delivery" ~ NA,
      Diagnosis_6 == "Z392 - Routine postpartum follow-up" ~ NA,
      Diagnosis_6 == "Z419 - Procedure for purposes other than remedying health state, unspecified" ~ NA,
      Diagnosis_6 == "Z429 - Follow-up care involving plastic surgery, unspecified" ~ NA,
      Diagnosis_6 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosis_6 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosis_6 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosis_6 == "Z480 - Attention to surgical dressings and sutures" ~ NA,
      Diagnosis_6 == "Z488 - Other specified surgical follow-up care" ~ NA,
      Diagnosis_6 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosis_6 == "Z490 - Preparatory care for dialysis" ~ NA,
      Diagnosis_6 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosis_6 == "Z492 - Other dialysis" ~ NA,
      Diagnosis_6 == "Z500 - Cardiac rehabilitation" ~ NA,
      Diagnosis_6 == "Z501 - Other physical therapy" ~ NA,
      Diagnosis_6 == "Z502 - Alcohol rehabilitation" ~ NA,
      Diagnosis_6 == "Z503 - Drug rehabilitation" ~ NA,
      Diagnosis_6 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosis_6 == "Z505 - Speech therapy" ~ NA,
      Diagnosis_6 == "Z506 - Orthoptic training" ~ NA,
      Diagnosis_6 == "Z507 - Occupational therapy and vocational rehabilitation, not elsewhere classified" ~ NA,
      Diagnosis_6 == "Z508 - Care involving use of other rehabilitation procedures" ~ NA,
      Diagnosis_6 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosis_6 == "Z510 - Radiotherapy session" ~ NA,
      Diagnosis_6 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosis_6 == "Z512 - Other chemotherapy" ~ NA,
      Diagnosis_6 == "Z513 - Blood transfusion (without reported diagnosis)" ~ NA,
      Diagnosis_6 == "Z514 - Preparatory care for subsequent treatment, not elsewhere classified" ~ NA,
      Diagnosis_6 == "Z515 - Palliative care" ~ NA,
      Diagnosis_6 == "Z516 - Desensitization to allergens" ~ NA,
      Diagnosis_6 == "Z518 - Other specified medical care" ~ NA,
      Diagnosis_6 == "Z519 - Medical care, unspecified" ~ NA,
      Diagnosis_6 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosis_6 == "Z590 - Homelessness" ~ NA,
      Diagnosis_6 == "Z596 - Low income" ~ NA,
      Diagnosis_6 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosis_6 == "Z760 - Issue of repeat prescription" ~ NA,
      Diagnosis_6 == "Z801 - Family history of malignant neoplasm of trachea, bronchus and lung" ~ NA,
      Diagnosis_6 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosis_6 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosis_6 == "Z941 - Heart transplant status" ~ NA,
      Diagnosis_6 == "Z955 - Presence of coronary angioplasty implant and graft" ~ NA,
      Diagnosis_6 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosis_6 == "Z988 - Other specified postsurgical states" ~ NA,
      TRUE ~ Diagnosis_6)) %>%
  select(-Diagnosis_6)

sectio1 <- sectio1 %>%
  mutate(
    Diagnosis_07 = case_when(
      Diagnosis_7 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosis_7 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosis_7 == "O822 - Delivery by caesarean hysterectomy" ~ NA,
      Diagnosis_7 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosis_7 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosis_7 == "Z014 - Gynaecological examination (general)(routine)" ~ NA,
      Diagnosis_7 == "Z016 - Radiological examination, not elsewhere classified" ~ NA,
      Diagnosis_7 == "Z030 - Observation for suspected tuberculosis" ~ NA,
      Diagnosis_7 == "Z031 - Observation for suspected malignant neoplasm" ~ NA,
      Diagnosis_7 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosis_7 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosis_7 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosis_7 == "Z048 - Examination and observation for other specified reasons" ~ NA,
      Diagnosis_7 == "Z080 - Follow-up examination after surgery for malignant neoplasm" ~ NA,
      Diagnosis_7 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosis_7 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosis_7 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosis_7 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosis_7 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosis_7 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosis_7 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosis_7 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosis_7 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosis_7 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosis_7 == "Z209 - Contact with and exposure to unspecified communicable disease" ~ NA,
      Diagnosis_7 == "Z291 - Prophylactic immunotherapy" ~ NA,
      Diagnosis_7 == "Z301 - Insertion of (intrauterine) contraceptive device" ~ NA,
      Diagnosis_7 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosis_7 == "Z348 - Supervision of other normal pregnancy" ~ NA,
      Diagnosis_7 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosis_7 == "Z351 - Supervision of pregnancy with history of abortive outcome" ~ NA,
      Diagnosis_7 == "Z358 - Supervision of other high-risk pregnancies" ~ NA,
      Diagnosis_7 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosis_7 == "Z370 - Single live birth" ~ NA,
      Diagnosis_7 == "Z390 - Care and examination immediately after delivery" ~ NA,
      Diagnosis_7 == "Z392 - Routine postpartum follow-up" ~ NA,
      Diagnosis_7 == "Z419 - Procedure for purposes other than remedying health state, unspecified" ~ NA,
      Diagnosis_7 == "Z429 - Follow-up care involving plastic surgery, unspecified" ~ NA,
      Diagnosis_7 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosis_7 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosis_7 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosis_7 == "Z480 - Attention to surgical dressings and sutures" ~ NA,
      Diagnosis_7 == "Z488 - Other specified surgical follow-up care" ~ NA,
      Diagnosis_7 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosis_7 == "Z490 - Preparatory care for dialysis" ~ NA,
      Diagnosis_7 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosis_7 == "Z492 - Other dialysis" ~ NA,
      Diagnosis_7 == "Z500 - Cardiac rehabilitation" ~ NA,
      Diagnosis_7 == "Z501 - Other physical therapy" ~ NA,
      Diagnosis_7 == "Z502 - Alcohol rehabilitation" ~ NA,
      Diagnosis_7 == "Z503 - Drug rehabilitation" ~ NA,
      Diagnosis_7 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosis_7 == "Z505 - Speech therapy" ~ NA,
      Diagnosis_7 == "Z506 - Orthoptic training" ~ NA,
      Diagnosis_7 == "Z507 - Occupational therapy and vocational rehabilitation, not elsewhere classified" ~ NA,
      Diagnosis_7 == "Z508 - Care involving use of other rehabilitation procedures" ~ NA,
      Diagnosis_7 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosis_7 == "Z510 - Radiotherapy session" ~ NA,
      Diagnosis_7 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosis_7 == "Z512 - Other chemotherapy" ~ NA,
      Diagnosis_7 == "Z513 - Blood transfusion (without reported diagnosis)" ~ NA,
      Diagnosis_7 == "Z514 - Preparatory care for subsequent treatment, not elsewhere classified" ~ NA,
      Diagnosis_7 == "Z515 - Palliative care" ~ NA,
      Diagnosis_7 == "Z516 - Desensitization to allergens" ~ NA,
      Diagnosis_7 == "Z518 - Other specified medical care" ~ NA,
      Diagnosis_7 == "Z519 - Medical care, unspecified" ~ NA,
      Diagnosis_7 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosis_7 == "Z590 - Homelessness" ~ NA,
      Diagnosis_7 == "Z596 - Low income" ~ NA,
      Diagnosis_7 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosis_7 == "Z760 - Issue of repeat prescription" ~ NA,
      Diagnosis_7 == "Z801 - Family history of malignant neoplasm of trachea, bronchus and lung" ~ NA,
      Diagnosis_7 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosis_7 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosis_7 == "Z941 - Heart transplant status" ~ NA,
      Diagnosis_7 == "Z955 - Presence of coronary angioplasty implant and graft" ~ NA,
      Diagnosis_7 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosis_7 == "Z988 - Other specified postsurgical states" ~ NA,
      TRUE ~ Diagnosis_7)) %>%
  select(-Diagnosis_7)

sectio1 <- sectio1 %>%
  mutate(Diagnosa = paste0(as.character(Diagnosis_01),";",
                           as.character(Diagnosis_02),";",
                           as.character(Diagnosis_03),";",
                           as.character(Diagnosis_04),";",
                           as.character(Diagnosis_05),";",
                           as.character(Diagnosis_06),";",
                           as.character(Diagnosis_07)))

sectio1$Diagnosa <- as.character(trimws(gsub("NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;","",sectio1$Diagnosa)),"both")
sectio1 <- sectio1 %>%
  select(-Diagnosis_01,-Diagnosis_02,-Diagnosis_03,-Diagnosis_04,-Diagnosis_05,
         -Diagnosis_06,-Diagnosis_07)
sectio1 <- sectio1 %>% mutate(Diagnosa = na_if(Diagnosa, ""))

sectio2 <- concat.split(sectio1, "Diagnosa", ";")
colnames(sectio2)

sectio3 <- sectio2 %>%
  mutate(
    klasifikasi1 = case_when(
      str_detect(Diagnosa_1, pattern = "O15") ~ "Eklampsia",
      str_detect(Diagnosa_1, pattern = "O32") ~ "Malpresentasi letak",
      str_detect(Diagnosa_1, pattern = "O34") ~ "Bekas SC",
      str_detect(Diagnosa_1, pattern = "O359") ~ "Malformasi janin",
      str_detect(Diagnosa_1, pattern = "O365") ~ "Janin IUGR",
      str_detect(Diagnosa_1, pattern = "O36") ~ "Distres janin",
      str_detect(Diagnosa_1, pattern = "O41") ~ "Oligohidroamnion",
      str_detect(Diagnosa_1, pattern = "O42") ~ "Ketuban pecah dini",
      str_detect(Diagnosa_1, pattern = "O44") ~ "Plasenta previa",
      str_detect(Diagnosa_1, pattern = "O45") ~ "Solusio plasenta",
      str_detect(Diagnosa_1, pattern = "O48") ~ "Kehamilan lewat waktu",
      str_detect(Diagnosa_1, pattern = "O63") ~ "Persalinan lama",
      str_detect(Diagnosa_1, pattern = "O68") ~ "Fetal stress",
      TRUE ~ NA))

sectio3 <- sectio3 %>%
  mutate(
    klasifikasi2 = case_when(
      str_detect(Diagnosa_2, pattern = "O15") ~ "Eklampsia",
      str_detect(Diagnosa_2, pattern = "O32") ~ "Malpresentasi letak",
      str_detect(Diagnosa_2, pattern = "O34") ~ "Bekas SC",
      str_detect(Diagnosa_2, pattern = "O359") ~ "Malformasi janin",
      str_detect(Diagnosa_2, pattern = "O365") ~ "Janin IUGR",
      str_detect(Diagnosa_2, pattern = "O36") ~ "Distres janin",
      str_detect(Diagnosa_2, pattern = "O41") ~ "Oligohidroamnion",
      str_detect(Diagnosa_2, pattern = "O42") ~ "Ketuban pecah dini",
      str_detect(Diagnosa_2, pattern = "O44") ~ "Plasenta previa",
      str_detect(Diagnosa_2, pattern = "O45") ~ "Solusio plasenta",
      str_detect(Diagnosa_2, pattern = "O48") ~ "Kehamilan lewat waktu",
      str_detect(Diagnosa_2, pattern = "O63") ~ "Persalinan lama",
      str_detect(Diagnosa_2, pattern = "O68") ~ "Fetal stress",
      TRUE ~ NA))

sectio3 <- sectio3 %>%
  mutate(
    klasifikasi3 = case_when(
      str_detect(Diagnosa_3, pattern = "O15") ~ "Eklampsia",
      str_detect(Diagnosa_3, pattern = "O32") ~ "Malpresentasi letak",
      str_detect(Diagnosa_3, pattern = "O34") ~ "Bekas SC",
      str_detect(Diagnosa_3, pattern = "O359") ~ "Malformasi janin",
      str_detect(Diagnosa_3, pattern = "O365") ~ "Janin IUGR",
      str_detect(Diagnosa_3, pattern = "O36") ~ "Distres janin",
      str_detect(Diagnosa_3, pattern = "O41") ~ "Oligohidroamnion",
      str_detect(Diagnosa_3, pattern = "O42") ~ "Ketuban pecah dini",
      str_detect(Diagnosa_3, pattern = "O44") ~ "Plasenta previa",
      str_detect(Diagnosa_3, pattern = "O45") ~ "Solusio plasenta",
      str_detect(Diagnosa_3, pattern = "O48") ~ "Kehamilan lewat waktu",
      str_detect(Diagnosa_3, pattern = "O63") ~ "Persalinan lama",
      str_detect(Diagnosa_3, pattern = "O68") ~ "Fetal stress",
      TRUE ~ NA))

sectio3 <- sectio3 %>%
  mutate(
    klasifikasi4 = case_when(
      str_detect(Diagnosa_4, pattern = "O15") ~ "Eklampsia",
      str_detect(Diagnosa_4, pattern = "O32") ~ "Malpresentasi letak",
      str_detect(Diagnosa_4, pattern = "O34") ~ "Bekas SC",
      str_detect(Diagnosa_4, pattern = "O359") ~ "Malformasi janin",
      str_detect(Diagnosa_4, pattern = "O365") ~ "Janin IUGR",
      str_detect(Diagnosa_4, pattern = "O36") ~ "Distres janin",
      str_detect(Diagnosa_4, pattern = "O41") ~ "Oligohidroamnion",
      str_detect(Diagnosa_4, pattern = "O42") ~ "Ketuban pecah dini",
      str_detect(Diagnosa_4, pattern = "O44") ~ "Plasenta previa",
      str_detect(Diagnosa_4, pattern = "O45") ~ "Solusio plasenta",
      str_detect(Diagnosa_4, pattern = "O48") ~ "Kehamilan lewat waktu",
      str_detect(Diagnosa_4, pattern = "O63") ~ "Persalinan lama",
      str_detect(Diagnosa_4, pattern = "O68") ~ "Fetal stress",
      TRUE ~ NA))

sectio3 <- sectio3 %>%
  mutate(
    klasifikasi5 = case_when(
      str_detect(Diagnosa_5, pattern = "O15") ~ "Eklampsia",
      str_detect(Diagnosa_5, pattern = "O32") ~ "Malpresentasi letak",
      str_detect(Diagnosa_5, pattern = "O34") ~ "Bekas SC",
      str_detect(Diagnosa_5, pattern = "O359") ~ "Malformasi janin",
      str_detect(Diagnosa_5, pattern = "O365") ~ "Janin IUGR",
      str_detect(Diagnosa_5, pattern = "O36") ~ "Distres janin",
      str_detect(Diagnosa_5, pattern = "O41") ~ "Oligohidroamnion",
      str_detect(Diagnosa_5, pattern = "O42") ~ "Ketuban pecah dini",
      str_detect(Diagnosa_5, pattern = "O44") ~ "Plasenta previa",
      str_detect(Diagnosa_5, pattern = "O45") ~ "Solusio plasenta",
      str_detect(Diagnosa_5, pattern = "O48") ~ "Kehamilan lewat waktu",
      str_detect(Diagnosa_5, pattern = "O63") ~ "Persalinan lama",
      str_detect(Diagnosa_5, pattern = "O68") ~ "Fetal stress",
      TRUE ~ NA))

sectio3 <- sectio3 %>%
  mutate(
    klasifikasi6 = case_when(
      str_detect(Diagnosa_6, pattern = "O15") ~ "Eklampsia",
      str_detect(Diagnosa_6, pattern = "O32") ~ "Malpresentasi letak",
      str_detect(Diagnosa_6, pattern = "O34") ~ "Bekas SC",
      str_detect(Diagnosa_6, pattern = "O359") ~ "Malformasi janin",
      str_detect(Diagnosa_6, pattern = "O365") ~ "Janin IUGR",
      str_detect(Diagnosa_6, pattern = "O36") ~ "Distres janin",
      str_detect(Diagnosa_6, pattern = "O41") ~ "Oligohidroamnion",
      str_detect(Diagnosa_6, pattern = "O42") ~ "Ketuban pecah dini",
      str_detect(Diagnosa_6, pattern = "O44") ~ "Plasenta previa",
      str_detect(Diagnosa_6, pattern = "O45") ~ "Solusio plasenta",
      str_detect(Diagnosa_6, pattern = "O48") ~ "Kehamilan lewat waktu",
      str_detect(Diagnosa_6, pattern = "O63") ~ "Persalinan lama",
      str_detect(Diagnosa_6, pattern = "O68") ~ "Fetal stress",
      TRUE ~ NA))

sectio3 <- sectio3 %>%
  mutate(
    klasifikasi7 = case_when(
      str_detect(Diagnosa_7, pattern = "O15") ~ "Eklampsia",
      str_detect(Diagnosa_7, pattern = "O32") ~ "Malpresentasi letak",
      str_detect(Diagnosa_7, pattern = "O34") ~ "Bekas SC",
      str_detect(Diagnosa_7, pattern = "O359") ~ "Malformasi janin",
      str_detect(Diagnosa_7, pattern = "O365") ~ "Janin IUGR",
      str_detect(Diagnosa_7, pattern = "O36") ~ "Distres janin",
      str_detect(Diagnosa_7, pattern = "O41") ~ "Oligohidroamnion",
      str_detect(Diagnosa_7, pattern = "O42") ~ "Ketuban pecah dini",
      str_detect(Diagnosa_7, pattern = "O44") ~ "Plasenta previa",
      str_detect(Diagnosa_7, pattern = "O45") ~ "Solusio plasenta",
      str_detect(Diagnosa_7, pattern = "O48") ~ "Kehamilan lewat waktu",
      str_detect(Diagnosa_7, pattern = "O63") ~ "Persalinan lama",
      str_detect(Diagnosa_7, pattern = "O68") ~ "Fetal stress",
      TRUE ~ NA))

sectio3 <- sectio3 %>%
  mutate(klasifikasi = paste0(as.character(klasifikasi1),";",
                              as.character(klasifikasi2),";",
                              as.character(klasifikasi3),";",
                              as.character(klasifikasi4),";",
                              as.character(klasifikasi5),";",
                              as.character(klasifikasi6),";",
                              as.character(klasifikasi7)))

sectio3$klasifikasi <- as.character(trimws(gsub("NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;","",sectio3$klasifikasi )),"both")
sectio3 <- sectio3 %>%
  select(-klasifikasi1,-klasifikasi2,-klasifikasi3,-klasifikasi4,-klasifikasi5,
         -klasifikasi6,-klasifikasi7)
sectio3 <- sectio3 %>% mutate(klasifikasi = na_if(klasifikasi, ""))
sectio4 <- sectio3 %>%
  select(klasifikasi,Nmppklayan,Nokapst,Umur,Jkpst,,Nosjp,Tgldtgsjp,Tglplgsjp,
         Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Nmjnspulang,Biayaverifikasi,Namadpjp)
sectio4 <- sectio4[order(sectio4$Nokapst,sectio4$Tglplgsjp),]

write.xlsx(sectio4, file = "Bahan Audit Medis SC.xlsx")

sectio4 <- concat.split(sectio3, "klasifikasi", ";")
colnames(sectio2)

df %>%
  mutate(col1 = case_when(
    str_detect(col2, pattern = "a") ~ "A"))

stroke2 <- stroke2 %>%
  mutate(
    Komorbid = case_when(
      Diagnosa_1 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_1 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_1 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_1 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_1 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_1 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_1 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_1 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_1 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_1 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_1 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_1 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_1 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_1 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_1 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_1 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_1 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_1 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_1 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_1 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_1 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_1 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_1 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_1 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      Diagnosa_2 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_2 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_2 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_2 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_2 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_2 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_2 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_2 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_2 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_2 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_2 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_2 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_2 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_2 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_2 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_2 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_2 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_2 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_2 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_2 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_2 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_2 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_2 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_2 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      Diagnosa_3 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_3 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_3 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_3 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_3 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_3 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_3 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_3 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_3 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_3 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_3 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_3 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_3 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_3 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_3 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_3 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_3 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_3 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_3 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_3 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_3 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_3 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_3 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_3 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      Diagnosa_4 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_4 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_4 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_4 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_4 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_4 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_4 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_4 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_4 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_4 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_4 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_4 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_4 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_4 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_4 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_4 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_4 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_4 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_4 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_4 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_4 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_4 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_4 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_4 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      Diagnosa_5 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_5 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_5 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_5 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_5 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_5 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_5 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_5 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_5 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_5 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_5 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_5 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_5 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_5 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_5 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_5 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_5 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_5 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_5 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_5 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_5 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_5 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_5 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_5 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      Diagnosa_6 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_6 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_6 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_6 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_6 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_6 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_6 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_6 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_6 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_6 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_6 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_6 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_6 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_6 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_6 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_6 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_6 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_6 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_6 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_6 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_6 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_6 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_6 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_6 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      Diagnosa_7 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_7 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_7 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_7 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_7 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_7 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_7 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_7 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_7 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_7 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_7 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_7 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_7 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_7 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_7 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_7 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_7 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_7 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_7 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_7 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_7 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_7 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_7 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_7 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      Diagnosa_8 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_8 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_8 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_8 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_8 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_8 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_8 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_8 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_8 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_8 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_8 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_8 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_8 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_8 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_8 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_8 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_8 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_8 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_8 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_8 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_8 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_8 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_8 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_8 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      Diagnosa_9 == "E110 - Non-insulin-dependent diabetes mellitus with coma" ~ "DM Tipe II",
      Diagnosa_9 == "E111 - Non-insulin-dependent diabetes mellitus with ketoacidosis" ~ "DM Tipe II",
      Diagnosa_9 == "E112 - Non-insulin-dependent diabetes mellitus with renal complications" ~ "DM Tipe II",
      Diagnosa_9 == "E113 - Non-insulin-dependent diabetes mellitus with ophthalmic complications" ~ "DM Tipe II",
      Diagnosa_9 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "DM Tipe II",
      Diagnosa_9 == "E115 - Non-insulin-dependent diabetes mellitus with peripheral circulatory complications" ~ "DM Tipe II",
      Diagnosa_9 == "E116 - Non-insulin-dependent diabetes mellitus with other specified complications" ~ "DM Tipe II",
      Diagnosa_9 == "E117 - Non-insulin-dependent diabetes mellitus with multiple complications" ~ "DM Tipe II",
      Diagnosa_9 == "E118 - Non-insulin-dependent diabetes mellitus with unspecified complications" ~ "DM Tipe II",
      Diagnosa_9 == "E119 - Non-insulin-dependent diabetes mellitus without complications" ~ "DM Tipe II",
      Diagnosa_9 == "I10 - Essential (primary) hypertension" ~ "Hipertensi",
      Diagnosa_9 == "I110 - Hypertensive heart disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_9 == "I119 - Hypertensive heart disease without (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_9 == "I120 - Hypertensive renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_9 == "I129 - Hypertensive renal disease without renal failure" ~ "Hipertensi",
      Diagnosa_9 == "I130 - Hypertensive heart and renal disease with (congestive) heart failure" ~ "Hipertensi",
      Diagnosa_9 == "I131 - Hypertensive heart and renal disease with renal failure" ~ "Hipertensi",
      Diagnosa_9 == "I132 - Hypertensive heart and renal disease with both (congestive) heart failure and renal failure" ~ "Hipertensi",
      Diagnosa_9 == "I139 - Hypertensive heart and renal disease, unspecified" ~ "Hipertensi",
      Diagnosa_9 == "I150 - Renovascular hypertension" ~ "Hipertensi",
      Diagnosa_9 == "I151 - Hypertension secondary to other renal disorders" ~ "Hipertensi",
      Diagnosa_9 == "I152 - Hypertension secondary to endocrine disorders" ~ "Hipertensi",
      Diagnosa_9 == "I158 - Other secondary hypertension" ~ "Hipertensi",
      Diagnosa_9 == "I159 - Secondary hypertension, unspecified" ~ "Hipertensi",
      TRUE ~ NA))
stroke2 <- stroke2 %>%
  select(-Diagnosa_1,-Diagnosa_2,-Diagnosa_3,-Diagnosa_4,-Diagnosa_5,
         -Diagnosa_6,-Diagnosa_7,-Diagnosa_8,-Diagnosa_9)
stroke2 <- stroke2 %>% mutate(Komorbid = na_if(Komorbid, ""))

write.csv(stroke2, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_stroke rajal.csv",
          na="", row.names = FALSE)
#===============================SKIP======================================

necrosispulp1 <- necrosispulp1 %>% rename(Diagnosa = Diagakhir)

necrosispulp1$Diagnosa <- with(necrosispulp1,
                               ifelse(Diagnosa == "K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K052 - Acute periodontitis",
                                      ifelse(Diagnosa == "K045 - Chronic apical periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K045 - Chronic apical periodontitis",
                                             ifelse(Diagnosa == "K011 - Impacted teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth",
                                                    ifelse(Diagnosa == "S025 - Fracture of tooth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;S025 - Fracture of tooth",
                                                           ifelse(Diagnosa == "K040 - Pulpitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K040 - Pulpitis",
                                                                  ifelse(Diagnosa == "K010 - Embedded teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth",
                                                                         ifelse(Diagnosa == "G442 - Tension-type headache;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;G442 - Tension-type headache",
                                                                                ifelse(Diagnosa == "K053 - Chronic periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K053 - Chronic periodontitis",
                                                                                       ifelse(Diagnosa == "J329 - Chronic sinusitis, unspecified;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;J329 - Chronic sinusitis, unspecified",
                                                                                              ifelse(Diagnosa == "K047 - Periapical abscess without sinus;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K047 - Periapical abscess without sinus",
                                                                                                     ifelse(Diagnosa == "K010 - Embedded teeth;K041 - Necrosis of pulp;K052 - Acute periodontitis", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                            ifelse(Diagnosa == "K010 - Embedded teeth;K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                   ifelse(Diagnosa == "K010 - Embedded teeth;K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                          ifelse(Diagnosa == "K041 - Necrosis of pulp;K052 - Acute periodontitis;K010 - Embedded teeth", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                 ifelse(Diagnosa == "K052 - Acute periodontitis;K041 - Necrosis of pulp;K010 - Embedded teeth", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                        ifelse(Diagnosa == "K052 - Acute periodontitis;K010 - Embedded teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                               ifelse(Diagnosa == "K029 - Dental caries, unspecified;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K029 - Dental caries, unspecified",
                                                                                                                                                      ifelse(Diagnosa == "K054 - Periodontosis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K054 - Periodontosis",
                                                                                                                                                             ifelse(Diagnosa == "K011 - Impacted teeth;K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                    ifelse(Diagnosa == "K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                           ifelse(Diagnosa == "K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth;K011 - Impacted teeth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                  ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp;K011 - Impacted teeth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                         ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K011 - Impacted teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                                ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                                       Diagnosa)))))))))))))))))))))))))

necrosispulp1$Procedure <- as.character(trimws(necrosispulp1$Procedure), "both")
necrosispulp1 <- concat.split(necrosispulp1, "Procedure", ";")
colnames(necrosispulp1)

necrosispulp1$Procedure_1 <- as.character(trimws(necrosispulp1$Procedure_1))
necrosispulp1$Procedure_2 <- as.character(trimws(necrosispulp1$Procedure_2))
necrosispulp1$Procedure_3 <- as.character(trimws(necrosispulp1$Procedure_3))
necrosispulp1$Procedure_4 <- as.character(trimws(necrosispulp1$Procedure_4))

necrosispulp1$Procedure_1 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Procedure_1),NA,Procedure_1))
necrosispulp1$Procedure_2 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Procedure_2),NA,Procedure_2))
necrosispulp1$Procedure_3 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Procedure_3),NA,Procedure_3))
necrosispulp1$Procedure_4 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Procedure_4),NA,Procedure_4))

necrosispulp1 <- necrosispulp1 %>%
  mutate(Procedure_new = paste0(as.character(Procedure_1),";",
                                as.character(Procedure_2),";",
                                as.character(Procedure_3),";",
                                as.character(Procedure_4)))

necrosispulp1$Procedure_new <- as.character(trimws(gsub("NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA",
                                                   "",necrosispulp1$Procedure_new)),"both")
necrosispulp1 <- necrosispulp1 %>%
  select(-Procedure_1,-Procedure_2,-Procedure_3,-Procedure_4)

necrosispulp1$Procedure_new <- with(necrosispulp1,
                                    ifelse(Procedure_new == "2349 - Other dental restoration;2370 - Root canal, not otherwise specified;8712 - Other dental x-ray",
                                           "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                           ifelse(Procedure_new == "2349 - Other dental restoration;8712 - Other dental x-ray;2370 - Root canal, not otherwise specified",
                                                  "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                                  ifelse(Procedure_new == "2370 - Root canal, not otherwise specified;8712 - Other dental x-ray;2349 - Other dental restoration",
                                                         "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                                         ifelse(Procedure_new == "8712 - Other dental x-ray;2370 - Root canal, not otherwise specified;2349 - Other dental restoration",
                                                                "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                                                ifelse(Procedure_new == "8712 - Other dental x-ray;2349 - Other dental restoration;2370 - Root canal, not otherwise specified",
                                                                       "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                                                       ifelse(Procedure_new == "2349 - Other dental restoration;2370 - Root canal, not otherwise specified;8711 - Full-mouth x-ray of teeth",
                                                                              "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                              ifelse(Procedure_new == "2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth;2370 - Root canal, not otherwise specified",
                                                                                     "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                     ifelse(Procedure_new == "2370 - Root canal, not otherwise specified;8711 - Full-mouth x-ray of teeth;2349 - Other dental restoration",
                                                                                            "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                            ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2370 - Root canal, not otherwise specified;2349 - Other dental restoration",
                                                                                                   "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                                   ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2349 - Other dental restoration;2370 - Root canal, not otherwise specified",
                                                                                                          "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                                          ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2349 - Other dental restoration",
                                                                                                                 "2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                                                 ifelse(Procedure_new == "8712 - Other dental x-ray;2349 - Other dental restoration",
                                                                                                                        "2349 - Other dental restoration;8712 - Other dental x-ray",
                                                                                                                        ifelse(Procedure_new == "2349 - Other dental restoration;2370 - Root canal, not otherwise specified",
                                                                                                                               "2370 - Root canal, not otherwise specified;2349 - Other dental restoration",
                                                                                                                               ifelse(Procedure_new == "2349 - Other dental restoration;2371 - Root canal therapy with irrigation",
                                                                                                                                      "2371 - Root canal therapy with irrigation;2349 - Other dental restoration",
                                                                                                                                      ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2370 - Root canal, not otherwise specified",
                                                                                                                                             "2370 - Root canal, not otherwise specified;8711 - Full-mouth x-ray of teeth",
                                                                                                                                             ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2371 - Root canal therapy with irrigation",
                                                                                                                                                    "2371 - Root canal therapy with irrigation;8711 - Full-mouth x-ray of teeth",
                                                                                                                                                    ifelse(Procedure_new == "8712 - Other dental x-ray;2370 - Root canal, not otherwise specified",
                                                                                                                                                           "2370 - Root canal, not otherwise specified;8712 - Other dental x-ray",
                                                                                                                                                           ifelse(Procedure_new == "8712 - Other dental x-ray;2371 - Root canal therapy with irrigation",
                                                                                                                                                                  "2371 - Root canal therapy with irrigation;8712 - Other dental x-ray",
                                                                                                                                                                  Procedure_new)))))))))))))))))))

necrosispulp1 <- necrosispulp1 %>% mutate(Diagnosa = na_if(Diagnosa, ""))
necrosispulp1 <- necrosispulp1 %>% mutate(Procedure_new = na_if(Procedure_new, ""))

necrosispulp1 <- necrosispulp1[order(necrosispulp1$Nokapst,necrosispulp1$Tglplgsjp),]
necrosispulp1 <- necrosispulp1 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
necrosispulp1$jeda <- as.integer(necrosispulp1$Tgldtgsjp) - as.integer(necrosispulp1$tgl_before)
necrosispulp1$jeda <- as.numeric(necrosispulp1$jeda)

necrosispulp1 <- necrosispulp1[order(necrosispulp1$Nokapst,necrosispulp1$Tglplgsjp),]
necrosispulp1 <- necrosispulp1 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))

necrosispulp1 <- necrosispulp1[order(necrosispulp1$Nokapst,necrosispulp1$Tglplgsjp),]
necrosispulp1 <- necrosispulp1 %>%
  group_by(Norjkawalsep) %>%
  mutate(rujuk_ke = sequence(n()))

necrosispulp1$kunjung_ke <- abs(necrosispulp1$kunjung_ke )

necrosispulp1$keterangan <- with(necrosispulp1,
                                 ifelse(is.na(necrosispulp1$Procedure_new),NA,
                                        ifelse(str_detect(Procedure_new,"Root canal|root canal"),"Perawatan saluran akar gigi",
                                               ifelse(str_detect(Procedure_new,"Restoration of tooth by filling"),"Tambal gigi",
                                                      ifelse(str_detect(Procedure_new,"dental x-ray|x-ray of teeth"),"Rontgen gigi",
                                                             ifelse(str_detect(Procedure_new,"dental restoration"),"Perbaikan gigi",
                                                                    ifelse(str_detect(Procedure_new,"Extraction|extraction"),"Pencabutan gigi",
                                                                           ifelse(str_detect(Procedure_new,"Dental examination"),"Pemeriksaan gigi",
                                                                                  ifelse(str_detect(Procedure_new,"Fitting of denture"),"Pemasangan gigi palsu",
                                                                                         "Prosedur lain")))))))))


necrosispulp2 <- necrosispulp1 %>% subset(keterangan  == "Perawatan saluran akar gigi")
necrosispulp2 <- necrosispulp2[order(necrosispulp2$Nokapst,necrosispulp2$Tglplgsjp),]
necrosispulp2 <- necrosispulp2 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))

necrosispulp2$Kel_kunjungan  <- with(necrosispulp2,
                                     ifelse(kunjung_ke > 0 & kunjung_ke <= 5, "Kunjungan 1-5x",
                                            ifelse(kunjung_ke > 10 & kunjung_ke <= 20, "Kunjungan 11-20x",
                                                   ifelse(kunjung_ke > 20 & kunjung_ke <= 30, "Kunjungan 21-30x",
                                                          ifelse(kunjung_ke > 30 & kunjung_ke <= 40, "Kunjungan 31-40x",
                                                                 ifelse(kunjung_ke > 40 & kunjung_ke <= 50, "Kunjungan 41-50x",
                                                                        ifelse(kunjung_ke > 50, "Kunjungan >50x",
                                                                               kunjung_ke)))))))




necrosispulp2 <- necrosispulp2 %>% subset(Kel_kunjungan  != "Kunjungan 1-5x") %>%
  select(Nokapst,Norjkawalsep) %>% unique()
necrosispulp2 <- left_join(necrosispulp2,necrosispulp,by = c("Nokapst"="Nokapst")) %>%
  select(-Norjkawalsep.x,-Jenisppkperujuk,-Typeppkperujuk,-Nmppkperujuk,
         -Tglstjkeu,-Norjkawalsep.y,-Diagflag,-Flag)

necrosispulp2 <- necrosispulp2[order(necrosispulp2$Nokapst,necrosispulp2$Tglplgsjp),]
necrosispulp2 <- necrosispulp2 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
necrosispulp2$jeda <- as.integer(necrosispulp2$Tgldtgsjp) - as.integer(necrosispulp2$tgl_before)
necrosispulp2$jeda <- as.numeric(necrosispulp2$jeda)


necrosispulp2 <- necrosispulp2[order(necrosispulp2$Nokapst,necrosispulp2$Tglplgsjp),]
necrosispulp2 <- necrosispulp2 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))

necrosispulp2$Kel_kunjungan  <- with(necrosispulp2,
                                     ifelse(kunjung_ke > 0 & kunjung_ke <= 5, "Kunjungan 1-5x",
                                            ifelse(kunjung_ke > 5 & kunjung_ke <= 6, "Kunjungan 6x",
                                                   ifelse(kunjung_ke > 6 & kunjung_ke <= 7, "Kunjungan 7x",
                                                          ifelse(kunjung_ke > 7 & kunjung_ke <= 8, "Kunjungan 8x",
                                                                 ifelse(kunjung_ke > 8 & kunjung_ke <= 9, "Kunjungan 9x",
                                                                               ifelse(kunjung_ke > 9 & kunjung_ke <= 10, "Kunjungan 10x",
                                                                                      ifelse(kunjung_ke > 10 & kunjung_ke <= 12, "Kunjungan 11-12x",
                                                                                             ifelse(kunjung_ke > 12 & kunjung_ke <= 15, "Kunjungan 13-15x",
                                                                                                    ifelse(kunjung_ke > 15 & kunjung_ke <= 20, "Kunjungan 16-20x",
                                                                                                           ifelse(kunjung_ke > 20 & kunjung_ke <= 30, "Kunjungan 21-30x",
                                                                                                                  ifelse(kunjung_ke > 30 & kunjung_ke <= 40, "Kunjungan 31-40x",
                                                                                                                         ifelse(kunjung_ke > 40 & kunjung_ke <= 50, "Kunjungan 41-50x",
                                                                                                                                ifelse(kunjung_ke > 50, "Kunjungan >50x",
                                                                                                                                       "Salah"))))))))))))))

necrosispulp2$Kel_kunjungan  <- with(necrosispulp2,
                                     ifelse(kunjung_ke == 1, "Kunjungan 1x",
                                            ifelse(kunjung_ke > 1 & kunjung_ke <= 4, "Kunjungan 2-4x",
                                                   ifelse(kunjung_ke > 4 & kunjung_ke <= 6, "Kunjungan 5-6x",
                                                          ifelse(kunjung_ke > 6 & kunjung_ke <= 8, "Kunjungan 7-8x",
                                                                 ifelse(kunjung_ke > 8 & kunjung_ke <= 10, "Kunjungan 9-10x",
                                                                        ifelse(kunjung_ke > 10 & kunjung_ke <= 12, "Kunjungan 11-12x",
                                                                               ifelse(kunjung_ke > 12 & kunjung_ke <= 15, "Kunjungan 13-15x",
                                                                                      ifelse(kunjung_ke > 15 & kunjung_ke <= 20, "Kunjungan 16-20x",
                                                                                             ifelse(kunjung_ke > 20 & kunjung_ke <= 30, "Kunjungan 21-30x",
                                                                                                    ifelse(kunjung_ke > 30 & kunjung_ke <= 40, "Kunjungan 31-40x",
                                                                                                           ifelse(kunjung_ke > 40 & kunjung_ke <= 50, "Kunjungan 41-50x",
                                                                                                                  ifelse(kunjung_ke > 50, "Kunjungan >50x",
                                                                                                                         "Salah")))))))))))))


cek <- necrosispulp1 %>% subset(kunjung_ke == "Salah")
#kunjungan ke-1
procedure apa saja
#kunjungan ke-2
procedure apa saja dst

%>%
  replace(is.na(.), 0)

nonspes1$Procedure_1 [is.na(nonspes1$Procedure_1)] <- ""
nonspes1$Procedure_2 [is.na(nonspes1$Procedure_2)] <- ""
nonspes1$Procedure_3 [is.na(nonspes1$Procedure_3)] <- ""
nonspes1$Procedure_4 [is.na(nonspes1$Procedure_4)] <- ""
nonspes1$Procedure_5 [is.na(nonspes1$Procedure_5)] <- ""
nonspes1$Procedure_6 [is.na(nonspes1$Procedure_6)] <- ""
nonspes1$Procedure_7 [is.na(nonspes1$Procedure_7)] <- ""
nonspes1$Procedure_8 [is.na(nonspes1$Procedure_8)] <- ""


necrosispulp2$Nokapst <- as.factor(necrosispulp2$Nokapst)
necrosispulp3 <- necrosispulp2 %>%
  dplyr::group_by(Nokapst) %>%
  dplyr::count(n()) %>%
  rename(kunj_noka = n) %>%
  select(kunj_noka)

Procedure_1 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_1) %>% unique() %>% na.omit()
Procedure_2 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_2) %>% unique() %>% na.omit()
Procedure_3 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_3) %>% unique() %>% na.omit()
Procedure_4 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_4) %>% unique() %>% na.omit()
Procedure_5 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_5) %>% unique() %>% na.omit()
Procedure_6 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_6) %>% unique() %>% na.omit()
Procedure_7 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_7) %>% unique() %>% na.omit()
Procedure_8 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_8) %>% unique() %>% na.omit()

colnames(Procedure_1)=colnames(Procedure_2)=colnames(Procedure_3)=colnames(Procedure_4)=colnames(Procedure_5)=colnames(Procedure_6)=colnames(Procedure_7)=colnames(Procedure_8)
Procedure <- rbind(Procedure_1,Procedure_2,Procedure_3,Procedure_4,
                   Procedure_5,Procedure_6,Procedure_7,Procedure_8) %>%
  unique()
write.xlsx(necrosispulp2, file = "K041 lebih dari 5 kali.xlsx")

write.csv(necrosispulp, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_stroke rajal.csv",
          na="", row.names = FALSE)
write.xlsx(necrosispulp3, file = "Normal_curve.xlsx")
#======================================================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
library(excel.link)

refpoli <- xl.read.file("01082020 referensi poli.xlsx",
                        header = TRUE,
                        row.names = NULL,
                        col.names = NULL,
                        xl.sheet = "Sheet1",
                        top.left.cell = "A1",
                        na = "",
                        excel.visible = FALSE) %>%
  select(1,4)

data0 <- left_join(nonspes,refpoli, by = c("Politujsjp"="KDPOLI")) %>%
  select(-Politujsjp)
data0 <- data0 %>% rename(Politujsjp = NMPOLI)
rm(refpoli)
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

data0 <- data0 %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data0 <- data0 %>% mutate(Procedure = na_if(Procedure, "-"))

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("ur_all.rda")

data0 <- data0 %>%
  select(Nosjp,Flagspesialistik,Flagtacc,Jenisppkperujuk,Typeppkperujuk,
         Typeppklayan,Diagnosa,Pstprb,Tmtpstprb,Pstprolanis,
         Tmtpstprolanis) %>% unique()

data0 <- left_join(data0,data) %>%
  select(-CMG,-CBG,-Spec,-Sevel,-tipe)
#===============================SKIP======================================
data0 <- data0 %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data0 <- data0 %>% mutate(Procedure = na_if(Procedure, "-"))
data0$Diagnosa <- with(data0,
                         ifelse(Diagnosa == "I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension",
                                ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                       "E119 - Non-insulin-dependent diabetes mellitus without complications;I119 - Hypertensive heart disease without (congestive) heart failure",
                                       ifelse(Diagnosa == "I119 - Hypertensive heart disease without (congestive) heart failure;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                              "E119 - Non-insulin-dependent diabetes mellitus without complications;I119 - Hypertensive heart disease without (congestive) heart failure",
                                              ifelse(Diagnosa == "K30 - Dyspepsia;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                     "E119 - Non-insulin-dependent diabetes mellitus without complications;K30 - Dyspepsia",
                                                     ifelse(Diagnosa == "E119 - Non-insulin-dependent diabetes mellitus without complications;E785 - Hyperlipidaemia, unspecified;I10 - Essential (primary) hypertension",
                                                            "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                            ifelse(Diagnosa == "I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications;E785 - Hyperlipidaemia, unspecified",
                                                                   "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                                   ifelse(Diagnosa == "I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                          "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                                          ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                 "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                                                 ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension",
                                                                                        "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                                                        ifelse(Diagnosa == "I630 - Cerebral infarction due to thrombosis of precerebral arteries;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                               "E119 - Non-insulin-dependent diabetes mellitus without complications;I630 - Cerebral infarction due to thrombosis of precerebral arteries",
                                                                                               ifelse(Diagnosa == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                                      "E119 - Non-insulin-dependent diabetes mellitus without complications;E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease",
                                                                                                      ifelse(Diagnosa == "I251 - Atherosclerotic heart disease;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                                             "E119 - Non-insulin-dependent diabetes mellitus without complications;I251 - Atherosclerotic heart disease",
                                                                                                             ifelse(Diagnosa == "I633 - Cerebral infarction due to thrombosis of cerebral arteries;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                                                    "E119 - Non-insulin-dependent diabetes mellitus without complications;I633 - Cerebral infarction due to thrombosis of cerebral arteries",
                                                                                                                    Diagnosa))))))))))))))
write.csv(data0, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_E119_DM tanpa komplikasi.csv",
          na="", row.names = FALSE)
#===============================SKIP======================================
#agar bisa SUBSET
data$Kdppklayan <- as.character(trimws(substr(data$Nosjp,1,8)), "both")
data <- data %>%


#========================================================================
write.xlsx(readmisi_5, file = "readmisi_rajal.xlsx")
#========================================================================