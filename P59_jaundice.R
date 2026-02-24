setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

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
data <- data %>%
  select(-CMG,-CBG,-Spec,-Sevel,-tipe)
#========================================================================
TURP <- data %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagnosa,"P59"))

TURP <- TURP %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
TURP <- TURP %>% mutate(Diagsekunder = na_if(Diagsekunder, ""))
TURP <- TURP %>% mutate(Procedure = na_if(Procedure, "-"))
TURP <- TURP %>% mutate(Procedure = na_if(Procedure, ""))

TURP <- TURP %>%
  mutate(
    Kddiagprimer1 = case_when(
      Kddiagprimer == "Z014" ~ NA,
      Kddiagprimer == "Z016" ~ NA,
      Kddiagprimer == "Z030" ~ NA,
      Kddiagprimer == "Z031" ~ NA,
      Kddiagprimer == "Z038" ~ NA,
      Kddiagprimer == "Z039" ~ NA,
      Kddiagprimer == "Z041" ~ NA,
      Kddiagprimer == "Z291" ~ NA,
      Kddiagprimer == "Z340" ~ NA,
      Kddiagprimer == "Z349" ~ NA,
      Kddiagprimer == "Z358" ~ NA,
      Kddiagprimer == "Z359" ~ NA,
      Kddiagprimer == "Z370" ~ NA,
      Kddiagprimer == "Z390" ~ NA,
      Kddiagprimer == "Z392" ~ NA,
      Kddiagprimer == "Z419" ~ NA,
      Kddiagprimer == "Z429" ~ NA,
      Kddiagprimer == "Z488" ~ NA,
      Kddiagprimer == "Z489" ~ NA,
      Kddiagprimer == "Z490" ~ NA,
      Kddiagprimer == "Z491" ~ NA,
      Kddiagprimer == "Z492" ~ NA,
      Kddiagprimer == "Z941" ~ NA,
      Kddiagprimer == "Z501" ~ NA,
      Kddiagprimer == "Z504" ~ NA,
      Kddiagprimer == "Z505" ~ NA,
      Kddiagprimer == "Z507" ~ NA,
      Kddiagprimer == "Z509" ~ NA,
      Kddiagprimer == "Z511" ~ NA,
      Kddiagprimer == "Z549" ~ NA,
      Kddiagprimer == "Z719" ~ NA,
      Kddiagprimer == "Z048" ~ NA,
      Kddiagprimer == "Z080" ~ NA,
      Kddiagprimer == "Z088" ~ NA,
      Kddiagprimer == "Z089" ~ NA,
      Kddiagprimer == "Z090" ~ NA,
      Kddiagprimer == "Z091" ~ NA,
      Kddiagprimer == "Z092" ~ NA,
      Kddiagprimer == "Z093" ~ NA,
      Kddiagprimer == "Z094" ~ NA,
      Kddiagprimer == "Z095" ~ NA,
      Kddiagprimer == "Z096" ~ NA,
      Kddiagprimer == "Z097" ~ NA,
      Kddiagprimer == "Z098" ~ NA,
      Kddiagprimer == "Z099" ~ NA,
      Kddiagprimer == "Z209" ~ NA,
      Kddiagprimer == "Z301" ~ NA,
      Kddiagprimer == "Z348" ~ NA,
      Kddiagprimer == "Z351" ~ NA,
      Kddiagprimer == "Z470" ~ NA,
      Kddiagprimer == "Z478" ~ NA,
      Kddiagprimer == "Z479" ~ NA,
      Kddiagprimer == "Z480" ~ NA,
      Kddiagprimer == "Z500" ~ NA,
      Kddiagprimer == "Z502" ~ NA,
      Kddiagprimer == "Z506" ~ NA,
      Kddiagprimer == "Z508" ~ NA,
      Kddiagprimer == "Z510" ~ NA,
      Kddiagprimer == "Z516" ~ NA,
      Kddiagprimer == "Z590" ~ NA,
      Kddiagprimer == "Z596" ~ NA,
      Kddiagprimer == "Z760" ~ NA,
      Kddiagprimer == "Z801" ~ NA,
      Kddiagprimer == "Z898" ~ NA,
      Kddiagprimer == "Z908" ~ NA,
      Kddiagprimer == "Z955" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z988" ~ NA,
      TRUE ~ Kddiagprimer))
TURP$Nmdiagprimer1 <- with(TURP,
                            ifelse(is.na(TURP$Kddiagprimer1),NA,Nmdiagprimer))
TURP <- TURP %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
TURP$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,TURP$Diagprimer)), "both")
TURP <- TURP %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
TURP$Diagnosa <- as.character(trimws(gsub(";NA|NA;","",TURP$Diagnosa)), "both")

TURP <- TURP %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)

TURP <- TURP %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
TURP <- TURP %>% mutate(Procedure = na_if(Procedure, "-"))
TURP$Diagnosa <- as.character(trimws(TURP$Diagnosa), "both")

library(splitstackshape)
TURP1 <- concat.split(TURP, "Diagnosa", ";")
colnames(TURP1)

TURP1$Diagnosa_1 <- as.character(trimws(TURP1$Diagnosa_1))
TURP1$Diagnosa_2 <- as.character(trimws(TURP1$Diagnosa_2))
TURP1$Diagnosa_3 <- as.character(trimws(TURP1$Diagnosa_3))
TURP1$Diagnosa_4 <- as.character(trimws(TURP1$Diagnosa_4))
TURP1$Diagnosa_5 <- as.character(trimws(TURP1$Diagnosa_5))
TURP1$Diagnosa_6 <- as.character(trimws(TURP1$Diagnosa_6))
TURP1$Diagnosa_7 <- as.character(trimws(TURP1$Diagnosa_7))
TURP1$Diagnosa_8 <- as.character(trimws(TURP1$Diagnosa_8))

TURP1$Diagnosa_1 <- with(TURP1,ifelse(is.na(TURP1$Diagnosa_1),NA,Diagnosa_1))
TURP1$Diagnosa_2 <- with(TURP1,ifelse(is.na(TURP1$Diagnosa_2),NA,Diagnosa_2))
TURP1$Diagnosa_3 <- with(TURP1,ifelse(is.na(TURP1$Diagnosa_3),NA,Diagnosa_3))
TURP1$Diagnosa_4 <- with(TURP1,ifelse(is.na(TURP1$Diagnosa_4),NA,Diagnosa_4))
TURP1$Diagnosa_5 <- with(TURP1,ifelse(is.na(TURP1$Diagnosa_5),NA,Diagnosa_5))
TURP1$Diagnosa_6 <- with(TURP1,ifelse(is.na(TURP1$Diagnosa_6),NA,Diagnosa_6))
TURP1$Diagnosa_7 <- with(TURP1,ifelse(is.na(TURP1$Diagnosa_7),NA,Diagnosa_7))
TURP1$Diagnosa_8 <- with(TURP1,ifelse(is.na(TURP1$Diagnosa_8),NA,Diagnosa_8))

TURP1 <- TURP1 %>%
  mutate(
    Diagnosa_01 = case_when(
      Diagnosa_1 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_1 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_1 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_1 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_1 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_1 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_1 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_1 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_1 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_1 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_1 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_1 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_1 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_1 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_1 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_1 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_1 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_1 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_1 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_1 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_1 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_1 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_1 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_1 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_1 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_1 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_1)) %>%
  select(-Diagnosa_1)

TURP1 <- TURP1 %>%
  mutate(
    Diagnosa_02 = case_when(
      Diagnosa_2 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_2 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_2 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_2 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_2 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_2 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_2 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_2 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_2 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_2 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_2 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_2 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_2 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_2 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_2 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_2 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_2 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_2 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_2 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_2 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_2 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_2 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_2 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_2 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_2 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_2 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_2)) %>%
  select(-Diagnosa_2)

TURP1 <- TURP1 %>%
  mutate(
    Diagnosa_03 = case_when(
      Diagnosa_3 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_3 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_3 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_3 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_3 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_3 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_3 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_3 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_3 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_3 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_3 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_3 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_3 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_3 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_3 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_3 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_3 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_3 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_3 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_3 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_3 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_3 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_3 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_3 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_3 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_3 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_3)) %>%
  select(-Diagnosa_3)

TURP1 <- TURP1 %>%
  mutate(
    Diagnosa_04 = case_when(
      Diagnosa_4 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_4 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_4 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_4 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_4 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_4 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_4 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_4 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_4 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_4 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_4 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_4 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_4 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_4 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_4 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_4 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_4 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_4 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_4 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_4 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_4 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_4 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_4 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_4 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_4 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_4 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_4)) %>%
  select(-Diagnosa_4)

TURP1 <- TURP1 %>%
  mutate(
    Diagnosa_05 = case_when(
      Diagnosa_5 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_5 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_5 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_5 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_5 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_5 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_5 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_5 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_5 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_5 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_5 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_5 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_5 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_5 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_5 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_5 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_5 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_5 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_5 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_5 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_5 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_5 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_5 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_5 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_5 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_5 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_5)) %>%
  select(-Diagnosa_5)

TURP1 <- TURP1 %>%
  mutate(
    Diagnosa_06 = case_when(
      Diagnosa_6 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_6 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_6 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_6 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_6 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_6 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_6 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_6 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_6 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_6 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_6 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_6 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_6 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_6 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_6 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_6 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_6 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_6 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_6 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_6 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_6 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_6 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_6 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_6 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_6 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_6 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_6)) %>%
  select(-Diagnosa_6)

TURP1 <- TURP1 %>%
  mutate(
    Diagnosa_07 = case_when(
      Diagnosa_7 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_7 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_7 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_7 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_7 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_7 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_7 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_7 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_7 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_7 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_7 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_7 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_7 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_7 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_7 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_7 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_7 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_7 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_7 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_7 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_7 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_7 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_7 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_7 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_7 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_7 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_7)) %>%
  select(-Diagnosa_7)

TURP1 <- TURP1 %>%
  mutate(
    Diagnosa_08 = case_when(
      Diagnosa_8 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_8 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_8 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_8 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_8 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_8 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_8 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_8 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_8 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_8 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_8 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_8 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_8 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_8 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_8 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_8 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_8 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_8 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_8 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_8 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_8 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_8 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_8 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_8 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_8 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_8 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_8)) %>%
  select(-Diagnosa_8)

TURP1 <- TURP1 %>%
  mutate(Diagakhir = paste0(as.character(Diagnosa_01),";",
                            as.character(Diagnosa_02),";",
                            as.character(Diagnosa_03),";",
                            as.character(Diagnosa_04),";",
                            as.character(Diagnosa_05),";",
                            as.character(Diagnosa_06),";",
                            as.character(Diagnosa_07),";",
                            as.character(Diagnosa_08)))

TURP1$Diagakhir <- as.character(trimws(gsub(";NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;","",TURP1$Diagakhir)),"both")
TURP1 <- TURP1 %>%
  select(-Diagnosa_01,-Diagnosa_02,-Diagnosa_03,-Diagnosa_04,-Diagnosa_05,
         -Diagnosa_06,-Diagnosa_07,-Diagnosa_08,-Diagnosa)
TURP1 <- TURP1 %>% mutate(Diagakhir = na_if(Diagakhir, ""))
TURP1 <- TURP1 %>% rename(Diagnosa = Diagakhir)

lowbp1$Diagnosa <- with(lowbp1,
                        ifelse(Diagnosa == "M179 - narthrosis, unspecified;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M179 - narthrosis, unspecified",
                                      ifelse(Diagnosa == "M179 - narthrosis, unspecified;M545 - Low back pain", "M545 - Low back pain;M179 - narthrosis, unspecified",
                                             ifelse(Diagnosa == "R252 - Cramp and spasm;M545 - Low back pain", "M545 - Low back pain;R252 - Cramp and spasm",
                                                    ifelse(Diagnosa == "I10 - Essential (primary) hypertension;M5457 - Low back pain, lumbosacral region", "M5457 - Low back pain, lumbosacral region;I10 - Essential (primary) hypertension",
                                                           ifelse(Diagnosa == "I10 - Essential (primary) hypertension;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;I10 - Essential (primary) hypertension",
                                                                  ifelse(Diagnosa == "M1999 - Arthrosis, unspecified, unspecified site;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M1999 - Arthrosis, unspecified, unspecified site",
                                                                         ifelse(Diagnosa == "I10 - Essential (primary) hypertension;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;I10 - Essential (primary) hypertension",
                                                                                ifelse(Diagnosa == "M1996 - Arthrosis, unspecified, lower leg;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M1996 - Arthrosis, unspecified, lower leg",
                                                                                       ifelse(Diagnosa == "M1999 - Arthrosis, unspecified, unspecified site;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M1999 - Arthrosis, unspecified, unspecified site",
                                                                                              ifelse(Diagnosa == "G629 - Polyneuropathy, unspecified;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;G629 - Polyneuropathy, unspecified",
                                                                                                     ifelse(Diagnosa == "G542 - Cervical root disorders, not elsewhere classified;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;G542 - Cervical root disorders, not elsewhere classified",
                                                                                                            ifelse(Diagnosa == "M512 - Other specified intervertebral disc displacement;M545 - Low back pain", "M545 - Low back pain;M512 - Other specified intervertebral disc displacement",
                                                                                                                   ifelse(Diagnosa == "M1386 - Other specified arthritis, lower leg;M5456 - Low back pain, lumbar region", "M5456 - Low back pain, lumbar region;M1386 - Other specified arthritis, lower leg",
                                                                                                                          ifelse(Diagnosa == "M1999 - Arthrosis, unspecified, unspecified site;M5457 - Low back pain, lumbosacral region", "M5457 - Low back pain, lumbosacral region;M1999 - Arthrosis, unspecified, unspecified site",
                                                                                                                                 ifelse(Diagnosa == "M1399 - Arthritis, unspecified, unspecified site;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M1399 - Arthritis, unspecified, unspecified site",
                                                                                                                                        ifelse(Diagnosa == "K30 - Dyspepsia;M545 - Low back pain", "M545 - Low back pain;K30 - Dyspepsia",
                                                                                                                                               ifelse(Diagnosa == "I10 - Essential (primary) hypertension;M545 - Low back pain", "M545 - Low back pain;I10 - Essential (primary) hypertension",
                                                                                                                                                      Diagnosa))))))))))))))))))

TURP1$Procedure <- as.character(trimws(TURP1$Procedure), "both")
TURP1 <- concat.split(TURP1, "Procedure", ";")
colnames(TURP1)

TURP1$Procedure_1 <- as.character(trimws(TURP1$Procedure_1))
TURP1$Procedure_2 <- as.character(trimws(TURP1$Procedure_2))
TURP1$Procedure_3 <- as.character(trimws(TURP1$Procedure_3))
TURP1$Procedure_4 <- as.character(trimws(TURP1$Procedure_4))
TURP1$Procedure_5 <- as.character(trimws(TURP1$Procedure_5))
TURP1$Procedure_6 <- as.character(trimws(TURP1$Procedure_6))
TURP1$Procedure_7 <- as.character(trimws(TURP1$Procedure_7))
TURP1$Procedure_8 <- as.character(trimws(TURP1$Procedure_8))
TURP1$Procedure_9 <- as.character(trimws(TURP1$Procedure_9))

TURP1$Procedure_1 <- with(TURP1,ifelse(is.na(TURP1$Procedure_1),NA,Procedure_1))
TURP1$Procedure_2 <- with(TURP1,ifelse(is.na(TURP1$Procedure_2),NA,Procedure_2))
TURP1$Procedure_3 <- with(TURP1,ifelse(is.na(TURP1$Procedure_3),NA,Procedure_3))
TURP1$Procedure_4 <- with(TURP1,ifelse(is.na(TURP1$Procedure_4),NA,Procedure_4))
TURP1$Procedure_5 <- with(TURP1,ifelse(is.na(TURP1$Procedure_5),NA,Procedure_5))
TURP1$Procedure_6 <- with(TURP1,ifelse(is.na(TURP1$Procedure_6),NA,Procedure_6))
TURP1$Procedure_7 <- with(TURP1,ifelse(is.na(TURP1$Procedure_7),NA,Procedure_7))
TURP1$Procedure_8 <- with(TURP1,ifelse(is.na(TURP1$Procedure_8),NA,Procedure_8))
TURP1$Procedure_9 <- with(TURP1,ifelse(is.na(TURP1$Procedure_9),NA,Procedure_9))

TURP1 <- TURP1 %>%
  mutate(Procedure_new = paste0(as.character(Procedure_1),";",
                                as.character(Procedure_2),";",
                                as.character(Procedure_3),";",
                                as.character(Procedure_4),";",
                                as.character(Procedure_5),";",
                                as.character(Procedure_6),";",
                                as.character(Procedure_7),";",
                                as.character(Procedure_8),";",
                                as.character(Procedure_9)))

TURP1$Procedure_new <- as.character(trimws(gsub("NA;NA;NA;NA;NA;NA;NA;NA;NA|NA;NA;NA;NA;NA;NA;NA;NA|NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA",
                                                   "",TURP1$Procedure_new)),"both")
TURP1 <- TURP1 %>%
  select(-Procedure_1,-Procedure_2,-Procedure_3,-Procedure_4,-Procedure_5,-Procedure_6,-Procedure_7,-Procedure_8,-Procedure_9)

lowbp1$Procedure_new <- with(lowbp1,
                             ifelse(Procedure_new == "9339 - Other physical therapy;9334 - Diathermy", "9334 - Diathermy;9339 - Other physical therapy",
                                    ifelse(Procedure_new == "9335 - Other heat therapy;9339 - Other physical therapy", "9339 - Other physical therapy;9335 - Other heat therapy",
                                           ifelse(Procedure_new == "9339 - Other physical therapy;9334 - Diathermy;9313 - Resistive exercise", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                  ifelse(Procedure_new == "9339 - Other physical therapy;9313 - Resistive exercise;9334 - Diathermy", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                         ifelse(Procedure_new == "9334 - Diathermy;9313 - Resistive exercise;9339 - Other physical therapy", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                                ifelse(Procedure_new == "9313 - Resistive exercise;9334 - Diathermy;9339 - Other physical therapy", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                                              ifelse(Procedure_new == "9313 - Resistive exercise;9339 - Other physical therapy;9334 - Diathermy", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                                                     Procedure_new))))))))

TURP1 <- TURP1 %>% mutate(Diagnosa = na_if(Diagnosa, ""))
TURP1 <- TURP1 %>% mutate(Procedure_new = na_if(Procedure_new, ""))

TURP1 <- TURP1[order(TURP1$Nokapst,TURP1$Tglplgsjp),]
TURP1 <- TURP1 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
TURP1$jeda <- as.integer(TURP1$Tgldtgsjp) - as.integer(TURP1$tgl_before)
TURP1$jeda <- as.numeric(TURP1$jeda)

TURP1 <- TURP1[order(TURP1$Nokapst,TURP1$Tglplgsjp),]
TURP1 <- TURP1 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))

TURP1 <- TURP1[order(TURP1$Nokapst,TURP1$Tglplgsjp),]
TURP1 <- TURP1 %>%
  group_by(Norjkawalsep) %>%
  mutate(rujuk_ke = sequence(n()))

TURP1$kunjung_ke <- abs(TURP1$kunjung_ke )

TURP1$Kel_kunjungan  <- with(TURP1,
                              ifelse(kunjung_ke > 0 & kunjung_ke <= 2, "Kunjungan 1-2x",
                                     ifelse(kunjung_ke > 2 & kunjung_ke <= 4, "Kunjungan 3-4x",
                                            ifelse(kunjung_ke > 4 & kunjung_ke <= 6, "Kunjungan 5-6x",
                                                   ifelse(kunjung_ke > 6 & kunjung_ke <= 8, "Kunjungan 7-8x",
                                                          ifelse(kunjung_ke > 8 & kunjung_ke <= 10, "Kunjungan 9-10x",
                                                                 ifelse(kunjung_ke > 10, "Kunjungan >10x",
                                                                        kunjung_ke)))))))



lowbp1$Kel_kunjungan  <- with(lowbp1,
                              ifelse(kunjung_ke > 0 & kunjung_ke <= 8, "Kunjungan 1-8x",
                                     ifelse(kunjung_ke > 8 & kunjung_ke <= 16, "Kunjungan 9-16x",
                                            ifelse(kunjung_ke > 16 & kunjung_ke <= 24, "Kunjungan 17-24x",
                                                   ifelse(kunjung_ke > 24 & kunjung_ke <= 32, "Kunjungan 25-32x",
                                                          ifelse(kunjung_ke > 32 & kunjung_ke <= 40, "Kunjungan 33-40x",
                                                                 ifelse(kunjung_ke > 40 & kunjung_ke <= 48, "Kunjungan 41-48x",
                                                                        ifelse(kunjung_ke > 48 & kunjung_ke <= 56, "Kunjungan 49-56x",
                                                                               ifelse(kunjung_ke > 56 & kunjung_ke <= 64, "Kunjungan 57-64x",
                                                                                      ifelse(kunjung_ke > 64 & kunjung_ke <= 72, "Kunjungan 65-72x",
                                                                                             ifelse(kunjung_ke > 72 & kunjung_ke <= 80, "Kunjungan 73-80x",
                                                                                                    ifelse(kunjung_ke > 80 & kunjung_ke <= 88, "Kunjungan 81-88x",
                                                                                                           ifelse(kunjung_ke > 88 & kunjung_ke <= 96, "Kunjungan 89-96x",
                                                                                                                  ifelse(kunjung_ke > 96 & kunjung_ke <= 104, "Kunjungan 97-104x",
                                                                                                                         ifelse(kunjung_ke > 104 & kunjung_ke <= 112, "Kunjungan 105-112x",
                                                                                                                                ifelse(kunjung_ke > 112 & kunjung_ke <= 120, "Kunjungan 113-120x",
                                                                                                                                       ifelse(kunjung_ke > 120 & kunjung_ke <= 128, "Kunjungan 121-128x",
                                                                                                                                              ifelse(kunjung_ke > 128 & kunjung_ke <= 136, "Kunjungan 129-136x",
                                                                                                                                                     ifelse(kunjung_ke > 136 & kunjung_ke <= 144, "Kunjungan 137-144x",
                                                                                                                                                            ifelse(kunjung_ke > 144 & kunjung_ke <= 152, "Kunjungan 145-152x",
                                                                                                                                                                   ifelse(kunjung_ke > 152 & kunjung_ke <= 160, "Kunjungan 153-160x",
                                                                                                                                                                          ifelse(kunjung_ke > 160 & kunjung_ke <= 168, "Kunjungan 161-168x",
                                                                                                                                                                                 ifelse(kunjung_ke > 168 & kunjung_ke <= 176, "Kunjungan 169-176x",
                                                                                                                                                                                        ifelse(kunjung_ke > 176 & kunjung_ke <= 184, "Kunjungan 177-184x",
                                                                                                                                                                                               ifelse(kunjung_ke > 184 & kunjung_ke <= 192, "Kunjungan 185-192x",
                                                                                                                                                                                                      ifelse(kunjung_ke > 192 & kunjung_ke <= 200, "Kunjungan 193-200x",
                                                                                                                                                                                                             ifelse(kunjung_ke > 200 & kunjung_ke <= 208, "Kunjungan 201-208x",
                                                                                                                                                                                                                    ifelse(kunjung_ke > 208 & kunjung_ke <= 216, "Kunjungan 209-216x",
                                                                                                                                                                                                                           ifelse(kunjung_ke > 216 & kunjung_ke <= 224, "Kunjungan 217-224x",
                                                                                                                                                                                                                                  ifelse(kunjung_ke > 224 & kunjung_ke <= 232, "Kunjungan 225-232x",
                                                                                                                                                                                                                                         ifelse(kunjung_ke > 232 & kunjung_ke <= 240, "Kunjungan 233-240x",
                                                                                                                                                                                                                                                ifelse(kunjung_ke > 240 & kunjung_ke <= 248, "Kunjungan 241-248x",
                                                                                                                                                                                                                                                       ifelse(kunjung_ke > 248 & kunjung_ke <= 256, "Kunjungan 249-256x",
                                                                                                                                                                                                                                                              ifelse(kunjung_ke > 256 & kunjung_ke <= 264, "Kunjungan 257-264x",
                                                                                                                                                                                                                                                                     ifelse(kunjung_ke > 264 & kunjung_ke <= 272, "Kunjungan 265-272x",
                                                                                                                                                                                                                                                                            ifelse(kunjung_ke > 272 & kunjung_ke <= 280, "Kunjungan 273-280x",
                                                                                                                                                                                                                                                                                   ifelse(kunjung_ke > 280 & kunjung_ke <= 288, "Kunjungan 281-288x",
                                                                                                                                                                                                                                                                                          ifelse(kunjung_ke > 288 & kunjung_ke <= 296, "Kunjungan 289-296x",
                                                                                                                                                                                                                                                                                                 ifelse(kunjung_ke > 296 & kunjung_ke <= 304, "Kunjungan 297-304x",
                                                                                                                                                                                                                                                                                                        ifelse(kunjung_ke > 304 & kunjung_ke <= 312, "Kunjungan 305-312x",
                                                                                                                                                                                                                                                                                                               ifelse(kunjung_ke > 312 & kunjung_ke <= 320, "Kunjungan 313-320x",
                                                                                                                                                                                                                                                                                                                      ifelse(kunjung_ke > 320 & kunjung_ke <= 328, "Kunjungan 321-328x",
                                                                                                                                                                                                                                                                                                                             ifelse(kunjung_ke > 328 & kunjung_ke <= 336, "Kunjungan 329-336x",
                                                                                                                                                                                                                                                                                                                                    ifelse(kunjung_ke > 336 & kunjung_ke <= 344, "Kunjungan 337-344x",
                                                                                                                                                                                                                                                                                                                                           ifelse(kunjung_ke > 344 & kunjung_ke <= 352, "Kunjungan 345-352x",
                                                                                                                                                                                                                                                                                                                                                  ifelse(kunjung_ke > 352 & kunjung_ke <= 360, "Kunjungan 353-360x",
                                                                                                                                                                                                                                                                                                                                                         ifelse(kunjung_ke > 360 & kunjung_ke <= 368, "Kunjungan 361-368x",
                                                                                                                                                                                                                                                                                                                                                                ifelse(kunjung_ke > 368 & kunjung_ke <= 376, "Kunjungan 369-376x",
                                                                                                                                                                                                                                                                                                                                                                       ifelse(kunjung_ke > 376 & kunjung_ke <= 384, "Kunjungan 377-384x",
                                                                                                                                                                                                                                                                                                                                                                              ifelse(kunjung_ke > 384, "Kunjungan >384x",
                                                                                                                                                                                                                                                                                                                                                                                     "Salah"))))))))))))))))))))))))))))))))))))))))))))))))))


lowbp1$asesmen  <- with(lowbp1,
                        ifelse(kunjung_ke > 0 & kunjung_ke <= 8, 1,
                               ifelse(kunjung_ke > 8 & kunjung_ke <= 16, 2,
                                      ifelse(kunjung_ke > 16 & kunjung_ke <= 24, 3,
                                             ifelse(kunjung_ke > 24 & kunjung_ke <= 32, 4,
                                                    ifelse(kunjung_ke > 32 & kunjung_ke <= 40, 5,
                                                           ifelse(kunjung_ke > 40 & kunjung_ke <= 48, 6,
                                                                  ifelse(kunjung_ke > 48 & kunjung_ke <= 56, 7,
                                                                         ifelse(kunjung_ke > 56 & kunjung_ke <= 64, 8,
                                                                                ifelse(kunjung_ke > 64 & kunjung_ke <= 72, 9,
                                                                                       ifelse(kunjung_ke > 72 & kunjung_ke <= 80, 10,
                                                                                              ifelse(kunjung_ke > 80 & kunjung_ke <= 88, 11,
                                                                                                     ifelse(kunjung_ke > 88 & kunjung_ke <= 96, 12,
                                                                                                            ifelse(kunjung_ke > 96 & kunjung_ke <= 104, 13,
                                                                                                                   ifelse(kunjung_ke > 104 & kunjung_ke <= 112, 14,
                                                                                                                          ifelse(kunjung_ke > 112 & kunjung_ke <= 120, 15,
                                                                                                                                 ifelse(kunjung_ke > 120 & kunjung_ke <= 128, 16,
                                                                                                                                        ifelse(kunjung_ke > 128 & kunjung_ke <= 136, 17,
                                                                                                                                               ifelse(kunjung_ke > 136 & kunjung_ke <= 144, 18,
                                                                                                                                                      ifelse(kunjung_ke > 144 & kunjung_ke <= 152, 19,
                                                                                                                                                             ifelse(kunjung_ke > 152 & kunjung_ke <= 160, 20,
                                                                                                                                                                    ifelse(kunjung_ke > 160 & kunjung_ke <= 168, 21,
                                                                                                                                                                           ifelse(kunjung_ke > 168 & kunjung_ke <= 176, 22,
                                                                                                                                                                                  ifelse(kunjung_ke > 176 & kunjung_ke <= 184, 23,
                                                                                                                                                                                         ifelse(kunjung_ke > 184 & kunjung_ke <= 192, 24,
                                                                                                                                                                                                ifelse(kunjung_ke > 192 & kunjung_ke <= 200, 25,
                                                                                                                                                                                                       ifelse(kunjung_ke > 200 & kunjung_ke <= 208, 26,
                                                                                                                                                                                                              ifelse(kunjung_ke > 208 & kunjung_ke <= 216, 27,
                                                                                                                                                                                                                     ifelse(kunjung_ke > 216 & kunjung_ke <= 224, 28,
                                                                                                                                                                                                                            ifelse(kunjung_ke > 224 & kunjung_ke <= 232, 29,
                                                                                                                                                                                                                                   ifelse(kunjung_ke > 232 & kunjung_ke <= 240, 30,
                                                                                                                                                                                                                                          ifelse(kunjung_ke > 240 & kunjung_ke <= 248, 31,
                                                                                                                                                                                                                                                 ifelse(kunjung_ke > 248 & kunjung_ke <= 256, 32,
                                                                                                                                                                                                                                                        ifelse(kunjung_ke > 256 & kunjung_ke <= 264, 33,
                                                                                                                                                                                                                                                               ifelse(kunjung_ke > 264 & kunjung_ke <= 272, 34,
                                                                                                                                                                                                                                                                      ifelse(kunjung_ke > 272 & kunjung_ke <= 280, 35,
                                                                                                                                                                                                                                                                             ifelse(kunjung_ke > 280 & kunjung_ke <= 288, 36,
                                                                                                                                                                                                                                                                                    ifelse(kunjung_ke > 288 & kunjung_ke <= 296, 37,
                                                                                                                                                                                                                                                                                           ifelse(kunjung_ke > 296 & kunjung_ke <= 304, 38,
                                                                                                                                                                                                                                                                                                  ifelse(kunjung_ke > 304 & kunjung_ke <= 312, 39,
                                                                                                                                                                                                                                                                                                         ifelse(kunjung_ke > 312 & kunjung_ke <= 320, 40,
                                                                                                                                                                                                                                                                                                                ifelse(kunjung_ke > 320 & kunjung_ke <= 328, 41,
                                                                                                                                                                                                                                                                                                                       ifelse(kunjung_ke > 328 & kunjung_ke <= 336, 42,
                                                                                                                                                                                                                                                                                                                              ifelse(kunjung_ke > 336 & kunjung_ke <= 344, 43,
                                                                                                                                                                                                                                                                                                                                     ifelse(kunjung_ke > 344 & kunjung_ke <= 352, 44,
                                                                                                                                                                                                                                                                                                                                            ifelse(kunjung_ke > 352 & kunjung_ke <= 360, 45,
                                                                                                                                                                                                                                                                                                                                                   ifelse(kunjung_ke > 360 & kunjung_ke <= 368, 46,
                                                                                                                                                                                                                                                                                                                                                          ifelse(kunjung_ke > 368 & kunjung_ke <= 376, 47,
                                                                                                                                                                                                                                                                                                                                                                 ifelse(kunjung_ke > 376 & kunjung_ke <= 384, 48,
                                                                                                                                                                                                                                                                                                                                                                        ifelse(kunjung_ke > 384, 49,
                                                                                                                                                                                                                                                                                                                                                                               "Salah"))))))))))))))))))))))))))))))))))))))))))))))))))





lowbp1 <- lowbp1[order(lowbp1$Nokapst,lowbp1$Tgldtgsjp),]
lowbp1 <- lowbp1 %>%
  group_by(Nokapst) %>% 
  mutate(pertama = first(Tgldtgsjp))
lowbp1$setahun <- as.Date(lowbp1$pertama) + 365
lowbp1$duatahun <- as.Date(lowbp1$pertama) + 730
lowbp1$tigatahun <- as.Date(lowbp1$pertama) + 1095
lowbp1$empattahun <- as.Date(lowbp1$pertama) + 1460
lowbp1$limatahun <- as.Date(lowbp1$pertama) + 1825
lowbp1$enamtahun <- as.Date(lowbp1$pertama) + 2190
lowbp1$tujuhtahun <- as.Date(lowbp1$pertama) + 2555

lowbp1$tahun  <- with(lowbp1,
                        ifelse(Tgldtgsjp >= pertama & Tgldtgsjp <= setahun,"ke-1",
                               ifelse(Tgldtgsjp >= setahun & Tgldtgsjp <= duatahun, "ke-2",
                                      ifelse(Tgldtgsjp >= duatahun & Tgldtgsjp <= tigatahun, "ke-3",
                                             ifelse(Tgldtgsjp >= tigatahun & Tgldtgsjp <= empattahun, "ke-4",
                                                    ifelse(Tgldtgsjp >= empattahun & Tgldtgsjp <= limatahun, "ke-5",
                                                           ifelse(Tgldtgsjp >= limatahun & Tgldtgsjp <= enamtahun, "ke-6",
                                                                  ifelse(Tgldtgsjp >= enamtahun & Tgldtgsjp <= tujuhtahun, "ke-7",
                                                                         "Salah"))))))))

cek <- lowbp1 %>% subset(tahun == "Salah")

setahun <- lowbp1 %>%
  group_by(Nokapst, .drop = F) %>%
  filter(Tgldtgsjp >= pertama & Tgldtgsjp <= setahun) %>%
  summarise(Kunjungan = (COUNT(Nosjp)))
duatahun <- lowbp1 %>%
  group_by(Nokapst, .drop = F) |>
  filter(Tgldtgsjp >= pertama & Tgldtgsjp <= duatahun) %>%
  summarise(Kunjungan = (COUNT(Nosjp)))
tigatahun <- lowbp1 %>%
  group_by(Nokapst, .drop = F) %>%
  filter(Tgldtgsjp >= pertama & Tgldtgsjp <= tigatahun) %>%
  summarise(Kunjungan = (COUNT(Nosjp)))
empattahun <- lowbp1 %>%
  group_by(Nokapst, .drop = F) |>
  filter(Tgldtgsjp >= pertama & Tgldtgsjp <= empattahun) %>%
  summarise(Kunjungan = (COUNT(Nosjp)))
limatahun <- lowbp1 %>%
  group_by(Nokapst, .drop = F) %>%
  filter(Tgldtgsjp >= pertama & Tgldtgsjp <= limatahun) %>%
  summarise(Kunjungan = (COUNT(Nosjp)))
enamtahun <- lowbp1 %>%
  group_by(Nokapst, .drop = F) |>
  filter(Tgldtgsjp >= pertama & Tgldtgsjp <= enamtahun) %>%
  summarise(Kunjungan = (COUNT(Nosjp)))

biaya=(sum(biaya)))

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


necrosispulp1$Nokapst <- as.factor(necrosispulp1$Nokapst)
necrosispulp2 <- necrosispulp1 %>%
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
write.xlsx(Procedure, file = "Procedure K041.xlsx")

write.csv(TURP1, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_turp.csv",
          na="", row.names = FALSE)
write.xlsx(necrosispulp2, file = "Normal_curve.xlsx")
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
write.csv(TURP, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_jaundice.csv",
          na="", row.names = FALSE)
#===============================SKIP======================================
#agar bisa SUBSET
data$Kdppklayan <- as.character(trimws(substr(data$Nosjp,1,8)), "both")
data <- data %>%


#========================================================================
write.xlsx(readmisi_5, file = "readmisi_rajal.xlsx")
#========================================================================