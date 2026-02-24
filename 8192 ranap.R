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

data19 <- read_csv("Sheet 1_Full Data_kediri 2019.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  filter(str_detect(Procedure,c("8192")))

data19$Politujsjp <- data19$Politujsjp %>%
  modify_if(is.character, toupper)

data20 <- read_csv("Sheet 1_Full Data_kediri 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  filter(str_detect(Procedure,c("8192")))

data20$Politujsjp <- data20$Politujsjp %>%
  modify_if(is.character, toupper)

data21 <- read_csv("Sheet 1_Full Data_kediri 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  filter(str_detect(Procedure,c("8192")))

data21$Politujsjp <- data21$Politujsjp %>%
  modify_if(is.character, toupper)

data22 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2022.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  filter(str_detect(Procedure,c("8192")))

data22$Politujsjp <- data22$Politujsjp %>%
  modify_if(is.character, toupper)

data23 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  filter(str_detect(Procedure,c("8192")))

data23$Politujsjp <- data23$Politujsjp %>%
  modify_if(is.character, toupper)

data24 <- read_csv("Sheet 1_Full Data_kediri 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  filter(str_detect(Procedure,c("8192")))

data24$Politujsjp <- data24$Politujsjp %>%
  modify_if(is.character, toupper)

datamei24 <- read_csv("Sheet 1_Full Data_kediri (2025).csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  filter(str_detect(Procedure,c("8192")))

datamei24$Politujsjp <- datamei24$Politujsjp %>%
  modify_if(is.character, toupper)

injeksi <- rbind(data19,data20,data21,data22,data23,data24,datamei24)
rm(data19,data20,data21,data22,data23,data24,datamei24)

injeksi <- injeksi %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
injeksi <- injeksi %>% mutate(Procedure = na_if(Procedure, "-"))

injeksi$Tgldtgsjp <- as.Date(injeksi$Tgldtgsjp, format = "%m/%d/%Y")
injeksi$Tglplgsjp <- as.Date(injeksi$Tglplgsjp, format = "%m/%d/%Y")
injeksi$Tglpelayanan <- as.Date(injeksi$Tglpelayanan, format = "%m/%d/%Y")

injeksi <- injeksi %>% 
  mutate(
    Nmppklayan = case_when(
      Kdppklayan == "0185R003" ~ "RSU An Nisaa",
      Kdppklayan == "0185R004" ~ "RSU Aulia",
      Kdppklayan == "0185R005" ~ "RSU Ananda",
      Kdppklayan == "0185R008" ~ "RSU Al Ittihad",
      Kdppklayan == "0185R009" ~ "RS Medika Utama",
      Kdppklayan == "0185R010" ~ "RSUD Srengat",
      Kdppklayan == "0185R011" ~ "RS Wava Husada Kesamben",
      Kdppklayan == "0185S001" ~ "KU Puspa Husada",
      Kdppklayan == "0186R002" ~ "RSU Aura Syifa",
      Kdppklayan == "0186R003" ~ "RS Muhammadiyah Siti Khodijah",
      Kdppklayan == "0186R004" ~ "IHC RS Toeloengredjo (HVA)",
      Kdppklayan == "0186R005" ~ "RS Arga Husada",
      Kdppklayan == "0186R007" ~ "RSU Muhammadiyah Surya Melati",
      Kdppklayan == "0186R012" ~ "RSU Wilujeng",
      Kdppklayan == "0186R013" ~ "RS Amelia",
      Kdppklayan == "0186R016" ~ "RSUD Simpang Lima Gumul Kediri",
      Kdppklayan == "0186R014" ~ "RSUD Simpang Lima Gumul Kediri (IGD)",
      Kdppklayan == "0186S001" ~ "KU Rawat Inap Medika Utama",
      Kdppklayan == "0186S002" ~ "KU Rawat Inap Dia Husada",
      Kdppklayan == "0198R004" ~ "RSI Aisyiyah",
      Kdppklayan == "0198S001" ~ "Klinik Mata EDC Warujayeng",
      Kdppklayan == "0198S003" ~ "Klinik Mata Ayu Siwi",
      Kdppklayan == "0210R005" ~ "RS Daha Husada",
      Kdppklayan == "0210R006" ~ "RSU TNI AD Tk. IV DKT",
      Kdppklayan == "0210R007" ~ "RSU Ratih",
      Kdppklayan == "0210R008" ~ "RS Muhammadiyah Ahmad Dahlan",
      Kdppklayan == "0210R009" ~ "RSIA Melinda",
      Kdppklayan == "0210R010" ~ "RS Baptis",
      Kdppklayan == "0210R016" ~ "RSU Lirboyo",
      Kdppklayan == "0210R017" ~ "RSIA Citra Keluarga",
      Kdppklayan == "0210R018" ~ "RSIA Nirmala Kediri",
      Kdppklayan == "0210R019" ~ "RSUD Kilisuci",
      Kdppklayan == "0210R021" ~ "RSGM IIK Bhakti Wiyata",
      Kdppklayan == "0211R004" ~ "RSU Aminah",
      Kdppklayan == "0211R006" ~ "RS Katolik Budi Rahayu",
      Kdppklayan == "0211R008" ~ "RS Syuhada' Haji",
      Kdppklayan == "0211R009" ~ "RSI Aminah",
      Kdppklayan == "1313R001" ~ "RSUD Mardi Waluyo",
      Kdppklayan == "1314R001" ~ "RSUD Ngudi Waluyo",
      Kdppklayan == "1317R001" ~ "RSUD Gambiran",
      Kdppklayan == "1317R003" ~ "RS Bhayangkara Kediri",
      Kdppklayan == "1318R001" ~ "RSUD Kabupaten Kediri",
      Kdppklayan == "1319R001" ~ "RSUD Nganjuk",
      Kdppklayan == "1319R002" ~ "RSD Kertosono",
      Kdppklayan == "1319R003" ~ "RS Bhayangkara Nganjuk",
      TRUE ~ Kdppklayan)) %>%
  select(-Kdppklayan)
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

injeksi <- left_join(injeksi,refpoli, by = c("Politujsjp"="KDPOLI")) %>%
  select(-Politujsjp)
injeksi <- injeksi %>% rename(Politujsjp = NMPOLI)
cek <- injeksi %>%
  subset(is.na(Politujsjp))
rm(refpoli,cek)

injeksi <- injeksi[order(injeksi$Nokapst,injeksi$Tglplgsjp),]
injeksi <- injeksi %>% subset(Tglpelayanan >= "2019-01-01")

injeksi$Namadpjp01 <- trimws(gsub("dr.", "", tolower(injeksi$Namadpjp)),"both")
injeksi$Namadpjp01 <- trimws(gsub("dr", "", injeksi$Namadpjp01),"both")
injeksi$Namadpjp01 <- str_replace_all(injeksi$Namadpjp01, "[^[:alnum:]]", " ")
injeksi$Namadpjp01 <- trimws(gsub("sp", "sp ", injeksi$Namadpjp01),"both")
injeksi$Namadpjp01 <- gsub("\\s+"," ",injeksi$Namadpjp01)
injeksi$Namadpjp <- NULL
injeksi <- injeksi %>% rename(Namadpjp = Namadpjp01)
#======================================================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri")
load("df_pintumsk.rda")
df2 <- df2 %>% select(Nokapst,Nosjp,Norjkawalsep,Sumber,tgl_before,selisih_hari,
                      keterangan,Norjk_ke)
injeksi2 <- left_join(injeksi,df2, by = c("Nosjp"="Nosjp"))

cek <- injeksi2 %>% subset(Nokapst.x != Nokapst.y)
rm(cek)
injeksi2 <- injeksi2 %>%
  select(-Nokapst.y) %>% rename(Nokapst = Nokapst.x)
injeksi <- injeksi2
rm(df2,injeksi2)

injeksi <- injeksi[order(injeksi$Nokapst,injeksi$Tglplgsjp),]
injeksi <- injeksi %>%
  group_by(Nokapst,Norjkawalsep) %>%
  mutate(Poli_asal = first(Politujsjp))

injeksi <- injeksi[order(injeksi$Nokapst,injeksi$Tglplgsjp),]
injeksi <- injeksi %>%
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
      Kddiagprimer == "Z873" ~ NA,
      Kddiagprimer == "Z877" ~ NA,
      Kddiagprimer == "Z878" ~ NA,
      Kddiagprimer == "Z883" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z966" ~ NA,
      Kddiagprimer == "Z992" ~ NA,
      Kddiagprimer == "Z967" ~ NA, 
      TRUE ~ Kddiagprimer))

injeksi$Nmdiagprimer1 <- with(injeksi,
                                   ifelse(is.na(injeksi$Kddiagprimer1),NA,Nmdiagprimer))
injeksi <- injeksi %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
injeksi$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,injeksi$Diagprimer)), "both")
injeksi <- injeksi %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
injeksi$Diagnosa <- as.character(trimws(gsub(";NA|NA;","",injeksi$Diagnosa)), "both")

injeksi <- injeksi %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)

injeksi <- injeksi %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
injeksi <- injeksi %>% mutate(Procedure = na_if(Procedure, "-"))
injeksi$Diagnosa <- as.character(trimws(injeksi$Diagnosa), "both")

library(splitstackshape)
injeksi1 <- concat.split(injeksi, "Diagnosa", ";")
colnames(injeksi1)

injeksi1$Diagnosa_1 <- as.character(trimws(injeksi1$Diagnosa_1))
injeksi1$Diagnosa_2 <- as.character(trimws(injeksi1$Diagnosa_2))
injeksi1$Diagnosa_3 <- as.character(trimws(injeksi1$Diagnosa_3))
injeksi1$Diagnosa_4 <- as.character(trimws(injeksi1$Diagnosa_4))
injeksi1$Diagnosa_5 <- as.character(trimws(injeksi1$Diagnosa_5))
injeksi1$Diagnosa_6 <- as.character(trimws(injeksi1$Diagnosa_6))
injeksi1$Diagnosa_7 <- as.character(trimws(injeksi1$Diagnosa_7))
injeksi1$Diagnosa_8 <- as.character(trimws(injeksi1$Diagnosa_8))

injeksi1$Diagnosa_1 <- with(injeksi1,ifelse(is.na(injeksi1$Diagnosa_1),NA,Diagnosa_1))
injeksi1$Diagnosa_2 <- with(injeksi1,ifelse(is.na(injeksi1$Diagnosa_2),NA,Diagnosa_2))
injeksi1$Diagnosa_3 <- with(injeksi1,ifelse(is.na(injeksi1$Diagnosa_3),NA,Diagnosa_3))
injeksi1$Diagnosa_4 <- with(injeksi1,ifelse(is.na(injeksi1$Diagnosa_4),NA,Diagnosa_4))
injeksi1$Diagnosa_5 <- with(injeksi1,ifelse(is.na(injeksi1$Diagnosa_5),NA,Diagnosa_5))
injeksi1$Diagnosa_6 <- with(injeksi1,ifelse(is.na(injeksi1$Diagnosa_6),NA,Diagnosa_6))
injeksi1$Diagnosa_7 <- with(injeksi1,ifelse(is.na(injeksi1$Diagnosa_7),NA,Diagnosa_7))
injeksi1$Diagnosa_8 <- with(injeksi1,ifelse(is.na(injeksi1$Diagnosa_8),NA,Diagnosa_8))


injeksi1 <- injeksi1 %>%
  mutate(
    Diagnosa_01 = case_when(
      Diagnosa_1 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_1 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_1 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_1 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_1 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_1 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
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
      Diagnosa_1 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_1 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_1 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_1 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_1 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_1 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_1 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_1 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_1 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_1 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_1 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_1 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_1 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_1 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_1 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_1 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_1 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_1 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_1 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_1 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_1 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_1 == "Z713 - Dietary counselling and surveillance" ~ NA,
            TRUE ~ Diagnosa_1)) %>%
  select(-Diagnosa_1)

injeksi1 <- injeksi1 %>%
  mutate(
    Diagnosa_02 = case_when(
      Diagnosa_2 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_2 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_2 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_2 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_2 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_2 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
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
      Diagnosa_2 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_2 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_2 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_2 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_2 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_2 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_2 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_2 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_2 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_2 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_2 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_2 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_2 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_2 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_2 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_2 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_2 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_2 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_2 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_2 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_2 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_2 == "Z713 - Dietary counselling and surveillance" ~ NA,
      TRUE ~ Diagnosa_2)) %>%
  select(-Diagnosa_2)

injeksi1 <- injeksi1 %>%
  mutate(
    Diagnosa_03 = case_when(
      Diagnosa_3 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_3 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_3 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_3 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_3 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_3 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
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
      Diagnosa_3 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_3 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_3 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_3 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_3 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_3 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_3 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_3 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_3 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_3 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_3 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_3 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_3 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_3 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_3 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_3 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_3 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_3 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_3 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_3 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_3 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_3 == "Z713 - Dietary counselling and surveillance" ~ NA,
      TRUE ~ Diagnosa_3)) %>%
  select(-Diagnosa_3)

injeksi1 <- injeksi1 %>%
  mutate(
    Diagnosa_04 = case_when(
      Diagnosa_4 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_4 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_4 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_4 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_4 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_4 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
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
      Diagnosa_4 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_4 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_4 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_4 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_4 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_4 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_4 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_4 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_4 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_4 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_4 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_4 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_4 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_4 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_4 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_4 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_4 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_4 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_4 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_4 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_4 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_4 == "Z713 - Dietary counselling and surveillance" ~ NA,
      TRUE ~ Diagnosa_4)) %>%
  select(-Diagnosa_4)

injeksi1 <- injeksi1 %>%
  mutate(
    Diagnosa_05 = case_when(
      Diagnosa_5 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_5 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_5 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_5 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_5 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_5 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
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
      Diagnosa_5 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_5 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_5 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_5 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_5 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_5 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_5 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_5 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_5 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_5 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_5 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_5 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_5 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_5 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_5 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_5 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_5 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_5 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_5 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_5 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_5 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_5 == "Z713 - Dietary counselling and surveillance" ~ NA,
      TRUE ~ Diagnosa_5)) %>%
  select(-Diagnosa_5)

injeksi1 <- injeksi1 %>%
  mutate(
    Diagnosa_06 = case_when(
      Diagnosa_6 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_6 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_6 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_6 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_6 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_6 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
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
      Diagnosa_6 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_6 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_6 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_6 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_6 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_6 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_6 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_6 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_6 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_6 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_6 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_6 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_6 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_6 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_6 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_6 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_6 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_6 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_6 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_6 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_6 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_6 == "Z713 - Dietary counselling and surveillance" ~ NA,
      TRUE ~ Diagnosa_6)) %>%
  select(-Diagnosa_6)

injeksi1 <- injeksi1 %>%
  mutate(
    Diagnosa_07 = case_when(
      Diagnosa_7 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_7 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_7 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_7 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_7 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_7 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
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
      Diagnosa_7 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_7 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_7 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_7 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_7 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_7 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_7 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_7 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_7 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_7 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_7 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_7 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_7 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_7 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_7 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_7 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_7 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_7 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_7 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_7 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_7 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_7 == "Z713 - Dietary counselling and surveillance" ~ NA,
      TRUE ~ Diagnosa_7)) %>%
  select(-Diagnosa_7)

injeksi1 <- injeksi1 %>%
  mutate(
    Diagnosa_08 = case_when(
      Diagnosa_8 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_8 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_8 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_8 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_8 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_8 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
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
      Diagnosa_8 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_8 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_8 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_8 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_8 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_8 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_8 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_8 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_8 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_8 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_8 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_8 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_8 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_8 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_8 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_8 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_8 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_8 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_8 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_8 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_8 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_8 == "Z713 - Dietary counselling and surveillance" ~ NA,
      TRUE ~ Diagnosa_8)) %>%
  select(-Diagnosa_8)

injeksi1 <- injeksi1 %>%
  mutate(Diagakhir = paste0(as.character(Diagnosa_01),";",
                            as.character(Diagnosa_02),";",
                            as.character(Diagnosa_03),";",
                            as.character(Diagnosa_04),";",
                            as.character(Diagnosa_05),";",
                            as.character(Diagnosa_06),";",
                            as.character(Diagnosa_07),";",
                            as.character(Diagnosa_08)))

injeksi1$Diagakhir <- as.character(trimws(gsub(";NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;|NA","",injeksi1$Diagakhir)),"both")
injeksi1 <- injeksi1 %>%
  select(-Diagnosa_01,-Diagnosa_02,-Diagnosa_03,-Diagnosa_04,-Diagnosa_05,
         -Diagnosa_06,-Diagnosa_07,-Diagnosa_08,-Diagnosa)
injeksi1 <- injeksi1 %>% mutate(Diagakhir = na_if(Diagakhir, ""))
injeksi1 <- injeksi1 %>% rename(Diagnosa = Diagakhir)

injeksi1 <- injeksi1[order(injeksi1$Nokapst,injeksi1$Tglplgsjp),]
injeksi1 <- injeksi1 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))

injeksi1$Kel_kunjungan  <- with(injeksi1,
                                ifelse(kunjung_ke > 0 & kunjung_ke <= 4, "Kunjungan 1-4x",
                                       ifelse(kunjung_ke > 4 & kunjung_ke <= 6, "Kunjungan 4-6x",
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

injeksi1 <- injeksi1[order(injeksi1$Nokapst,injeksi1$Tglplgsjp),]
injeksi1 <- injeksi1 %>% group_by(Nokapst,Norjkawalsep) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
injeksi1$jeda <- as.integer(injeksi1$Tgldtgsjp) - as.integer(injeksi1$tgl_before)

injeksi1$selisih_hari  <- with(injeksi1,
                               ifelse(jeda <= 90 & jeda > 0, jeda,NA))

injeksi1$selisih_hari <- as.numeric(injeksi1$selisih_hari)

write.csv(injeksi1, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_injeksi.csv",
          na="", row.names = FALSE)

inj_ia_rinapsjp <- inj_ia_rinap %>%
  select(Nosjp)
inj_ia_rinapsjp$Keterangan <- "audit anomali 7"

data1 <- left_join(data1,inj_ia_rinapsjp, by = c("Nosjp"="Nosjp"))

data1$`Audit Tematik` <- "Injeksi intraartikular seharusnya ditagihkan rawat jalan"
data1$Keterangan <- with(data1,
                         ifelse(is.na(Keterangan),"tambahan KC Gresik",Keterangan))

data1 <- data1[order(data1$Tglplgsjp),]

data1 <- data1 %>%
  select(`Audit Tematik`,Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppklayan,Nmtkp,
         Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,Nmjnspulang,
         Biayaverifikasi,Keterangan,Keterangan)

write.xlsx(data1, file = "injeksi intraartikular inap.xlsx")



data1 <- data %>% subset(Tglpelayanan >= "2021-01-01")

data1$Potensi <- "jika komplikasi DM maka tidak boleh I20-I25"

setwd("D:/Downloads")

nosjp <- read.xlsx(xlsxFile = "Nosjp.xlsx",
                      sheet = "Sheet1",
                      colNames = TRUE,
                      rowNames = FALSE,
                      detectDates = TRUE,
                      fillMergedCells = TRUE)

nosjp$Keterangan <- "Sudah audit MKU Tahap III"


df_data <- left_join(data1,nosjp, by = c("Nosjp"="Nosjp"))

df_data <- df_data[order(df_data$Nokapst,df_data$Tgldtgsjp),]
df_data <- df_data %>%
  select(Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppklayan,Tglpelayanan,
         Tgldtgsjp,Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Biayaverifikasi,
         Potensi)
write.xlsx(df_data, file = "peny jantung iskemik.xlsx")

data$Nminacbgs <- as.character(trimws(data$Nminacbgs), "both")

data <- data %>% group_by(Nokapst) %>%
  mutate(Keterangan = paste0("operasi ke-", sequence(n())))



write.xlsx(data, file = "db katarak.xlsx")

df_des20 <- read_csv("Sheet_1_Full_Data_data gresik des 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jan21 <- read_csv("Sheet_1_Full_Data_data gresik jan 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_feb21 <- read_csv("Sheet_1_Full_Data_data gresik feb 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_mar21 <- read_csv("Sheet_1_Full_Data_data gresik mar 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_apr21 <- read_csv("Sheet_1_Full_Data_data gresik apr 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_mei21 <- read_csv("Sheet_1_Full_Data_data gresik mei 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jun21 <- read_csv("Sheet_1_Full_Data_data gresik jun 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jul21 <- read_csv("Sheet_1_Full_Data_data gresik jul 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_agu21 <- read_csv("Sheet_1_Full_Data_data gresik agu 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_sep21 <- read_csv("Sheet_1_Full_Data_data gresik sep 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_okt21 <- read_csv("Sheet_1_Full_Data_data gresik okt 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_nov21 <- read_csv("Sheet_1_Full_Data_data gresik nov 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_des21 <- read_csv("Sheet_1_Full_Data_data gresik des 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jan22 <- read_csv("Sheet_1_Full_Data_data gresik jan 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_feb22 <- read_csv("Sheet_1_Full_Data_data gresik feb 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_mar22 <- read_csv("Sheet_1_Full_Data_data gresik mar 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_apr22 <- read_csv("Sheet_1_Full_Data_data gresik apr 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_mei22 <- read_csv("Sheet_1_Full_Data_data gresik mei 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jun22 <- read_csv("Sheet_1_Full_Data_data gresik jun 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jul22 <- read_csv("Sheet_1_Full_Data_data gresik jul 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_agu22 <- read_csv("Sheet_1_Full_Data_data gresik agu 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_sep22 <- read_csv("Sheet 1_Full Data_data gresik sep 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_okt22 <- read_csv("Sheet 1_Full Data_data gresik okt 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_nov22 <- read_csv("Sheet 1_Full Data_data gresik nov 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_des22 <- read_csv("Sheet 1_Full Data_data gresik des 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jan23 <- read_csv("Sheet_1_Full_Data_data gresik jan 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_feb23 <- read_csv("Sheet_1_Full_Data_data gresik feb 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_mar23 <- read_csv("Sheet_1_Full_Data_data gresik mar 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_apr23 <- read_csv("Sheet_1_Full_Data_data gresik apr 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_mei23 <- read_csv("Sheet_1_Full_Data_data gresik mei 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jun23 <- read_csv("Sheet_1_Full_Data_data gresik jun 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jul23 <- read_csv("Sheet_1_Full_Data_data gresik jul 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_agu23 <- read_csv("Sheet_1_Full_Data_data gresik agu 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_sep23 <- read_csv("Sheet_1_Full_Data_data gresik sep 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_okt23 <- read_csv("Sheet_1_Full_Data_data gresik okt 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_nov23 <- read_csv("Sheet 1_Full Data_data gresik nov 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_des23 <- read_csv("Sheet 1_Full Data_data gresik des 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_jan24 <- read_csv("Sheet 1_Full Data_data gresik jan 24.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,biayars,Biayaverifikasi,
          Nmtkp,Politujsjp) %>%
  subset(Nmtkp == "RJTL" & Politujsjp == "MAT"
         & Biayaverifikasi <300000) %>%
  select(-Nmtkp,-Politujsjp)

df_kontrol <- rbind(df_des20,
                    df_jan21, df_feb21, df_mar21, df_apr21, df_mei21, df_jun21,
                    df_jul21, df_agu21, df_sep21, df_okt21, df_nov21, df_des21,
                    df_jan22, df_feb22, df_mar22, df_apr22, df_mei22, df_jun22,
                    df_jul22, df_agu22, df_sep22, df_okt22, df_nov22, df_des22,
                    df_jan23, df_feb23, df_mar23, df_apr23, df_mei23, df_jun23,
                    df_jul23, df_agu23, df_sep23, df_okt23, df_nov23, df_des23,
                    df_jan24)

rm(df_des20,
   df_jan21, df_feb21, df_mar21, df_apr21, df_mei21,df_jun21,
   df_jul21, df_agu21, df_sep21, df_okt21, df_nov21, df_des21,
   df_jan22, df_feb22, df_mar22, df_apr22, df_mei22,df_jun22,
   df_jul22, df_agu22, df_sep22, df_okt22, df_nov22, df_des22,
   df_jan23, df_feb23, df_mar23, df_apr23, df_mei23,df_jun23,
   df_jul23, df_agu23, df_sep23, df_okt23, df_nov23, df_des23,
   df_jan24)

df_kontrol$keterangan <- "Kontrol"

df_kontrol$Tgldtgsjp <- as.Date(df_kontrol$Tgldtgsjp, format = "%m/%d/%Y")
df_kontrol$Tglplgsjp <- as.Date(df_kontrol$Tglplgsjp, format = "%m/%d/%Y")

#mencari pasien fakoemulsifikasi
datanokapst <- data %>%
  dplyr::group_by(Nokapst)
datanokapst <- datanokapst %>% select(Nokapst)

#mencari pasien kontrol post fako
df_kontrolfako <- left_join(datanokapst, df_kontrol, by = "Nokapst")
df_kontrolfako <- df_kontrolfako %>% drop_na(Nosjp)

df_kontrolfakonoka <- df_kontrolfako %>% select(Nokapst)
df_kontrolfakonoka <- df_kontrolfakonoka[!(duplicated(df_kontrolfakonoka) |
                                             duplicated(df_kontrolfakonoka, fromLast = TRUE)), ]

#mencari pasien kontrol pada data fako (fako saja)
datafakokontrol <- left_join(df_kontrolfakonoka, data,
                             by = "Nokapst")

#menggabungkan fako & kontrol
datafinal <- rbind(datafakokontrol,df_kontrolfako)
datafinal <- datafinal[order(datafinal$Nokapst,
                             datafinal$Tglplgsjp),]

rm(datafakokontrol,datanokapst,df_kontrolfako,df_kontrolfakonoka)

datafinal <- datafinal %>%
  group_by(Nokapst) %>%
  mutate(interval = Tglplgsjp - lag(Tglplgsjp))

datafinal$interval <- as.numeric(datafinal$interval) %>%
  replace(is.na(.), 0)

datafinal01 <- datafinal %>% group_by(Nokapst) %>%
  mutate(keterangan1= dplyr::lead(keterangan, order_by=Tglplgsjp,
                                  n = 1, default = NA))
datafinal01 <- datafinal01 %>% group_by(Nokapst) %>%
  mutate(keterangan2= dplyr::lead(keterangan, order_by=Tglplgsjp,
                                  n = 2, default = NA))
datafinal01 <- datafinal01 %>% group_by(Nokapst) %>%
  mutate(keterangan3= dplyr::lead(keterangan, order_by=Tglplgsjp,
                                  n = 3, default = NA))
datafinal01 <- datafinal01 %>% group_by(Nokapst) %>%
  mutate(keterangan4= dplyr::lead(keterangan, order_by=Tglplgsjp,
                                  n = 4, default = NA))

datafinal01$keterangan5 <- paste0(
  as.character(datafinal01$keterangan),"-",
  as.character(datafinal01$keterangan1),"-",
  as.character(datafinal01$keterangan2),"-",
  as.character(datafinal01$keterangan3),"-",
  as.character(datafinal01$keterangan4))

datafinal02 <- datafinal01 %>%
  filter(str_detect(keterangan5,c("Fakoemulsifikasi")))
datafinal02 <- datafinal02[order(datafinal02$Nokapst,
                                 datafinal02$Tglplgsjp),]
datafinal02 <- datafinal02 %>%
  group_by(Nokapst) %>% 
  mutate(keterangan0 = sequence(n()))

datafinal01 <- datafinal |>
  arrange(Nokapst,Tglplgsjp) |>
  group_by(Nokapst) |>
  mutate(keterangan1 = lag(keterangan))
datafinal01 <- datafinal01 |>
  arrange(Nokapst,Tglplgsjp) |>
  group_by(Nokapst) |>
  mutate(keterangan2 = lag(keterangan1))
datafinal01 <- datafinal01 |>
  arrange(Nokapst,Tglplgsjp) |>
  group_by(Nokapst) |>
  mutate(keterangan3 = lag(keterangan2))







datafinal01 <- datafinal01 %>%
  group_by(Nokapst,grp = cumsum(keterangan == "Fakoemulsifikasi")) %>%
  mutate(new = first(Tglplgsjp)) %>%
  ungroup %>%
  mutate(new = replace(lag(new), seq_len(match(1, grp)), NA)) %>%
  select(-grp)

datafinal01 <- datafinal %>%
  group_by(Nokapst,Tglplgsjp, keterangan = "Fakoemulsifikasi") %>% 
  mutate(keterangan2 = sequence(n()))

datafinal01 <- datafinal01[order(datafinal01$Nokapst,
                               datafinal01$Tglplgsjp),]

write.xlsx(data, file = "Odontektomi.xlsx")

#8. kasus dirujuk dengan over biaya RS
data <- data %>%
  filter(str_detect(Nmtkp,c("RITL"))) %>%
  filter(str_detect(Nmjnspulang,c("Rujuk")))%>%
  group_by(Nokapst) %>%
  mutate(netbiaya = Biayaverifikasi-biayars)
data$netbiaya <- as.integer(ifelse(data$netbiaya <= 0, 1, 0))

#7. kasus fako pbpu mandiri sebelum satu bulan
%>%
  filter(str_detect(Nmjnspulang,c("Rujuk"))) %>%
  filter(str_detect(Procedure,c("1341|1371|992")))

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$LOS <- as.integer(data$Tglplgsjp) - as.integer(data$Tgldtgsjp) +1

#Transform Nmtkp menjadi 1&2 --> 1= berarti RITL, 2= berarti RJTL
data$Nmtkp[data$Nmtkp=="RITL"] <- "1"
data$Nmtkp[data$Nmtkp=="RJTL"] <- "2"
data$Nmtkp <- as.factor(data$Nmtkp)

#Transform Jkpst menjadi 1&2 --> 1= berarti Laki-laki, 2= berarti Perempuan
data$Jkpst[data$Jkpst=="Laki-laki"] <- "1"
data$Jkpst[data$Jkpst=="Perempuan"] <- "2"
data$Jkpst <- as.factor(data$Jkpst)

#Transform Kelashak menjadi 1,2,3
data$Kelashak[data$Kelashak=="Kelas I"] <- "1"
data$Kelashak[data$Kelashak=="Kelas II"] <- "2"
data$Kelashak[data$Kelashak=="Kelas III"] <- "3"
data$Kelashak <- as.factor(data$Kelashak)

#Transform Klsrawat menjadi 1,2,3
data$Klsrawat[data$Klsrawat=="Kelas I"] <- "1"
data$Klsrawat[data$Klsrawat=="Kelas II"] <- "2"
data$Klsrawat[data$Klsrawat=="Kelas III"] <- "3"
data$Klsrawat <- as.factor(data$Klsrawat)
#========================================================================
data <- data %>% rename(DM = Kddiagmasuk,
                        NmDM = Nmdiagmasuk,
                        Poli = Politujsjp,
                        Sumber = Sumberkunjungan,
                        SEP = Nosjp,
                        DU =  Kddiagprimer, 
                        NmDU = Nmdiagprimer,
                        DS = Diagsekunder,
                        Start = Tgldtgsjp,
                        End = Tglplgsjp,
                        Pulang = Nmjnspulang,
                        BiayaRS = biayars,
                        Biaya = Biayaverifikasi)

colnames(data)
glimpse(data)
str(data)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
load("datasep.rda")
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

dfcm <- left_join(data, df, by = c("SEP"="Nosep")) %>%
  select(-Sumber.y) %>%
  rename(Sumber = Sumber.x)
rm(data,df)

data <- data[order(data$Nokapst, data$End),]
dfcm <- dfcm %>%
  group_by(Nokapst) %>%
  mutate(Prvtkp = lag(Nmtkp))

dfcm$Prvtkp <- as.numeric(dfcm$Prvtkp) %>%
  replace(is.na(.), "")
dfcm$Prvtkp <- as.factor(dfcm$Prvtkp)

dfcm <- dfcm[order(dfcm$Nokapst, dfcm$End),]
dfcm <- dfcm %>%
  group_by(Nokapst) %>%
  mutate(interval = End - lag(End))

dfcm$interval <- as.numeric(dfcm$interval) %>%
  replace(is.na(.), "")
dfcm$interval <- as.factor(dfcm$interval)

dfcm <- dfcm %>% subset(Tglpelayanan >= "2021-12-01")

dfcm$Namadpjp01 <- trimws(gsub("dr.", "", tolower(dfcm$Namadpjp)),"both")
dfcm$Namadpjp01 <- trimws(gsub("dr", "", dfcm$Namadpjp01),"both")
dfcm$Namadpjp01 <- str_replace_all(dfcm$Namadpjp01, "[^[:alnum:]]", " ")
dfcm$Namadpjp01 <- trimws(gsub("sp", "sp ", dfcm$Namadpjp01),"both")
dfcm$Namadpjp01 <- gsub("\\s+"," ",dfcm$Namadpjp01)
dfcm <- dfcm %>% rename(DPJP = Namadpjp01)
dfcm$Namadpjp <- NULL
#========================================================================

#CMG, Specific CBG, SEVERITY
dfcm$CMG <- as.character(trimws(substr(dfcm$Kdinacbgs,1,1)), "both")
dfcm$CBG <- as.character(trimws(substr(dfcm$Kdinacbgs,1,6)), "both")
dfcm$Spec <- as.character(trimws(substr(dfcm$Kdinacbgs,5,6)), "both")
dfcm$Kdsevel <- as.character(trimws(substr(dfcm$Kdinacbgs,8,10)), "both")
dfcm <- dfcm %>% 
  mutate(
    Sevel = case_when(
      Kdsevel == "0" ~ "0",
      Kdsevel == "I" ~ "1",
      Kdsevel == "II" ~ "2",
      Kdsevel == "III" ~ "3",
      TRUE ~ Kdsevel)) %>%
  select(-Kdsevel) 

dfcm$tp <- as.character(trimws(substr(dfcm$Kdinacbgs,3,3)), "both")
dfcm <- data.frame(dfcm)
dfcm <- dfcm %>% 
  mutate(
    tipe = case_when(
      tp == "1" ~ "Prosedur Rawat Inap",
      tp == "2" ~ "Prosedur Besar Rawat Jalan",
      tp == "3" ~ "Prosedur Signifikan Rawat Jalan",
      tp == "4" ~ "Rawat Inap Bukan Prosedur",
      tp == "5" ~ "Rawat Jalan Bukan Prosedur",
      tp == "6" ~ "Rawat Inap Kebidanan",
      tp == "7" ~ "Rawat Jalan Kebidanan",
      tp == "8" ~ "Rawat Inap Neonatal",
      tp == "9" ~ "Rawat Jalan Neonatal",
      tp == "0" ~ "Error",
      TRUE ~ tp)) %>%
  select(-tp)

library(splitstackshape)
dfcm <- concat.split(dfcm, "DS", ";")
colnames(dfcm)

dfcm$DS01 <- as.character(trimws(gsub("-","",substr(dfcm$DS_1,1,6)), "both"))
dfcm$DS01 [is.na(dfcm$DS01)] <- ""
dfcm$DS01 <- as.factor(dfcm$DS01)
dfcm$DS02 <- as.character(trimws(gsub("-","",substr(dfcm$DS_2,1,6)), "both"))
dfcm$DS02 [is.na(dfcm$DS02)] <- ""
dfcm$DS02 <- as.factor(dfcm$DS02)
dfcm$DS03 <- as.character(trimws(gsub("-","",substr(dfcm$DS_3,1,6)), "both"))
dfcm$DS03 [is.na(dfcm$DS03)] <- ""
dfcm$DS03 <- as.factor(dfcm$DS03)
dfcm$DS04 <- as.character(trimws(gsub("-","",substr(dfcm$DS_4,1,6)), "both"))
dfcm$DS04 [is.na(dfcm$DS04)] <- ""
dfcm$DS04 <- as.factor(dfcm$DS04)
dfcm$DS05 <- as.character(trimws(gsub("-","",substr(dfcm$DS_5,1,6)), "both"))
dfcm$DS05 [is.na(dfcm$DS05)] <- ""
dfcm$DS05 <- as.factor(dfcm$DS05)
dfcm$DS06 <- as.character(trimws(gsub("-","",substr(dfcm$DS_6,1,6)), "both"))
dfcm$DS06 [is.na(dfcm$DS06)] <- ""
dfcm$DS06 <- as.factor(dfcm$DS06)
dfcm$DS07 <- as.character(trimws(gsub("-","",substr(dfcm$DS_7,1,6)), "both"))
dfcm$DS07 [is.na(dfcm$DS07)] <- ""
dfcm$DS07 <- as.factor(dfcm$DS07)
#======================================================================
dfcm <- concat.split(dfcm, "Procedure", ";")

colnames(dfcm)

dfcm$Proc01 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_1,1,4)), "both"))
dfcm$Proc01 [is.na(dfcm$Proc01)] <- ""
dfcm$Proc01 <- as.factor(dfcm$Proc01)
dfcm$Proc02 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_2,1,4)), "both"))
dfcm$Proc02 [is.na(dfcm$Proc02)] <- ""
dfcm$Proc02 <- as.factor(dfcm$Proc02)
dfcm$Proc03 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_3,1,4)), "both"))
dfcm$Proc03 [is.na(dfcm$Proc03)] <- ""
dfcm$Proc03 <- as.factor(dfcm$Proc03)
dfcm$Proc04 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_4,1,4)), "both"))
dfcm$Proc04 [is.na(dfcm$Proc04)] <- ""
dfcm$Proc04 <- as.factor(dfcm$Proc04)

##DATA_03 = Gabung DIAG & PROC
#remove empty cell with NA
#============================================================================
library(readxl)
setwd("D:/data gresik/UR/Data Sampel BPJS Kesehatan 2015-2020 v01/Metadata")
DIAGFKTP <- read_excel("Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx", 
                       sheet = "Sheet1", range = NULL, 
                       col_names = TRUE, col_types = NULL, na = "",
                       trim_ws = TRUE, skip = 0)
colnames(dfcm)
dfcm00 <- dfcm %>%
  mutate(DP = as.factor(substr(dfcm$DU,1,3)),
         DS_01 = as.factor(substr(dfcm$DS01,1,3)),
         DS_02 = as.factor(substr(dfcm$DS02,1,3)),
         DS_03 = as.factor(substr(dfcm$DS03,1,3)),
         DS_04 = as.factor(substr(dfcm$DS04,1,3)),
         DS_05 = as.factor(substr(dfcm$DS05,1,3)),
         DS_06 = as.factor(substr(dfcm$DS06,1,3)),
         DS_07 = as.factor(substr(dfcm$DS07,1,3)))
rm(dfcm)

dfcm00$DIAG3 <- paste(dfcm00$DP,dfcm00$DS_01,dfcm00$DS_02,dfcm00$DS_03,dfcm00$DS_04,
                      dfcm00$DS_05,dfcm00$DS0_6,sep=" ")
dfcm00$DIAG3 <- trimws(dfcm00$DIAG3, whitespace = " ")
dfcm00$DIAG3 <- gsub(" ",", ",dfcm00$DIAG3)
dfcm00$DIAG3 <- sapply(dfcm00$DIAG3,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
PROC <- read.csv2("sub chapter proc.csv",
                  colClasses=c("factor", "character", "character"))
glimpse(PROC)

dfcm01 <- dfcm00 %>%
  mutate(Proc_01 = as.factor(substr(dfcm00$Proc01,1,2)),
         Proc_02 = as.factor(substr(dfcm00$Proc02,1,2)),
         Proc_03 = as.factor(substr(dfcm00$Proc03,1,2)),
         Proc_04 = as.factor(substr(dfcm00$Proc04,1,2)))
rm(dfcm00)

dfcm01$PROC2 <- paste(dfcm01$Proc_01,dfcm01$Proc_02,dfcm01$Proc_03,
                      dfcm01$Proc_04,sep=" ")
dfcm01$PROC2 <- trimws(dfcm01$PROC2, whitespace = " ")
dfcm01$PROC2 <- gsub(" ",", ",dfcm01$PROC2)
dfcm01$PROC2 <- sapply(dfcm01$PROC2,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

PROC2 <- read.csv2("Chapter Procedure.csv", 
                   colClasses=c("factor", "character",
                                "character", "character"))

dfcm02 <- dfcm01 %>%
  mutate(Proc_001 = as.factor(substr(dfcm01$Proc01,1,3)),
         Proc_002 = as.factor(substr(dfcm01$Proc02,1,3)),
         Proc_003 = as.factor(substr(dfcm01$Proc03,1,3)),
         Proc_004 = as.factor(substr(dfcm01$Proc04,1,3)))
rm(dfcm01)

dfcm02$PROC3 <- paste(dfcm02$Proc_001,dfcm02$Proc_002,dfcm02$Proc_003,dfcm02$Proc_004,
                      dfcm02$Proc_005,dfcm02$Proc_006,dfcm02$Proc_007,sep=" ")
dfcm02$PROC3 <- trimws(dfcm02$PROC3, whitespace = " ")
dfcm02$PROC3 <- gsub(" ",", ",dfcm02$PROC3)
dfcm02$PROC3 <- sapply(dfcm02$PROC3,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

dfcm02 <- dfcm02 %>%
  select(-DP,-DS_01,-DS_02,-DS_03,-DS_04,-DS_05,-DS_06,-DS_07,-Proc_01,
         -Proc_02,-Proc_03,-Proc_04,-Proc_001,-Proc_002,-Proc_003,-Proc_004)
dfcm02 <- concat.split(dfcm02, "DIAG3", ", ")
dfcm02 <- concat.split(dfcm02, "PROC2", ", ")
dfcm02 <- concat.split(dfcm02, "PROC3", ", ")
colnames(dfcm02)
#===========================================================================
#Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx

dfcm03 <- left_join(dfcm02, DIAGFKTP, by = c("DIAG3_1"="ICD10_Code")) %>%
  select(-DIAG3_1, -FKP14) %>%
  rename(DIAG3_1 = ICD10_Text)
dfcm03$DIAG3_1 [is.na(dfcm03$DIAG3_1)] <- ""
rm(dfcm02)
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_2"="ICD10_Code")) %>%
  select(-DIAG3_2, -FKP14) %>%
  rename(DIAG3_2 = ICD10_Text)
dfcm03$DIAG3_2 [is.na(dfcm03$DIAG3_2)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_3"="ICD10_Code")) %>%
  select(-DIAG3_3, -FKP14) %>%
  rename(DIAG3_3 = ICD10_Text)
dfcm03$DIAG3_3 [is.na(dfcm03$DIAG3_3)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_4"="ICD10_Code")) %>%
  select(-DIAG3_4, -FKP14) %>%
  rename(DIAG3_4 = ICD10_Text)
dfcm03$DIAG3_4 [is.na(dfcm03$DIAG3_4)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_5"="ICD10_Code")) %>%
  select(-DIAG3_5, -FKP14) %>%
  rename(DIAG3_5 = ICD10_Text)
dfcm03$DIAG3_5 [is.na(dfcm03$DIAG3_5)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_6"="ICD10_Code")) %>%
  select(-DIAG3_6, -FKP14) %>%
  rename(DIAG3_6 = ICD10_Text)
dfcm03$DIAG3_6 [is.na(dfcm03$DIAG3_6)] <- ""
#---------------------------------------------------------------------------
#Kode ICD9CM untuk Procedure Sub Chapter

dfcm03$PROC2_1 <- factor(dfcm03$PROC2_1)
dfcm03$PROC2_2 <- factor(dfcm03$PROC2_2)
dfcm03$PROC2_3 <- factor(dfcm03$PROC2_3)

dfcm04 <- left_join(dfcm03, PROC,
                  by = c("PROC2_1"="No.Sub.Chap")) %>%
  select(-PROC2_1, -Chap.Proc) %>%
  rename(PROC2_1 = Sub.Chap.Proc)
dfcm03$Proc_01 [is.na(dfcm03$Proc_01)] <- ""
rm(dfcm03)

dfcm04 <- left_join(dfcm04, PROC,
                    by = c("PROC2_2"="No.Sub.Chap")) %>%
  select(-PROC2_2, -Chap.Proc) %>%
  rename(PROC2_2 = Sub.Chap.Proc)
dfcm04$PROC2_2 [is.na(dfcm04$PROC2_2)] <- ""
dfcm04 <- left_join(dfcm04, PROC,
                    by = c("PROC2_3"="No.Sub.Chap")) %>%
  select(-PROC2_3, -Chap.Proc) %>%
  rename(PROC2_3 = Sub.Chap.Proc)
dfcm04$PROC2_3 [is.na(dfcm04$PROC2_3)] <- ""
#---------------------------------------------------------------------------
#Kode ICD9CM untuk Procedure Chapter

dfcm04$PROC3_1 <- factor(dfcm04$PROC3_1)
dfcm04$PROC3_2 <- factor(dfcm04$PROC3_2)
dfcm04$PROC3_3 <- factor(dfcm04$PROC3_3)
dfcm04$PROC3_4 <- factor(dfcm04$PROC3_4)

dfcm05 <- left_join(dfcm04, PROC2, by = c("PROC3_1"="No.Sub.Chap")) %>%
  select(-PROC3_1, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_1 = Sub.Chap.Proc)
dfcm05$PROC3_1 [is.na(dfcm05$PROC3_1)] <- ""
rm(dfcm04)

dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_2"="No.Sub.Chap")) %>%
  select(-PROC3_2, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_2 = Sub.Chap.Proc)
dfcm05$PROC3_2 [is.na(dfcm05$PROC3_2)] <- ""
dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_3"="No.Sub.Chap")) %>%
  select(-PROC3_3, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_3 = Sub.Chap.Proc)
dfcm05$PROC3_3 [is.na(dfcm05$PROC3_3)] <- ""
dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_4"="No.Sub.Chap")) %>%
  select(-PROC3_4, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_4 = Sub.Chap.Proc)
dfcm05$PROC3_4 [is.na(dfcm05$PROC3_4)] <- ""

dfcm05 <- dfcm05 %>%
  mutate(DIAGP = as.factor(substr(dfcm05$DU,1,3)))

rm(DIAGFKTP,PROC,PROC2)

dfcm05 <- dfcm05 %>%
  select(-DS01,-DS02,-DS03,-DS04,-DS05,-DS06,-DS07,-Proc01,-Proc02,
         -Proc03,-Proc04)
dfcm05 <- dfcm05[order(dfcm05$Nokapst, dfcm05$End),]

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
save(dfcm05, file="dfcm05.rda")

write.csv(dfcm05, "D:/data gresik/MTF KC GRESIK/dfcm.csv",
          na="", row.names = FALSE)


data$DIAGSEK <- paste(data$DS01,data$DS02,data$DS03,data$DS04,
                      data$DS05,data$DS06,data$DS07,data$DS08,
                      data$DS09,data$DS10,data$DS11,data$DS12,
                      sep=", ")
data$DIAGSEK <- gsub("NA, ","",data$DIAGSEK)
data$DIAGSEK <- gsub(", NA","",data$DIAGSEK)
data$DIAGSEK <- gsub("NA","",data$DIAGSEK)
data$DIAGSEK <- sapply(data$DIAGSEK, 
                    function(x) paste(unique(unlist(str_split(x,", "))), 
                                      collapse = ", "))
#=================================================================
#--------------------------LEFT JOIN------------------------------
#=================================================================

colnames(data)
data00 <- data %>%
  mutate(DP = as.factor(substr(data$DU,1,3)),
         DS_01 = as.factor(substr(data$DS01,1,3)),
         DS_02 = as.factor(substr(data$DS02,1,3)),
         DS_03 = as.factor(substr(data$DS03,1,3)),
         DS_04 = as.factor(substr(data$DS04,1,3)),
         DS_05 = as.factor(substr(data$DS05,1,3)),
         DS_06 = as.factor(substr(data$DS06,1,3)),
         DS_07 = as.factor(substr(data$DS07,1,3)),
         DS_08 = as.factor(substr(data$DS08,1,3)),
         DS_09 = as.factor(substr(data$DS09,1,3)),
         DS_10 = as.factor(substr(data$DS10,1,3)),
         DS_11 = as.factor(substr(data$DS11,1,3)),
         DS_12 = as.factor(substr(data$DS12,1,3)))

rm(data)

data00$DIAG3 <- paste(data00$DP,data00$DS_01,data00$DS_02,data00$DS_03, 
                   data00$DS_04,data00$DS_05,data00$DS_06,data00$DS_07,
                   data00$DS_08,data00$DS_09,data00$DS_10,data00$DS_11,
                   data00$DS_12,
                   sep=", ")
data00$DIAG3 <- gsub("NA, ","",data00$DIAG3)
data00$DIAG3 <- gsub(", NA","",data00$DIAG3)
data00$DIAG3 <- sapply(data00$DIAG3, 
                    function(x) paste(unique(unlist(str_split(x,", "))), 
                                      collapse = ", "))
data00$DIAGSEK3 <- paste(data00$DS_01,data00$DS_02,data00$DS_03, 
                      data00$DS_04,data00$DS_05,data00$DS_06,
                      data00$DS_07,data00$DS_08,data00$DS_09,
                      data00$DS_10,data00$DS_11,data00$DS_12,
                      sep=", ")
data00$DIAGSEK3 <- gsub("NA, ","",data00$DIAGSEK3)
data00$DIAGSEK3 <- gsub(", NA","",data00$DIAGSEK3)
data00$DIAGSEK3 <- gsub("NA","",data00$DIAGSEK3)
data00$DIAGSEK3 <- sapply(data00$DIAGSEK3, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

#===========================================================================

setwd("D://UR//ICD 10 & 9-CM//icd102010enMeta")

chapters <- read.delim2('chapters.txt', header = FALSE, sep = ";", dec = ".")

chapters_headings <- c('chapter_number','chapter_title')
names(chapters) <- chapters_headings
chapters$chapters <- paste(chapters$chapter_number, "-", 
                           chapters$chapter_title)
chapters <- chapters %>% select(chapter_number,chapters)

blocks <- read.delim2('blocks.txt', header = FALSE, sep = ";", dec = ".")
blocks_headings <- c('character_31a', 'character_31b', 'chapter_number',
                     'block_title')
names(blocks) <- blocks_headings
blocks$blocks <- paste(blocks$character_31a, "-", blocks$character_31b,
                       " ", blocks$block_title)
blocks <- blocks %>%
  select(character_31a,blocks)

codes <- read.delim2('codes.txt', header = FALSE, sep = ";", dec = ".")
codes_headings <- c('level_classification', 'place_classification_tree',
                    'terminal_node', 'chapter_number', 'character_31a',
                    'code_wo_dagger', 'code_wo_asterisk', 'code_wo_dot',
                    'title', 'reference_mortality_1', 'reference_mortality_2',
                    'reference_mortality_3', 'reference_mortality_4',
                    'reference_morbidity')
names(codes) <- codes_headings

codes <- codes %>%
  select(chapter_number,character_31a,code_wo_dot, title)

codes <- left_join(codes, chapters,
                   by = c("chapter_number"="chapter_number"),
                   all.x = TRUE) %>%
  select(-chapter_number)

codes <- left_join(codes, blocks,
                   by = c("character_31a"="character_31a"),
                   all.x = TRUE) %>%
  select(-character_31a)

data00$NmDU <- NULL
data00$NmDM <- NULL

library(data.table)
library(caret)

data01 <- preProcess(as.data.frame(data01))

data01 <- left_join(data00, codes,
                    by = c("DU"="code_wo_dot"),all.x = TRUE)

data01 <- data01 %>%
  rename(NmDU = title, ChapDU = chapters,
         DP = blocks, DiaU = codes)

data01 <- left_join(data01, codes,
                    by = c("DS01"="code_wo_dot"),all.x = TRUE) %>%
  select(-title) %>%
  rename(ChapDS01 = chapters,
         blocksDS01 = blocks, DS01 = codes)
data01$DS01 [is.na(data01$DS01)] <- ""

data01 <- left_join(data01, codes,
                    by = c("DS02"="code_wo_dot"),all.x = TRUE) %>%
  select(-title) %>%
  rename(ChapDS02 = chapters,
         blocksDS02 = blocks, DS02 = codes)
data01$DS02 [is.na(data01$DS02)] <- ""

#============================================================================

library(readxl)
setwd("D:/data gresik/UR/Data Sampel BPJS Kesehatan 2015-2020 v01/Metadata")

DIAGFKTP <- read_excel("Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx", 
                       sheet = "Sheet1", range = NULL, 
                       col_names = TRUE, col_types = NULL, na = "",
                       trim_ws = TRUE, skip = 0)

#Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx

data01 <- left_join(data00, DIAGFKTP,
                    by = c("DP"="ICD10_Code"),all.x = TRUE) %>%
  select(-DP, -FKP14) %>%
  rename(DP = ICD10_Text)

rm(data00)

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_01"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_01, -FKP14) %>%
  rename(DS_01 = ICD10_Text)
data01$DS_01 [is.na(data01$DS_01)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_02"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_02, -FKP14) %>%
  rename(DS_02 = ICD10_Text)
data01$DS_02 [is.na(data01$DS_02)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_03"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_03, -FKP14) %>%
  rename(DS_03 = ICD10_Text)
data01$DS_03 [is.na(data01$DS_03)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_04"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_04, -FKP14) %>%
  rename(DS_04 = ICD10_Text)
data01$DS_04 [is.na(data01$DS_04)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_05"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_05, -FKP14) %>%
  rename(DS_05 = ICD10_Text)
data01$DS_05 [is.na(data01$DS_05)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_06"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_06, -FKP14) %>%
  rename(DS_06 = ICD10_Text)
data01$DS_06 [is.na(data01$DS_06)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_07"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_07, -FKP14) %>%
  rename(DS_07 = ICD10_Text)
data01$DS_07 [is.na(data01$DS_07)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_08"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_08, -FKP14) %>%
  rename(DS_08 = ICD10_Text)
data01$DS_08 [is.na(data01$DS_08)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_09"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_09, -FKP14) %>%
  rename(DS_09 = ICD10_Text)
data01$DS_09 [is.na(data01$DS_09)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_10"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_10, -FKP14) %>%
  rename(DS_10 = ICD10_Text)
data01$DS_10 [is.na(data01$DS_10)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_11"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_11, -FKP14) %>%
  rename(DS_11 = ICD10_Text)
data01$DS_11 [is.na(data01$DS_11)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_12"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_12, -FKP14) %>%
  rename(DS_12 = ICD10_Text)
data01$DS_12 [is.na(data01$DS_12)] <- ""

PROC <- read.csv2("sub chapter proc.csv", 
                  colClasses=c("factor", "character", "character"))
glimpse(PROC)

data02 <- data01 %>%
  mutate(Proc_01 = as.factor(substr(data01$Proc01,1,2)),
         Proc_02 = as.factor(substr(data01$Proc02,1,2)),
         Proc_03 = as.factor(substr(data01$Proc03,1,2)),
         Proc_04 = as.factor(substr(data01$Proc04,1,2)),
         Proc_05 = as.factor(substr(data01$Proc05,1,2)),
         Proc_06 = as.factor(substr(data01$Proc06,1,2)),
         Proc_07 = as.factor(substr(data01$Proc07,1,2)),
         Proc_08 = as.factor(substr(data01$Proc08,1,2)),
         Proc_09 = as.factor(substr(data01$Proc09,1,2)),
         Proc_10 = as.factor(substr(data01$Proc10,1,2)),
         Proc_11 = as.factor(substr(data01$Proc11,1,2)))

rm(data01)

data03 <- left_join(data02, PROC,
                    by = c("Proc_01"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_01, -Chap.Proc) %>%
  rename(Proc_01 = Sub.Chap.Proc)

rm(data02)

data03$Proc_01 [is.na(data03$Proc_01)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_02"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_02, -Chap.Proc) %>%
  rename(Proc_02 = Sub.Chap.Proc)
data03$Proc_02 [is.na(data03$Proc_02)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_03"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_03, -Chap.Proc) %>%
  rename(Proc_03 = Sub.Chap.Proc)
data03$Proc_03 [is.na(data03$Proc_03)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_04"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_04, -Chap.Proc) %>%
  rename(Proc_04 = Sub.Chap.Proc)
data03$Proc_04 [is.na(data03$Proc_04)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_05"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_05, -Chap.Proc) %>%
  rename(Proc_05 = Sub.Chap.Proc)
data03$Proc_05 [is.na(data03$Proc_05)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_06"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_06, -Chap.Proc) %>%
  rename(Proc_06 = Sub.Chap.Proc)
data03$Proc_06 [is.na(data03$Proc_06)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_07"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_07, -Chap.Proc) %>%
  rename(Proc_07 = Sub.Chap.Proc)
data03$Proc_07 [is.na(data03$Proc_07)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_08"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_08, -Chap.Proc) %>%
  rename(Proc_08 = Sub.Chap.Proc)
data03$Proc_08 [is.na(data03$Proc_08)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_09"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_09, -Chap.Proc) %>%
  rename(Proc_09 = Sub.Chap.Proc)
data03$Proc_09 [is.na(data03$Proc_09)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_10"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_10, -Chap.Proc) %>%
  rename(Proc_10 = Sub.Chap.Proc)
data03$Proc_10 [is.na(data03$Proc_10)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_11"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_11, -Chap.Proc) %>%
  rename(Proc_11 = Sub.Chap.Proc)
data03$Proc_11 [is.na(data03$Proc_11)] <- ""

PROC2 <- read.csv2("Chapter Procedure.csv", 
                  colClasses=c("factor", "character",
                               "character", "character"))

data04 <- data03 %>%
  mutate(Proc_001 = as.factor(substr(data03$Proc01,1,3)),
         Proc_002 = as.factor(substr(data03$Proc02,1,3)),
         Proc_003 = as.factor(substr(data03$Proc03,1,3)),
         Proc_004 = as.factor(substr(data03$Proc04,1,3)),
         Proc_005 = as.factor(substr(data03$Proc05,1,3)),
         Proc_006 = as.factor(substr(data03$Proc06,1,3)),
         Proc_007 = as.factor(substr(data03$Proc07,1,3)),
         Proc_008 = as.factor(substr(data03$Proc08,1,3)),
         Proc_009 = as.factor(substr(data03$Proc09,1,3)),
         Proc_010 = as.factor(substr(data03$Proc10,1,3)),
         Proc_011 = as.factor(substr(data03$Proc11,1,3)))

rm(data03)

data04$PROC3 <- paste(data04$Proc_001,data04$Proc_002,data04$Proc_003,data04$Proc_004,
                      data04$Proc_005,data04$Proc_006,data04$Proc_007,data04$Proc_008,
                      data04$Proc_009,data04$Proc_010,data04$Proc_011,sep=", ")
data04$PROC3 <- gsub("NA, ","",data04$PROC3)
data04$PROC3 <- gsub(", NA","",data04$PROC3)
data04$PROC3 <- gsub("NA","",data04$PROC3)
data04$PROC3 <- sapply(data04$PROC3, 
                     function(x) paste(unique(unlist(str_split(x,", "))), 
                                       collapse = ", "))

data05 <- left_join(data04, PROC2,
                    by = c("Proc_001"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_001, -Prosedur, -Chap.Proc) %>%
  rename(Proc_001 = Sub.Chap.Proc)

rm(data04)

data05$Proc_001 [is.na(data05$Proc_001)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_002"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_002, -Prosedur, -Chap.Proc) %>%
  rename(Proc_002 = Sub.Chap.Proc)
data05$Proc_002 [is.na(data05$Proc_002)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_003"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_003, -Prosedur, -Chap.Proc) %>%
  rename(Proc_003 = Sub.Chap.Proc)
data05$Proc_003 [is.na(data05$Proc_003)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_004"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_004, -Prosedur, -Chap.Proc) %>%
  rename(Proc_004 = Sub.Chap.Proc)
data05$Proc_004 [is.na(data05$Proc_004)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_005"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_005, -Prosedur, -Chap.Proc) %>%
  rename(Proc_005 = Sub.Chap.Proc)
data05$Proc_005 [is.na(data05$Proc_005)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_006"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_006, -Prosedur, -Chap.Proc) %>%
  rename(Proc_006 = Sub.Chap.Proc)
data05$Proc_006 [is.na(data05$Proc_006)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_007"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_007, -Prosedur, -Chap.Proc) %>%
  rename(Proc_007 = Sub.Chap.Proc)
data05$Proc_007 [is.na(data05$Proc_007)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_008"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_008, -Prosedur, -Chap.Proc) %>%
  rename(Proc_008 = Sub.Chap.Proc)
data05$Proc_008 [is.na(data05$Proc_008)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_009"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_009, -Prosedur, -Chap.Proc) %>%
  rename(Proc_009 = Sub.Chap.Proc)
data05$Proc_009 [is.na(data05$Proc_009)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_010"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_010, -Prosedur, -Chap.Proc) %>%
  rename(Proc_010 = Sub.Chap.Proc)
data05$Proc_010 [is.na(data05$Proc_010)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_011"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_011, -Prosedur, -Chap.Proc) %>%
  rename(Proc_011 = Sub.Chap.Proc)
data05$Proc_011 [is.na(data05$Proc_011)] <- ""

#LALI
data05 <- data05 %>%
  mutate(DIAGP = as.factor(substr(data05$DU,1,3)))

#==========================================================================
dataklaim <- data05 %>%
  select(Nokapst,Jkpst,Segmen,kdpst,pisa,Kelashak,Umur,
         Rangeumur,Nmdati2Terdaftar,Kdppkterdaftar,Nmppkterdaftar,
         Kdpks,Nmpks,prb,Nmdati2Perujuk,Kdppkperujuk,Nmppkperujuk,
         SEP,Nmdati2Layan,Kdppk,NmFKRTL,Pemilik,Kelasrsmenkes,Klsrawat,
         Nmtkp,Prvtkp,interval,Poli,DM,NmDM,Tglpelayanan,Start,End,
         Tglreg,Tglstjkeu,CBG,Kdinacbgs,CMG,Nmcmg,Nminacbgs,tipe,Spec,
         Sevel,DIAG,PROC,DU,DP,NmDU,DS,Noreg,DS01,DS_01,DS02,DS_02,DS03,
         DS_03,DS04,DS_04,DS05,DS_05,DS06,DS_06,DS07,DS_07,DS08,DS_08,
         DS09,DS_09,DS10,DS_10,DS11,DS_11,DS12,DS_12,DIAGP,DIAG3,DIAGSEK,
         DIAGSEK3,Jmlproc,PROC3,Procedure,Proc01,Proc_01,Proc02,Proc_02,
         Proc03,Proc_03,Proc04,Proc_04,Proc05,Proc_05,Proc06,Proc_06,
         Proc07,Proc_07,Proc08,Proc_08,Proc09,Proc_09,Proc10,Proc_10,
         Proc11,Proc_11,Proc_001,Proc_002,Proc_003,Proc_004,Proc_005,
         Proc_006,Proc_007,Proc_008,Proc_009,Proc_010,Proc_011,DPJP,
         Nmkronis,Nmkatastrofik,Kdjnsplg,LOS,Tarif,BiayaRS,Biaya,
         maxcase,maxuc)

dataklaim <- dataklaim[order(dataklaim$Nokapst, dataklaim$End),]
rm(data05)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
save(dataklaim, file="dataklaim.rda")

#=================================================================
#--------------------------LEFT JOIN------------------------------
#=================================================================

#4470 - AV shunt RJTL
## SUBSET
data_Q5 <- data %>%
  subset(Nmtkp == "RITL")


data_01 <- data_Q5 %>% 
  arrange(Nokapst, Start, End) %>% 
  group_by(Nokapst) %>%
  mutate(LOS = 1 + difftime(End, Start, units = 'days')) %>%
  mutate(laggedTimeElapsed = difftime(Start, lag(End),
                                      units = 'days'))

data_01 = data_01 %>% rename(DPJP = Namadpjp01)

#DATA_02 = koding variabel
data_02 <- data_01 %>%
  select(Nokapst, Tglpelayanan, Start, End, laggedTimeElapsed, Jkpst, Segmen,
         Kelashak, Umur, SEP, NmFKRTL, Kelasrsmenkes, Klsrawat, Nmtkp, Poli,
         CBG, CMG, tipe, Spec, Sevel, DU, NmDU, DS, DS01, DS02, DS03, DS04, 
         DS05, DS06, DS07, DS08, DS09, DS10, Procedure, Proc01, Proc02, 
         Proc03, Proc04, Proc05, Proc06, Proc07, Proc08, Proc09, Proc10, 
         Proc11, DPJP, Nmkatastrofik, Kdjnsplg, BiayaRS, Biaya, LOS)

#Transform Jkpst menjadi 1&0 --> 0= berarti Laki-laki, 1= berarti Perempuan
data_02$Jkpst[data_02$Jkpst=="Laki-laki"] <- "0"
data_02$Jkpst[data_02$Jkpst=="Perempuan"] <- "1"

#Transform Nmtkp menjadi 1&2 --> 1= berarti RITL, 2= berarti RJTL
data_02$Nmtkp[data_02$Nmtkp=="RITL"] <- "1"
data_02$Nmtkp[data_02$Nmtkp=="RJTL"] <- "2"

#Transform Segmen menjadi 1&0 --> 0= berarti PBI, 1= berarti Non-PBI
data_02$Segmen[data_02$Segmen=="PBI APBN"| 
                 data_02$Segmen=="PBI APBD"]<- "0"
data_02$Segmen[data_02$Segmen=="BP" |
                 data_02$Segmen=="PBPU" |
                 data_02$Segmen=="PPU BU" |
                 data_02$Segmen=="PPU PN" ] <- "1"

#Transform Kelashak menjadi 1,2,3
data_02$Kelashak[data_02$Kelashak=="Kelas I"] <- "1"
data_02$Kelashak[data_02$Kelashak=="Kelas II"] <- "2"
data_02$Kelashak[data_02$Kelashak=="Kelas III"] <- "3"

#Transform Klsrawat menjadi 1,2,3
data_02$Klsrawat[data_02$Klsrawat=="Kelas I"] <- "1"
data_02$Klsrawat[data_02$Klsrawat=="Kelas II"] <- "2"
data_02$Klsrawat[data_02$Klsrawat=="Kelas III"] <- "3"

glimpse(data_02)


#DATA_03 = Gabung DIAG & PROC
#remove empty cell with NA
data_03 <- na_if(data_02, "")
data_03$DIAG <- paste(data_03$DU,data_03$DS01,data_03$DS02,data_03$DS03, 
                      data_03$DS04,data_03$DS05,data_03$DS06,data_03$DS07, 
                      data_03$DS08,data_03$DS09, sep=", ")
data_03$DIAG <- gsub("NA, ","",data_03$DIAG)
data_03$DIAG <- gsub(", NA","",data_03$DIAG)
data_03$DIAG <- sapply(data_03$DIAG, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
#nanti dihapus yaaa..
data_03$DIAG3 <- paste(data_03$DP,data_03$DS_01,data_03$DS_02,data_03$DS_03, 
                       data_03$DS_04,data_03$DS_05,data_03$DS_06,data_03$DS_07, 
                       data_03$DS_08,data_03$DS_09, sep=", ")
data_03$DIAG3 <- gsub("NA, ","",data_03$DIAG3)
data_03$DIAG3 <- gsub(", NA","",data_03$DIAG3)
data_03$DIAG3 <- sapply(data_03$DIAG3, 
                        function(x) paste(unique(unlist(str_split(x,", "))), 
                                          collapse = ", "))


data_03$PROC <- paste(data_03$Proc01,data_03$Proc02,data_03$Proc03,data_03$Proc04,
                      data_03$Proc05,sep=", ")
data_03$PROC <- gsub("NA, ","",data_03$PROC)
data_03$PROC <- gsub(", NA","",data_03$PROC)
data_03$PROC <- gsub("NA","",data_03$PROC)
data_03$PROC <- sapply(data_03$PROC, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
#nanti dihapus yaaa..
data_03$PROC2 <- paste(data_03$Proc_01,data_03$Proc_02,data_03$Proc_03,data_03$Proc_04,
                       data_03$Proc_05,sep=", ")
data_03$PROC2 <- gsub("NA, ","",data_03$PROC2)
data_03$PROC2 <- gsub(", NA","",data_03$PROC2)
data_03$PROC2 <- gsub("NA","",data_03$PROC2)
data_03$PROC2 <- sapply(data_03$PROC2, 
                        function(x) paste(unique(unlist(str_split(x,", "))), 
                                          collapse = ", "))

data_03 <- na_if(data_03, "")

colnames(data_03)

SEP_DIAG <- data_03 %>% 
  select(SEP, DU, DS01, DS02, DS03, DS04, DS05, DS06, DS07, DS08, DS09,
         DP, DS_01, DS_02, DS_03, DS_04, DS_05, DS_06, DS_07, DS_08, DS_09,
         Proc01, Proc02, Proc03, Proc04, Proc05, Proc_01, Proc_02, Proc_03,
         Proc_04, Proc_05)

SEP_DIAG01 <- SEP_DIAG %>% 
  select(SEP, DU, DS01, DP, DS_01) %>% 
  rename(DS = DS01, DS3 = DS_01) %>%
  na.omit()

SEP_DIAG02 <- SEP_DIAG %>% 
  select(SEP, DU, DS02, DP, DS_02) %>% 
  rename(DS = DS02, DS3 = DS_02) %>%
  na.omit()

SEP_DIAG03 <- SEP_DIAG %>% 
  select(SEP, DU, DS03, DP, DS_03) %>% 
  rename(DS = DS03, DS3 = DS_03) %>%
  na.omit()

SEP_DIAG04 <- SEP_DIAG %>% 
  select(SEP, DU, DS04, DP, DS_04) %>% 
  rename(DS = DS04, DS3 = DS_04) %>%
  na.omit()

SEP_DIAG05 <- SEP_DIAG %>% 
  select(SEP, DU, DS05, DP, DS_05) %>% 
  rename(DS = DS05, DS3 = DS_05) %>%
  na.omit()

SEP_DIAG06 <- SEP_DIAG %>% 
  select(SEP, DU, DS06, DP, DS_06) %>% 
  rename(DS = DS06, DS3 = DS_06) %>%
  na.omit()

SEP_DIAG07 <- SEP_DIAG %>% 
  select(SEP, DU, DS07, DP, DS_07) %>% 
  rename(DS = DS07, DS3 = DS_07) %>%
  na.omit()

SEP_DIAG08 <- SEP_DIAG %>% 
  select(SEP, DU, DS08, DP, DS_08) %>% 
  rename(DS = DS08, DS3 = DS_08) %>%
  na.omit()

SEP_DIAG09 <- SEP_DIAG %>% 
  select(SEP, DU, DS09, DP, DS_09) %>% 
  rename(DS = DS09, DS3 = DS_09) %>%
  na.omit()

SEP_PROC01 <- SEP_DIAG %>% 
  select(SEP, DU, Proc01, DP, Proc_01) %>% 
  rename(PROC = Proc01, PROC2 = Proc_01) %>%
  na.omit()

SEP_PROC02 <- SEP_DIAG %>% 
  select(SEP, DU, Proc02, DP, Proc_02) %>% 
  rename(PROC = Proc02, PROC2 = Proc_02) %>%
  na.omit()

SEP_PROC03 <- SEP_DIAG %>% 
  select(SEP, DU, Proc03, DP, Proc_03) %>% 
  rename(PROC = Proc03, PROC2 = Proc_03) %>%
  na.omit()

SEP_PROC04 <- SEP_DIAG %>% 
  select(SEP, DU, Proc04, DP, Proc_04) %>% 
  rename(PROC = Proc04, PROC2 = Proc_04) %>%
  na.omit()

SEP_PROC05 <- SEP_DIAG %>% 
  select(SEP, DU, Proc05, DP, Proc_05) %>% 
  rename(PROC = Proc05, PROC2 = Proc_05) %>%
  na.omit()

SEP_DIAG00 <- rbind(SEP_DIAG01, SEP_DIAG02, SEP_DIAG03, SEP_DIAG04, SEP_DIAG05,
                    SEP_DIAG06, SEP_DIAG07, SEP_DIAG08, SEP_DIAG09) %>%
  arrange(SEP)

SEP_PROC00 <- rbind(SEP_PROC01, SEP_PROC02, SEP_PROC03, SEP_PROC04, SEP_PROC05) %>%
  arrange(SEP)

df <- merge(x = SEP_DIAG00, y = SEP_PROC00, by = c( "SEP","DU", "DP"), 
            all = TRUE) %>%
  arrange(SEP)
save(df, file = "dfdiagproc.rda")

save(data_03, file = "Q455.rda")

data_03 <- data_03 %>%
  select(Nokapst, Tglpelayanan, Start, End, Jkpst, Segmen, Kelashak, SEP, 
         NmFKRTL, Kelasrsmenkes, Klsrawat, CBG, Sevel, DIAG, PROC, DPJP, 
         Kdjnsplg, BiayaRS, Biaya, LOS)

data_03 <- data_03 %>% 
  filter(str_detect(PROC, c("4470")))


%>%
  subset(Nmtkp == 1)

data_03 <- data_03 %>% 
  filter(str_detect(DIAG, c("P030|P031|P032|P033|P034|P035|P036"))) %>%
  subset(Klsrawat != 3)

glimpse(data_03)
colnames(data_03)

data_03 <- data_03 %>%
  filter(str_detect(DIAG, c("B342")))


%>%
  subset(Kdjnsplg == 2)

data_03 <- subset(data_03, data_03$Tglpelayanan >= "2020-03-01")

%>%
  filter(Kdjnsplg == 2)

write.csv(data, "D://dataextractdumai//buban bojonegoro//2014-2021//data_lap_kewil.csv",
          na="", row.names = FALSE)

df_DS = read_csv("data_lap_kewil.csv") %>% select(SEP, DS)

library(splitstackshape)
df_DS <- concat.split(df_DS, "DS", ";")
df_DS$DS01 <- as.character(trimws(df_DS$DS_01, "both"))
df_DS$DS02 <- as.character(trimws(df_DS$DS_02, "both"))
df_DS$DS03 <- as.character(trimws(df_DS$DS_03, "both"))
df_DS$DS04 <- as.character(trimws(df_DS$DS_04, "both"))
df_DS$DS05 <- as.character(trimws(df_DS$DS_05, "both"))
df_DS$DS06 <- as.character(trimws(df_DS$DS_06, "both"))
df_DS$DS07 <- as.character(trimws(df_DS$DS_07, "both"))
df_DS$DS08 <- as.character(trimws(df_DS$DS_08, "both"))
df_DS$DS09 <- as.character(trimws(df_DS$DS_09, "both"))
df_DS$DS10 <- as.character(trimws(df_DS$DS_10, "both"))
df_DS <- df_DS %>% select(-2:-12)

df.DS01 = df_DS %>% select(SEP, DS01)
df.DS01 = df.DS01 %>% rename(DS = DS01)
df.DS01 = na.omit(df.DS01)
df.DS02 = df_DS %>% select(SEP, DS02)
df.DS02 = df.DS02 %>% rename(DS = DS02)
df.DS02 = na.omit(df.DS02)
df.DS03 = df_DS %>% select(SEP, DS03)
df.DS03 = df.DS03 %>% rename(DS = DS03)
df.DS03 = na.omit(df.DS03)
df.DS04 = df_DS %>% select(SEP, DS04)
df.DS04 = df.DS04 %>% rename(DS = DS04)
df.DS04 = na.omit(df.DS04)
df.DS05 = df_DS %>% select(SEP, DS05)
df.DS05 = df.DS05 %>% rename(DS = DS05)
df.DS05 = na.omit(df.DS05)
df.DS06 = df_DS %>% select(SEP, DS06)
df.DS06 = df.DS06 %>% rename(DS = DS06)
df.DS06 = na.omit(df.DS06)
df.DS07 = df_DS %>% select(SEP, DS07)
df.DS07 = df.DS07 %>% rename(DS = DS07)
df.DS07 = na.omit(df.DS07)
df.DS08 = df_DS %>% select(SEP, DS08)
df.DS08 = df.DS08 %>% rename(DS = DS08)
df.DS08 = na.omit(df.DS08)
df.DS09 = df_DS %>% select(SEP, DS09)
df.DS09 = df.DS09 %>% rename(DS = DS09)
df.DS09 = na.omit(df.DS09)
df.DS10 = df_DS %>% select(SEP, DS10)
df.DS10 = df.DS10 %>% rename(DS = DS10)
df.DS10 = na.omit(df.DS10)

df.DS <- rbind(df.DS01, df.DS02, df.DS03, df.DS04, df.DS05, df.DS06, df.DS07, 
               df.DS08, df.DS09, df.DS10)

write.csv(df.DS, "D://dataextractdumai//buban bojonegoro//2014-2021//DS.csv",
          na="", row.names = FALSE)

df_Proc = read_csv("data_lap_kewil.csv") %>% select(SEP, Procedure)
df_Proc <- concat.split(df_Proc, "Procedure", ";")

df_Proc$Proc01 <- as.character(trimws(df_Proc$Procedure_01, "both"))
df_Proc$Proc02 <- as.character(trimws(df_Proc$Procedure_02, "both"))
df_Proc$Proc03 <- as.character(trimws(df_Proc$Procedure_03, "both"))
df_Proc$Proc04 <- as.character(trimws(df_Proc$Procedure_04, "both"))
df_Proc$Proc05 <- as.character(trimws(df_Proc$Procedure_05, "both"))
df_Proc$Proc06 <- as.character(trimws(df_Proc$Procedure_06, "both"))
df_Proc$Proc07 <- as.character(trimws(df_Proc$Procedure_07, "both"))
df_Proc$Proc08 <- as.character(trimws(df_Proc$Procedure_08, "both"))
df_Proc$Proc09 <- as.character(trimws(df_Proc$Procedure_09, "both"))
df_Proc$Proc10 <- as.character(trimws(df_Proc$Procedure_10, "both"))
df_Proc$Proc11 <- as.character(trimws(df_Proc$Procedure_11, "both"))
df_Proc <- df_Proc %>% select(-2:-13)

df.Proc01 = df_Proc %>% select(SEP, Proc01)
df.Proc01 = df.Proc01 %>% rename(Proc = Proc01)
df.Proc01 = na.omit(df.Proc01)
df.Proc02 = df_Proc %>% select(SEP, Proc02)
df.Proc02 = df.Proc02 %>% rename(Proc = Proc02)
df.Proc02 = na.omit(df.Proc02)
df.Proc03 = df_Proc %>% select(SEP, Proc03)
df.Proc03 = df.Proc03 %>% rename(Proc = Proc03)
df.Proc03 = na.omit(df.Proc03)
df.Proc04 = df_Proc %>% select(SEP, Proc04)
df.Proc04 = df.Proc04 %>% rename(Proc = Proc04)
df.Proc04 = na.omit(df.Proc04)
df.Proc05 = df_Proc %>% select(SEP, Proc05)
df.Proc05 = df.Proc05 %>% rename(Proc = Proc05)
df.Proc05 = na.omit(df.Proc05)
df.Proc06 = df_Proc %>% select(SEP, Proc06)
df.Proc06 = df.Proc06 %>% rename(Proc = Proc06)
df.Proc06 = na.omit(df.Proc06)
df.Proc07 = df_Proc %>% select(SEP, Proc07)
df.Proc07 = df.Proc07 %>% rename(Proc = Proc07)
df.Proc07 = na.omit(df.Proc07)
df.Proc08 = df_Proc %>% select(SEP, Proc08)
df.Proc08 = df.Proc08 %>% rename(Proc = Proc08)
df.Proc08 = na.omit(df.Proc08)
df.Proc09 = df_Proc %>% select(SEP, Proc09)
df.Proc09 = df.Proc09 %>% rename(Proc = Proc09)
df.Proc09 = na.omit(df.Proc09)
df.Proc10 = df_Proc %>% select(SEP, Proc10)
df.Proc10 = df.Proc10 %>% rename(Proc = Proc10)
df.Proc10 = na.omit(df.Proc10)
df.Proc11 = df_Proc %>% select(SEP, Proc11)
df.Proc11 = df.Proc11 %>% rename(Proc = Proc11)
df.Proc11 = na.omit(df.Proc11)