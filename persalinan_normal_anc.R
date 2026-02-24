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
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P")))

data19$Politujsjp <- data19$Politujsjp %>%
  modify_if(is.character, toupper)

data20 <- read_csv("Sheet 1_Full Data_kediri 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P")))

data20$Politujsjp <- data20$Politujsjp %>%
  modify_if(is.character, toupper)

data21 <- read_csv("Sheet 1_Full Data_kediri 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P")))

data21$Politujsjp <- data21$Politujsjp %>%
  modify_if(is.character, toupper)

data22 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2022.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P")))

data22$Politujsjp <- data22$Politujsjp %>%
  modify_if(is.character, toupper)

data23 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P")))

data23$Politujsjp <- data23$Politujsjp %>%
  modify_if(is.character, toupper)

data24 <- read_csv("Sheet 1_Full Data_kediri 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P")))

data24$Politujsjp <- data24$Politujsjp %>%
  modify_if(is.character, toupper)

datamei24 <- read_csv("Sheet 1_Full Data_kediri (2025).csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P")))

datamei24$Politujsjp <- datamei24$Politujsjp %>%
  modify_if(is.character, toupper)

persalinan <- rbind(data19,data20,data21,data22,data23,data24,datamei24)
rm(data19,data20,data21,data22,data23,data24,datamei24)

persalinan <- persalinan %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
persalinan <- persalinan %>% mutate(Procedure = na_if(Procedure, "-"))

persalinan$Tgldtgsjp <- as.Date(persalinan$Tgldtgsjp, format = "%m/%d/%Y")
persalinan$Tglplgsjp <- as.Date(persalinan$Tglplgsjp, format = "%m/%d/%Y")
persalinan$Tglpelayanan <- as.Date(persalinan$Tglpelayanan, format = "%m/%d/%Y")

persalinan <- persalinan %>% 
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

data19 <- read_csv("Sheet 1_Full Data_kediri 2019.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL")

data19$Politujsjp <- data19$Politujsjp %>%
  modify_if(is.character, toupper)

data19 <- data19 %>%
  subset(Politujsjp == "OBG")

data20 <- read_csv("Sheet 1_Full Data_kediri 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL")

data20$Politujsjp <- data20$Politujsjp %>%
  modify_if(is.character, toupper)

data20 <- data20 %>%
  subset(Politujsjp == "OBG")

data21 <- read_csv("Sheet 1_Full Data_kediri 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL")

data21$Politujsjp <- data21$Politujsjp %>%
  modify_if(is.character, toupper)

data21 <- data21 %>%
  subset(Politujsjp == "OBG")

data22 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2022.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL")

data22$Politujsjp <- data22$Politujsjp %>%
  modify_if(is.character, toupper)

data22 <- data22 %>%
  subset(Politujsjp == "OBG")

data23 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL")

data23$Politujsjp <- data23$Politujsjp %>%
  modify_if(is.character, toupper)

data23 <- data23 %>%
  subset(Politujsjp == "OBG")

data24 <- read_csv("Sheet 1_Full Data_kediri 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL")

data24$Politujsjp <- data24$Politujsjp %>%
  modify_if(is.character, toupper)

data24 <- data24 %>%
  subset(Politujsjp == "OBG")

datamei24 <- read_csv("Sheet 1_Full Data_kediri (2025).csv") %>%
  select (Nokapst,Umur,Jkpst,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL")

datamei24$Politujsjp <- datamei24$Politujsjp %>%
  modify_if(is.character, toupper)

datamei24 <- datamei24 %>%
  subset(Politujsjp == "OBG")

poliobg <- rbind(data19,data20,data21,data22,data23,data24,datamei24)
rm(data19,data20,data21,data22,data23,data24,datamei24)

poliobg <- poliobg %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
poliobg <- poliobg %>% mutate(Procedure = na_if(Procedure, "-"))

poliobg$Tgldtgsjp <- as.Date(poliobg$Tgldtgsjp, format = "%m/%d/%Y")
poliobg$Tglplgsjp <- as.Date(poliobg$Tglplgsjp, format = "%m/%d/%Y")
poliobg$Tglpelayanan <- as.Date(poliobg$Tglpelayanan, format = "%m/%d/%Y")

poliobg <- poliobg %>% 
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
#========================================================================
persalinan <- data %>%
  subset(Nmtkp == "RITL") %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P")))

poliobg <- data %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Poli_asal == "Obgyn")
#========================================================================
noka_persalinan <- persalinan %>% select(Nokapst) %>% distinct()

noka_poli <- left_join(noka_persalinan,poliobg, by = c("Nokapst"="Nokapst"))
noka_poli$Klasifikasi <- "ANC"
noka_poli$kelompok <- "Poli"

Sectio_Caesarea <- persalinan %>%
  mutate(Sectio_Caesarea = paste0(as.character(Nmdiagprimer),";",
                                  as.character(Diagsekunder),";",
                                  as.character(Procedure))) %>%
  filter(!str_detect(Kdinacbgs,c("O-6-13-I|O-6-13-II|O-6-13-III|O-6-11-I|O-6-11-II|O-6-11-III|O-6-12-I|O-6-12-II|O-6-12-III"))) %>%
  filter(str_detect(Sectio_Caesarea,c("Cesarean|cesarean|Caesarean|caesarean"))) %>%
  select(-Sectio_Caesarea) %>%
  subset(Kdinacbgs != "O-6-13-I")
pervaginam <- persalinan %>%
  subset(Kdinacbgs == "O-6-13-I")
gabung <- rbind(Sectio_Caesarea,pervaginam)
irisan <- anti_join(persalinan,gabung)

Sectio_Caesarea$Klasifikasi <- "Sectio Caesarea"
pervaginam$Klasifikasi <- "Pervaginam Tanpa Penyulit"
irisan$Klasifikasi <- "Pervaginam Dengan Penyulit"
persalinan <- rbind(Sectio_Caesarea,pervaginam,irisan)
persalinan$kelompok <- "Persalinan"
rm(gabung,Sectio_Caesarea,pervaginam,irisan)

persalinandanpoli <- rbind(persalinan,noka_poli)
persalinandanpoli <- persalinandanpoli[order(persalinandanpoli$Nokapst,persalinandanpoli$Tglplgsjp),]

persalinandanpoli <- persalinandanpoli %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))

persalinandanpoli <- persalinandanpoli[order(persalinandanpoli$Nokapst,persalinandanpoli$Tglplgsjp),]
persalinandanpoli$jeda <- as.integer(persalinandanpoli$Tgldtgsjp) - as.integer(persalinandanpoli$tgl_before)
persalinandanpoli$jeda <- as.numeric(persalinandanpoli$jeda) %>%
  replace(is.na(.), 0)

#cek <- persalinandanpoli %>% select(Nokapst,Tglplgsjp,Politujsjp,Nmtkp,jeda,Diagnosa,kelompok,Klasifikasi)

persalinandanpoli <- persalinandanpoli[order(persalinandanpoli$Nokapst,persalinandanpoli$Tglplgsjp),]
persalinandanpoli$ANC <- with(persalinandanpoli,
                              ifelse(Nokapst == lag(Nokapst) &
                                       kelompok == "Persalinan" &
                                       lag(kelompok) == "Poli" & 
                                       jeda <= 280 & jeda > 1,
                                     "Dengan ANC","Tanpa ANC"))
persalinan <- persalinandanpoli %>% subset(kelompok == "Persalinan")
persalinan <- persalinan %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder)))
library(splitstackshape)
persalinan <- concat.split(persalinan, "Diagnosa", ";")
colnames(persalinan)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_1 = case_when(
      Diagnosa_01 == "Z370 - Single live birth" ~ NA,
      Diagnosa_01 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_01 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_01 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_01 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_01 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_01 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_01 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_01 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_01 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_01 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_01 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_01 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_01 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_01 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_01 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_01 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_01 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_01)) %>%
  select(-Diagnosa_01)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_2 = case_when(
      Diagnosa_02 == "Z370 - Single live birth" ~ NA,
      Diagnosa_02 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_02 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_02 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_02 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_02 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_02 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_02 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_02 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_02 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_02 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_02 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_02 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_02 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_02 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_02 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_02 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_02 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_02)) %>%
  select(-Diagnosa_02)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_3 = case_when(
      Diagnosa_03 == "Z370 - Single live birth" ~ NA,
      Diagnosa_03 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_03 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_03 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_03 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_03 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_03 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_03 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_03 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_03 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_03 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_03 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_03 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_03 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_03 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_03 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_03 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_03 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_03)) %>%
  select(-Diagnosa_03)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_4 = case_when(
      Diagnosa_04 == "Z370 - Single live birth" ~ NA,
      Diagnosa_04 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_04 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_04 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_04 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_04 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_04 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_04 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_04 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_04 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_04 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_04 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_04 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_04 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_04 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_04 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_04 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_04 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_04)) %>%
  select(-Diagnosa_04)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_5 = case_when(
      Diagnosa_05 == "Z370 - Single live birth" ~ NA,
      Diagnosa_05 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_05 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_05 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_05 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_05 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_05 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_05 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_05 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_05 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_05 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_05 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_05 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_05 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_05 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_05 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_05 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_05 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_05)) %>%
  select(-Diagnosa_05)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_6 = case_when(
      Diagnosa_06 == "Z370 - Single live birth" ~ NA,
      Diagnosa_06 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_06 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_06 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_06 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_06 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_06 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_06 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_06 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_06 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_06 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_06 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_06 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_06 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_06 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_06 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_06 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_06 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_06)) %>%
  select(-Diagnosa_06)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_7 = case_when(
      Diagnosa_07 == "Z370 - Single live birth" ~ NA,
      Diagnosa_07 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_07 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_07 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_07 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_07 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_07 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_07 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_07 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_07 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_07 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_07 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_07 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_07 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_07 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_07 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_07 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_07 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_07)) %>%
  select(-Diagnosa_07)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_8 = case_when(
      Diagnosa_08 == "Z370 - Single live birth" ~ NA,
      Diagnosa_08 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_08 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_08 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_08 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_08 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_08 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_08 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_08 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_08 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_08 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_08 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_08 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_08 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_08 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_08 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_08 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_08 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_08)) %>%
  select(-Diagnosa_08)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_9 = case_when(
      Diagnosa_09 == "Z370 - Single live birth" ~ NA,
      Diagnosa_09 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_09 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_09 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_09 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_09 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_09 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_09 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_09 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_09 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_09 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_09 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_09 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_09 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_09 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_09 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_09 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_09 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_09)) %>%
  select(-Diagnosa_09)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_010 = case_when(
      Diagnosa_10 == "Z370 - Single live birth" ~ NA,
      Diagnosa_10 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_10 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_10 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_10 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_10 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_10 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_10 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_10 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_10 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_10 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_10 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_10 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_10 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_10 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_10 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_10 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_10 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_10)) %>%
  select(-Diagnosa_10)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_011 = case_when(
      Diagnosa_11 == "Z370 - Single live birth" ~ NA,
      Diagnosa_11 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_11 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_11 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_11 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_11 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_11 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_11 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_11 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_11 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_11 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_11 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_11 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_11 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_11 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_11 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_11 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_11 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_11)) %>%
  select(-Diagnosa_11)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_012 = case_when(
      Diagnosa_12 == "Z370 - Single live birth" ~ NA,
      Diagnosa_12 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_12 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_12 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_12 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_12 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_12 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_12 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_12 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_12 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_12 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_12 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_12 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_12 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_12 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_12 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_12 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_12 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_12)) %>%
  select(-Diagnosa_12)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_013 = case_when(
      Diagnosa_13 == "Z370 - Single live birth" ~ NA,
      Diagnosa_13 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_13 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_13 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_13 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_13 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_13 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_13 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_13 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_13 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_13 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_13 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_13 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_13 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_13 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_13 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_13 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_13 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_13)) %>%
  select(-Diagnosa_13)

persalinan <- persalinan %>%
  mutate(
    Diagnosa_014 = case_when(
      Diagnosa_14 == "Z370 - Single live birth" ~ NA,
      Diagnosa_14 == "Z371 - Single stillbirth" ~ NA,
      Diagnosa_14 == "Z372 - Twins, both liveborn" ~ NA,
      Diagnosa_14 == "Z380 - Singleton, born in hospital" ~ NA,
      Diagnosa_14 == "O800 - Spontaneous vertex delivery" ~ NA,
      Diagnosa_14 == "O801 - Spontaneous breech delivery" ~ NA,
      Diagnosa_14 == "O808 - Other single spontaneous delivery" ~ NA,
      Diagnosa_14 == "O809 - Single spontaneous delivery, unspecified" ~ NA,
      Diagnosa_14 == "O820 - Delivery by elective caesarean section" ~ NA,
      Diagnosa_14 == "O821 - Delivery by emergency caesarean section" ~ NA,
      Diagnosa_14 == "O828 - Other single delivery by caesarean section" ~ NA,
      Diagnosa_14 == "O829 - Delivery by caesarean section, unspecified" ~ NA,
      Diagnosa_14 == "O831 - Other assisted breech delivery" ~ NA,
      Diagnosa_14 == "O838 - Other specified assisted single delivery" ~ NA,
      Diagnosa_14 == "O839 - Assisted single delivery, unspecified" ~ NA,
      Diagnosa_14 == "O842 - Multiple delivery, all by caesarean section" ~ NA,
      Diagnosa_14 == "O848 - Other multiple delivery" ~ NA,
      Diagnosa_14 == "O849 - Multiple delivery, unspecified" ~ NA,
      TRUE ~ Diagnosa_14)) %>%
  select(-Diagnosa_14)

persalinan <- persalinan %>%
  mutate(Diagakhir = paste0(as.character(Diagnosa_1),";",
                            as.character(Diagnosa_2),";",
                            as.character(Diagnosa_3),";",
                            as.character(Diagnosa_4),";",
                            as.character(Diagnosa_5),";",
                            as.character(Diagnosa_6),";",
                            as.character(Diagnosa_7),";",
                            as.character(Diagnosa_8),";",
                            as.character(Diagnosa_9),";",
                            as.character(Diagnosa_010),";",
                            as.character(Diagnosa_011),";",
                            as.character(Diagnosa_012),";",
                            as.character(Diagnosa_013),";",
                            as.character(Diagnosa_014)))

persalinan$Diagakhir <- as.character(trimws(gsub("NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;","",persalinan$Diagakhir)),"both")
persalinan <- persalinan %>%
  select(-Diagnosa_1,-Diagnosa_2,-Diagnosa_3,-Diagnosa_4,-Diagnosa_5,
         -Diagnosa_6,-Diagnosa_7,-Diagnosa_8,-Diagnosa_9,-Diagnosa_010,
         -Diagnosa_011,-Diagnosa_012,-Diagnosa_013,-Diagnosa_014)
persalinan <- persalinan %>% mutate(Diagakhir = na_if(Diagakhir, ""))
persalinan1 <- persalinan %>% subset(is.na(Diagakhir))
#========================================================================
write.csv(persalinan, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_persalinan.csv",
          na="", row.names = FALSE)
#========================================================================