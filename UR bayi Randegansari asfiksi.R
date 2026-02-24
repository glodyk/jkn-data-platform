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
library(openxlsx)
library(data.table)
#=============================data nadyah=============================
data22 <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Nosjp,Umur,Jkpst,Nmtkp,Nmdati2Layan,Kdppklayan,
          Klsrawat,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdcmg,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>% 
  subset(Nmtkp == "RITL") %>%
  subset(Kdcmg == "P") %>%
  select(-Kdcmg) %>%
  subset(Kdppklayan == "0205R030") %>% 
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Procedure,c("9390"))) %>%
  select(-Diagnosa)

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Nosjp,Umur,Jkpst,Nmtkp,Nmdati2Layan,Kdppklayan,
          Klsrawat,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdcmg,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>% 
  subset(Nmtkp == "RITL") %>%
  subset(Kdcmg == "P") %>%
  select(-Kdcmg) %>%
  subset(Kdppklayan == "0205R030") %>% 
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Procedure,c("9390"))) %>%
  select(-Diagnosa)

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Nosjp,Umur,Jkpst,Nmtkp,Nmdati2Layan,Kdppklayan,
          Klsrawat,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdcmg,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>% 
  subset(Nmtkp == "RITL") %>%
  subset(Kdcmg == "P") %>%
  select(-Kdcmg) %>%
  subset(Kdppklayan == "0205R030") %>% 
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Procedure,c("9390"))) %>%
  select(-Diagnosa)

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Nosjp,Umur,Jkpst,Nmtkp,Nmdati2Layan,Kdppklayan,
          Klsrawat,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdcmg,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>% 
  subset(Nmtkp == "RITL") %>%
  subset(Kdcmg == "P") %>%
  select(-Kdcmg) %>%
  subset(Kdppklayan == "0205R030") %>% 
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Procedure,c("9390"))) %>%
  select(-Diagnosa)

datamei24 <- read_csv("Sheet 1_Full Data gresik mei_jul 2024.csv") %>%
  select (Nokapst,Nosjp,Umur,Jkpst,Nmtkp,Nmdati2Layan,Kdppklayan,
          Klsrawat,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdcmg,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>% 
  subset(Nmtkp == "RITL") %>%
  subset(Kdcmg == "P") %>%
  select(-Kdcmg) %>%
  subset(Kdppklayan == "0205R030") %>% 
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Procedure,c("9390"))) %>%
  select(-Diagnosa)

dataagu24 <- read_csv("Sheet 1_Full Data gresik agu 2024.csv") %>%
  select (Nokapst,Nosjp,Umur,Jkpst,Nmtkp,Nmdati2Layan,Kdppklayan,
          Klsrawat,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdcmg,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>% 
  subset(Nmtkp == "RITL") %>%
  subset(Kdcmg == "P") %>%
  select(-Kdcmg) %>%
  subset(Kdppklayan == "0205R030") %>% 
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Procedure,c("9390"))) %>%
  select(-Diagnosa)

data <- rbind(data22,data23,data24,dataapr24,datamei24,dataagu24)
rm(data22,data23,data24,dataapr24,datamei24,dataagu24)

data <- data %>% 
  mutate(
    Nmppklayan = case_when(
      Kdppklayan == "0204R003" ~ "RSUD Ngimbang",
      Kdppklayan == "0204R004" ~ "RS Suyudi Paciran (JST)",
      Kdppklayan == "0204R009" ~ "RSI Nashrul Ummah",
      Kdppklayan == "0204R010" ~ "RS Muhammadiyah Lamongan",
      Kdppklayan == "0204R012" ~ "RS Muhammadiyah Babat",
      Kdppklayan == "0204R019" ~ "RSU Muhammadiyah Babat",
      Kdppklayan == "0204R013" ~ "RS Fatimah",
      Kdppklayan == "0204R014" ~ "RS Intan Medika",
      Kdppklayan == "0204R015" ~ "RS Arsy Paciran",
      Kdppklayan == "0204R017" ~ "RS Citra Medika",
      Kdppklayan == "0204R018" ~ "RS Bedah Mitra Sehat",
      Kdppklayan == "0204S001" ~ "Klinik Mata Utama Lamongan",
      Kdppklayan == "0205R009" ~ "RS Denisa",
      Kdppklayan == "0205R011" ~ "RS Muhammadiyah Gresik",
      Kdppklayan == "0205R012" ~ "RS Semen Gresik",
      Kdppklayan == "0205R013" ~ "RS PKG Grha Husada",
      Kdppklayan == "0205R014" ~ "RS Petrokimia Driyorejo",
      Kdppklayan == "0205R019" ~ "RS Wali Songo I",
      Kdppklayan == "0205R021" ~ "RS Fathma Medika",
      Kdppklayan == "0205R022" ~ "RS Wates Husada",
      Kdppklayan == "0205R023" ~ "RS Surya Medika",
      Kdppklayan == "0205R024" ~ "RS PKU Muhammadiyah Sekapuk",
      Kdppklayan == "0205R025" ~ "RSI Mabarrot MWC NU Bungah",
      Kdppklayan == "0205R026" ~ "RS Rachmi Dewi",
      Kdppklayan == "0205R027" ~ "RSI Nyai Ageng Pinatih",
      Kdppklayan == "0205R028" ~ "RSI Cahaya Giri",
      Kdppklayan == "0205R029" ~ "RSUD Umar Mas'ud Bawean",
      Kdppklayan == "0205S100" ~ "Klinik Mata Utama",
      Kdppklayan == "1302R001" ~ "RSUD Ibnu Sina",
      Kdppklayan == "1302R002" ~ "RS Petrokimia Gresik",
      Kdppklayan == "1306R001" ~ "RSUD Dr Soegiri",
      Kdppklayan == "0205R031" ~ "RS Eka Husada",
      Kdppklayan == "0205R030" ~ "RS Randegansari Husada",
      Kdppklayan == "0204R020" ~ "RS Permata Bunda",
      Kdppklayan == "0204R021" ~ "RSUD Karangkembang",
      Kdppklayan == "0204R022" ~ "RS Nahdlatul Ulama Babat",
      Kdppklayan == "0204R023" ~ "RS Permata Hati",
      Kdppklayan == "0204R024" ~ "RS Muhammadiyah Kalikapas",
      Kdppklayan == "0205R032" ~ "RSIA Khodijah",
      Kdppklayan == "0205S101" ~ "Klinik Utama Cerme",
      TRUE ~ Kdppklayan))

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data <- data[order(data$Nmppklayan, data$Tglpelayanan),]
data <- data %>% subset(Tglpelayanan >= "2022-01-01")

data <- data %>% 
  mutate(
    Nmppklayan = case_when(
      Kdppklayan == "0204R003" ~ "RSUD Ngimbang",
      Kdppklayan == "0204R004" ~ "RS Suyudi Paciran (JST)",
      Kdppklayan == "0204R009" ~ "RSI Nashrul Ummah",
      Kdppklayan == "0204R010" ~ "RS Muhammadiyah Lamongan",
      Kdppklayan == "0204R012" ~ "RS Muhammadiyah Babat",
      Kdppklayan == "0204R019" ~ "RSU Muhammadiyah Babat",
      Kdppklayan == "0204R013" ~ "RS Fatimah",
      Kdppklayan == "0204R014" ~ "RS Intan Medika",
      Kdppklayan == "0204R015" ~ "RS Arsy Paciran",
      Kdppklayan == "0204R017" ~ "RS Citra Medika",
      Kdppklayan == "0204R018" ~ "RS Bedah Mitra Sehat",
      Kdppklayan == "0204S001" ~ "Klinik Mata Utama Lamongan",
      Kdppklayan == "0205R009" ~ "RS Denisa",
      Kdppklayan == "0205R011" ~ "RS Muhammadiyah Gresik",
      Kdppklayan == "0205R012" ~ "RS Semen Gresik",
      Kdppklayan == "0205R013" ~ "RS PKG Grha Husada",
      Kdppklayan == "0205R014" ~ "RS Petrokimia Driyorejo",
      Kdppklayan == "0205R019" ~ "RS Wali Songo I",
      Kdppklayan == "0205R021" ~ "RS Fathma Medika",
      Kdppklayan == "0205R022" ~ "RS Wates Husada",
      Kdppklayan == "0205R023" ~ "RS Surya Medika",
      Kdppklayan == "0205R024" ~ "RS PKU Muhammadiyah Sekapuk",
      Kdppklayan == "0205R025" ~ "RSI Mabarrot MWC NU Bungah",
      Kdppklayan == "0205R026" ~ "RS Rachmi Dewi",
      Kdppklayan == "0205R027" ~ "RSI Nyai Ageng Pinatih",
      Kdppklayan == "0205R028" ~ "RSI Cahaya Giri",
      Kdppklayan == "0205R029" ~ "RSUD Umar Mas'ud Bawean",
      Kdppklayan == "0205S100" ~ "Klinik Mata Utama",
      Kdppklayan == "1302R001" ~ "RSUD Ibnu Sina",
      Kdppklayan == "1302R002" ~ "RS Petrokimia Gresik",
      Kdppklayan == "1306R001" ~ "RSUD Dr Soegiri",
      Kdppklayan == "0205R031" ~ "RS Eka Husada",
      Kdppklayan == "0205R030" ~ "RS Randegansari Husada",
      Kdppklayan == "0204R020" ~ "RS Permata Bunda",
      Kdppklayan == "0204R021" ~ "RSUD Karangkembang",
      Kdppklayan == "0204R022" ~ "RS Nahdlatul Ulama Babat",
      Kdppklayan == "0204R023" ~ "RS Permata Hati",
      Kdppklayan == "0204R024" ~ "RS Muhammadiyah Kalikapas",
      Kdppklayan == "0205R032" ~ "RSIA Khodijah",
      Kdppklayan == "0205S101" ~ "Klinik Utama Gresik",
      TRUE ~ Kdppklayan))

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data <- data[order(data$Nmppklayan, data$Tglpelayanan),]
data$LOS <- as.integer(data$Tglplgsjp) - as.integer(data$Tgldtgsjp) +1

data$Diagprimer <- paste0(as.character(data$Kddiagprimer)," - ",
                          as.character(data$Nmdiagprimer))
data <- data %>% select(-Kddiagprimer,-Nmdiagprimer)
#======================================================================================
data <- data %>%
  select (Nokapst,Nosjp,Umur,Jkpst,Nmtkp,Nmdati2Layan,Nmppklayan,
          Klsrawat,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,Nminacbgs,
          Diagprimer,Diagsekunder,Procedure,Namadpjp,Nmjnspulang,LOS,
          biayars,Biayaverifikasi)

data <- data[order(data$Nokapst, data$Tglplgsjp),]
#========================================================================
write.xlsx(data, file = "CPAP Randegansari.xlsx")
#========================================================================