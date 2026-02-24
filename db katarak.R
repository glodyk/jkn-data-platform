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
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),"; ",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"cataract|Cataract")) %>%
  select(-Diagnosa)

data19$jnsop <- with(data19,
                     ifelse(str_detect(data19$Procedure,c("extracapsular extraction|Extracapsular extraction|1351|1359|intracapsular extraction|Intracapsular extraction|1311|1319")),
                                       "Extracapsular/Intracapsular cataract extraction (ICCE/ECCE)",NA))
data19$jnsop <- with(data19,
                     ifelse(str_detect(data19$Procedure,c("1369|1370|1371")),
                            "Ekstraksi katarak lainnya",jnsop))
data19$jnsop <- with(data19,
                     ifelse(deskripsisp == "PHACOEMULSIFICATION",
                            "Phacoemulsification",jnsop))
data19$jnsop <- with(data19,
                     ifelse(str_detect(data19$Procedure,c("1341")),
                            "Phacoemulsification",jnsop))
data19$jnsop <- with(data19,
                     ifelse(str_detect(data19$Procedure,c("1364|1365")),
                            "[after cataract]",jnsop))

data19 <- data19 %>% mutate(kdsa = na_if(kdsa, "None"))
data19 <- data19 %>% mutate(kdsd = na_if(kdsd, "None"))
data19 <- data19 %>% mutate(kdsi = na_if(kdsi, "None"))
data19 <- data19 %>% mutate(kdsp = na_if(kdsp, "None"))
data19 <- data19 %>% mutate(kdsr = na_if(kdsr, "None"))
data19 <- data19 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data19 <- data19 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data19 <- data19 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data19 <- data19 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data20 <- read_csv("Sheet 1_Full Data_kediri 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),"; ",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"cataract|Cataract")) %>%
  select(-Diagnosa)

data20$jnsop <- with(data20,
                     ifelse(str_detect(data20$Procedure,c("extracapsular extraction|Extracapsular extraction|1351|1359|intracapsular extraction|Intracapsular extraction|1311|1319")),
                            "Extracapsular/Intracapsular cataract extraction (ICCE/ECCE)",NA))
data20$jnsop <- with(data20,
                     ifelse(str_detect(data20$Procedure,c("1369|1370|1371")),
                            "Ekstraksi katarak lainnya",jnsop))
data20$jnsop <- with(data20,
                     ifelse(deskripsisp == "PHACOEMULSIFICATION",
                            "Phacoemulsification",jnsop))
data20$jnsop <- with(data20,
                     ifelse(str_detect(data20$Procedure,c("1341")),
                            "Phacoemulsification",jnsop))
data20$jnsop <- with(data20,
                     ifelse(str_detect(data20$Procedure,c("1364|1365")),
                            "[after cataract]",jnsop))

data20 <- data20 %>% mutate(kdsa = na_if(kdsa, "None"))
data20 <- data20 %>% mutate(kdsd = na_if(kdsd, "None"))
data20 <- data20 %>% mutate(kdsi = na_if(kdsi, "None"))
data20 <- data20 %>% mutate(kdsp = na_if(kdsp, "None"))
data20 <- data20 %>% mutate(kdsr = na_if(kdsr, "None"))
data20 <- data20 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data20 <- data20 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data20 <- data20 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data20 <- data20 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data21 <- read_csv("Sheet 1_Full Data_kediri 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),"; ",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"cataract|Cataract")) %>%
  select(-Diagnosa)

data21$jnsop <- with(data21,
                     ifelse(str_detect(data21$Procedure,c("extracapsular extraction|Extracapsular extraction|1351|1359|intracapsular extraction|Intracapsular extraction|1311|1319")),
                            "Extracapsular/Intracapsular cataract extraction (ICCE/ECCE)",NA))
data21$jnsop <- with(data21,
                     ifelse(str_detect(data21$Procedure,c("1369|1370|1371")),
                            "Ekstraksi katarak lainnya",jnsop))
data21$jnsop <- with(data21,
                     ifelse(deskripsisp == "PHACOEMULSIFICATION",
                            "Phacoemulsification",jnsop))
data21$jnsop <- with(data21,
                     ifelse(str_detect(data21$Procedure,c("1341")),
                            "Phacoemulsification",jnsop))
data21$jnsop <- with(data21,
                     ifelse(str_detect(data21$Procedure,c("1364|1365")),
                            "[after cataract]",jnsop))

data21 <- data21 %>% mutate(kdsa = na_if(kdsa, "None"))
data21 <- data21 %>% mutate(kdsd = na_if(kdsd, "None"))
data21 <- data21 %>% mutate(kdsi = na_if(kdsi, "None"))
data21 <- data21 %>% mutate(kdsp = na_if(kdsp, "None"))
data21 <- data21 %>% mutate(kdsr = na_if(kdsr, "None"))
data21 <- data21 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data21 <- data21 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data21 <- data21 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data21 <- data21 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data22 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2022.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),"; ",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"cataract|Cataract")) %>%
  select(-Diagnosa)

data22$jnsop <- with(data22,
                     ifelse(str_detect(data22$Procedure,c("extracapsular extraction|Extracapsular extraction|1351|1359|intracapsular extraction|Intracapsular extraction|1311|1319")),
                            "Extracapsular/Intracapsular cataract extraction (ICCE/ECCE)",NA))
data22$jnsop <- with(data22,
                     ifelse(str_detect(data22$Procedure,c("1369|1370|1371")),
                            "Ekstraksi katarak lainnya",jnsop))
data22$jnsop <- with(data22,
                     ifelse(deskripsisp == "PHACOEMULSIFICATION",
                            "Phacoemulsification",jnsop))
data22$jnsop <- with(data22,
                     ifelse(str_detect(data22$Procedure,c("1341")),
                            "Phacoemulsification",jnsop))
data22$jnsop <- with(data22,
                     ifelse(str_detect(data22$Procedure,c("1364|1365")),
                            "[after cataract]",jnsop))

data22 <- data22 %>% mutate(kdsa = na_if(kdsa, "None"))
data22 <- data22 %>% mutate(kdsd = na_if(kdsd, "None"))
data22 <- data22 %>% mutate(kdsi = na_if(kdsi, "None"))
data22 <- data22 %>% mutate(kdsp = na_if(kdsp, "None"))
data22 <- data22 %>% mutate(kdsr = na_if(kdsr, "None"))
data22 <- data22 %>% mutate(kdsa = na_if(kdsa, "NONE"))
data22 <- data22 %>% mutate(kdsd = na_if(kdsd, "NONE"))
data22 <- data22 %>% mutate(kdsi = na_if(kdsi, "NONE"))
data22 <- data22 %>% mutate(kdsp = na_if(kdsp, "NONE"))
data22 <- data22 %>% mutate(kdsr = na_if(kdsr, "NONE"))
data22 <- data22 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data22 <- data22 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data22 <- data22 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data22 <- data22 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data23 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2023.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),"; ",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"cataract|Cataract")) %>%
  select(-Diagnosa)

data23$jnsop <- with(data23,
                     ifelse(str_detect(data23$Procedure,c("extracapsular extraction|Extracapsular extraction|1351|1359|intracapsular extraction|Intracapsular extraction|1311|1319")),
                            "Extracapsular/Intracapsular cataract extraction (ICCE/ECCE)",NA))
data23$jnsop <- with(data23,
                     ifelse(str_detect(data23$Procedure,c("1369|1370|1371")),
                            "Ekstraksi katarak lainnya",jnsop))
data23$jnsop <- with(data23,
                     ifelse(deskripsisp == "PHACOEMULSIFICATION",
                            "Phacoemulsification",jnsop))
data23$jnsop <- with(data23,
                     ifelse(str_detect(data23$Procedure,c("1341")),
                            "Phacoemulsification",jnsop))
data23$jnsop <- with(data23,
                     ifelse(str_detect(data23$Procedure,c("1364|1365")),
                            "[after cataract]",jnsop))

data23 <- data23 %>% mutate(kdsa = na_if(kdsa, "None"))
data23 <- data23 %>% mutate(kdsd = na_if(kdsd, "None"))
data23 <- data23 %>% mutate(kdsi = na_if(kdsi, "None"))
data23 <- data23 %>% mutate(kdsp = na_if(kdsp, "None"))
data23 <- data23 %>% mutate(kdsr = na_if(kdsr, "None"))
data23 <- data23 %>% mutate(kdsa = na_if(kdsa, "NONE"))
data23 <- data23 %>% mutate(kdsd = na_if(kdsd, "NONE"))
data23 <- data23 %>% mutate(kdsi = na_if(kdsi, "NONE"))
data23 <- data23 %>% mutate(kdsp = na_if(kdsp, "NONE"))
data23 <- data23 %>% mutate(kdsr = na_if(kdsr, "NONE"))
data23 <- data23 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data23 <- data23 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data23 <- data23 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data23 <- data23 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data24 <- read_csv("Sheet 1_Full Data_kediri 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),"; ",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"cataract|Cataract")) %>%
  select(-Diagnosa)

data24$jnsop <- with(data24,
                     ifelse(str_detect(data24$Procedure,c("extracapsular extraction|Extracapsular extraction|1351|1359|intracapsular extraction|Intracapsular extraction|1311|1319")),
                            "Extracapsular/Intracapsular cataract extraction (ICCE/ECCE)",NA))
data24$jnsop <- with(data24,
                     ifelse(str_detect(data24$Procedure,c("1369|1370|1371")),
                               "Ekstraksi katarak lainnya",jnsop))
data24$jnsop <- with(data24,
                     ifelse(deskripsisp == "PHACOEMULSIFICATION",
                            "Phacoemulsification",jnsop))
data24$jnsop <- with(data24,
                     ifelse(str_detect(data24$Procedure,c("1341")),
                            "Phacoemulsification",jnsop))
data24$jnsop <- with(data24,
                     ifelse(str_detect(data24$Procedure,c("1364|1365")),
                            "[after cataract]",jnsop))

data24 <- data24 %>% mutate(kdsa = na_if(kdsa, "None"))
data24 <- data24 %>% mutate(kdsd = na_if(kdsd, "None"))
data24 <- data24 %>% mutate(kdsi = na_if(kdsi, "None"))
data24 <- data24 %>% mutate(kdsp = na_if(kdsp, "None"))
data24 <- data24 %>% mutate(kdsr = na_if(kdsr, "None"))
data24 <- data24 %>% mutate(kdsa = na_if(kdsa, "NONE"))
data24 <- data24 %>% mutate(kdsd = na_if(kdsd, "NONE"))
data24 <- data24 %>% mutate(kdsi = na_if(kdsi, "NONE"))
data24 <- data24 %>% mutate(kdsp = na_if(kdsp, "NONE"))
data24 <- data24 %>% mutate(kdsr = na_if(kdsr, "NONE"))
data24 <- data24 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data24 <- data24 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data24 <- data24 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data24 <- data24 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

dataapr24 <- read_csv("Sheet 1_Full Data_kediri (2025).csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),"; ",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"cataract|Cataract")) %>%
  select(-Diagnosa)

dataapr24$jnsop <- with(dataapr24,
                     ifelse(str_detect(dataapr24$Procedure,c("extracapsular extraction|Extracapsular extraction|1351|1359|intracapsular extraction|Intracapsular extraction|1311|1319")),
                            "Extracapsular/Intracapsular cataract extraction (ICCE/ECCE)",NA))
dataapr24$jnsop <- with(dataapr24,
                        ifelse(str_detect(dataapr24$Procedure,c("1369|1370|1371")),
                               "Ekstraksi katarak lainnya",jnsop))
dataapr24$jnsop <- with(dataapr24,
                     ifelse(deskripsisp == "PHACOEMULSIFICATION",
                            "Phacoemulsification",jnsop))
dataapr24$jnsop <- with(dataapr24,
                     ifelse(str_detect(dataapr24$Procedure,c("1341")),
                            "Phacoemulsification",jnsop))
dataapr24$jnsop <- with(dataapr24,
                     ifelse(str_detect(dataapr24$Procedure,c("1364|1365")),
                            "[after cataract]",jnsop))

dataapr24 <- dataapr24 %>% mutate(kdsa = na_if(kdsa, "None"))
dataapr24 <- dataapr24 %>% mutate(kdsd = na_if(kdsd, "None"))
dataapr24 <- dataapr24 %>% mutate(kdsi = na_if(kdsi, "None"))
dataapr24 <- dataapr24 %>% mutate(kdsp = na_if(kdsp, "None"))
dataapr24 <- dataapr24 %>% mutate(kdsr = na_if(kdsr, "None"))
dataapr24 <- dataapr24 %>% mutate(kdsa = na_if(kdsa, "NONE"))
dataapr24 <- dataapr24 %>% mutate(kdsd = na_if(kdsd, "NONE"))
dataapr24 <- dataapr24 %>% mutate(kdsi = na_if(kdsi, "NONE"))
dataapr24 <- dataapr24 %>% mutate(kdsp = na_if(kdsp, "NONE"))
dataapr24 <- dataapr24 %>% mutate(kdsr = na_if(kdsr, "NONE"))
dataapr24 <- dataapr24 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
dataapr24 <- dataapr24 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
dataapr24 <- dataapr24 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
dataapr24 <- dataapr24 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data <- rbind(data19,data20,data21,data22,data23,data24,dataapr24)
rm(data19,data20,data21,data22,data23,data24,dataapr24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$Tglreg <- as.Date(data$Tglreg, format = "%m/%d/%Y")
data$Tglstjkeu <- as.Date(data$Tglstjkeu, format = "%m/%d/%Y")

data <- data %>% 
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

data$Namadpjp01 <- trimws(gsub("dr.", "", tolower(data$Namadpjp)),"both")
data$Namadpjp01 <- trimws(gsub("dr", "", data$Namadpjp01),"both")
data$Namadpjp01 <- str_replace_all(data$Namadpjp01, "[^[:alnum:]]", " ")
data$Namadpjp01 <- trimws(gsub("sp", "sp ", data$Namadpjp01),"both")
data$Namadpjp01 <- gsub("\\s+"," ",data$Namadpjp01)
data$Namadpjp <- NULL
data <- data %>% rename(Namadpjp = Namadpjp01)

data <- data %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data <- data %>% mutate(Procedure = na_if(Procedure, "-"))

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>%
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
      Kddiagprimer == "Z011" ~ NA,
      Kddiagprimer == "Z460" ~ NA,
      Kddiagprimer == "Z530" ~ NA,
      Kddiagprimer == "Z538" ~ NA,
      Kddiagprimer == "Z539" ~ NA,
      Kddiagprimer == "Z962" ~ NA,
      Kddiagprimer == "Z969" ~ NA,
      TRUE ~ Kddiagprimer))

data$Nmdiagprimer1 <- with(data,
                           ifelse(is.na(data$Kddiagprimer1),NA,Nmdiagprimer))
data <- data %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
data$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,data$Diagprimer)), "both")
data <- data %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
data$Diagnosa <- as.character(trimws(gsub(";NA|NA;","",data$Diagnosa)), "both")

data <- data %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)

data <- data %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data <- data %>% mutate(Procedure = na_if(Procedure, "-"))
data$Diagnosa <- as.character(trimws(data$Diagnosa), "both")

library(splitstackshape)
data1 <- concat.split(data, "Diagnosa", ";")
colnames(data1)

data1$Diagnosa_01 <- as.character(trimws(data1$Diagnosa_01))
data1$Diagnosa_02 <- as.character(trimws(data1$Diagnosa_02))
data1$Diagnosa_03 <- as.character(trimws(data1$Diagnosa_03))
data1$Diagnosa_04 <- as.character(trimws(data1$Diagnosa_04))
data1$Diagnosa_05 <- as.character(trimws(data1$Diagnosa_05))
data1$Diagnosa_06 <- as.character(trimws(data1$Diagnosa_06))
data1$Diagnosa_07 <- as.character(trimws(data1$Diagnosa_07))
data1$Diagnosa_08 <- as.character(trimws(data1$Diagnosa_08))
data1$Diagnosa_09 <- as.character(trimws(data1$Diagnosa_09))
data1$Diagnosa_10 <- as.character(trimws(data1$Diagnosa_10))

data1$Diagnosa_01 <- with(data1,ifelse(is.na(data1$Diagnosa_01),NA,Diagnosa_01))
data1$Diagnosa_02 <- with(data1,ifelse(is.na(data1$Diagnosa_02),NA,Diagnosa_02))
data1$Diagnosa_03 <- with(data1,ifelse(is.na(data1$Diagnosa_03),NA,Diagnosa_03))
data1$Diagnosa_04 <- with(data1,ifelse(is.na(data1$Diagnosa_04),NA,Diagnosa_04))
data1$Diagnosa_05 <- with(data1,ifelse(is.na(data1$Diagnosa_05),NA,Diagnosa_05))
data1$Diagnosa_06 <- with(data1,ifelse(is.na(data1$Diagnosa_06),NA,Diagnosa_06))
data1$Diagnosa_07 <- with(data1,ifelse(is.na(data1$Diagnosa_07),NA,Diagnosa_07))
data1$Diagnosa_08 <- with(data1,ifelse(is.na(data1$Diagnosa_08),NA,Diagnosa_08))
data1$Diagnosa_09 <- with(data1,ifelse(is.na(data1$Diagnosa_09),NA,Diagnosa_09))
data1$Diagnosa_10 <- with(data1,ifelse(is.na(data1$Diagnosa_10),NA,Diagnosa_10))

data1 <- data1 %>%
  mutate(
    Diagnosa_1 = case_when(
      Diagnosa_01 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_01 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_01 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_01 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_01 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_01 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_01 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_01 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_01 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_01 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_01 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_01 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_01 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_01 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_01 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_01 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_01 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_01 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_01 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_01 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_01 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_01 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_01 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_01 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_01 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_01 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_01 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_01 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_01 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_01 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_01 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_01 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_01 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_01 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_01 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_01 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_01 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_01 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_01 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_01 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_01 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_01 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_01 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_01 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_01 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_01 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_01 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_01 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_01 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_01 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_01 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_01 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_01 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_01 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_01 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_01 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_01 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_01 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_01 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_01 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_01)) %>%
  select(-Diagnosa_01)

data1 <- data1 %>%
  mutate(
    Diagnosa_2 = case_when(
      Diagnosa_02 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_02 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_02 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_02 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_02 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_02 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_02 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_02 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_02 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_02 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_02 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_02 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_02 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_02 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_02 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_02 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_02 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_02 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_02 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_02 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_02 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_02 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_02 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_02 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_02 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_02 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_02 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_02 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_02 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_02 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_02 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_02 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_02 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_02 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_02 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_02 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_02 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_02 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_02 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_02 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_02 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_02 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_02 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_02 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_02 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_02 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_02 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_02 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_02 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_02 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_02 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_02 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_02 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_02 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_02 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_02 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_02 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_02 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_02 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_02 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_02)) %>%
  select(-Diagnosa_02)

data1 <- data1 %>%
  mutate(
    Diagnosa_3 = case_when(
      Diagnosa_03 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_03 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_03 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_03 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_03 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_03 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_03 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_03 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_03 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_03 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_03 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_03 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_03 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_03 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_03 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_03 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_03 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_03 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_03 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_03 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_03 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_03 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_03 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_03 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_03 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_03 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_03 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_03 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_03 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_03 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_03 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_03 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_03 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_03 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_03 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_03 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_03 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_03 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_03 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_03 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_03 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_03 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_03 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_03 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_03 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_03 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_03 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_03 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_03 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_03 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_03 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_03 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_03 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_03 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_03 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_03 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_03 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_03 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_03 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_03 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_03)) %>%
  select(-Diagnosa_03)

data1 <- data1 %>%
  mutate(
    Diagnosa_4 = case_when(
      Diagnosa_04 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_04 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_04 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_04 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_04 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_04 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_04 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_04 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_04 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_04 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_04 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_04 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_04 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_04 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_04 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_04 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_04 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_04 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_04 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_04 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_04 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_04 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_04 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_04 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_04 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_04 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_04 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_04 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_04 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_04 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_04 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_04 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_04 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_04 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_04 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_04 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_04 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_04 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_04 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_04 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_04 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_04 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_04 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_04 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_04 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_04 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_04 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_04 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_04 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_04 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_04 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_04 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_04 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_04 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_04 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_04 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_04 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_04 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_04 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_04 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_04)) %>%
  select(-Diagnosa_04)

data1 <- data1 %>%
  mutate(
    Diagnosa_5 = case_when(
      Diagnosa_05 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_05 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_05 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_05 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_05 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_05 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_05 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_05 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_05 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_05 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_05 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_05 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_05 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_05 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_05 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_05 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_05 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_05 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_05 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_05 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_05 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_05 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_05 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_05 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_05 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_05 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_05 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_05 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_05 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_05 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_05 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_05 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_05 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_05 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_05 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_05 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_05 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_05 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_05 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_05 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_05 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_05 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_05 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_05 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_05 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_05 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_05 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_05 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_05 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_05 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_05 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_05 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_05 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_05 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_05 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_05 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_05 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_05 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_05 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_05 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_05)) %>%
  select(-Diagnosa_05)

data1 <- data1 %>%
  mutate(
    Diagnosa_6 = case_when(
      Diagnosa_06 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_06 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_06 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_06 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_06 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_06 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_06 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_06 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_06 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_06 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_06 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_06 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_06 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_06 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_06 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_06 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_06 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_06 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_06 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_06 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_06 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_06 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_06 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_06 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_06 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_06 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_06 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_06 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_06 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_06 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_06 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_06 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_06 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_06 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_06 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_06 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_06 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_06 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_06 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_06 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_06 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_06 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_06 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_06 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_06 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_06 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_06 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_06 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_06 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_06 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_06 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_06 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_06 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_06 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_06 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_06 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_06 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_06 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_06 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_06 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_06)) %>%
  select(-Diagnosa_06)

data1 <- data1 %>%
  mutate(
    Diagnosa_7 = case_when(
      Diagnosa_07 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_07 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_07 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_07 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_07 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_07 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_07 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_07 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_07 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_07 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_07 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_07 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_07 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_07 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_07 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_07 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_07 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_07 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_07 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_07 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_07 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_07 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_07 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_07 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_07 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_07 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_07 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_07 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_07 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_07 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_07 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_07 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_07 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_07 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_07 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_07 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_07 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_07 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_07 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_07 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_07 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_07 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_07 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_07 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_07 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_07 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_07 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_07 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_07 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_07 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_07 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_07 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_07 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_07 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_07 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_07 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_07 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_07 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_07 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_07 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_07)) %>%
  select(-Diagnosa_07)

data1 <- data1 %>%
  mutate(
    Diagnosa_8 = case_when(
      Diagnosa_08 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_08 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_08 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_08 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_08 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_08 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_08 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_08 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_08 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_08 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_08 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_08 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_08 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_08 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_08 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_08 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_08 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_08 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_08 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_08 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_08 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_08 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_08 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_08 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_08 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_08 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_08 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_08 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_08 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_08 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_08 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_08 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_08 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_08 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_08 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_08 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_08 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_08 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_08 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_08 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_08 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_08 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_08 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_08 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_08 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_08 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_08 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_08 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_08 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_08 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_08 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_08 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_08 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_08 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_08 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_08 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_08 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_08 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_08 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_08 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_08)) %>%
  select(-Diagnosa_08)

data1 <- data1 %>%
  mutate(
    Diagnosa_9 = case_when(
      Diagnosa_09 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_09 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_09 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_09 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_09 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_09 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_09 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_09 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_09 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_09 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_09 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_09 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_09 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_09 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_09 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_09 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_09 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_09 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_09 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_09 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_09 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_09 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_09 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_09 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_09 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_09 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_09 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_09 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_09 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_09 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_09 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_09 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_09 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_09 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_09 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_09 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_09 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_09 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_09 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_09 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_09 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_09 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_09 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_09 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_09 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_09 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_09 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_09 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_09 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_09 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_09 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_09 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_09 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_09 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_09 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_09 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_09 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_09 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_09 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_09 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_09)) %>%
  select(-Diagnosa_09)

data1 <- data1 %>%
  mutate(
    Diagnosa_010 = case_when(
      Diagnosa_10 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_10 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_10 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_10 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_10 == "Z511 - Chemotherapy session for neoplasm" ~ NA,
      Diagnosa_10 == "Z131 - Special screening examination for diabetes mellitus" ~ NA,
      Diagnosa_10 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_10 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_10 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_10 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_10 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_10 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_10 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_10 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_10 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_10 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_10 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_10 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_10 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_10 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_10 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_10 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_10 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_10 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_10 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_10 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_10 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_10 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      Diagnosa_10 == "T932 - Sequelae of other fractures of lower limb" ~ NA,
      Diagnosa_10 == "Z038 - Observation for other suspected diseases and conditions" ~ NA,
      Diagnosa_10 == "Z033 - Observation for suspected nervous system disorder" ~ NA,
      Diagnosa_10 == "Z470 - Follow-up care involving removal of fracture plate and other internal fixation device" ~ NA,
      Diagnosa_10 == "Z478 - Other specified orthopaedic follow-up care" ~ NA,
      Diagnosa_10 == "Z479 - Orthopaedic follow-up care, unspecified" ~ NA,
      Diagnosa_10 == "Z544 - Convalescence following treatment of fracture" ~ NA,
      Diagnosa_10 == "Z813 - Family history of other psychoactive substance abuse" ~ NA,
      Diagnosa_10 == "Z837 - Family history of diseases of the digestive system" ~ NA,
      Diagnosa_10 == "Z843 - Family history of consanguinity" ~ NA,
      Diagnosa_10 == "Z853 - Personal history of malignant neoplasm of breast" ~ NA,
      Diagnosa_10 == "Z861 - Personal history of infectious and parasitic diseases" ~ NA,
      Diagnosa_10 == "Z863 - Personal history of endocrine, nutritional and metabolic diseases" ~ NA,
      Diagnosa_10 == "Z866 - Personal history of diseases of the nervous system and sense organs" ~ NA,
      Diagnosa_10 == "Z867 - Personal history of diseases of the circulatory system" ~ NA,
      Diagnosa_10 == "Z871 - Personal history of diseases of the digestive system" ~ NA,
      Diagnosa_10 == "Z872 - Personal history of diseases of the skin and subcutaneous tissue" ~ NA,
      Diagnosa_10 == "Z873 - Personal history of diseases of the musculoskeletal system and connective tissue" ~ NA,
      Diagnosa_10 == "Z966 - Presence of orthopaedic joint implants" ~ NA,
      Diagnosa_10 == "Z876 - Personal history of certain conditions arising in the perinatal period" ~ NA,
      Diagnosa_10 == "Z870 - Personal history of diseases of the respiratory system" ~ NA,
      Diagnosa_10 == "Z713 - Dietary counselling and surveillance" ~ NA,
      Diagnosa_10 == "Z010 - Examination of eyes and vision" ~ NA,
      Diagnosa_10 == "Z960 - Presence of urogenital implants" ~ NA,
      Diagnosa_10 == "Z548 - Convalescence following other treatment" ~ NA,
      Diagnosa_10 == "Z011 - Examination of ears and hearing" ~ NA,
      Diagnosa_10 == "Z460 - Fitting and adjustment of spectacles and contact lenses" ~ NA,
      Diagnosa_10 == "Z530 - Procedure not carried out because of contraindication" ~ NA,
      Diagnosa_10 == "Z538 - Procedure not carried out for other reasons" ~ NA,
      Diagnosa_10 == "Z539 - Procedure not carried out, unspecified reason" ~ NA,
      Diagnosa_10 == "Z962 - Presence of otological and audiological implants" ~ NA,
      Diagnosa_10 == "Z969 - Presence of functional implant, unspecified" ~ NA,
      TRUE ~ Diagnosa_10)) %>%
  select(-Diagnosa_10)

data1 <- data1 %>%
  mutate(Diagnosa = paste0(as.character(Diagnosa_1),";",
                           as.character(Diagnosa_2),";",
                           as.character(Diagnosa_3),";",
                           as.character(Diagnosa_4),";",
                           as.character(Diagnosa_5),";",
                           as.character(Diagnosa_6),";",
                           as.character(Diagnosa_7),";",
                           as.character(Diagnosa_8),";",
                           as.character(Diagnosa_9),";",
                           as.character(Diagnosa_010)))

data1$Diagnosa <- as.character(trimws(gsub("NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;|NA","",data1$Diagnosa)),"both")
data1 <- data1 %>% mutate(Diagnosa = na_if(Diagnosa, ""))
data1 <- data1 %>%
  select(-Diagnosa_1,-Diagnosa_2,-Diagnosa_3,-Diagnosa_4,-Diagnosa_5,
         -Diagnosa_6,-Diagnosa_7,-Diagnosa_8,-Diagnosa_9,-Diagnosa_010)
data1 <- data1 %>% mutate(Diagnosa = na_if(Diagnosa, ""))

data1$Diagnosa_nu <- with(data1,
                          ifelse(Diagnosa == "H409 - Glaucoma, unspecified;H269 - Cataract, unspecified",
                                           "H269 - Cataract, unspecified;H409 - Glaucoma, unspecified",
                                 ifelse(Diagnosa == "H028 - Other specified disorders of eyelid;H269 - Cataract, unspecified",
                                        "H269 - Cataract, unspecified;H028 - Other specified disorders of eyelid",
                                        ifelse(Diagnosa =="H028 - Other specified disorders of eyelid;H259 - Senile cataract, unspecified",
                                               "H259 - Senile cataract, unspecified;H028 - Other specified disorders of eyelid",
                                               ifelse(Diagnosa == "H409 - Glaucoma, unspecified;H259 - Senile cataract, unspecified",
                                                      "H259 - Senile cataract, unspecified;H409 - Glaucoma, unspecified",
                                                      ifelse(Diagnosa == "H401 - Primary open-angle glaucoma;H250 - Senile incipient cataract",
                                                             "H250 - Senile incipient cataract;H401 - Primary open-angle glaucoma",
                                                             ifelse(Diagnosa == "H041 - Other disorders of lacrimal gland;H269 - Cataract, unspecified",
                                                                    "H269 - Cataract, unspecified;H041 - Other disorders of lacrimal gland",
                                                                    ifelse(Diagnosa == "H409 - Glaucoma, unspecified;H250 - Senile incipient cataract",
                                                                           "H250 - Senile incipient cataract;H409 - Glaucoma, unspecified",Diagnosa))))))))


data1$Umur <- abs(data1$Umur)
data1$kel_umur <- with(data1,
                      ifelse(Umur >= 0 & Umur < 1, "< 1 Tahun",
                             ifelse(Umur >= 1 & Umur < 5, "1-4 Tahun",
                                    ifelse(Umur >= 5 & Umur < 12, "5-11 Tahun",
                                           ifelse(Umur >= 12 & Umur < 17, "12-16 Tahun",
                                                  ifelse(Umur >= 17 & Umur <= 25, "17-25 Tahun",
                                                         ifelse(Umur > 25 & Umur <= 35, "26-35 Tahun",
                                                                ifelse(Umur > 35 & Umur <= 45, "36-45 Tahun",
                                                                       ifelse(Umur > 45 & Umur <= 55, "46-55 Tahun",
                                                                              ifelse(Umur > 55 & Umur <= 65, "56-65 Tahun",
                                                                                     ifelse(Umur > 65 & Umur < 75, "66-74 Tahun",
                                                                                            ifelse(Umur >= 75 & Umur <= 90, "75-90 Tahun",
                                                                                                   ifelse(Umur > 90, "> 90 Tahun",
                                                                                                          "Salah")))))))))))))


data2 <- data1 %>%
  dplyr::group_by(Nokapst) %>%
  dplyr::summarise(operasi=paste0(jnsop, collapse = ";"))

data2$operasi <- sapply(strsplit(data2$operasi, ";"),
                      function(x) paste(unique(x), collapse = ";"))
data2 <- data2 %>% mutate(operasi = na_if(operasi, "NA"))
data2$Flagg <- with(data2,
                    ifelse(is.na(operasi),"Non Operasi","Operasi"))
data2 <- data2 %>% select(Nokapst,Flagg)
data1 <- left_join(data1,data2,by = c("Nokapst"="Nokapst"))

write.csv(data1,
          "D://data gresik//MTF KC GRESIK//gresik_2020//tmp//kediri//db katarak.csv",
          na="", row.names = FALSE)

data1 <- data1 %>%
  select(Nokapst,Umur,Jkpst,jnsop,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
         Typeppkperujuk,Nmppkperujuk,Nmppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
         Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
         Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
         deskripsisr,Diagsekunder,Procedure,Diagnosa,Namadpjp,Nmjnspulang,tarifsa,
         tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi)

write.csv(data1,
          "D://data gresik//MTF KC GRESIK//gresik_2020//tmp//kediri//db katarak.csv",
          na="", row.names = FALSE)
#===================================================================================
write.xlsx(data, file = "db katarak.xlsx")
