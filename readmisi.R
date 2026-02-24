setwd("D:/Downloads")

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
library(readxl)
library(excel.link)
library(data.table)

readmisi <- xl.read.file("readmisi.xlsx",
                          header = TRUE,
                          row.names = NULL,
                          col.names = NULL,
                          xl.sheet = "Sheet1",
                          top.left.cell = "A1",
                          na = "",
                          excel.visible = FALSE)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  subset(Nmjnspulang != "Rujuk")

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  subset(Nmjnspulang != "Rujuk")

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  subset(Nmjnspulang != "Rujuk")

datamei24 <- read_csv("Sheet 1_Full Data gresik mei_jul 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  subset(Nmjnspulang != "Rujuk")

dataagu24 <- read_csv("Sheet 1_Full Data gresik agu_okt 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL") %>%
  subset(Nmjnspulang != "Rujuk")

data <- rbind(data23,data24,dataapr24,datamei24,dataagu24)
rm(data23,data24,dataapr24,datamei24,dataagu24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$Tglstjkeu <- as.Date(data$Tglstjkeu, format = "%m/%d/%Y")
data <- data %>% subset(Tglpelayanan >= "2023-01-01")

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
      TRUE ~ Kdppklayan)) %>%
  select(-Kdppklayan)

readmisi_1 <- data %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_1 <- readmisi_1[order(readmisi_1$Nokapst,readmisi_1$Tglplgsjp),]
readmisi_1 <- readmisi_1 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_1$interval <- as.integer(readmisi_1$Tgldtgsjp) - as.integer(readmisi_1$tgl_before)
readmisi_2 <- readmisi_1 %>%
  subset(!is.na(interval))
readmisi_3 <- readmisi_2 %>%
  subset(interval <= 30) %>% select(Nokapst,Nmppklayan) %>% unique()
readmisi_4 <- left_join(readmisi_3,readmisi_1, by = c("Nokapst","Nmppklayan")) %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_4 <- readmisi_4[order(readmisi_4$Nokapst,readmisi_4$Tglplgsjp),]
readmisi_4 <- readmisi_4 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_4$interval <- as.integer(readmisi_4$Tgldtgsjp) - as.integer(readmisi_4$tgl_before)
readmisi_4$status <- with(readmisi_4,
                          ifelse(interval <= 30 , "readmisi", interval))
readmisi_4 <- readmisi_4 %>% group_by(Nokapst) %>%
  mutate(status1 = lead(status, n=1))

readmisi_5 <- readmisi_4 %>%
  subset(status == "readmisi"|status1 == "readmisi") %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_5 <- readmisi_5[order(readmisi_5$Nokapst,readmisi_5$Tglplgsjp),]
readmisi_5 <- readmisi_5 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_5$interval <- as.integer(readmisi_5$Tgldtgsjp) - as.integer(readmisi_5$tgl_before)

readmisi_5 <- readmisi_5 %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,interval,
         Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_5 <- readmisi_5[order(readmisi_5$Nokapst,readmisi_5$Tglplgsjp),]
#========================================================================
write.xlsx(readmisi_5, file = "readmisi 30 hr_ranap.xlsx")
#========================================================================

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN","SAR","MAT")) %>%
  filter(!str_detect(Procedure,"3995"))

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN","SAR","MAT")) %>%
  filter(!str_detect(Procedure,"3995"))

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN","SAR","MAT")) %>%
  filter(!str_detect(Procedure,"3995"))

datamei24 <- read_csv("Sheet 1_Full Data gresik mei_jul 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN","SAR","MAT")) %>%
  filter(!str_detect(Procedure,"3995"))

dataagu24 <- read_csv("Sheet 1_Full Data gresik agu_okt 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN","SAR","MAT")) %>%
  filter(!str_detect(Procedure,"3995"))

data <- rbind(data23,data24,dataapr24,datamei24,dataagu24)
rm(data23,data24,dataapr24,datamei24,dataagu24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$Tglstjkeu <- as.Date(data$Tglstjkeu, format = "%m/%d/%Y")
data <- data %>% subset(Tglpelayanan >= "2023-01-01")

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
      TRUE ~ Kdppklayan)) %>%
  select(-Kdppklayan)

readmisi_1 <- data %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_1 <- readmisi_1[order(readmisi_1$Nokapst,readmisi_1$Tglplgsjp),]
readmisi_1 <- readmisi_1 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_1$interval <- as.integer(readmisi_1$Tgldtgsjp) - as.integer(readmisi_1$tgl_before)
readmisi_2 <- readmisi_1 %>%
  subset(!is.na(interval))
readmisi_3 <- readmisi_2 %>%
  subset(interval <= 7) %>% select(Nokapst) %>% unique()
readmisi_4 <- left_join(readmisi_3,readmisi_1, by = c("Nokapst"="Nokapst")) %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_4 <- readmisi_4[order(readmisi_4$Nokapst,readmisi_4$Tglplgsjp),]
readmisi_4 <- readmisi_4 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_4$interval <- as.integer(readmisi_4$Tgldtgsjp) - as.integer(readmisi_4$tgl_before)
readmisi_4$status <- with(readmisi_4,
                      ifelse(interval <= 7 , "readmisi", interval))
readmisi_4 <- readmisi_4 %>% group_by(Nokapst) %>%
  mutate(status1 = lead(status, n=1))

readmisi_5 <- readmisi_4 %>%
  subset(status == "readmisi"|status1 == "readmisi") %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_5 <- readmisi_5[order(readmisi_5$Nokapst,readmisi_5$Tglplgsjp),]
readmisi_5 <- readmisi_5 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_5$interval <- as.integer(readmisi_5$Tgldtgsjp) - as.integer(readmisi_5$tgl_before)

readmisi_5 <- readmisi_5 %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,interval,
         Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)

readmisi_1 <- left_join(readmisi,data, by = c("Nosjp"="Nosjp")) %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
         Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
         Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_1 <- readmisi_1[order(readmisi_1$Nokapst,readmisi_1$Tglplgsjp),]
readmisi_1 <- readmisi_1 %>%
  group_by(Nokapst) %>%
  mutate(interval = Tgldtgsjp - lag(Tglplgsjp))

#========================================================================
write.xlsx(readmisi_5, file = "readmisi_rajal.xlsx")
#========================================================================