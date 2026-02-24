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
  subset(Nmtkp == "RITL")
data19 <- data19 %>% mutate(kdsa = na_if(kdsa, "None"))
data19 <- data19 %>% mutate(kdsd = na_if(kdsd, "None"))
data19 <- data19 %>% mutate(kdsi = na_if(kdsi, "None"))
data19 <- data19 %>% mutate(kdsp = na_if(kdsp, "None"))
data19 <- data19 %>% mutate(kdsr = na_if(kdsr, "None"))
data19 <- data19 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data19 <- data19 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data19 <- data19 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data19 <- data19 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data19$Politujsjp <- data19$Politujsjp %>%
  modify_if(is.character, toupper)

data20 <- read_csv("Sheet 1_Full Data_kediri 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL")
data20 <- data20 %>% mutate(kdsa = na_if(kdsa, "None"))
data20 <- data20 %>% mutate(kdsd = na_if(kdsd, "None"))
data20 <- data20 %>% mutate(kdsi = na_if(kdsi, "None"))
data20 <- data20 %>% mutate(kdsp = na_if(kdsp, "None"))
data20 <- data20 %>% mutate(kdsr = na_if(kdsr, "None"))
data20 <- data20 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data20 <- data20 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data20 <- data20 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data20 <- data20 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data20$Politujsjp <- data20$Politujsjp %>%
  modify_if(is.character, toupper)

data21 <- read_csv("Sheet 1_Full Data_kediri 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL")
data21 <- data21 %>% mutate(kdsa = na_if(kdsa, "None"))
data21 <- data21 %>% mutate(kdsd = na_if(kdsd, "None"))
data21 <- data21 %>% mutate(kdsi = na_if(kdsi, "None"))
data21 <- data21 %>% mutate(kdsp = na_if(kdsp, "None"))
data21 <- data21 %>% mutate(kdsr = na_if(kdsr, "None"))
data21 <- data21 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
data21 <- data21 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
data21 <- data21 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
data21 <- data21 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

data21$Politujsjp <- data21$Politujsjp %>%
  modify_if(is.character, toupper)

data22 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2022.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL")
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

data22$Politujsjp <- data22$Politujsjp %>%
  modify_if(is.character, toupper)

data23 <- read_csv("Sheet 1_Full Data_FactCBGKCLayanan_HV_2023.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL")
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

data23$Politujsjp <- data23$Politujsjp %>%
  modify_if(is.character, toupper)

data24 <- read_csv("Sheet 1_Full Data_kediri 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL")
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

data24$Politujsjp <- data24$Politujsjp %>%
  modify_if(is.character, toupper)

datamei24 <- read_csv("Sheet 1_Full Data_kediri 2025.csv") %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RITL")
datamei24 <- datamei24 %>% mutate(kdsa = na_if(kdsa, "None"))
datamei24 <- datamei24 %>% mutate(kdsd = na_if(kdsd, "None"))
datamei24 <- datamei24 %>% mutate(kdsi = na_if(kdsi, "None"))
datamei24 <- datamei24 %>% mutate(kdsp = na_if(kdsp, "None"))
datamei24 <- datamei24 %>% mutate(kdsr = na_if(kdsr, "None"))
datamei24 <- datamei24 %>% mutate(kdsa = na_if(kdsa, "NONE"))
datamei24 <- datamei24 %>% mutate(kdsd = na_if(kdsd, "NONE"))
datamei24 <- datamei24 %>% mutate(kdsi = na_if(kdsi, "NONE"))
datamei24 <- datamei24 %>% mutate(kdsp = na_if(kdsp, "NONE"))
datamei24 <- datamei24 %>% mutate(kdsr = na_if(kdsr, "NONE"))
datamei24 <- datamei24 %>% mutate(deskripsisd = na_if(deskripsisd, "-"))
datamei24 <- datamei24 %>% mutate(deskripsisi = na_if(deskripsisi, "-"))
datamei24 <- datamei24 %>% mutate(deskripsisp = na_if(deskripsisp, "-"))
datamei24 <- datamei24 %>% mutate(deskripsisr = na_if(deskripsisr, "-"))

datamei24$Politujsjp <- datamei24$Politujsjp %>%
  modify_if(is.character, toupper)

data <- rbind(data19,data20,data21,data22,data23,data24,datamei24)
rm(data19,data20,data21,data22,data23,data24,datamei24)

data <- data %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data <- data %>% mutate(Procedure = na_if(Procedure, "-"))

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

data$Diagprimer <- paste0(as.character(data$Kddiagprimer)," - ",
                          as.character(data$Nmdiagprimer))

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$Tglreg <- as.Date(data$Tglreg, format = "%m/%d/%Y")
data$Tglstjkeu <- as.Date(data$Tglstjkeu, format = "%m/%d/%Y")

data <- data[order(data$Nmppklayan, data$Tglplgsjp),]
data$LOS <- as.integer(data$Tglplgsjp) - as.integer(data$Tgldtgsjp) +1

data <- data %>% subset(Tglpelayanan >= "2019-01-01")
#======================================================================================
library(excel.link)
readmisi <- xl.read.file("readmisi 30 hari.xlsx",
                        header = TRUE,
                        row.names = NULL,
                        col.names = NULL,
                        xl.sheet = "Sheet1",
                        top.left.cell = "A1",
                        na = "",
                        excel.visible = FALSE)

data <- left_join(readmisi,data, by = c("Nosjp"="Nosjp"))

data <- data %>%
  select (Nokapst,Nosjp,Umur,Jkpst,Nmtkp,Nmdati2Layan,Nmppklayan,
          Klsrawat,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,Nminacbgs,
          Diagprimer,Diagsekunder,Procedure,Namadpjp,Nmjnspulang,LOS,
          biayars,Biayaverifikasi)

data <- data[order(data$Nokapst, data$Tglplgsjp),]
data <- data %>%
  group_by(Nokapst) %>%
  mutate(interval = difftime(Tgldtgsjp, lag(Tglplgsjp),
                             units = 'days'))
#========================================================================
write.xlsx(data, file = "Readmisi 30 hari.xlsx")
#========================================================================
data <- data[order(data$Tglplgsjp,
                   data$Kdppklayan,
                   data$Nosjp),]

data <- data %>%
  subset(Keterangan %in% c("Infection perinatal","Hipoglikemia",
                           "Ikterus neonatus","Poor-feeding neonatal",
                           "Gangguan suhu tubuh",
                           "Gangguan pencernaan perinatal"))
data <- data %>%
  select(Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppklayan,
         Tgldtgsjp,Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
         Biayaverifikasi,Keterangan)

#========================================================================
write.xlsx(data, file = "vpk_bayi.xlsx")
#========================================================================