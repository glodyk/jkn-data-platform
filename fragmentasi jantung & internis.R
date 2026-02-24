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
library(readxl)
library(excel.link)
library(data.table)

data19 <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

data20 <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

data21 <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

data22 <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

datamei24 <- read_csv("Sheet 1_Full Data gresik mei_jul 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

dataagu24 <- read_csv("Sheet 1_Full Data gresik agu_okt 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Politujsjp %in% c("INT","JAN"))

data <- rbind(data19,data20,data21,data22,data23,data24,dataapr24,datamei24,
              dataagu24)
rm(data19,data20,data21,data22,data23,data24,dataapr24,datamei24,dataagu24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$Tglstjkeu <- as.Date(data$Tglstjkeu, format = "%m/%d/%Y")
data <- data %>% subset(Tglpelayanan >= "2019-01-01")

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

#===============================SKIP======================================
write.csv(data, "D://data gresik//MTF KC GRESIK//gresik_2020//fragmentasi_int_jan.csv",
          na="", row.names = FALSE)
#===============================SKIP======================================
#agar bisa SUBSET
data$Kdppklayan <- as.character(trimws(substr(data$Nosjp,1,8)), "both")
data <- data %>%
  mutate(koreksi = paste0(as.character(Kdppklayan),"; ",
                          as.character(Kdinacbgs))) %>%
  select(-Kdppklayan)
#===============================SKIP======================================
cek <- data %>% 
  subset(Nmtkp == "RJTL") %>%
  subset(is.na(Politujsjp))
ceknoka <- data %>% 
  subset(Nmtkp == "RJTL") %>%
  subset(Nokapst %in% c("451898163","99360313"))
cek1 <- data %>%
  subset(Nosjp == "0204R0040321V000856")
cek1$Politujsjp <- "SAR"
cek2 <- data %>%
  subset(Nosjp == "1302R0021123V016039")
cek2$Politujsjp <- "IRM"
dataeror <- data %>% 
  subset(Nosjp != "0204R0040321V000856") %>%
  subset(Nosjp != "1302R0021123V016039")
data <- rbind(dataeror,cek1,cek2)
cek <- data %>% 
  subset(Nmtkp == "RJTL") %>%
  subset(is.na(Politujsjp))
rm(cek,cek1,cek2,dataeror)
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

df_data <- left_join(data,refpoli, by = c("Politujsjp"="KDPOLI")) %>%
  select(-Politujsjp)
df_data <- df_data %>% rename(Politujsjp = NMPOLI)
rm(data,refpoli)

df_data <- df_data[order(df_data$Nokapst,df_data$Tglplgsjp),]
df_data <- df_data %>% subset(Tglpelayanan >= "2019-01-01")

df_data$Namadpjp01 <- trimws(gsub("dr.", "", tolower(df_data$Namadpjp)),"both")
df_data$Namadpjp01 <- trimws(gsub("dr", "", df_data$Namadpjp01),"both")
df_data$Namadpjp01 <- str_replace_all(df_data$Namadpjp01, "[^[:alnum:]]", " ")
df_data$Namadpjp01 <- trimws(gsub("sp", "sp ", df_data$Namadpjp01),"both")
df_data$Namadpjp01 <- gsub("\\s+"," ",df_data$Namadpjp01)
df_data$Namadpjp <- NULL
df_data <- df_data %>% rename(Namadpjp = Namadpjp01)
#========================================================================
#CMG, Specific CBG, SEVERITY
df_data$CMG <- as.character(trimws(substr(df_data$Kdinacbgs,1,1)), "both")
df_data$CBG <- as.character(trimws(substr(df_data$Kdinacbgs,1,6)), "both")
df_data$Spec <- as.character(trimws(substr(df_data$Kdinacbgs,5,6)), "both")
df_data$Kdsevel <- as.character(trimws(substr(df_data$Kdinacbgs,8,10)), "both")
df_data <- df_data %>%
  mutate(
    Sevel = case_when(
      Kdsevel == "0" ~ "0",
      Kdsevel == "I" ~ "1",
      Kdsevel == "II" ~ "2",
      Kdsevel == "III" ~ "3",
      TRUE ~ Kdsevel)) %>%
  select(-Kdsevel) 

df_data$tp <- as.character(trimws(substr(df_data$Kdinacbgs,3,3)), "both")
df_data <- data.frame(df_data)
df_data <- df_data %>%
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
df_data$Nminacbgs <- as.character(trimws(df_data$Nminacbgs), "both")
df_data$kel_umur <- with(df_data,
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


#==============================================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")

df18 <- read_csv("Sheet_1_Full_Data_data (2018).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df19 <- read_csv("Sheet_1_Full_Data_data (2019).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df20 <- read_csv("Sheet_1_Full_Data_data (2020).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df21 <- read_csv("Sheet_1_Full_Data_data (2021).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df22 <- read_csv("Sheet_1_Full_Data_data (2022).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df23 <- read_csv("Sheet_1_Full_Data_data (2023).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df24 <- read_csv("Sheet_1_Full_Data_data (2024).csv") %>%
  select(Nosep,Norjkawalsep,Sumber,jenisrujukaninternal) %>%
  rename(Nosjp = Nosep,
         Sumber2 = Sumber)

df <- rbind(df18,df19,df20,df21,df22,df23,df24)
rm(df18,df19,df20,df21,df22,df23,df24)

df$Sumber <- with(df,
                  ifelse(is.na(df$jenisrujukaninternal),
                         Sumber2,jenisrujukaninternal))
cek <- df %>% subset(is.na(Sumber))
rm(cek)
df <- df %>% select(-Sumber2,-jenisrujukaninternal)

cek <- df %>% subset(is.na(Sumber))
rm(cek)
#=========================================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("df_rujukan.rda")

df_ruj <- df_ruj %>% select(nokapst,no_kunjungan,Diagnosa)
df_ruj <- df_ruj %>% rename(Diagmasuk = Diagnosa)
#=========================================================================
df1 <- left_join(df,df_ruj, by = c("Norjkawalsep"="no_kunjungan"))
cek <- df1 %>%
  subset(Sumber == "Rujukan FKTP") %>%
  subset(is.na(Diagmasuk))
rm(cek,df,df_ruj)

data <- left_join(df_data,df1, by = c("Nosjp"="Nosjp"))

cek <- data %>% subset(Nokapst != nokapst)
rm(cek)
data <- data %>% select(-nokapst)
rm(df1,df_data)

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>%
  group_by(Nokapst) %>%
  mutate(Poli_asal = lag(Politujsjp,n=1))

datarj <- data %>% subset(Nmtkp == "RJTL")
datari <- data %>% subset(Nmtkp == "RITL")

datarj <- datarj[order(datarj$Nokapst,datarj$Tglplgsjp),]

datarjIGD <- datarj %>%
  subset(is.na(Norjkawalsep)) %>%
  subset(Politujsjp == "Instalasi Gawat Darurat")
cek <- datarjIGD %>% subset(is.na(Sumber))
cek$Sumber <- "UGD"
dataeror <- datarjIGD %>% subset(!is.na(Sumber))
datarjIGD <- rbind(cek,dataeror)
rm(cek,dataeror)

datarjnonIGD <- datarj %>%
  subset(Politujsjp != "Instalasi Gawat Darurat")
datarjnonIGD <- datarjnonIGD[order(datarjnonIGD$Nokapst,datarjnonIGD$Tglplgsjp),]

datarjnonIGD <- datarjnonIGD %>%
  group_by(Nokapst,Norjkawalsep) %>%
  mutate(Poli_asal = first(Politujsjp))

dataempty <- datarjnonIGD %>% subset(is.na(Sumber))
dataempty$Sumber2 <- with(dataempty,
                          ifelse(Politujsjp == Poli_asal,
                                 "Kontrol Ulang","Rujukan Internal"))
dataempty <- dataempty %>% select(-Sumber)
dataempty <- dataempty %>% rename(Sumber = Sumber2)

datarjfktp <- datarjnonIGD %>% subset(Sumber == "Rujukan FKTP")
datarjantars <- datarjnonIGD %>% subset(Sumber == "Rujukan Antar RS")
datarjkontri <- datarjnonIGD %>% subset(Sumber %in% c("Rujukan Internal",
                                                      "Kontrol Ulang"))

cek <- datarjkontri %>% subset(is.na(Sumber))
rm(cek)

datarjkontri$Sumber2 <- with(datarjkontri,
                             ifelse(Politujsjp == Poli_asal,
                                    "Kontrol Ulang","Rujukan Internal"))
datarjkontri <- datarjkontri %>% select(-Sumber)
datarjkontri <- datarjkontri %>% rename(Sumber = Sumber2)

datarj <- rbind(datarjIGD,datarjfktp,datarjkontri,datarjantars,dataempty)
rm(datarjIGD,datarjnonIGD,datarjfktp,datarjkontri,datarjantars,dataempty)

data <- rbind(datari,datarj)
rm(datari,datarj)
data <- data[order(data$Nokapst,data$Tglplgsjp),]
#=======================================================================
cek$Sumber <- NULL 
cek <- cek %>% rename(Sumber = Sumber3)
dataeror <- datarjnonIGD %>%
  subset(!is.na(Sumber))

datarjnonIGD <- rbind(dataeror,cek)
rm(dataeror,cek)

datarjnonIGD <- datarjnonIGD[order(datarjnonIGD$Nokapst,datarjnonIGD$Tglplgsjp),]
#=======================================================================
datarj <- rbind(datarjnonIGD,datarjIGD)
rm(datarjnonIGD,datarjIGD)
datarj <- datarj[order(datarj$Nokapst,datarj$Tglplgsjp),]
cek <- datarj %>% subset(is.na(Sumber))
rm(cek)

data <- rbind(datarj,datari)
rm(datarj,datari)


data <- data %>% select(-Sumber2,-jenisrujukaninternal)

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
data$jeda <- as.integer(data$Tgldtgsjp) - as.integer(data$tgl_before)
data$jeda <- as.numeric(data$jeda) %>%
  replace(is.na(.), "")
data$jeda <- as.factor(data$jeda)

data$Umur <- abs(data$Umur)
data$kel_umur <- with(data,
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