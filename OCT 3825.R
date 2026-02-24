setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

# Core packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(stringr)
library(lubridate)
library(randomForest)
library(styler)

data14 <- read_csv("Sheet 1_Full Data gresik 2014.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data15 <- read_csv("Sheet 1_Full Data gresik 2015.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data16 <- read_csv("Sheet 1_Full Data gresik 2016.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data17 <- read_csv("Sheet 1_Full Data gresik 2017.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data18 <- read_csv("Sheet 1_Full Data gresik 2018.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data19 <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data20 <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data21 <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data22 <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

apr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

mei24 <- read_csv("Sheet 1_Full Data gresik mei_jun 2024.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmdati2Layan,Nmppkperujuk,Kdppklayan,,
          Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi,kdsp,deskripsisp) %>%
  subset(Nmtkp == "RJTL") %>%
  filter(str_detect(Procedure, c("3825")))

data <- rbind(data14,data15,data16,data17,data18,data19,data20,
              data21,data22,data23,data24,apr24,mei24)
rm(data14,data15,data16,data17,data18,data19,data20,
   data21,data22,data23,data24,apr24,mei24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")

data <- data %>% subset(Tglpelayanan >= "2016-01-01")
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

df_data <- left_join(data,refpoli, by = c("Politujsjp"="KDPOLI"))
df_data <- df_data %>%
  rename(NmPoli = NMPOLI)
rm(data,refpoli)

df_data <- df_data %>%
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

df_data <- df_data %>% select (-Kdppklayan)

df_data <- df_data[order(df_data$Nokapst, df_data$Tglplgsjp),]
df_data <- df_data %>%
  group_by(Nokapst) %>%
  mutate(interval = Tglplgsjp - lag(Tglplgsjp))

df_data$interval <- as.numeric(df_data$interval) %>%
  replace(is.na(.),0)
df_data$interval <- as.factor(df_data$interval)
write.csv(df_data, "D://data gresik//MTF KC GRESIK//gresik_2020//ngimbang_ur.csv",
          na="", row.names = FALSE)


data14$Diagprimer <- paste0(as.character(data$Kddiagprimer)," - ",
                            as.character(data$Nmdiagprimer))
data14$Diagsekunder [is.na(data$Diagsekunder)] <- ""





data <- rbind(data14,data15,data16,data17,data18,data19,data20,
              data21,data22,data23,df_jan24,df_feb24,df_mar24)

rm(data14,data15,data16,data17,data18,data19,data20,
   data21,data22,data23,df_jan24,df_feb24,df_mar24)


data$LOS <- as.integer(data$Tglplgsjp) - as.integer(data$Tgldtgsjp) +1

#Transform Nmtkp menjadi 1&2 --> 1= berarti RITL, 2= berarti RJTL
data$Nmtkp[data$Nmtkp=="RITL"] <- "1"
data$Nmtkp[data$Nmtkp=="RJTL"] <- "2"
data$Nmtkp <- as.factor(data$Nmtkp)

#========================================================================
data <- data %>% rename(pisa = Pisapst,
                        kdpst = Kdjnspeserta,
                        prb = Pstprb,
                        DM = Kddiagmasuk,
                        NmDM = Nmdiagmasuk,
                        Poli = Politujsjp,
                        SEP = Nosjp,
                        DU =  Kddiagprimer, 
                        NmDU = Nmdiagprimer,
                        DS = Diagsekunder,
                        Jmlproc = Jmlprocedure,
                        Kdjnsplg = Kdjnspulang,
                        Start = Tgldtgsjp,
                        End = Tglplgsjp, 
                        BiayaRS = biayars,
                        Biaya = Biayaverifikasi,
                        Tarif = Tarifgroup,
                        Pemilik = Kepemilikanppklayan)

data$Namadpjp01 <- trimws(gsub("dr.", "", tolower(data$Namadpjp)),"both")
data$Namadpjp01 <- trimws(gsub("dr", "", data$Namadpjp01),"both")
data$Namadpjp01 <- str_replace_all(data$Namadpjp01, "[^[:alnum:]]", " ")
data$Namadpjp01 <- trimws(gsub("sp", "sp ", data$Namadpjp01),"both")
data$Namadpjp01 <- gsub("\\s+"," ",data$Namadpjp01)
data <- data %>% rename(DPJP = Namadpjp01)
data$Namadpjp <- NULL

colnames(data)
glimpse(data)
str(data)

data <- data[order(data$Nokapst, data$End),]
data <- data %>%
  group_by(Nokapst) %>%
  mutate(Prvtkp = lag(Nmtkp))

data$Prvtkp <- as.numeric(data$Prvtkp) %>%
  replace(is.na(.), "")
data$Prvtkp <- as.factor(data$Prvtkp)
#========================================================================
library(MASS)
library(caTools)
split <- sample.split(data$NmFKRTL, SplitRatio = 0.7)
#========================================================================

#CMG, Specific CBG, SEVERITY
data$CMG <- as.character(trimws(substr(data$Kdinacbgs,1,1)), "both")
data$CBG <- as.character(trimws(substr(data$Kdinacbgs,1,6)), "both")
data$Spec <- as.character(trimws(substr(data$Kdinacbgs,5,6)), "both")
data$Kdsevel <- as.character(trimws(substr(data$Kdinacbgs,8,10)), "both")
data <- data %>% 
  mutate(
    Sevel = case_when(
      Kdsevel == "0" ~ "0",
      Kdsevel == "I" ~ "1",
      Kdsevel == "II" ~ "2",
      Kdsevel == "III" ~ "3",
      TRUE ~ Kdsevel)) %>%
  select(-Kdsevel) 

data$tp <- as.character(trimws(substr(data$Kdinacbgs,3,3)), "both")
data <- data.frame(data)
data <- data %>% 
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

#Transform Jkpst menjadi 1&2 --> 1= berarti Laki-laki, 2= berarti Perempuan
data$Jkpst[data$Jkpst=="Laki-laki"] <- "1"
data$Jkpst[data$Jkpst=="Perempuan"] <- "2"
data$Jkpst <- as.factor(data$Jkpst)

#Transform prb menjadi 0&1 --> 0= berarti FALSE, 1= berarti TRUE
data$prb[data$prb=="FALSE"] <- "0"
data$prb[data$prb=="TRUE"] <- "1"
data$prb <- as.factor(data$prb)

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

#agar bisa SUBSET
data$Kdppk <- as.character(trimws(substr(data$SEP,1,8)), "both")
#===============================SKIP======================================

data <- data %>% 
  mutate(
    NmFKRTL = case_when(
      Kdppk == "0204R003" ~ "RSUD Ngimbang",
      Kdppk == "0204R004" ~ "RS Suyudi Paciran (JST)",
      Kdppk == "0204R009" ~ "RSI Nashrul Ummah",
      Kdppk == "0204R010" ~ "RS Muhammadiyah Lamongan",
      Kdppk == "0204R012" ~ "RS Muhammadiyah Babat",
      Kdppk == "0204R019" ~ "RSU Muhammadiyah Babat",
      Kdppk == "0204R013" ~ "RS Fatimah",
      Kdppk == "0204R014" ~ "RS Intan Medika",
      Kdppk == "0204R015" ~ "RS Arsy Paciran",
      Kdppk == "0204R017" ~ "RS Citra Medika",
      Kdppk == "0204R018" ~ "RS Bedah Mitra Sehat",
      Kdppk == "0204S001" ~ "Klinik Mata Utama Lamongan",
      Kdppk == "0205R009" ~ "RS Denisa",
      Kdppk == "0205R011" ~ "RS Muhammadiyah Gresik",
      Kdppk == "0205R012" ~ "RS Semen Gresik",
      Kdppk == "0205R013" ~ "RS PKG Grha Husada",
      Kdppk == "0205R014" ~ "RS Petrokimia Driyorejo",
      Kdppk == "0205R019" ~ "RS Wali Songo I",
      Kdppk == "0205R021" ~ "RS Fathma Medika",
      Kdppk == "0205R022" ~ "RS Wates Husada",
      Kdppk == "0205R023" ~ "RS Surya Medika",
      Kdppk == "0205R024" ~ "RS PKU Muhammadiyah Sekapuk",
      Kdppk == "0205R025" ~ "RSI Mabarrot MWC NU Bungah",
      Kdppk == "0205R026" ~ "RS Rachmi Dewi",
      Kdppk == "0205R027" ~ "RSI Nyai Ageng Pinatih",
      Kdppk == "0205R028" ~ "RSI Cahaya Giri",
      Kdppk == "0205R029" ~ "RSUD Umar Mas'ud Bawean",
      Kdppk == "0205S100" ~ "Klinik Mata Utama",
      Kdppk == "1302R001" ~ "RSUD Ibnu Sina",
      Kdppk == "1302R002" ~ "RS Petrokimia Gresik",
      Kdppk == "1306R001" ~ "RSUD Dr Soegiri",
      Kdppk == "0205R031" ~ "RS Eka Husada",
      Kdppk == "0205R030" ~ "RS Randegansari Husada",
      Kdppk == "0204R020" ~ "RS Permata Bunda",
      Kdppk == "0204R021" ~ "RSUD Karangkembang",
      Kdppk == "0204R022" ~ "RS Nahdlatul Ulama Babat",
      Kdppk == "0204R023" ~ "RS Permata Hati",
      Kdppk == "0204R024" ~ "RS Muhammadiyah Kalikapas",
      TRUE ~ Kdppk))

#======================================================================
data <- data %>% 
  mutate(
    NmVerif = case_when(
      Kdppk == "0204R003" ~ "Tuti Wijaya",
      Kdppk == "0204R004" ~ "Rodiyah",
      Kdppk == "0204R009" ~ "Tuti Wijaya",
      Kdppk == "0204R010" ~ "Lingga Aris Sandy",
      Kdppk == "0204R012" ~ "Rodiyah",
      Kdppk == "0204R019" ~ "Rodiyah",
      Kdppk == "0204R013" ~ "Tuti Wijaya",
      Kdppk == "0204R014" ~ "Mutiara Ayu Faradisa",
      Kdppk == "0204R015" ~ "Rodiyah",
      Kdppk == "0204R017" ~ "Tuti Wijaya",
      Kdppk == "0204R018" ~ "Rodiyah",
      Kdppk == "0204S001" ~ "Miftahur Rohman",
      Kdppk == "0205R009" ~ "Priskylia Mahayu Oktavianti",
      Kdppk == "0205R011" ~ "Mutiara Ayu Faradisa",
      Kdppk == "0205R012" ~ "Fithriyah Sunan Rais",
      Kdppk == "0205R013" ~ "Widya Sariningtias",
      Kdppk == "0205R014" ~ "Priskylia Mahayu Oktavianti",
      Kdppk == "0205R019" ~ "Musyaadah",
      Kdppk == "0205R021" ~ "Priskylia Mahayu Oktavianti",
      Kdppk == "0205R022" ~ "Lingga Aris Sandy",
      Kdppk == "0205R023" ~ "Mutiara Ayu Faradisa",
      Kdppk == "0205R024" ~ "Priskylia Mahayu Oktavianti",
      Kdppk == "0205R025" ~ "Fithriyah Sunan Rais",
      Kdppk == "0205R026" ~ "Musyaadah",
      Kdppk == "0205R027" ~ "Widya Sariningtias",
      Kdppk == "0205R028" ~ "Mutiara Ayu Faradisa",
      Kdppk == "0205R029" ~ "Lingga Aris Sandy",
      Kdppk == "0205S100" ~ "Mutiara Ayu Faradisa",
      Kdppk == "1302R001" ~ "Musyaadah",
      Kdppk == "1302R002" ~ "Widya Sariningtias",
      Kdppk == "1306R001" ~ "Miftahur Rohman",
      Kdppk == "0205R031" ~ "Lingga Aris Sandy",
      Kdppk == "0205R030" ~ "Priskylia Mahayu Oktavianti",
      Kdppk == "0204R020" ~ "Tuti Wijaya",
      Kdppk == "0204R021" ~ "Miftahur Rohman",
      Kdppk == "0204R022" ~ "Rodiyah",
      Kdppk == "0204R023" ~ "Mutiara Ayu Faradisa",
      Kdppk == "0204R024" ~ "Lingga Aris Sandy",
      TRUE ~ Kdppk)) %>%
  arrange(SEP, Start)
#======================================================================

#Cek Kdppk distinct
Kodeppk <- data %>%
  group_by(NmVerif,NmFKRTL)%>%
  summarise(Kasus=n_distinct(SEP))

datari <- data %>%
  subset(Nmtkp == "1")
datarj <- data %>%
  subset(Nmtkp == "2")

rm(data)

datarj <- datarj %>% 
  mutate(
    maxcase = case_when(
      Kdppk == "0204R003" ~ "2586",
      Kdppk == "0204R004" ~ "2411",
      Kdppk == "0204R009" ~ "1943",
      Kdppk == "0204R010" ~ "5063",
      Kdppk == "0204R012" ~ "1852",
      Kdppk == "0204R019" ~ "667",
      Kdppk == "0204R013" ~ "219",
      Kdppk == "0204R014" ~ "760",
      Kdppk == "0204R015" ~ "565",
      Kdppk == "0204R017" ~ "180",
      Kdppk == "0204R018" ~ "100",
      Kdppk == "0204S001" ~ "1475",
      Kdppk == "0205R009" ~ "541",
      Kdppk == "0205R011" ~ "2056",
      Kdppk == "0205R012" ~ "9179",
      Kdppk == "0205R013" ~ "2233",
      Kdppk == "0205R014" ~ "3535",
      Kdppk == "0205R019" ~ "412",
      Kdppk == "0205R021" ~ "2716",
      Kdppk == "0205R022" ~ "825",
      Kdppk == "0205R023" ~ "682",
      Kdppk == "0205R024" ~ "2174",
      Kdppk == "0205R025" ~ "253",
      Kdppk == "0205R026" ~ "38",
      Kdppk == "0205R027" ~ "171",
      Kdppk == "0205R028" ~ "159",
      Kdppk == "0205R029" ~ "135",
      Kdppk == "0205S100" ~ "1269",
      Kdppk == "1302R001" ~ "9341",
      Kdppk == "1302R002" ~ "9844",
      Kdppk == "1306R001" ~ "9490",
      Kdppk == "0205R031" ~ "896",
      Kdppk == "0205R030" ~ "101",
      Kdppk == "0204R020" ~ "106",
      Kdppk == "0204R021" ~ "50",
      Kdppk == "0204R022" ~ "55",
      Kdppk == "0204R023" ~ "32",
      Kdppk == "0204R024" ~ "25",
      TRUE ~ Kdppk))

datarj <- datarj %>% 
  mutate(
    maxuc = case_when(
      Kdppk == "0204R003" ~ "440169",
      Kdppk == "0204R004" ~ "284192",
      Kdppk == "0204R009" ~ "241507",
      Kdppk == "0204R010" ~ "375502",
      Kdppk == "0204R012" ~ "399701",
      Kdppk == "0204R019" ~ "199332",
      Kdppk == "0204R013" ~ "286909",
      Kdppk == "0204R014" ~ "232201",
      Kdppk == "0204R015" ~ "298975",
      Kdppk == "0204R017" ~ "225386",
      Kdppk == "0204R018" ~ "205668",
      Kdppk == "0204S001" ~ "937132",
      Kdppk == "0205R009" ~ "234261",
      Kdppk == "0205R011" ~ "223621",
      Kdppk == "0205R012" ~ "348210",
      Kdppk == "0205R013" ~ "158650",
      Kdppk == "0205R014" ~ "278539",
      Kdppk == "0205R019" ~ "263537",
      Kdppk == "0205R021" ~ "248720",
      Kdppk == "0205R022" ~ "311957",
      Kdppk == "0205R023" ~ "196955",
      Kdppk == "0205R024" ~ "220157",
      Kdppk == "0205R025" ~ "228546",
      Kdppk == "0205R026" ~ "281845",
      Kdppk == "0205R027" ~ "246574",
      Kdppk == "0205R028" ~ "1486682",
      Kdppk == "0205R029" ~ "221147",
      Kdppk == "0205S100" ~ "790305",
      Kdppk == "1302R001" ~ "353984",
      Kdppk == "1302R002" ~ "259752",
      Kdppk == "1306R001" ~ "356855",
      Kdppk == "0205R031" ~ "324574",
      Kdppk == "0205R030" ~ "213898",
      Kdppk == "0204R020" ~ "231907",
      Kdppk == "0204R021" ~ "158643",
      Kdppk == "0204R022" ~ "158643",
      Kdppk == "0204R023" ~ "186353",
      Kdppk == "0204R024" ~ "158643",
      TRUE ~ Kdppk))

datari <- datari %>% 
  mutate(
    maxcase = case_when(
      Kdppk == "0204R003" ~ "395",
      Kdppk == "0204R004" ~ "197",
      Kdppk == "0204R009" ~ "271",
      Kdppk == "0204R010" ~ "804",
      Kdppk == "0204R012" ~ "215",
      Kdppk == "0204R019" ~ "203",
      Kdppk == "0204R013" ~ "203",
      Kdppk == "0204R014" ~ "303",
      Kdppk == "0204R015" ~ "143",
      Kdppk == "0204R017" ~ "120",
      Kdppk == "0204R018" ~ "86",
      Kdppk == "0204S001" ~ "0",
      Kdppk == "0205R009" ~ "115",
      Kdppk == "0205R011" ~ "529",
      Kdppk == "0205R012" ~ "476",
      Kdppk == "0205R013" ~ "134",
      Kdppk == "0205R014" ~ "265",
      Kdppk == "0205R019" ~ "190",
      Kdppk == "0205R021" ~ "270",
      Kdppk == "0205R022" ~ "197",
      Kdppk == "0205R023" ~ "184",
      Kdppk == "0205R024" ~ "181",
      Kdppk == "0205R025" ~ "126",
      Kdppk == "0205R026" ~ "10",
      Kdppk == "0205R027" ~ "119",
      Kdppk == "0205R028" ~ "46",
      Kdppk == "0205R029" ~ "55",
      Kdppk == "0205S100" ~ "0",
      Kdppk == "1302R001" ~ "746",
      Kdppk == "1302R002" ~ "468",
      Kdppk == "1306R001" ~ "817",
      Kdppk == "0205R031" ~ "133",
      Kdppk == "0205R030" ~ "122",
      Kdppk == "0204R020" ~ "83",
      Kdppk == "0204R021" ~ "53",
      Kdppk == "0204R022" ~ "73",
      Kdppk == "0204R023" ~ "71",
      Kdppk == "0204R024" ~ "35",
      TRUE ~ Kdppk))

datari <- datari %>% 
  mutate(
    maxuc = case_when(
      Kdppk == "0204R003" ~ "3454410",
      Kdppk == "0204R004" ~ "3773722",
      Kdppk == "0204R009" ~ "3589901",
      Kdppk == "0204R010" ~ "6657892",
      Kdppk == "0204R012" ~ "3273329",
      Kdppk == "0204R019" ~ "3837915",
      Kdppk == "0204R013" ~ "3614772",
      Kdppk == "0204R014" ~ "3677751",
      Kdppk == "0204R015" ~ "2390357",
      Kdppk == "0204R017" ~ "3747372",
      Kdppk == "0204R018" ~ "7357084",
      Kdppk == "0204S001" ~ "0",
      Kdppk == "0205R009" ~ "3041710",
      Kdppk == "0205R011" ~ "3561036",
      Kdppk == "0205R012" ~ "5146024",
      Kdppk == "0205R013" ~ "2475847",
      Kdppk == "0205R014" ~ "3470656",
      Kdppk == "0205R019" ~ "2592180",
      Kdppk == "0205R021" ~ "3219943",
      Kdppk == "0205R022" ~ "2622716",
      Kdppk == "0205R023" ~ "2600980",
      Kdppk == "0205R024" ~ "3245656",
      Kdppk == "0205R025" ~ "2703243",
      Kdppk == "0205R026" ~ "3692627",
      Kdppk == "0205R027" ~ "3055713",
      Kdppk == "0205R028" ~ "2574798",
      Kdppk == "0205R029" ~ "2720352",
      Kdppk == "0205S100" ~ "0",
      Kdppk == "1302R001" ~ "5469571",
      Kdppk == "1302R002" ~ "4315080",
      Kdppk == "1306R001" ~ "6556072",
      Kdppk == "0205R031" ~ "4341053",
      Kdppk == "0205R030" ~ "2682292",
      Kdppk == "0204R020" ~ "3563559",
      Kdppk == "0204R021" ~ "2340858",
      Kdppk == "0204R022" ~ "2340858",
      Kdppk == "0204R023" ~ "2340858",
      Kdppk == "0204R024" ~ "2526478",
      TRUE ~ Kdppk))

data <- rbind(datari,datarj)
rm(datari,datarj)
data <- data[order(data$Nokapst, data$End),]

data$Nokapst <- as.factor(data$Nokapst)
data$kdpst <- as.factor(data$kdpst)
data$pisa <- as.factor(data$pisa)
data$Kdjnsplg <- as.factor(data$Kdjnsplg)
data$maxcase <- as.numeric(data$maxcase)
data$maxuc <- as.numeric(data$maxuc)

#====================================================================

library(splitstackshape)
data <- concat.split(data, "DS", ";")

colnames(data)

data$DS01 <- as.character(trimws(gsub("-","",substr(data$DS_01,1,6)), "both"))
data$DS01 [is.na(data$DS01)] <- ""
data$DS01 <- as.factor(data$DS01)
data$DS02 <- as.character(trimws(gsub("-","",substr(data$DS_02,1,6)), "both"))
data$DS02 [is.na(data$DS02)] <- ""
data$DS02 <- as.factor(data$DS02)
data$DS03 <- as.character(trimws(gsub("-","",substr(data$DS_03,1,6)), "both"))
data$DS03 [is.na(data$DS03)] <- ""
data$DS03 <- as.factor(data$DS03)
data$DS04 <- as.character(trimws(gsub("-","",substr(data$DS_04,1,6)), "both"))
data$DS04 [is.na(data$DS04)] <- ""
data$DS04 <- as.factor(data$DS04)
data$DS05 <- as.character(trimws(gsub("-","",substr(data$DS_05,1,6)), "both"))
data$DS05 [is.na(data$DS05)] <- ""
data$DS05 <- as.factor(data$DS05)
data$DS06 <- as.character(trimws(gsub("-","",substr(data$DS_06,1,6)), "both"))
data$DS06 [is.na(data$DS06)] <- ""
data$DS06 <- as.factor(data$DS06)
data$DS07 <- as.character(trimws(gsub("-","",substr(data$DS_07,1,6)), "both"))
data$DS07 [is.na(data$DS07)] <- ""
data$DS07 <- as.factor(data$DS07)
data$DS08 <- as.character(trimws(gsub("-","",substr(data$DS_08,1,6)), "both"))
data$DS08 [is.na(data$DS08)] <- ""
data$DS08 <- as.factor(data$DS08)
data$DS09 <- as.character(trimws(gsub("-","",substr(data$DS_09,1,6)), "both"))
data$DS09 [is.na(data$DS09)] <- ""
data$DS09 <- as.factor(data$DS09)
data$DS10 <- as.character(trimws(gsub("-","",substr(data$DS_10,1,6)), "both"))
data$DS10 [is.na(data$DS10)] <- ""
data$DS10 <- as.factor(data$DS10)
data$DS11 <- as.character(trimws(gsub("-","",substr(data$DS_11,1,6)), "both"))
data$DS11 [is.na(data$DS11)] <- ""
data$DS11 <- as.factor(data$DS11)
data$DS12 <- as.character(trimws(gsub("-","",substr(data$DS_12,1,6)), "both"))
data$DS12 [is.na(data$DS12)] <- ""
data$DS12 <- as.factor(data$DS12)

data <- concat.split(data, "Procedure", ";")

colnames(data)

data$Proc01 <- as.character(trimws(gsub("-","",substr(data$Procedure_01,1,4)), "both"))
data$Proc01 [is.na(data$Proc01)] <- ""
data$Proc01 <- as.factor(data$Proc01)
data$Proc02 <- as.character(trimws(gsub("-","",substr(data$Procedure_02,1,4)), "both"))
data$Proc02 [is.na(data$Proc02)] <- ""
data$Proc02 <- as.factor(data$Proc02)
data$Proc03 <- as.character(trimws(gsub("-","",substr(data$Procedure_03,1,4)), "both"))
data$Proc03 [is.na(data$Proc03)] <- ""
data$Proc03 <- as.factor(data$Proc03)
data$Proc04 <- as.character(trimws(gsub("-","",substr(data$Procedure_04,1,4)), "both"))
data$Proc04 [is.na(data$Proc04)] <- ""
data$Proc04 <- as.factor(data$Proc04)
data$Proc05 <- as.character(trimws(gsub("-","",substr(data$Procedure_05,1,4)), "both"))
data$Proc05 [is.na(data$Proc05)] <- ""
data$Proc05 <- as.factor(data$Proc05)
data$Proc06 <- as.character(trimws(gsub("-","",substr(data$Procedure_06,1,4)), "both"))
data$Proc06 [is.na(data$Proc06)] <- ""
data$Proc06 <- as.factor(data$Proc06)
data$Proc07 <- as.character(trimws(gsub("-","",substr(data$Procedure_07,1,4)), "both"))
data$Proc07 [is.na(data$Proc07)] <- ""
data$Proc07 <- as.factor(data$Proc07)
data$Proc08 <- as.character(trimws(gsub("-","",substr(data$Procedure_08,1,4)), "both"))
data$Proc08 [is.na(data$Proc08)] <- ""
data$Proc08 <- as.factor(data$Proc08)
data$Proc09 <- as.character(trimws(gsub("-","",substr(data$Procedure_09,1,4)), "both"))
data$Proc09 [is.na(data$Proc09)] <- ""
data$Proc09 <- as.factor(data$Proc09)
data$Proc10 <- as.character(trimws(gsub("-","",substr(data$Procedure_10,1,4)), "both"))
data$Proc10 [is.na(data$Proc10)] <- ""
data$Proc10 <- as.factor(data$Proc10)
data$Proc11 <- as.character(trimws(gsub("-","",substr(data$Procedure_11,1,4)), "both"))
data$Proc11 [is.na(data$Proc11)] <- ""
data$Proc11 <- as.factor(data$Proc11)

##DATA_03 = Gabung DIAG & PROC
#remove empty cell with NA
data <- na_if(data, "")
data$DIAG <- paste(data$DU,data$DS01,data$DS02,data$DS03, 
                   data$DS04,data$DS05,data$DS06,data$DS07,
                   data$DS08,data$DS09,data$DS10,data$DS11,
                   data$DS12,sep=", ")
data$DIAG <- gsub("NA, ","",data$DIAG)
data$DIAG <- gsub(", NA","",data$DIAG)
data$DIAG <- sapply(data$DIAG, 
                    function(x) paste(unique(unlist(str_split(x,", "))), 
                                      collapse = ", "))
data$PROC <- paste(data$Proc01,data$Proc02,data$Proc03,data$Proc04,
                   data$Proc05,data$Proc06,data$Proc07,data$Proc08,
                   data$Proc09,data$Proc10,data$Proc11,sep=", ")
data$PROC <- gsub("NA, ","",data$PROC)
data$PROC <- gsub(", NA","",data$PROC)
data$PROC <- gsub("NA","",data$PROC)
data$PROC <- sapply(data$PROC, 
                    function(x) paste(unique(unlist(str_split(x,", "))), 
                                      collapse = ", "))

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