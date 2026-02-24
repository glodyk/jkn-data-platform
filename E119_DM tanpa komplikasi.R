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

data14 <- read_csv("Sheet 1_Full Data gresik 2014.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data15 <- read_csv("Sheet 1_Full Data gresik 2015.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data16 <- read_csv("Sheet 1_Full Data gresik 2016.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data17 <- read_csv("Sheet 1_Full Data gresik 2017.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data18 <- read_csv("Sheet 1_Full Data gresik 2018.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data19 <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data20 <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data21 <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data22 <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

datamei24 <- read_csv("Sheet 1_Full Data gresik mei_jul 2024.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

dataagu24 <- read_csv("Sheet 1_Full Data gresik agu_okt 2024.csv") %>%
  select (Nokapst,Rangeumur,Jkpst,Sumberkunjungan,Flagspesialistik,
          Flagtacc,Pstprb,Tmtpstprb,Pstprolanis,Tmtpstprolanis,
          Nmdati2Layan,Jenisppkperujuk,Typeppkperujuk,Nmppkperujuk,
          Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,
          Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"E119")) %>%
  select(-Diagnosa)

nonspes <- rbind(data14,data15,data16,data17,data18,data19,data20,data21,
                 data22,data23,data24,dataapr24,datamei24,dataagu24)
rm(data14,data15,data16,data17,data18,data19,data20,data21,
   data22,data23,data24,dataapr24,datamei24,dataagu24)

nonspes$Tgldtgsjp <- as.Date(nonspes$Tgldtgsjp, format = "%m/%d/%Y")
nonspes$Tglplgsjp <- as.Date(nonspes$Tglplgsjp, format = "%m/%d/%Y")
nonspes$Tglpelayanan <- as.Date(nonspes$Tglpelayanan, format = "%m/%d/%Y")
nonspes$Tglstjkeu <- as.Date(nonspes$Tglstjkeu, format = "%m/%d/%Y")
nonspes <- nonspes %>% subset(Tglpelayanan >= "2019-01-01")

nonspes <- nonspes %>%
  mutate(
    Kddiagprimer1 = case_when(
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
      TRUE ~ Kddiagprimer))
nonspes$Nmdiagprimer1 <- with(nonspes,
                              ifelse(is.na(nonspes$Kddiagprimer1),NA,Nmdiagprimer))
nonspes <- nonspes %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
nonspes$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,nonspes$Diagprimer)), "both")
nonspes <- nonspes %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
nonspes$Diagnosa <- as.character(trimws(gsub(";NA|NA;","",nonspes$Diagnosa)), "both")

nonspes <- nonspes %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)


nonspes <- nonspes %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
nonspes <- nonspes %>% mutate(Procedure = na_if(Procedure, "-"))

nonspes <- nonspes %>% 
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