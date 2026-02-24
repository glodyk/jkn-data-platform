setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/Kesesuaian Diagnosa")

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

ksdx25 <- read_csv("Sheet 1_Full Data_data (kesesuaian diagnosa 2025).csv") %>%
  subset(nmkcperujuk == "GRESIK") %>%
  select(Nokapst,namapeserta,norujukanpcare,Nosjp,Jenis,Diagakhir,
         Tglpelayanan,tglplgsjp,nmdati2perujuk,Kdppkperujuk,
         nmppkperujuk,nmdati2layan,Nmppklayan,Politujsjp,kdinacbgs,
         nminacbgs,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,Nmdiagprimer,
         Diagsekunder,Bytagsjp,Byversjp) %>%
  mutate(Diagmasuk = paste0(as.character(Kddiagmasuk)," - ",
                            as.character(Nmdiagmasuk)),
         Diagprimer = paste0(as.character(Kodediagprimer)," - ",
                             as.character(Nmdiagprimer))) %>%
  select(-Kddiagmasuk,-Nmdiagmasuk,-Kodediagprimer,-Nmdiagprimer)

ksdx25 <- ksdx25[order(ksdx25$Kdppkperujuk, ksdx25$tglplgsjp),]
ksdx250 <- ksdx25 %>%
  select(Kdppkperujuk,nmppkperujuk,norujukanpcare,namapeserta,Diagmasuk,Nosjp,
         Nmppklayan,Diagprimer,Tglpelayanan)
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("ur_all.rda")
ksdx250 <- left_join(ksdx250,data, by = "Nosjp") %>%
  subset(Flag != "Spesialistik")
ksdx250 <- ksdx250 %>%
  select(Kdppkperujuk,nmppkperujuk,norujukanpcare,namapeserta,Diagmasuk.x,Nosjp,
         Nmppklayan.x,Diagprimer,Diagsekunder,Procedure,Tglpelayanan.x)
library(splitstackshape)
ksdx250 <- concat.split(ksdx250, "Diagsekunder", ";")
colnames(ksdx250)
ksdx250$keterangan <- with(ksdx250,
                           ifelse(Diagmasuk.x == Diagprimer |
                                    Diagmasuk.x == Diagsekunder_1 |
                                    Diagmasuk.x == Diagsekunder_2 |
                                    Diagmasuk.x == Diagsekunder_3 |
                                    Diagmasuk.x == Diagsekunder_4 |
                                    Diagmasuk.x == Diagsekunder_5, "Salah","Benar"))
ksdx250$keterangan1 <- with(ksdx250,
                           ifelse(is.na(ksdx250$keterangan),"Benar",keterangan))
ksdx251 <- ksdx250 %>%
  subset(keterangan1 == "Benar")  %>%
  select(Kdppkperujuk,nmppkperujuk,norujukanpcare,namapeserta,Diagmasuk.x,Nosjp,
         Nmppklayan.x,Diagprimer,Diagsekunder,Procedure,Tglpelayanan.x)

ksdx251 <- concat.split(ksdx251, "Diagsekunder", ";")
colnames(ksdx251)
ksdx251$Diagmasuk.x01 <- as.character(trimws(substr(ksdx251$Diagmasuk.x,1,3)), "both")
ksdx251$Diagprimer01 <- as.character(trimws(substr(ksdx251$Diagprimer,1,3)), "both")
ksdx251$Diagsekunder_011 <- as.character(trimws(substr(ksdx251$Diagsekunder_01,1,3)), "both")
ksdx251$Diagsekunder_021 <- as.character(trimws(substr(ksdx251$Diagsekunder_02,1,3)), "both")
ksdx251$Diagsekunder_031 <- as.character(trimws(substr(ksdx251$Diagsekunder_03,1,3)), "both")
ksdx251$Diagsekunder_041 <- as.character(trimws(substr(ksdx251$Diagsekunder_04,1,3)), "both")
ksdx251$Diagsekunder_051 <- as.character(trimws(substr(ksdx251$Diagsekunder_05,1,3)), "both")

ksdx251$keterangan <- with(ksdx251,
                           ifelse(Diagmasuk.x01 == Diagprimer |
                                    Diagmasuk.x01 == Diagsekunder_011 |
                                    Diagmasuk.x01 == Diagsekunder_021 |
                                    Diagmasuk.x01 == Diagsekunder_031 |
                                    Diagmasuk.x01 == Diagsekunder_041 |
                                    Diagmasuk.x01 == Diagsekunder_051, "Salah","Benar"))

ksdx251$keterangan1 <- with(ksdx251,
                            ifelse(is.na(ksdx251$keterangan),"Benar",keterangan))

ksdx252 <- ksdx251 %>%
  subset(keterangan1 == "Benar")  %>%
  select(Kdppkperujuk,nmppkperujuk,norujukanpcare,namapeserta,Diagmasuk.x,Nosjp,
         Nmppklayan.x,Diagprimer,Diagsekunder,Procedure,Tglpelayanan.x)
ksdx252$Diagsekunder [is.na(ksdx252$Diagsekunder)] <- ""
ksdx252$Procedure [is.na(ksdx252$Procedure)] <- ""
ksdx252$Tglpelayanan.x <- as.Date(ksdx252$Tglpelayanan.x, format = "%m/%d/%Y")

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/Kesesuaian Diagnosa")
write.xlsx(ksdx252, file = "Audit Kesesuaian Diagnosa 2025 kirim verifikator.xlsx")


ksdx21 <- read_csv("Sheet 1_Full Ksesuaian_dx_21.csv") %>%
  select(Nokapst,namapeserta,norujukanpcare,Nosjp,Jenis,Diagakhir,
         Tglpelayanan,tglplgsjp,nmdati2perujuk,Kdppkperujuk,
         nmppkperujuk,nmdati2layan,Nmppklayan,Politujsjp,kdinacbgs,
         nminacbgs,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,Nmdiagprimer,
         Diagsekunder,Bytagsjp,Byversjp) %>%
  mutate(Diagmasuk = paste0(as.character(Kddiagmasuk)," - ",
                            as.character(Nmdiagmasuk)),
         Diagprimer = paste0(as.character(Kodediagprimer)," - ",
                             as.character(Nmdiagprimer))) %>%
  select(-Kddiagmasuk,-Nmdiagmasuk,-Kodediagprimer,-Nmdiagprimer)

ksdx22 <- read_csv("Sheet 1_Full Ksesuaian_dx_22.csv") %>%
  select(Nokapst,namapeserta,norujukanpcare,Nosjp,Jenis,Diagakhir,
         Tglpelayanan,tglplgsjp,nmdati2perujuk,Kdppkperujuk,
         nmppkperujuk,nmdati2layan,Nmppklayan,Politujsjp,kdinacbgs,
         nminacbgs,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,Nmdiagprimer,
         Diagsekunder,Bytagsjp,Byversjp) %>%
  mutate(Diagmasuk = paste0(as.character(Kddiagmasuk)," - ",
                            as.character(Nmdiagmasuk)),
         Diagprimer = paste0(as.character(Kodediagprimer)," - ",
                             as.character(Nmdiagprimer))) %>%
  select(-Kddiagmasuk,-Nmdiagmasuk,-Kodediagprimer,-Nmdiagprimer)

ksdx23 <- read_csv("Sheet 1_Full Ksesuaian_dx_23.csv") %>%
  select(Nokapst,namapeserta,norujukanpcare,Nosjp,Jenis,Diagakhir,
         Tglpelayanan,tglplgsjp,nmdati2perujuk,Kdppkperujuk,
         nmppkperujuk,nmdati2layan,Nmppklayan,Politujsjp,kdinacbgs,
         nminacbgs,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,Nmdiagprimer,
         Diagsekunder,Bytagsjp,Byversjp) %>%
  mutate(Diagmasuk = paste0(as.character(Kddiagmasuk)," - ",
                            as.character(Nmdiagmasuk)),
         Diagprimer = paste0(as.character(Kodediagprimer)," - ",
                             as.character(Nmdiagprimer))) %>%
  select(-Kddiagmasuk,-Nmdiagmasuk,-Kodediagprimer,-Nmdiagprimer)

ksdx24 <- read_csv("Sheet 1_Full Ksesuaian_dx_jan_mei24.csv") %>%
  select(Nokapst,namapeserta,norujukanpcare,Nosjp,Jenis,Diagakhir,
         Tglpelayanan,tglplgsjp,nmdati2perujuk,Kdppkperujuk,
         nmppkperujuk,nmdati2layan,Nmppklayan,Politujsjp,kdinacbgs,
         nminacbgs,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,Nmdiagprimer,
         Diagsekunder,Bytagsjp,Byversjp) %>%
  mutate(Diagmasuk = paste0(as.character(Kddiagmasuk)," - ",
                            as.character(Nmdiagmasuk)),
         Diagprimer = paste0(as.character(Kodediagprimer)," - ",
                             as.character(Nmdiagprimer))) %>%
  select(-Kddiagmasuk,-Nmdiagmasuk,-Kodediagprimer,-Nmdiagprimer)

ksdx241 <- read_csv("Sheet 1_Full Ksesuaian_dx_jun_jul24.csv") %>%
  select(Nokapst,namapeserta,norujukanpcare,Nosjp,Jenis,Diagakhir,
         Tglpelayanan,tglplgsjp,nmdati2perujuk,Kdppkperujuk,
         nmppkperujuk,nmdati2layan,Nmppklayan,Politujsjp,kdinacbgs,
         nminacbgs,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,Nmdiagprimer,
         Diagsekunder,Bytagsjp,Byversjp) %>%
  mutate(Diagmasuk = paste0(as.character(Kddiagmasuk)," - ",
                            as.character(Nmdiagmasuk)),
         Diagprimer = paste0(as.character(Kodediagprimer)," - ",
                             as.character(Nmdiagprimer))) %>%
  select(-Kddiagmasuk,-Nmdiagmasuk,-Kodediagprimer,-Nmdiagprimer)

#=============================data nadyah=============================
data <- read_csv("Sheet 1_Full Data_data (kesesuaiandiag).csv") %>%
  select(Nokapst,namapeserta,norujukanpcare,Nosjp,Jenis,Diagakhir,
         Tglpelayanan,tglplgsjp,nmdati2perujuk,Kdppkperujuk,
         nmppkperujuk,nmdati2layan,Nmppklayan,Politujsjp,kdinacbgs,
         nminacbgs,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,Nmdiagprimer,
         Diagsekunder,Bytagsjp,Byversjp) %>%
  mutate(Diagmasuk = paste0(as.character(Kddiagmasuk)," - ",
                            as.character(Nmdiagmasuk)),
         Diagprimer = paste0(as.character(Kodediagprimer)," - ",
                             as.character(Nmdiagprimer))) %>%
  select(-Kddiagmasuk,-Nmdiagmasuk,-Kodediagprimer,-Nmdiagprimer)

data <- rbind(ksdx20,ksdx21,ksdx22,ksdx23,ksdx24,ksdx241)
rm(ksdx20,ksdx21,ksdx22,ksdx23,ksdx24,ksdx241)

data$Keterangan <- with(data,
                        ifelse(str_detect(Jenis,c("SPESIALISTIK"))|
                                            str_detect(Diagakhir,c("NON SPESIALISTIK")),
                             "Audit","Tidak Audit"))

data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$tglplgsjp <- as.Date(data$tglplgsjp, format = "%m/%d/%Y")
data <- data[order(data$Nmppklayan, data$Tglpelayanan),]
data <- data %>% subset(Tglpelayanan >= "2020-01-01")
#========================================================================
write.csv(data, "D://data gresik//MTF KC GRESIK//gresik_2020//Kesesuaian Diagnosa//Kesesuaian Diagnosa.csv",
          na="", row.names = FALSE)
#========================================================================
write.xlsx(data, file = "vpk_bayi.xlsx")

data <- data %>%
  subset(Keterangan == "AUDIT")
#==================================================================
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