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
library(data.table)
library(excel.link)

gresik <- read_delim("Copy of Data Luaran HFIS Dokter Spesialis Edisi 25 Apr 24.csv") %>%
  subset(`Nama KC` == "GRESIK") %>%
  select(kodedokter) %>% distinct(kodedokter, .keep_all = TRUE)
nadyah <- read_delim("Copy of Data Luaran HFIS Dokter Spesialis Edisi 25 Apr 24.csv") 
hfis <- left_join(gresik, nadyah, by = "kodedokter")
outjatim <- hfis %>% subset(`Kedeputian Wilayah` != "KEDEPUTIAN WILAYAH VII")

hfis2 <- hfis %>%
  select(`Kedeputian Wilayah`,`Nama KC`,`Nama Dati 2`,`Kode FASKES`,
         `Nama Faskes`,SPESIALISTIK,NMKR,SPESIALIS,`SUB-SPESIALIS`,
         SUBSPESIALIS,Country,FLAGAKREDITASI,HARI,JAMPRAKTEK,`JENIS PELAYANAN`,
         JNSPPK,`Jumlah Faskes by NIK/SIP`,`Jumlah Faskes by SIP`,
         `Jumlah SIP by NIk`,`Jumlah SIP Dokter by nik`,KDDATI2,KDKC,KDKR,
         KdRegTarif,KELASRSMENKES,Keterangan,kodedokter,LAT,LNG,`Nama Tenaga Medis`,
         NMJNSFASKES,NmRegTarif,NomorSIP,`Number of Records`,POLIKLINIK,`Status SIP`,
         `TAT SIP`,`TMT SIP`)

hfis2$`TAT SIP` <- as.Date(hfis2$`TAT SIP`, format = "%m/%d/%Y")
hfis2$`TMT SIP` <- as.Date(hfis2$`TMT SIP`, format = "%m/%d/%Y")
rm(gresik,nadyah,outjatim,hfis)

write.xlsx(hfis2, file = "Data Luaran HFIS Dokter Spesialis Edisi 25 Apr 24_edit.xlsx")
