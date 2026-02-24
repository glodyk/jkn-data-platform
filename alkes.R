setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")

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
alkes <- read_csv("Sheet 1_Full Data_alkes.csv") %>%
  select(nmdati2layan,nmppklayan,nmtkp,
         nmjnsalkes,biayaverifikasi,tglstjkeu) %>%
  filter(!str_detect(nmjnsalkes,c("Ambulans|Kacamata"))) %>%
  filter(str_detect(nmtkp,c("RJTL|RITL")))
alkes$tglstjkeu <- as.Date(alkes$tglstjkeu, format = "%m/%d/%Y")
alkes <- alkes %>% subset(tglstjkeu >= "2024-01-01")
rekap <- alkes %>%
  group_by(nmdati2layan,nmppklayan,nmjnsalkes) %>%
  summarise(biayaverifikasi)
write.xlsx(rekap, file = "rekapalkes2.xlsx")
#=============================data nadyah=============================

alkes <- read_csv("Sheet 1_Full Data_alkes.csv") %>%
  select (nokapst,jkpst,umur,segmen,pisat,tglresep,tgllegalisasi,
          tglrealisasi,tglpelayanan,nmppklayan,tgldtgsjp,tglplgsjp,
          nosjp,norefflegal,nmtkp,kdjnsalkes,nmpks,nmdati2layan,
          nmppkasal,refasalsjp,nmppkterdaftar,nofpk,noreg,tglfpk,
          tglreg,tglstjkeu,biayatagih,biayaverifikasi) %>%
  subset(kdjnsalkes == "002") %>%
  select(-kdjnsalkes)

alkes$tglresep <- as.Date(alkes$tglresep, format = "%m/%d/%Y")
alkes$tgllegalisasi <- as.Date(alkes$tgllegalisasi, format = "%m/%d/%Y")
alkes$tglrealisasi <- as.Date(alkes$tglrealisasi, format = "%m/%d/%Y")
alkes$tglpelayanan <- as.Date(alkes$tglpelayanan, format = "%m/%d/%Y")
alkes$tgldtgsjp <- as.Date(alkes$tgldtgsjp, format = "%m/%d/%Y")
alkes$tglplgsjp <- as.Date(alkes$tglplgsjp, format = "%m/%d/%Y")
alkes$tglfpk <- as.Date(alkes$tglfpk, format = "%m/%d/%Y")
alkes$tglreg <- as.Date(alkes$tglreg, format = "%m/%d/%Y")
alkes$tglstjkeu <- as.Date(alkes$tglstjkeu, format = "%m/%d/%Y")

alkes$ketpisat <- with(alkes,
                    ifelse(jkpst == "Laki-laki" & pisat == "P","Suami",
                           ifelse(jkpst == "Perempuan" & pisat == "P","Istri",
                                  ifelse(pisat == "I","Istri",
                                         ifelse(pisat == "S","Suami",
                                                ifelse(pisat == "A","Anak",
                                                       ifelse(pisat == "T","Tambahan",
                                                              "Salah")))))))

alkes <- alkes[order(alkes$nokapst,alkes$tgllegalisasi),]
#==============================================================================
write.csv(alkes, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_optik.csv",
          na="", row.names = FALSE)
#==============================================================================