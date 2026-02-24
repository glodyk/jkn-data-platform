setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")


unlink(tempdir(), recursive = TRUE)
cat("unlink(tempdir(), recursive = TRUE)", 
    file="~/.Rprofile", append=TRUE)



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

diagns <- read_delim("Diagnonspesialistik.csv") %>%
  select(-1)
write.xlsx(diagns, file = "kdns.xlsx")

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi)

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")

load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GenomicFeatures")
BiocManager::install()
BiocManager::valid()
