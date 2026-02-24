setwd("D:/Downloads")
library(excel.link)
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

jan23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                        header = TRUE,
                        row.names = NULL,
                        col.names = NULL,
                        xl.sheet = "Januari 23",
                        top.left.cell = "A1",
                        na = "",
                        excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

feb23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Februari 23",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

mar23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Maret 23",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

apr23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "April 23",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
          `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
          `Kasus Non Spesialistik`,`Jumlah Rujukan`,
          `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
          `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
          `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
          `Jumlah indikator tercapai`)


mei23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Mei",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

jun23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Juni",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

jul23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Juli",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

agu23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Agustus",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

sep23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "September",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

okt23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Oktober",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

nov23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "November",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

des23 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Desember",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

jan24 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Januari",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

feb24 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Februari",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

mar24 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Maret 24",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)

apr24 <- xl.read.file("Capaian KBK (JAN 23 - APR 24).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "April 24",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>%
  select(`Bulan Kapitasi`,`KAB/KOTA`,`Kode PPK`,`NAMA FKTP`,`TIPE FKTP`,
         `JUMLAH KONTAK`,`PESERTA TERDAFTAR`,
         `Kasus Non Spesialistik`,`Jumlah Rujukan`,
         `Prolanis DM Terkendali`,`Peserta Diangnosa DM`,
         `Prolanis HT Terkendali`,`Peserta Diangnosa HT`,
         `Capaian KBK BI`,`Keterangan AK`,`Keterangan RNS`,`Keterangan RPPT`,
         `Jumlah indikator tercapai`)
data <- rbind(jan23,feb23,mar23,apr23,mei23,jun23,
              jul23,agu23,sep23,okt23,nov23,des23,
              jan24,feb24,mar24,apr24)
rm(jan23,feb23,mar23,apr23,mei23,jun23,
   jul23,agu23,sep23,okt23,nov23,des23,
   jan24,feb24,mar24,apr24)
data$`Bulan Kapitasi` <- as.Date(data$`Bulan Kapitasi`)
data <- data %>%
  na.omit()
#==============================================================================
write.csv(data, "D://Downloads//ur_fktp.csv", na="", row.names = FALSE)
#==============================================================================