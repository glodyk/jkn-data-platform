setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/history_rujukan_faskes")

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

data <- read_csv("Sheet 1_Full Data_history_rujukan_faskes.csv") %>%
  select (Nokapst,Segmenpeserta,Nmbu,Segmenpeserta,`Nmjns Prb`,`Nmdati2 Fktp`,
          `Nmjnsppk Fktp`,`Nmtypeppk Fktp`,Nmppkpeserta,Nmppkpeserta,
          nmppk_fktp,`Nmtkp Fktp`,`No Kunjungan Fktp`,`Kd Diagnosa Fktp`,
          `Nm Diagnosa Fktp`,Nmtacc,`Nmdati2 Fkrtl`,Kelasrsmenkes,
          `Kdppk Fkrtl`,nmppk_fkrtl,`Nmpoli Fkrtl`,`No Kunjungan Fkrtl`,
          `Kddiagprimer Fkrtl`,`Nmdiagprimer Fkrtl`,`Diagsekunder Fkrtl`,
          Spesialistik,`Tgldatang Fktp`,`Tglpulang Fktp`,`Waktu Kunjungan Fktp`,
          `Tgl Datang Fkrtl`,`Tgl Pulang Fkrtl`,`Waktu Kunjungan Fkrtl`,
          `Tglmulai Prb`,`Tglakhir Prb`,Biayaverifikasi,
          `User Pcare`,`User Fkrtl`) %>%
  mutate(Diagmasuk = paste0(as.character(`Kd Diagnosa Fktp`)," - ",
                            as.character(`Nm Diagnosa Fktp`)),
         Diagprimer = paste0(as.character(`Kddiagprimer Fkrtl`)," - ",
                             as.character(`Nmdiagprimer Fkrtl`))) %>%
  rename(Diagsekunder = `Diagsekunder Fkrtl`,
         Nofktp = `No Kunjungan Fktp`,
         Nosjp = `No Kunjungan Fkrtl`) %>%
  select(-`Kd Diagnosa Fktp`,-`Nm Diagnosa Fktp`,-`Kddiagprimer Fkrtl`,
         -`Nmdiagprimer Fkrtl`)

data2 <- read_csv("Sheet 1_Full Data_data (4).csv") %>%
  select (Nokapst,Segmenpeserta,Nmbu,Segmenpeserta,`Nmjns Prb`,`Nmdati2 Fktp`,
          `Nmjnsppk Fktp`,`Nmtypeppk Fktp`,Nmppkpeserta,Nmppkpeserta,
          nmppk_fktp,`Nmtkp Fktp`,`No Kunjungan Fktp`,`Kd Diagnosa Fktp`,
          `Nm Diagnosa Fktp`,Nmtacc,`Nmdati2 Fkrtl`,Kelasrsmenkes,
          `Kdppk Fkrtl`,nmppk_fkrtl,`Nmpoli Fkrtl`,`No Kunjungan Fkrtl`,
          `Kddiagprimer Fkrtl`,`Nmdiagprimer Fkrtl`,`Diagsekunder Fkrtl`,
          Spesialistik,`Tgldatang Fktp`,`Tglpulang Fktp`,`Waktu Kunjungan Fktp`,
          `Tgl Datang Fkrtl`,`Tgl Pulang Fkrtl`,`Waktu Kunjungan Fkrtl`,
          `Tglmulai Prb`,`Tglakhir Prb`,Biayaverifikasi,
          `User Pcare`,`User Fkrtl`) %>%
  mutate(Diagmasuk = paste0(as.character(`Kd Diagnosa Fktp`)," - ",
                            as.character(`Nm Diagnosa Fktp`)),
         Diagprimer = paste0(as.character(`Kddiagprimer Fkrtl`)," - ",
                             as.character(`Nmdiagprimer Fkrtl`))) %>%
  rename(Diagsekunder = `Diagsekunder Fkrtl`,
         Nofktp = `No Kunjungan Fktp`,
         Nosjp = `No Kunjungan Fkrtl`) %>%
  select(-`Kd Diagnosa Fktp`,-`Nm Diagnosa Fktp`,-`Kddiagprimer Fkrtl`,
         -`Nmdiagprimer Fkrtl`)

data3 <- read_csv("Sheet 1_Full Data_data.csv") %>%
  select (Nokapst,Segmenpeserta,Nmbu,Segmenpeserta,`Nmjns Prb`,`Nmdati2 Fktp`,
          `Nmjnsppk Fktp`,`Nmtypeppk Fktp`,Nmppkpeserta,Nmppkpeserta,
          nmppk_fktp,`Nmtkp Fktp`,`No Kunjungan Fktp`,`Kd Diagnosa Fktp`,
          `Nm Diagnosa Fktp`,Nmtacc,`Nmdati2 Fkrtl`,Kelasrsmenkes,
          `Kdppk Fkrtl`,nmppk_fkrtl,`Nmpoli Fkrtl`,`No Kunjungan Fkrtl`,
          `Kddiagprimer Fkrtl`,`Nmdiagprimer Fkrtl`,`Diagsekunder Fkrtl`,
          Spesialistik,`Tgldatang Fktp`,`Tglpulang Fktp`,`Waktu Kunjungan Fktp`,
          `Tgl Datang Fkrtl`,`Tgl Pulang Fkrtl`,`Waktu Kunjungan Fkrtl`,
          `Tglmulai Prb`,`Tglakhir Prb`,Biayaverifikasi,
          `User Pcare`,`User Fkrtl`) %>%
  mutate(Diagmasuk = paste0(as.character(`Kd Diagnosa Fktp`)," - ",
                            as.character(`Nm Diagnosa Fktp`)),
         Diagprimer = paste0(as.character(`Kddiagprimer Fkrtl`)," - ",
                             as.character(`Nmdiagprimer Fkrtl`))) %>%
  rename(Diagsekunder = `Diagsekunder Fkrtl`,
         Nofktp = `No Kunjungan Fktp`,
         Nosjp = `No Kunjungan Fkrtl`) %>%
  select(-`Kd Diagnosa Fktp`,-`Nm Diagnosa Fktp`,-`Kddiagprimer Fkrtl`,
         -`Nmdiagprimer Fkrtl`)
#========================================================================
data22 <- read_csv("history_rujukan_faskes_2022.csv") %>%
  select (Nokapst,Segmenpeserta,Nmbu,`Nmjns Prb`,`Nmdati2 Fktp`,
          `Nmjnsppk Fktp`,`Nmtypeppk Fktp`,Nmppkpeserta,
          nmppk_fktp,`Nmtkp Fktp`,`No Kunjungan Fktp`,`Kd Diagnosa Fktp`,
          `Nm Diagnosa Fktp`,Nmtacc,`Nmdati2 Fkrtl`,Kelasrsmenkes,
          `Kdppk Fkrtl`,nmppk_fkrtl,`Nmpoli Fkrtl`,`No Kunjungan Fkrtl`,
          `Kddiagprimer Fkrtl`,`Nmdiagprimer Fkrtl`,`Diagsekunder Fkrtl`,
          Spesialistik,`Tgldatang Fktp`,`Tglpulang Fktp`,`Waktu Kunjungan Fktp`,
          `Tgl Datang Fkrtl`,`Tgl Pulang Fkrtl`,`Waktu Kunjungan Fkrtl`,
          `Tglmulai Prb`,`Tglakhir Prb`,Biayaverifikasi,
          `User Pcare`,`User Fkrtl`) %>%
  mutate(Diagmasuk = paste0(as.character(`Kd Diagnosa Fktp`)," - ",
                            as.character(`Nm Diagnosa Fktp`)),
         Diagprimer = paste0(as.character(`Kddiagprimer Fkrtl`)," - ",
                             as.character(`Nmdiagprimer Fkrtl`))) %>%
  rename(Diagsekunder = `Diagsekunder Fkrtl`,
         Nofktp = `No Kunjungan Fktp`,
         Nosjp = `No Kunjungan Fkrtl`) %>%
  select(-`Kd Diagnosa Fktp`,-`Nm Diagnosa Fktp`,-`Kddiagprimer Fkrtl`,
         -`Nmdiagprimer Fkrtl`)

data23 <- read_csv("history_rujukan_faskes_2023.csv") %>%
  select (Nokapst,Segmenpeserta,Nmbu,`Nmjns Prb`,`Nmdati2 Fktp`,
          `Nmjnsppk Fktp`,`Nmtypeppk Fktp`,Nmppkpeserta,
          nmppk_fktp,`Nmtkp Fktp`,`No Kunjungan Fktp`,`Kd Diagnosa Fktp`,
          `Nm Diagnosa Fktp`,Nmtacc,`Nmdati2 Fkrtl`,Kelasrsmenkes,
          `Kdppk Fkrtl`,nmppk_fkrtl,`Nmpoli Fkrtl`,`No Kunjungan Fkrtl`,
          `Kddiagprimer Fkrtl`,`Nmdiagprimer Fkrtl`,`Diagsekunder Fkrtl`,
          Spesialistik,`Tgldatang Fktp`,`Tglpulang Fktp`,`Waktu Kunjungan Fktp`,
          `Tgl Datang Fkrtl`,`Tgl Pulang Fkrtl`,`Waktu Kunjungan Fkrtl`,
          `Tglmulai Prb`,`Tglakhir Prb`,Biayaverifikasi,
          `User Pcare`,`User Fkrtl`) %>%
  mutate(Diagmasuk = paste0(as.character(`Kd Diagnosa Fktp`)," - ",
                            as.character(`Nm Diagnosa Fktp`)),
         Diagprimer = paste0(as.character(`Kddiagprimer Fkrtl`)," - ",
                             as.character(`Nmdiagprimer Fkrtl`))) %>%
  rename(Diagsekunder = `Diagsekunder Fkrtl`,
         Nofktp = `No Kunjungan Fktp`,
         Nosjp = `No Kunjungan Fkrtl`) %>%
  select(-`Kd Diagnosa Fktp`,-`Nm Diagnosa Fktp`,-`Kddiagprimer Fkrtl`,
         -`Nmdiagprimer Fkrtl`)

data24 <- read_csv("Sheet 1_Full Data_data.csv") %>%
  select (Nokapst,Segmenpeserta,Nmbu,`Nmjns Prb`,`Nmdati2 Fktp`,
          `Nmjnsppk Fktp`,`Nmtypeppk Fktp`,Nmppkpeserta,
          nmppk_fktp,`Nmtkp Fktp`,`No Kunjungan Fktp`,`Kd Diagnosa Fktp`,
          `Nm Diagnosa Fktp`,Nmtacc,`Nmdati2 Fkrtl`,Kelasrsmenkes,
          `Kdppk Fkrtl`,nmppk_fkrtl,`Nmpoli Fkrtl`,`No Kunjungan Fkrtl`,
          `Kddiagprimer Fkrtl`,`Nmdiagprimer Fkrtl`,`Diagsekunder Fkrtl`,
          Spesialistik,`Tgldatang Fktp`,`Tglpulang Fktp`,`Waktu Kunjungan Fktp`,
          `Tgl Datang Fkrtl`,`Tgl Pulang Fkrtl`,`Waktu Kunjungan Fkrtl`,
          `Tglmulai Prb`,`Tglakhir Prb`,Biayaverifikasi,
          `User Pcare`,`User Fkrtl`) %>%
  mutate(Diagmasuk = paste0(as.character(`Kd Diagnosa Fktp`)," - ",
                            as.character(`Nm Diagnosa Fktp`)),
         Diagprimer = paste0(as.character(`Kddiagprimer Fkrtl`)," - ",
                             as.character(`Nmdiagprimer Fkrtl`))) %>%
  rename(Diagsekunder = `Diagsekunder Fkrtl`,
         Nofktp = `No Kunjungan Fktp`,
         Nosjp = `No Kunjungan Fkrtl`) %>%
  select(-`Kd Diagnosa Fktp`,-`Nm Diagnosa Fktp`,-`Kddiagprimer Fkrtl`,
         -`Nmdiagprimer Fkrtl`)

data <- rbind(data1,data2,data3,data22,data23,data24)
rm(data1,data2,data3,data22,data23,data24)

data <- distinct(data, Nosjp, .keep_all=TRUE)
#========================================================================
data$`Tgldatang Fktp` <- as.Date(data$`Tgldatang Fktp`, format = "%m/%d/%Y")
data$`Tglpulang Fktp` <- as.Date(data$`Tglpulang Fktp`, format = "%m/%d/%Y")
data$`Tgl Datang Fkrtl` <- as.Date(data$`Tgl Datang Fkrtl`, format = "%m/%d/%Y")
data$`Tgl Pulang Fkrtl` <- as.Date(data$`Tgl Pulang Fkrtl`, format = "%m/%d/%Y")
data$`Tglmulai Prb` <- as.Date(data$`Tglmulai Prb`, format = "%m/%d/%Y")
data$`Tglakhir Prb` <- as.Date(data$`Tglakhir Prb`, format = "%m/%d/%Y")
data$`Waktu Kunjungan Fktp` <- mdy_hms(data$`Waktu Kunjungan Fktp`)
data$`Waktu Kunjungan Fkrtl` <- mdy_hms(data$`Waktu Kunjungan Fkrtl`)

data <- data %>%
  mutate(Selisih = `Waktu Kunjungan Fkrtl` - `Waktu Kunjungan Fktp`)
data$Selisih <- as.numeric(data$Selisih)

data$Flag <- with(data,
                ifelse(Selisih <= 60, "< 1 menit",
                       ifelse(Selisih > 60 & Selisih <= 180, "1-3 menit",
                              ifelse(Selisih > 180 & Selisih <= 300, "3-5 menit",
                                     ifelse(Selisih > 300 & Selisih <= 600, "5-10 menit",
                                            ifelse(Selisih > 60 & Selisih <= 900, "10-15 menit",
                                                   ifelse(Selisih > 900 & Selisih <= 1800, "15-30 menit",
                                                          ifelse(Selisih > 1800 & Selisih <= 2700, "30-45 menit",
                                                                 ifelse(Selisih > 2700 & Selisih <= 3600, "45 menit - 1 jam",
                                                                        ifelse(Selisih > 3600 & Selisih <= 5400, "1-1.5 jam",
                                                                               ifelse(Selisih > 5400 & Selisih <= 7200, "1.5-2 jam",
                                                                                      ifelse(Selisih > 7200 & Selisih <= 9000, "2-2.5 jam",
                                                                                             ifelse(Selisih > 9000 & Selisih <= 10800, "2.5-3 jam",
                                                                                                    ifelse(Selisih > 10800 & Selisih <= 14400, "3-4 jam",
                                                                                                           ifelse(Selisih > 14400 & Selisih <= 18000, "4-5 jam",
                                                                                                                  ifelse(Selisih > 18000 & Selisih <= 21600, "5-6 jam",
                                                                                                                         ifelse(Selisih > 21600 & Selisih <= 25200, "6-7 jam",
                                                                                                                                ifelse(Selisih > 25200 & Selisih <= 28800, "7-8 jam",
                                                                                                                                       ifelse(Selisih > 28800 & Selisih <= 32400, "8-9 jam",
                                                                                                                                              ifelse(Selisih > 32400 & Selisih <= 36000, "9-10 jam",
                                                                                                                                                     ifelse(Selisih > 36000 & Selisih <= 39600, "10-11 jam",
                                                                                                                                                            ifelse(Selisih > 39600 & Selisih <= 43200, "11-12 jam",
                                                                                                                                                                   ifelse(Selisih > 43200 & Selisih <= 86400, "12 jam - 1 hari",
                                                                                                                                                                          ifelse(Selisih > 86400, "> 1 hari",
                                                                                                                                                                                 "aman"))))))))))))))))))))))))
  

data1 <- data %>% subset(`Tgl Datang Fkrtl` >= "2025-09-01")
data1 <- data1 %>% subset(Selisih <= 1800)
data1 <- data1 %>% subset(!is.na(Biayaverifikasi))
data1 <- data1 %>%
  mutate(Selisih = `Waktu Kunjungan Fkrtl` - `Waktu Kunjungan Fktp`)
data1 <- data1[order(data1$Selisih,data1$`Tgl Datang Fkrtl`),]
data1$Selisih <- seconds_to_period(data1$Selisih)
save(data1,file = "history_rujuk.rda")

library(excel.link)
user <- xl.read.file("DATA USER FASKES 18 SEP 24 (BPJ$grs_1315).xlsx",
                          password = "BPJ$grs_1315",
                          header = TRUE,
                          row.names = NULL,
                          col.names = NULL,
                          xl.sheet = "daftar user",
                          top.left.cell = "A1",
                          na = "",
                          excel.visible = FALSE) %>%
  mutate(`Nama lengkap` = paste0(as.character(Firstname)," ",
                                 as.character(Lastname))) %>%
  select(Username,`Nama lengkap`,Email,Status)

df_data1 <- left_join(data1,user, by = c("User Pcare"="Username")) %>%
  rename(`Nama lengkap Pcare` = `Nama lengkap`,
         `Email Pcare` = Email,
         `Status Pcare` = Status)
df_data2 <- left_join(df_data1,user, by = c("User Fkrtl"="Username")) %>%
  rename(`Nama lengkap Fkrtl` = `Nama lengkap`,
         `Email Fkrtl` = Email,
         `Status Fkrtl` = Status)
#========================================================================
write.xlsx(df_data2, file = "Selisih kurang 30 menit.xlsx")
#========================================================================
write.csv(df_data1, "D://data gresik//MTF KC GRESIK//gresik_2020//history_rujukan_faskes//kurang 30 menit.csv",
          na="", row.names = FALSE)
#========================================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("ur_all.rda")

df_data1 <- df_data1 %>%
  select(-Nokapst,-Nosjp,-Diagmasuk,-Diagsekunder,-Biayaverifikasi)
df_data2 <- left_join(df_data1,data, by = c("Nofktp"="Norjkawalsep")) %>%
  select(,-Nmdati2Layan,-Nmppklayan,-Poli_asal)

write.csv(df_data2, "D://data gresik//MTF KC GRESIK//gresik_2020//history_rujukan_faskes//Selisih kurang 30 menit.csv",
          na="", row.names = FALSE)

colnames(df_data1)

%>%
  rename(`Nama lengkap Pcare` = `Nama lengkap`,
         `Email Pcare` = Email,
         `Status Pcare` = Status)



%>%
  subset(Nmkclayan == "GRESIK")

save(data, file = "history_rujuk.rda")

write.csv(data, "D://data gresik//MTF KC GRESIK//gresik_2020//history_rujuk.csv",
          na="", row.names = FALSE)