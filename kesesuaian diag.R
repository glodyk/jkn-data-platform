setwd("D:/data gresik/FactKunjungan")
choose.files()

#Nokapst,Umur,Jenkel,Nosep,nmppkrjkawalsep,Nmtkp,`First Date`,
#`First Date Fp Sep`,`Last Date  Fp Sep`,`First Date Fp Approval`,
#`Last Date Fp Approval`,`Fp Sep`,`Fp Approval`,
#nmdati2asalpst,ppkasalpst,nmppkasalpst,ppkrjkawalsep,
#`Bulan Layan`,Nmdati2Layan,Kelasrsmenkes,Kdppklayan,Nmppklayan,Politujsep,nmdokter,kodediag,nmdiag,
#Flagprsklaimsep,`Ket Flagprsklaimsep`,Tglsep,Tgltagsep,
#Tglplgsep,Byversep

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

df_ksdiag20_21 <- read_csv("Sheet 1_Full Data_kesesuaian diag 20-21.csv") %>%
  subset(Diagakhir == "NON SPESIALISTIK") %>%
  select(norujukanpcare,Nokapst,Nosjp,Tglpelayanan,tglplgsjp,
         Jenis,Diagakhir,nmdati2perujuk,nmppkperujuk,Nmppklayan,
         Politujsjp,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,
         Nmdiagprimer,Diagsekunder,kdinacbgs,nminacbgs,Byversjp)

df_ksdiag22_23 <- read_csv("Sheet 1_Full Data_kesesuaian diag 22-23.csv") %>%
  subset(Diagakhir == "NON SPESIALISTIK") %>%
  select(norujukanpcare,Nokapst,Nosjp,Tglpelayanan,tglplgsjp,
         Jenis,Diagakhir,nmdati2perujuk,nmppkperujuk,Nmppklayan,
         Politujsjp,Kddiagmasuk,Nmdiagmasuk,Kodediagprimer,
         Nmdiagprimer,Diagsekunder,kdinacbgs,nminacbgs,Byversjp)

df_ksdiag <- rbind(df_ksdiag20_21,df_ksdiag22_23)
rm(df_ksdiag20_21,df_ksdiag22_23)

df_ksdiag$Tglpelayanan <- as.Date(df_ksdiag$Tglpelayanan,
                                     format = "%m/%d/%Y")
df_ksdiag$tglplgsjp <- as.Date(df_ksdiag$tglplgsjp,
                              format = "%m/%d/%Y")

#agar bisa SUBSET
df_approval$Kdppk <- as.character(trimws(substr(df_approval$Nosep,1,8)), "both")
#===============================SKIP======================================

df_approval <- df_approval %>% 
  mutate(
    NmppkLayan = case_when(
      Kdppk == "0204R024" ~ "RS Muhammadiyah Kalikapas",
      Kdppk == "0205S100" ~ "Klinik Mata Utama Gresik",
      Kdppk == "0204S001" ~ "Klinik Mata Utama Lamongan",
      Kdppk == "0204R015" ~ "RS Arsy Paciran",
      Kdppk == "0204R018" ~ "RS Bedah Mitra Sehat",
      Kdppk == "0205R009" ~ "RS Denisa",
      Kdppk == "0205R021" ~ "RS Fathma Medika",
      Kdppk == "0204R013" ~ "RS Fatimah",
      Kdppk == "0205R013" ~ "RS Grha Husada",
      Kdppk == "0204R014" ~ "RS Intan Medika",
      Kdppk == "0204R024" ~ "RS Muhammadiyah Kalikapas",
      Kdppk == "0204R012" ~ "RS Muhammadiyah Babat",
      Kdppk == "0205R011" ~ "RS Muhammadiyah Gresik",
      Kdppk == "0204R010" ~ "RS Muhammadiyah Lamongan",
      Kdppk == "0204R022" ~ "RS Nahdlatul Ulama Babat",
      Kdppk == "0204R023" ~ "RS Permata Hati",
      Kdppk == "1302R002" ~ "RS Petrokimia Gresik",
      Kdppk == "0205R014" ~ "RS Petrokimia Gresik Driyorejo",
      Kdppk == "0205R024" ~ "RS PKU Muhammadiyah Sekapuk",
      Kdppk == "0205R012" ~ "RS Semen Gresik",
      Kdppk == "0205R023" ~ "RS Surya Medika",
      Kdppk == "0205R019" ~ "RS Wali Songo I",
      Kdppk == "0205R022" ~ "RS Wates Husada",
      Kdppk == "0205R028" ~ "RSI Cahaya Giri",
      Kdppk == "0205R025" ~ "RSI Mabarrot MWC NU Bungah",
      Kdppk == "0204R009" ~ "RSI Nashrul Ummah",
      Kdppk == "0205R027" ~ "RSI Nyai Ageng Pinatih",
      Kdppk == "0205R032" ~ "RSIA Khodijah",
      Kdppk == "0204R017" ~ "RSU Citra Medika",
      Kdppk == "0204R004" ~ "RSU dr. Suyudi Paciran",
      Kdppk == "0205R031" ~ "RSU Eka Husada",
      Kdppk == "0204R019" ~ "RSU Muhammadiyah Babat",
      Kdppk == "0204R020" ~ "RSU Permata Bunda",
      Kdppk == "0205R026" ~ "RSU Rachmi Dewi",
      Kdppk == "0205R030" ~ "RSU Randegansari Husada",
      Kdppk == "1306R001" ~ "RSUD Dr Soegiri Lamongan",
      Kdppk == "1302R001" ~ "RSUD Ibnu Sina Gresik",
      Kdppk == "0204R021" ~ "RSUD Karangkembang",
      Kdppk == "0204R003" ~ "RSUD Ngimbang Lamongan",
      Kdppk == "0205R029" ~ "RSUD Umar Mas'ud Bawean",
      Kdppk == "0205S101" ~ "Klinik Utama Cerme",
      TRUE ~ Kdppk))

write.csv(df_ksdiag,
          "D://data gresik//FactKunjungan//df_ksdiag.csv",
          na="", row.names = FALSE)

df_approval$`First Date` <- as.Date(df_approval$`First Date`,
                                    format = "%m/%d/%Y")
df_approval$`First Date Fp Sep` <- as.Date(df_approval$`First Date Fp Sep`,
                                           format = "%m/%d/%Y")
df_approval$`Last Date  Fp Sep` <- as.Date(df_approval$`Last Date  Fp Sep`,
                                           format = "%m/%d/%Y")
df_approval$`First Date Fp Approval` <- as.Date(df_approval$`First Date Fp Approval`,
                                                format = "%m/%d/%Y")
df_approval$`Last Date Fp Approval` <- as.Date(df_approval$`Last Date Fp Approval`,
                                               format = "%m/%d/%Y")
df_approval$Tgltagsep <- as.Date(df_approval$Tgltagsep,
                                 format = "%m/%d/%Y")
df_approval$Tglplgsep <- as.Date(df_approval$Tglplgsep,
                                 format = "%m/%d/%Y")

#if first date = first date fp approval
df_approval$keterangan <- with(df_approval,
                               ifelse(`First Date` == `First Date Fp Approval`,
                                      "Tidak digunakan", "Next"))

df_rujukan$tgl_kunjungan <- as.Date(df_rujukan$tgl_kunjungan,
                                    format = "%m/%d/%Y")

df_rujukan <- df_rujukan[order(df_rujukan$nokapst,
                               df_rujukan$tgl_kunjungan),]

df_rujukan <- df_rujukan %>% group_by(nokapst) %>%
  mutate(tgl_before = lag(tgl_kunjungan, n=1))

df_rujukan$jeda <- as.integer(df_rujukan$tgl_kunjungan) - as.integer(df_rujukan$tgl_before)

df_rujukan <- df_rujukan[order(df_rujukan$nokapst,
                               df_rujukan$tgl_kunjungan),]
df_rujukan$Flag <- with(df_rujukan,
                        ifelse(is.na(df_rujukan$jeda), "rujukan FKTP",
                               ifelse(nmppkrujukan == lag(nmppkrujukan) & poli_rujuk == lag(poli_rujuk) & jeda > 88, "rujukan FKTP",
                                      ifelse(nmppkrujukan == lag(nmppkrujukan) & poli_rujuk != lag(poli_rujuk) & jeda > 88, "rujukan FKTP",
                                             ifelse(nmppkrujukan != lag(nmppkrujukan) & poli_rujuk != lag(poli_rujuk) & jeda > 88, "rujukan FKTP",
                                                    ifelse(nmppkrujukan != lag(nmppkrujukan) & poli_rujuk == lag(poli_rujuk) & jeda > 88, "rujukan FKTP",
                                                           ifelse(nmppkrujukan != lag(nmppkrujukan) & poli_rujuk != lag(poli_rujuk) & jeda < 88, "rujukan FKTP",
                                                                  ifelse(nmppkrujukan != lag(nmppkrujukan) & poli_rujuk == lag(poli_rujuk) & jeda < 88, "rujukan antar FKTL",
                                                                         ifelse(nmppkrujukan == lag(nmppkrujukan) & poli_rujuk == lag(poli_rujuk) & jeda < 88, "kontrol FKTL",
                                                                                ifelse(nmppkrujukan == lag(nmppkrujukan) & poli_rujuk != lag(poli_rujuk) & jeda < 88, "rujukan internal",
                                                                                       "Salah"))))))))))

df_rujukan$periode <- with(df_rujukan,
                        ifelse(is.na(df_rujukan$jeda), "beda periode",
                               ifelse(jeda < 88, "satu periode",
                               ifelse(jeda > 88, "beda periode",
                                      "beda periode"))))
# Buat ID berdasarkan grup
df_rujukan$id <- cumsum(df_rujukan$index)

df_rujukan$tf <- with(df_rujukan,
                      ifelse(is.na(df_rujukan$jeda),0,
                             ifelse(jeda > 88,0,
                             ifelse(periode == "beda periode",0,1))))

df_rujukan <- df_rujukan %>%
  mutate(diag1 = str_c(kd_diagnosa, nm_diagnosa, sep = " - ")) %>%
  select(-kd_diagnosa,-nm_diagnosa)
df_rujukan <- df_rujukan %>%
  mutate(diag2 = str_c(kd_diagnosa2, nm_diagnosa2, sep = " - ")) %>%
  select(-kd_diagnosa2,-nm_diagnosa2)
df_rujukan <- df_rujukan %>%
  mutate(diag3 = str_c(kd_diagnosa3, nm_diagnosa3, sep = " - ")) %>%
  select(-kd_diagnosa3,-nm_diagnosa3)

df_rujukan$diag12 <- ifelse(is.na(df_rujukan$diag2),
                            df_rujukan$diag1,
                            paste0(as.character(df_rujukan$diag1)
                                   ,"; ",
                                   as.character(df_rujukan$diag2)))

df_rujukan$diagrujukan <- ifelse(is.na(df_rujukan$diag3),
                                 df_rujukan$diag12,
                                 paste0(as.character(df_rujukan$diag12)
                                        ,"; ",
                                        as.character(df_rujukan$diag3)))

df_rujukan <- df_rujukan %>%
  select(-tgl_before,-diag1,-diag2,
         -diag3,-diag12,-jeda)

df_rujukan <- df_rujukan %>%
  group_by (nokapst,periode == "satu periode")  %>% 
  dplyr :: mutate (kunjungan =  sequence(n())) 


library(plm)
df_rujukan$index <- pdata.frame(df_rujukan, index = c("Flag","nokapst"))


df_rujukan$polippk <- paste0(as.character(df_rujukan$poli_rujuk),"; ",
                             as.character(df_rujukan$nmppkrujukan))


#kunjungan ke-
df_rujukan <- df_rujukan[order(df_rujukan$nokapst,
                               df_rujukan$tgl_kunjungan),]
df_rujukan <- df_rujukan %>%
  group_by(nokapst,nmppkrujukan,poli_rujuk,Flag) %>% 
  mutate(kunjungan_ke = sequence(n()))

#mengurutkan
df_rujukan <- df_rujukan[order(df_rujukan$nokapst,
                               df_rujukan$tgl_kunjungan),]
df_rujukan <- df_rujukan %>% group_by(nokapst) %>%
  mutate(poli2ppk = lag(polippk, n = 1))
df_rujukan <- df_rujukan[order(df_rujukan$nokapst,
                               df_rujukan$tgl_kunjungan),]

df_rujukan <- df_rujukan[order(df_rujukan$nokapst,
                               df_rujukan$tgl_kunjungan),]
df_rujukan$Flag <- with(df_rujukan,
                        ifelse(is.na(df_rujukan$jeda), "rujukan FKTP",
                               ifelse(polippk == poli2ppk & jeda >= 89, "rujukan FKTP",
                                      ifelse(polippk != poli2ppk & jeda >= 89, "rujukan FKTP",
                                             ifelse(polippk == poli2ppk & jeda <= 89, "kontrol FKTL",
                                                    ifelse(nmppkrujukan == 
                                                           polippk != poli2ppk & jeda <= 89, "rujukan internal",
                                                           "Salah"))))))


