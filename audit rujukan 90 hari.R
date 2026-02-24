setwd("D:/data gresik/FactKunjungan")

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

df_rujukan <- read_csv("Sheet 1_Full Data_Kunjungan.csv") %>%
  filter(!str_detect(poli_rujuk,c("IGD"))) %>%
  select(no_kunjungan,tgl_kunjungan,nokapst,nmtkp,nmppkkunjungan,
         nmppkrujukan,poli_rujuk,kd_diagnosa,kd_diagnosa2,
         kd_diagnosa3,nm_diagnosa,nm_diagnosa2,nm_diagnosa3,
         spesialistik,kdtacc,nmtacc) %>%
  filter(str_detect(nmtkp,c("RJTP"))) 

df_rujukan$tgl_kunjungan <- as.Date(df_rujukan$tgl_kunjungan,
                                    format = "%m/%d/%Y")
df_rujukan <- df_rujukan %>% subset(tgl_kunjungan >= "2022-12-01")
df_rujukan <- df_rujukan[order(df_rujukan$nokapst,
                               df_rujukan$tgl_kunjungan),]

df_rujukan <- df_rujukan %>%
  mutate(date = lubridate::dmy(tgl_kunjungan)) %>% 
  group_by(nokapst) %>% 
  mutate(day = tgl_kunjungan - lag(tgl_kunjungan))

loc_teste2 %>% 
  mutate(Ptt = as.character(Ptt), Date = as.character(Date), Date = str_replace(Date, pattern = "Dez", replacement = "Dec"), Date = parse_date_time(Date, order = "hmsdmy")) %>% 
  group_by(Ptt) %>% 
  mutate(Threshold = min(Date) + days(40)) %>% 
  ungroup() %>% 
  mutate(Past_Threshold = Date > Threshold)


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
                                    ifelse(jeda < 88,1,
                                           ifelse(periode == "beda periode",0,1)))))

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

write.csv(df_rujukan,
          "D://data gresik//FactKunjungan//df_kunjungan.csv",
          na="", row.names = FALSE)

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


