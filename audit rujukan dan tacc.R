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
library(readxl)
library(excel.link)
library(data.table)

df_ruj25 <- read_csv("Sheet 1_Full Data_rujukan 2025_.csv") %>%
  select(nokapst,nmpst,tgllhrpst,jkpst,nmpoli,no_kunjungan,tgl_kunjungan,
         nmtkp,nmdati2kunjungan,nmjnsppkkunjungan,nmtipeppkkunjungan,
         nmppkkunjungan,nmdati2rujukan,nmjnsppkrujukan,nmtipeppkrujukan,
         nmppkrujukan,poli_rujuk,kd_diagnosa,kd_diagnosa2,kd_diagnosa3,
         nm_diagnosa,nm_diagnosa2,nm_diagnosa3,spesialistik,kdtacc,nmtacc,
         pstprb,nmjenisprb,pstprolanis,nmjenisprolanis)

df_ruj24 <- read_csv("Sheet 1_Full Data_rujukan 2024.csv") %>%
  select(nokapst,nmpst,tgllhrpst,jkpst,nmpoli,no_kunjungan,tgl_kunjungan,
         nmtkp,nmdati2kunjungan,nmjnsppkkunjungan,nmtipeppkkunjungan,
         nmppkkunjungan,nmdati2rujukan,nmjnsppkrujukan,nmtipeppkrujukan,
         nmppkrujukan,poli_rujuk,kd_diagnosa,kd_diagnosa2,kd_diagnosa3,
         nm_diagnosa,nm_diagnosa2,nm_diagnosa3,spesialistik,kdtacc,nmtacc,
         pstprb,nmjenisprb,pstprolanis,nmjenisprolanis)

df_ruj23 <- read_csv("Sheet 1_Full Data_rujukan 2023.csv") %>%
  select(nokapst,nmpst,tgllhrpst,jkpst,nmpoli,no_kunjungan,tgl_kunjungan,
         nmtkp,nmdati2kunjungan,nmjnsppkkunjungan,nmtipeppkkunjungan,
         nmppkkunjungan,nmdati2rujukan,nmjnsppkrujukan,nmtipeppkrujukan,
         nmppkrujukan,poli_rujuk,kd_diagnosa,kd_diagnosa2,kd_diagnosa3,
         nm_diagnosa,nm_diagnosa2,nm_diagnosa3,spesialistik,kdtacc,nmtacc,
         pstprb,nmjenisprb,pstprolanis,nmjenisprolanis)

df_ruj22 <- read_csv("Sheet 1_Full Data_rujukan 2022.csv") %>%
  select(nokapst,nmpst,tgllhrpst,jkpst,nmpoli,no_kunjungan,tgl_kunjungan,
         nmtkp,nmdati2kunjungan,nmjnsppkkunjungan,nmtipeppkkunjungan,
         nmppkkunjungan,nmdati2rujukan,nmjnsppkrujukan,nmtipeppkrujukan,
         nmppkrujukan,poli_rujuk,kd_diagnosa,kd_diagnosa2,kd_diagnosa3,
         nm_diagnosa,nm_diagnosa2,nm_diagnosa3,spesialistik,kdtacc,nmtacc,
         pstprb,nmjenisprb,pstprolanis,nmjenisprolanis)

df_ruj21 <- read_csv("Sheet 1_Full Data_rujukan 2021.csv") %>%
  select(nokapst,nmpst,tgllhrpst,jkpst,nmpoli,no_kunjungan,tgl_kunjungan,
         nmtkp,nmdati2kunjungan,nmjnsppkkunjungan,nmtipeppkkunjungan,
         nmppkkunjungan,nmdati2rujukan,nmjnsppkrujukan,nmtipeppkrujukan,
         nmppkrujukan,poli_rujuk,kd_diagnosa,kd_diagnosa2,kd_diagnosa3,
         nm_diagnosa,nm_diagnosa2,nm_diagnosa3,spesialistik,kdtacc,nmtacc,
         pstprb,nmjenisprb,pstprolanis,nmjenisprolanis)

df_ruj <- rbind(df_ruj21,df_ruj22,df_ruj23,df_ruj24,df_ruj25)
rm(df_ruj21,df_ruj22,df_ruj23,df_ruj24,df_ruj25)

df_ruj$tgllhrpst <- as.Date(df_ruj$tgllhrpst, format = "%m/%d/%Y")
df_ruj$tgl_kunjungan <- as.Date(df_ruj$tgl_kunjungan, format = "%m/%d/%Y")

df_ruj <- df_ruj %>% subset(tgl_kunjungan >= "2021-01-01")

nmppkrujukna <- df_ruj %>% subset(is.na(nmppkrujukan))
polirujukna <- df_ruj %>% subset(is.na(poli_rujuk))
#======================================================================================
cek1 <- df25 %>%
  subset(Norjkawalsep %in% c("019800030825Y000318","0185B0100125P000051","019800031025Y000273"))
cek1 <- df_ruj %>%
  subset(no_kunjungan == "0205B0691024P000111")
cek1$poli_rujuk <- "INT"
dataeror <- df_ruj %>% 
  subset(no_kunjungan != "0205B0691024P000111")
df_ruj <- rbind(dataeror,cek1)
rm(nmppkrujukna,polirujukna,dataeror,cek1)
#======================================================================================
df_ruj$kd_diagnosa <- as.character(trimws(df_ruj$kd_diagnosa))
df_ruj$nm_diagnosa <- as.character(trimws(df_ruj$nm_diagnosa))
df_ruj$kd_diagnosa2 <- with(df_ruj,ifelse(is.na(df_ruj$kd_diagnosa2),"",kd_diagnosa2))
df_ruj$nm_diagnosa2 <- with(df_ruj,ifelse(is.na(df_ruj$nm_diagnosa2),"",nm_diagnosa2))
df_ruj$kd_diagnosa2 <- as.character(trimws(df_ruj$kd_diagnosa2))
df_ruj$nm_diagnosa2 <- as.character(trimws(df_ruj$nm_diagnosa2))
df_ruj$kd_diagnosa3 <- with(df_ruj,ifelse(is.na(df_ruj$kd_diagnosa3),"",kd_diagnosa3))
df_ruj$nm_diagnosa3 <- with(df_ruj,ifelse(is.na(df_ruj$nm_diagnosa3),"",nm_diagnosa3))
df_ruj$kd_diagnosa3 <- as.character(trimws(df_ruj$kd_diagnosa3))
df_ruj$nm_diagnosa3 <- as.character(trimws(df_ruj$nm_diagnosa3))

df_ruj$kd_diagnosa2 [is.na(df_ruj$kd_diagnosa2)] <- ""
df_ruj$nm_diagnosa2 [is.na(df_ruj$nm_diagnosa2)] <- ""
df_ruj$kd_diagnosa3 [is.na(df_ruj$kd_diagnosa3)] <- ""
df_ruj$nm_diagnosa3 [is.na(df_ruj$nm_diagnosa3)] <- ""

df_ruj <- df_ruj %>%
  mutate(Diagnosa = paste0(as.character(kd_diagnosa)," - ",
                           as.character(nm_diagnosa),";",
                           as.character(kd_diagnosa2)," - ",
                           as.character(nm_diagnosa2),";",
                           as.character(kd_diagnosa3)," - ",
                           as.character(nm_diagnosa3)))
df_ruj$Diagnosa <- as.character(trimws(gsub("; - ; - |; - |; -","",df_ruj$Diagnosa)),"both")

df_ruj <- df_ruj %>% select(-kd_diagnosa,-nm_diagnosa,-kd_diagnosa2,
                            -nm_diagnosa2,-kd_diagnosa3,-nm_diagnosa3)

df_ruj$Umur <- round(lubridate::time_length(difftime(df_ruj$tgl_kunjungan,
                                               df_ruj$tgllhrpst), "years"),
                     digits = 0)
df_ruj$tgllhrpst <- NULL


df_ruj <- df_ruj %>% mutate(Diagnosa = na_if(Diagnosa,""))
df_ruj <- df_ruj %>% mutate(Diagnosa = na_if(Diagnosa,"-"))
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

df_ruj <- left_join(df_ruj,refpoli, by = c("poli_rujuk"="KDPOLI")) %>%
  select(-poli_rujuk)
df_ruj <- df_ruj %>% rename(poli_rujuk = NMPOLI)
rm(refpoli)

polirujukna1 <- df_ruj %>% subset(is.na(poli_rujuk))
rm(polirujukna1)
#======================================================================================
df_ruj <- df_ruj[order(df_ruj$nokapst,df_ruj$tgl_kunjungan),]
df_ruj <- df_ruj %>% group_by(nokapst) %>%
  mutate(tglruj_before = lag(tgl_kunjungan, n=1))

df_ruj <- df_ruj[order(df_ruj$nokapst,df_ruj$tgl_kunjungan),]
df_ruj <- df_ruj %>% group_by(nokapst) %>%
  mutate(poli_before = lag(poli_rujuk, n=1))

df_ruj <- df_ruj[order(df_ruj$nokapst,df_ruj$tgl_kunjungan),]
df_ruj <- df_ruj %>% group_by(nokapst) %>%
  mutate(nmppkrujuk_before = lag(nmppkrujukan, n=1))

df_ruj <- df_ruj[order(df_ruj$nokapst,df_ruj$tgl_kunjungan),]
df_ruj <- df_ruj %>% group_by(nokapst) %>%
  mutate(nmtipeppkrujuk_before = lag(nmtipeppkrujukan, n=1))

df_ruj$jeda <- as.integer(df_ruj$tgl_kunjungan) - as.integer(df_ruj$tglruj_before)
df_ruj$Sumber1 <- with(df_ruj,
                       ifelse(is.na(df_ruj$tglruj_before), "Rujukan FKTP",
                              ifelse(nokapst == lag(nokapst) & jeda >= 90, "Rujukan FKTP",NA)))
df_ruj <- df_ruj[order(df_ruj$nokapst,df_ruj$tgl_kunjungan),]
df_ruj$Sumber2 <- with(df_ruj,
                       ifelse(nokapst == lag(nokapst) & nmppkrujukan == lag(nmppkrujukan) & poli_rujuk == lag(poli_rujuk), "Rujukan FKTP (kontrol)",
                              ifelse(nokapst == lag(nokapst) & nmppkrujukan == lag(nmppkrujukan) & poli_rujuk != lag(poli_rujuk), "Rujukan FKTP (rujukan internal)",
                                      ifelse(nokapst == lag(nokapst) & nmppkrujukan != lag(nmppkrujukan) & poli_rujuk == lag(poli_rujuk), "Rujukan FKTP (2nd opinion)",
                                             ifelse(nokapst == lag(nokapst) & nmppkrujukan != lag(nmppkrujukan) & poli_rujuk != lag(poli_rujuk), "Rujukan FKTP",
                                                    Sumber1)))))
df_ruj <- df_ruj[order(df_ruj$nokapst,df_ruj$tgl_kunjungan),]
df_ruj$Flag <- with(df_ruj,
                    ifelse(is.na(df_ruj$Sumber1),Sumber2,Sumber1))
df_ruj <- df_ruj %>% select(-Sumber1,-Sumber2)

#df_ruj$Flag <- with(df_ruj,
#                    ifelse(is.na(df_ruj$jeda), "rujukan baru FKTP",
#                           ifelse(nokapst == lag(nokapst) & jeda >= 90, "perbarui rujukan (habis masa berlaku)",
#                                  ifelse(nokapst == lag(nokapst) & nmppkrujukan == lag(nmppkrujukan) & poli_rujuk != lag(poli_rujuk) & jeda < 90, "rujukan internal (masih berlaku)",
#                                         ifelse(nokapst == lag(nokapst) & nmppkrujukan == lag(nmppkrujukan) & poli_rujuk == lag(poli_rujuk) & jeda < 90, "kontrol (masih berlaku)",
#                                                ifelse(nokapst == lag(nokapst) & nmppkrujukan != lag(nmppkrujukan) & poli_rujuk != lag(poli_rujuk) & jeda < 90, "rujukan baru (Beda Poli & beda Rumah Sakit)",
#                                                       ifelse(nokapst == lag(nokapst) & nmppkrujukan != lag(nmppkrujukan) & poli_rujuk == lag(poli_rujuk) & jeda < 90, "rujukan baru (Poli sama tp beda Rumah Sakit)",
#                                                             "Salah")))))))

ceksalah <- df_ruj %>% subset(Flag == "Salah")
ceksalah <- df_ruj %>% subset(is.na(Flag))
rm(ceksalah)

df_rujint <- df_ruj %>%
  subset(Flag == "rujukan internal (masih berlaku)") %>%
  select(nokapst) %>% unique()
df_rujint <- left_join(df_rujint,df_ruj, by = c("nokapst"="nokapst"))

df_rujkon <- df_ruj %>%
  subset(Flag == "kontrol (masih berlaku)") %>%
  select(nokapst) %>% unique()
df_rujkon <- left_join(df_rujkon,df_ruj, by = c("nokapst"="nokapst"))

df_rujbedars <- df_ruj %>%
  subset(Flag == "rujukan baru (Beda Poli & beda Rumah Sakit)")
df_ruj2opi <- df_ruj %>%
  subset(Flag == "rujukan baru (Poli sama tp beda Rumah Sakit)")


df_ruj <- df_ruj[order(df_ruj$nokapst,df_ruj$tgl_kunjungan),]
df_ruj <- df_ruj %>%
  group_by(nokapst) %>%
  mutate(rujuk_ke = sequence(n()))


cekpoli <- df_ruj %>% subset(is.na(poli_rujuk))
#cekppkrujuk <- df_ruj %>% subset(is.na(nmppkrujukan))
ceknoka <- df_ruj %>% subset(nokapst == "92409028")
cek <- df_ruj %>% subset(no_kunjungan == "130209010923Y003582")
cek$poli_rujuk <- "Anak"

dataeror <- df_ruj %>% subset(no_kunjungan != "130209010923Y003582")
df_ruj <-rbind(dataeror,cek)

rm(cek,dataeror)

df_ruj <- df_ruj %>%
  select(nokapst,nmpst,Umur,jkpst,nmpoli,no_kunjungan,tgl_kunjungan,
         nmtkp,nmdati2kunjungan,nmjnsppkkunjungan,nmtipeppkkunjungan,
         nmppkkunjungan,nmdati2rujukan,nmjnsppkrujukan,nmtipeppkrujukan,
         nmppkrujukan,nmppkrujuk_before,poli_rujuk,poli_before,Diagnosa,
         spesialistik,kdtacc,nmtacc,pstprb,nmjenisprb,pstprolanis,
         nmjenisprolanis,jeda)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
save(df_ruj, file="df_rujukan.rda")
load("df_rujukan.rda")

write.csv(df_ruj,
          "D://data gresik//FactKunjungan//df_kunjungan.csv",
          na="", row.names = FALSE)

df_rujukan <- df_rujukan %>%
  mutate(date = lubridate::dmy(tgl_kunjungan)) %>% 
  group_by(nokapst) %>% 
  mutate(day = tgl_kunjungan - first(tgl_kunjungan))

loc_teste2 %>% 
  mutate(Ptt = as.character(Ptt), Date = as.character(Date), Date = str_replace(Date, pattern = "Dez", replacement = "Dec"), Date = parse_date_time(Date, order = "hmsdmy")) %>% 
  group_by(Ptt) %>% 
  mutate(Threshold = min(Date) + days(40)) %>% 
  ungroup() %>% 
  mutate(Past_Threshold = Date > Threshold)



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


