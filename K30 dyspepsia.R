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

load("ur_all.rda")
data <- data %>%
  select(-CMG,-CBG,-Spec,-tipe)

nonspes <- data %>% subset(Tglpelayanan >= "2019-01-01") %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"K30")) %>%
  select(-Diagnosa)

nonspes <- nonspes %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
nonspes <- nonspes %>% mutate(Procedure = na_if(Procedure, "-"))
nonspes <- nonspes %>%
  mutate(
    Kddiagprimer1 = case_when(
      Kddiagprimer == "Z031" ~ NA,
      Kddiagprimer == "Z039" ~ NA,
      Kddiagprimer == "Z041" ~ NA,
      Kddiagprimer == "Z291" ~ NA,
      Kddiagprimer == "Z340" ~ NA,
      Kddiagprimer == "Z349" ~ NA,
      Kddiagprimer == "Z358" ~ NA,
      Kddiagprimer == "Z359" ~ NA,
      Kddiagprimer == "Z370" ~ NA,
      Kddiagprimer == "Z390" ~ NA,
      Kddiagprimer == "Z392" ~ NA,
      Kddiagprimer == "Z419" ~ NA,
      Kddiagprimer == "Z429" ~ NA,
      Kddiagprimer == "Z488" ~ NA,
      Kddiagprimer == "Z489" ~ NA,
      Kddiagprimer == "Z490" ~ NA,
      Kddiagprimer == "Z491" ~ NA,
      Kddiagprimer == "Z492" ~ NA,
      Kddiagprimer == "Z941" ~ NA,
      Kddiagprimer == "Z501" ~ NA,
      Kddiagprimer == "Z504" ~ NA,
      Kddiagprimer == "Z505" ~ NA,
      Kddiagprimer == "Z507" ~ NA,
      Kddiagprimer == "Z509" ~ NA,
      Kddiagprimer == "Z511" ~ NA,
      Kddiagprimer == "Z549" ~ NA,
      Kddiagprimer == "Z719" ~ NA,
      Kddiagprimer == "Z088" ~ NA,
      Kddiagprimer == "Z089" ~ NA,
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
      Kddiagprimer == "Z898" ~ NA,
      Kddiagprimer == "Z908" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z988" ~ NA,
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
nonspes$Diagnosa <- as.character(trimws(gsub(";NA|NA;|NA","",nonspes$Diagnosa)), "both")
nonspes <- nonspes %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)
nonspes <- nonspes %>% mutate(Diagnosa = na_if(Diagnosa, ""))

library(splitstackshape)
nonspes <- concat.split(nonspes, "Diagnosa", ";")
colnames(nonspes)

nonspes$Diagnosa_01 <- as.character(trimws(nonspes$Diagnosa_01))
nonspes$Diagnosa_02 <- as.character(trimws(nonspes$Diagnosa_02))
nonspes$Diagnosa_03 <- as.character(trimws(nonspes$Diagnosa_03))
nonspes$Diagnosa_04 <- as.character(trimws(nonspes$Diagnosa_04))
nonspes$Diagnosa_05 <- as.character(trimws(nonspes$Diagnosa_05))
nonspes$Diagnosa_06 <- as.character(trimws(nonspes$Diagnosa_06))
nonspes$Diagnosa_07 <- as.character(trimws(nonspes$Diagnosa_07))
nonspes$Diagnosa_08 <- as.character(trimws(nonspes$Diagnosa_08))
nonspes$Diagnosa_09 <- as.character(trimws(nonspes$Diagnosa_09))
nonspes$Diagnosa_10 <- as.character(trimws(nonspes$Diagnosa_10))
nonspes$Diagnosa_11 <- as.character(trimws(nonspes$Diagnosa_11))

nonspes$Diagnosa_01 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_01),NA,Diagnosa_01))
nonspes$Diagnosa_02 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_02),NA,Diagnosa_02))
nonspes$Diagnosa_03 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_03),NA,Diagnosa_03))
nonspes$Diagnosa_04 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_04),NA,Diagnosa_04))
nonspes$Diagnosa_05 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_05),NA,Diagnosa_05))
nonspes$Diagnosa_06 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_06),NA,Diagnosa_06))
nonspes$Diagnosa_07 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_07),NA,Diagnosa_07))
nonspes$Diagnosa_08 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_08),NA,Diagnosa_08))
nonspes$Diagnosa_09 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_09),NA,Diagnosa_09))
nonspes$Diagnosa_10 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_10),NA,Diagnosa_10))
nonspes$Diagnosa_11 <- with(nonspes,ifelse(is.na(nonspes$Diagnosa_11),NA,Diagnosa_11))

nonspes <- nonspes %>%
  mutate(Diagnosa = paste0(as.character(Diagnosa_01),";",
                           as.character(Diagnosa_02),";",
                           as.character(Diagnosa_03),";",
                           as.character(Diagnosa_04),";",
                           as.character(Diagnosa_05),";",
                           as.character(Diagnosa_06),";",
                           as.character(Diagnosa_07),";",
                           as.character(Diagnosa_08),";",
                           as.character(Diagnosa_09),";",
                           as.character(Diagnosa_10),";",
                           as.character(Diagnosa_11)))

nonspes$Diagnosa <- as.character(trimws(gsub("NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;|NA","",nonspes$Diagnosa)),"both")
nonspes <- nonspes %>%
  select(-Diagnosa_01,-Diagnosa_02,-Diagnosa_03,-Diagnosa_04,-Diagnosa_05,
         -Diagnosa_06,-Diagnosa_07,-Diagnosa_08,-Diagnosa_09,-Diagnosa_10,
         -Diagnosa_11)
nonspes <- nonspes %>% mutate(Diagnosa = na_if(Diagnosa, ""))
nonspes1 <- nonspes %>% subset(is.na(Diagnosa))

nonspes$Diagnosa <- with(nonspes,
                         ifelse(Diagnosa == "E876 - Hypokalaemia;K30 - Dyspepsia", "K30 - Dyspepsia;E876 - Hypokalaemia",
                                ifelse(Diagnosa == "R11 - Nausea and vomiting;K30 - Dyspepsia", "K30 - Dyspepsia;R11 - Nausea and vomiting",
                                       ifelse(Diagnosa == "R42 - Dizziness and giddiness;K30 - Dyspepsia", "K30 - Dyspepsia;R42 - Dizziness and giddiness",
                                              ifelse(Diagnosa == "E162 - Hypoglycaemia, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;E162 - Hypoglycaemia, unspecified",
                                                     ifelse(Diagnosa == "A162 - Tuberculosis of lung, without mention of bacteriological or histological confirmation;K30 - Dyspepsia", "K30 - Dyspepsia;A162 - Tuberculosis of lung, without mention of bacteriological or histological confirmation",
                                                            ifelse(Diagnosa == "J40 - Bronchitis, not specified as acute or chronic;K30 - Dyspepsia", "K30 - Dyspepsia;J40 - Bronchitis, not specified as acute or chronic",
                                                                   ifelse(Diagnosa == "J069 - Acute upper respiratory infection, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;J069 - Acute upper respiratory infection, unspecified",
                                                                          ifelse(Diagnosa == "A90 - Dengue fever [classical dengue];K30 - Dyspepsia", "K30 - Dyspepsia;A90 - Dengue fever [classical dengue]",
                                                                                 ifelse(Diagnosa == "A91 - Dengue haemorrhagic fever;K30 - Dyspepsia", "K30 - Dyspepsia;A91 - Dengue haemorrhagic fever",
                                                                                        ifelse(Diagnosa == "I10 - Essential (primary) hypertension;K30 - Dyspepsia", "K30 - Dyspepsia;I10 - Essential (primary) hypertension",
                                                                                               ifelse(Diagnosa == "E119 - Non-insulin-dependent diabetes mellitus without complications;K30 - Dyspepsia", "K30 - Dyspepsia;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                                      ifelse(Diagnosa == "H811 - Benign paroxysmal verti;K30 - Dyspepsia", "K30 - Dyspepsia;H811 - Benign paroxysmal verti",
                                                                                                             ifelse(Diagnosa == "A099 - Gastroenteritis and colitis of unspecified origin;K30 - Dyspepsia", "K30 - Dyspepsia;A099 - Gastroenteritis and colitis of unspecified origin",
                                                                                                                    ifelse(Diagnosa == "A090 - Other and unspecified gastroenteritis and colitis of infectious origin;K30 - Dyspepsia", "K30 - Dyspepsia;A090 - Other and unspecified gastroenteritis and colitis of infectious origin",
                                                                                                                           ifelse(Diagnosa == "A010 - Typhoid fever;K30 - Dyspepsia", "K30 - Dyspepsia;A010 - Typhoid fever",
                                                                                                                                  ifelse(Diagnosa == "H814 - Verti of central origin;K30 - Dyspepsia", "K30 - Dyspepsia;H814 - Verti of central origin",
                                                                                                                                         ifelse(Diagnosa == "D649 - Anaemia, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;D649 - Anaemia, unspecified",
                                                                                                                                                ifelse(Diagnosa == "N390 - Urinary tract infection, site not specified;K30 - Dyspepsia", "K30 - Dyspepsia;N390 - Urinary tract infection, site not specified",
                                                                                                                                                       ifelse(Diagnosa == "K590 - Constipation;K30 - Dyspepsia", "K30 - Dyspepsia;K590 - Constipation",
                                                                                                                                                              ifelse(Diagnosa == "R509 - Fever, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;R509 - Fever, unspecified",
                                                                                                                                                                     ifelse(Diagnosa == "G442 - Tension-type headache;K30 - Dyspepsia", "K30 - Dyspepsia;G442 - Tension-type headache",
                                                                                                                                                                            ifelse(Diagnosa == "R104 - Other and unspecified abdominal pain;K30 - Dyspepsia", "K30 - Dyspepsia;R104 - Other and unspecified abdominal pain",
                                                                                                                                                                                   ifelse(Diagnosa == "J459 - Asthma, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;J459 - Asthma, unspecified",
                                                                                                                                                                                          ifelse(Diagnosa == "J189 - Pneumonia, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;J189 - Pneumonia, unspecified",
                                                                                                                                                                                                 ifelse(Diagnosa == "I200 - Unstable angina;K30 - Dyspepsia", "K30 - Dyspepsia;I200 - Unstable angina",
                                                                                                                                                                                                        ifelse(Diagnosa == "K219 - Gastro-oesophageal reflux disease without oesophagitis;K30 - Dyspepsia", "K30 - Dyspepsia;K219 - Gastro-oesophageal reflux disease without oesophagitis",
                                                                                                                                                                                                               ifelse(Diagnosa == "I119 - Hypertensive heart disease without (congestive) heart failure;K30 - Dyspepsia", "K30 - Dyspepsia;I119 - Hypertensive heart disease without (congestive) heart failure",
                                                                                                                                                                                                                      ifelse(Diagnosa == "K921 - Melaena;K30 - Dyspepsia", "K30 - Dyspepsia;K921 - Melaena",
                                                                                                                                                                                                                             ifelse(Diagnosa == "B181 - Chronic viral hepatitis B without delta-agent;K30 - Dyspepsia", "K30 - Dyspepsia;B181 - Chronic viral hepatitis B without delta-agent",
                                                                                                                                                                                                                                    ifelse(Diagnosa == "E059 - Thyrotoxicosis, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;E059 - Thyrotoxicosis, unspecified",
                                                                                                                                                                                                                                           ifelse(Diagnosa == "J449 - Chronic obstructive pulmonary disease, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;J449 - Chronic obstructive pulmonary disease, unspecified",
                                                                                                                                                                                                                                                  ifelse(Diagnosa == "F419 - Anxiety disorder, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;F419 - Anxiety disorder, unspecified",
                                                                                                                                                                                                                                                         ifelse(Diagnosa == "M545 - Low back pain;K30 - Dyspepsia", "K30 - Dyspepsia;M545 - Low back pain",
                                                                                                                                                                                                                                                                ifelse(Diagnosa == "M5459 - Low back pain, site unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;M5459 - Low back pain, site unspecified",
                                                                                                                                                                                                                                                                       ifelse(Diagnosa == "K808 - Other cholelithiasis;K30 - Dyspepsia", "K30 - Dyspepsia;K808 - Other cholelithiasis",
                                                                                                                                                                                                                                                                              ifelse(Diagnosa == "R51 - Headache;K30 - Dyspepsia", "K30 - Dyspepsia;R51 - Headache",
                                                                                                                                                                                                                                                                                     ifelse(Diagnosa == "M329 - Systemic lupus erythematosus, unspecified;K30 - Dyspepsia", "K30 - Dyspepsia;M329 - Systemic lupus erythematosus, unspecified",
                                                                                                                                                                                                                                                                                            Diagnosa))))))))))))))))))))))))))))))))))))))
                                                    
#======================================================================================
write.csv(nonspes, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_dyspepsia_rajal.csv",
          na="", row.names = FALSE)


setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("ur_all.rda")

data0 <- data0 %>%
  select(Nosjp,Flagspesialistik,Flagtacc,Jenisppkperujuk,Typeppkperujuk,
         Typeppklayan,Diagnosa) %>% unique()

data0 <- left_join(data0,data) %>%
  select(-CMG,-CBG,-Spec,-Sevel,-tipe)

data0 <- data0 %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data0 <- data0 %>% mutate(Procedure = na_if(Procedure, "-"))

#===============================SKIP======================================
audit <- data0 %>% subset(Tglpelayanan >= "2016-01-01")
audit1 <- audit %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Nmjnspulang == "Sehat") %>%
  filter(str_detect(Kddiagprimer,c("Z|K"))) %>%
  subset(Diagsekunder %in% c(NA,"K30 - Dyspepsia")) %>%
  subset(is.na(Procedure)) %>%
  select(Nosjp) %>% unique()

audit2 <- audit %>%
  subset(Nmtkp == "RJTL") %>%
  subset(Nmjnspulang == "Sehat") %>%
  filter(str_detect(Kddiagprimer,c("Z|K"))) %>%
  subset(Diagsekunder %in% c(NA,"K30 - Dyspepsia")) %>%
  filter(!str_detect(Procedure,c("4413|4513|8872|8874|8876|8878|8879"))) %>%
  select(Nosjp) %>% unique()

audit3 <- rbind(audit1,audit2) %>% unique()

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("ur_all.rda")

data0 <- data0 %>%
  select(Nosjp,Flagspesialistik,Flagtacc,Jenisppkperujuk,Typeppkperujuk,
         Typeppklayan) %>% unique()

data0 <- left_join(data0,data)

audit0 <- audit0 %>%
  select(Nokapst,Nosjp,Norjkawalsep,Umur,kel_umur,Jkpst,Sumber,Nmppkperujuk,
         Nmdati2Layan,Nmppklayan,Nmtkp,Politujsjp,Poli_asal,Tglpelayanan,
         Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
         Biayaverifikasi) %>%
  subset(Sumber %in% c("Kontrol Ulang","Rujukan Internal"))

readmisi_1 <- audit0[order(audit0$Nokapst,audit0$Tglplgsjp),]
readmisi_1 <- readmisi_1 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_1$interval <- as.integer(readmisi_1$Tgldtgsjp) - as.integer(readmisi_1$tgl_before)
readmisi_2 <- readmisi_1 %>%
  subset(!is.na(interval))
readmisi_3 <- readmisi_2 %>%
  subset(interval <= 14) %>% select(Nokapst,Nmppklayan) %>% unique()
readmisi_4 <- left_join(readmisi_3,readmisi_1, by = c("Nokapst","Nmppklayan")) %>%
  select(Nokapst,Nosjp,Norjkawalsep,Umur,kel_umur,Jkpst,Sumber,Nmppkperujuk,
         Nmdati2Layan,Nmppklayan,Nmtkp,Politujsjp,Poli_asal,Tglpelayanan,
         Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
         Biayaverifikasi)
readmisi_4 <- readmisi_4[order(readmisi_4$Nokapst,readmisi_4$Tglplgsjp),]
readmisi_4 <- readmisi_4 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_4$interval <- as.integer(readmisi_4$Tgldtgsjp) - as.integer(readmisi_4$tgl_before)
readmisi_4$status <- with(readmisi_4,
                          ifelse(interval <= 14, "readmisi", interval))
readmisi_4 <- readmisi_4 %>% group_by(Nokapst) %>%
  mutate(status1 = lead(status, n=1))

readmisi_5 <- readmisi_4 %>%
  subset(status == "readmisi"|status1 == "readmisi") %>%
  select(Nokapst,Nosjp,Norjkawalsep,Umur,kel_umur,Jkpst,Sumber,Nmppkperujuk,
         Nmdati2Layan,Nmppklayan,Nmtkp,Politujsjp,Poli_asal,Tglpelayanan,
         Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
         Biayaverifikasi)
readmisi_5 <- readmisi_5[order(readmisi_5$Nokapst,readmisi_5$Tglplgsjp),]
readmisi_5 <- readmisi_5 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_5$interval <- as.integer(readmisi_5$Tgldtgsjp) - as.integer(readmisi_5$tgl_before)

readmisi_5 <- readmisi_5 %>%
  select(Nokapst,Nosjp,Norjkawalsep,Umur,kel_umur,Jkpst,Sumber,Nmppkperujuk,
         Nmdati2Layan,Nmppklayan,Nmtkp,Politujsjp,Poli_asal,Tglpelayanan,
         Tgldtgsjp,Tglplgsjp,interval,Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
         Biayaverifikasi)
readmisi_5 <- readmisi_5[order(readmisi_5$Nokapst,readmisi_5$Tglplgsjp),]
readmisi_5 <- readmisi_5 %>%
  group_by(Nokapst) %>% 
  mutate(kunjungan_all = sequence(n()))
readmisi_5 <- readmisi_5 %>%
  group_by(Norjkawalsep) %>% 
  mutate(kunjungan_ke = sequence(n()))
#========================================================================
write.xlsx(readmisi_5, file = "readmisi 14 hr_rajal_dyspepsia.xlsx")
#========================================================================