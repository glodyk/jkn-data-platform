setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/Nonkapitasi")

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
nonkap <- read_csv("Sheet 1_Full Data_nonkap agu24.csv") %>%
  select (Nokapst,Jkpst,Umur,Rangeumur,Segmen,Nmpks,Nosjp,Nmtkp,
          Kdppkterdaftar,Nmppkterdaftar,Nmdati2Layan,Jenisppklayan,
          Typeppklayan,Kdppklayan,Nmppklayan,Kddiagnosa,Nmdiagnosa,
          Tglpelayanan,Tglkunjungan,Tglpulang,Tgltindakan,Nmtindakan,
          Jnsnakes,Namanakes,Nmstatuspulang,status_fpk,lmstjklaim,
          Tglbayar,Biaya) %>%
  subset(Kdppklayan == "0204B012") %>%
  subset(Kdppkterdaftar %in% c("13060601","0204B014","0204B016","0204U140")) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagnosa)," - ",
                           as.character(Nmdiagnosa))) %>%
  select(-Kddiagnosa,-Nmdiagnosa) %>%
  subset(Biaya != 0) %>%
  mutate(Klasifikasi = case_when(
    str_detect(Nmtindakan,c("Persalinan|persalinan|keguguran")) ~ "Persalinan",
    str_detect(Nmtindakan,c("pra-rujukan")) ~ "Pra Rujukan",
    str_detect(Nmtindakan,c("ANC")) ~ "ANC",
    str_detect(Nmtindakan,c("PNC")) ~ "PNC",
    str_detect(Nmtindakan,c("Rawat Inap")) ~ "Rawat Inap",
    str_detect(Nmtindakan,c("Kolesterol")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HbA1c")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Mikroalbumin|Microalbumin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("LDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Trigliserida")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Asam Urat|Ureum")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Kreatinin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("KB")) ~ "KB",
    str_detect(Nmtindakan,c("Ambulans")) ~ "Ambulans",
    str_detect(Nmtindakan,c("Skrining DM")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Papsmear")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("IVA")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Krio")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("PRB/Prolanis")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Protesa Gigi")) ~ "Protesa Gigi",
    TRUE ~ "Non Kapitasi lain-lain")) %>%
  subset(Klasifikasi == "Rawat Inap") %>%
  select(Biaya,Nmdati2Layan,Tglpelayanan,Tglstjkeu)
kapitasi$tglpelayanan <- as.Date(kapitasi$tglpelayanan, format = "%m/%d/%Y")
kapitasi$tglstjkeu <- as.Date(kapitasi$tglstjkeu, format = "%m/%d/%Y")
kapitasi$tanggalbayar <- as.Date(kapitasi$tanggalbayar, format = "%m/%d/%Y")
kapitasi <- kapitasi %>%
  subset(tglpelayanan >= "2024-01-01")
rekap <- kapitasi %>%
  group_by(nmdati2,tglpelayanan) %>%
  summarise(bykapitasi)
write.xlsx(rekap, file = "rekapitasi.xlsx")
#=============================data nadyah=============================
nonkap01 <- read_csv("Sheet 1_Full Data_nonkap 14_18.csv") %>%
  select (Nokapst,Jkpst,Umur,Rangeumur,Segmen,Nmpks,Nosjp,Nmtkp,
          Kdppkterdaftar,Nmppkterdaftar,Nmdati2Layan,Jenisppklayan,
          Typeppklayan,Kdppklayan,Nmppklayan,Kddiagnosa,Nmdiagnosa,
          Tglpelayanan,Tglkunjungan,Tglpulang,Tgltindakan,Nmtindakan,
          Jnsnakes,Namanakes,Nmstatuspulang,status_fpk,lmstjklaim,
          Tglbayar,Biaya) %>%
  subset(Kdppklayan == "0204B012") %>%
  subset(Kdppkterdaftar %in% c("13060601","0204B014","0204B016","0204U140")) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagnosa)," - ",
                           as.character(Nmdiagnosa))) %>%
  select(-Kddiagnosa,-Nmdiagnosa) %>%
  subset(Biaya != 0) %>%
  mutate(Klasifikasi = case_when(
    str_detect(Nmtindakan,c("Persalinan|persalinan|keguguran")) ~ "Persalinan",
    str_detect(Nmtindakan,c("pra-rujukan")) ~ "Pra Rujukan",
    str_detect(Nmtindakan,c("ANC")) ~ "ANC",
    str_detect(Nmtindakan,c("PNC")) ~ "PNC",
    str_detect(Nmtindakan,c("Rawat Inap")) ~ "Rawat Inap",
    str_detect(Nmtindakan,c("Kolesterol")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HbA1c")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Mikroalbumin|Microalbumin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("LDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Trigliserida")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Asam Urat|Ureum")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Kreatinin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("KB")) ~ "KB",
    str_detect(Nmtindakan,c("Ambulans")) ~ "Ambulans",
    str_detect(Nmtindakan,c("Skrining DM")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Papsmear")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("IVA")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Krio")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("PRB/Prolanis")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Protesa Gigi")) ~ "Protesa Gigi",
    TRUE ~ "Non Kapitasi lain-lain")) %>%
  subset(Klasifikasi == "Rawat Inap")

nonkap01 <- nonkap01 %>%
  dplyr::group_by(Nosjp) %>%
  dplyr::summarise(Nokapst=paste(Nokapst, collapse = ";"),
                   Jkpst=paste(Jkpst, collapse = ";"),
                   Umur=max(Umur),
                   Rangeumur=paste(Rangeumur, collapse = ";"),
                   Segmen=paste(Segmen, collapse = ";"),
                   Nmpks=paste(Nmpks, collapse = ";"),
                   Nmtkp=paste(Nmtkp, collapse = ";"),
                   Nmdati2Layan=paste(Nmdati2Layan, collapse = ";"),
                   Jenisppklayan=paste(Jenisppklayan, collapse = ";"),
                   Typeppklayan=paste(Typeppklayan, collapse = ";"),
                   Nmppklayan=paste(Nmppklayan, collapse = ";"),
                   Diagnosa=paste(Diagnosa, collapse = ";"),
                   Tglpelayanan=paste(Tglpelayanan, collapse = ";"),
                   Tglkunjungan=paste(Tglkunjungan, collapse = ";"),
                   Tglpulang=paste(Tglpulang, collapse = ";"),
                   Tgltindakan=paste(Tgltindakan, collapse = ";"),
                   Nmtindakan=paste(Nmtindakan, collapse = ";"),
                   Jnsnakes=paste(Jnsnakes, collapse = ";"),
                   Namanakes=paste(Namanakes, collapse = ";"),
                   Nmstatuspulang=paste(Nmstatuspulang, collapse = ";"),
                   status_fpk=paste(status_fpk, collapse = ";"),
                   lmstjklaim=min(lmstjklaim),
                   Tglbayar=paste(Tglbayar, collapse = ";"),
                   Klasifikasi=paste(Klasifikasi, collapse = ";"),
                   Biaya=sum(Biaya))

nonkap01$Nokapst <- sapply(strsplit(nonkap01$Nokapst, ";"),
                           function(x) paste(unique(x), collapse = ";"))
nonkap01$Jkpst <- sapply(strsplit(nonkap01$Jkpst, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap01$Rangeumur <- sapply(strsplit(nonkap01$Rangeumur, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap01$Segmen <- sapply(strsplit(nonkap01$Segmen, ";"),
                          function(x) paste(unique(x), collapse = ";"))
nonkap01$Nmpks <- sapply(strsplit(nonkap01$Nmpks, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap01$Nmtkp <- sapply(strsplit(nonkap01$Nmtkp, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap01$Nmdati2Layan <- sapply(strsplit(nonkap01$Nmdati2Layan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap01$Jenisppklayan <- sapply(strsplit(nonkap01$Jenisppklayan, ";"),
                                 function(x) paste(unique(x), collapse = ";"))
nonkap01$Typeppklayan <- sapply(strsplit(nonkap01$Typeppklayan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap01$Nmppklayan <- sapply(strsplit(nonkap01$Nmppklayan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap01$Diagnosa <- sapply(strsplit(nonkap01$Diagnosa, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap01$Tglpelayanan <- sapply(strsplit(nonkap01$Tglpelayanan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap01$Tglkunjungan <- sapply(strsplit(nonkap01$Tglkunjungan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap01$Tglpulang <- sapply(strsplit(nonkap01$Tglpulang, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap01$Tgltindakan <- sapply(strsplit(nonkap01$Tgltindakan, ";"),
                               function(x) paste(unique(x), collapse = ";"))
nonkap01$Nmtindakan <- sapply(strsplit(nonkap01$Nmtindakan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap01$Jnsnakes <- sapply(strsplit(nonkap01$Jnsnakes, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap01$Namanakes <- sapply(strsplit(nonkap01$Namanakes, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap01$Nmstatuspulang <- sapply(strsplit(nonkap01$Nmstatuspulang, ";"),
                                  function(x) paste(unique(x), collapse = ";"))
nonkap01$status_fpk <- sapply(strsplit(nonkap01$status_fpk, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap01$Tglbayar <- sapply(strsplit(nonkap01$Tglbayar, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap01$Klasifikasi <- sapply(strsplit(nonkap01$Klasifikasi, ";"),
                            function(x) paste(unique(x), collapse = ";"))

nonkap02 <- read_csv("Sheet 1_Full Data_nonkap 19_23.csv") %>%
  select (Nokapst,Jkpst,Umur,Rangeumur,Segmen,Nmpks,Nosjp,Nmtkp,
          Kdppkterdaftar,Nmppkterdaftar,Nmdati2Layan,Jenisppklayan,
          Typeppklayan,Kdppklayan,Nmppklayan,Kddiagnosa,Nmdiagnosa,
          Tglpelayanan,Tglkunjungan,Tglpulang,Tgltindakan,Nmtindakan,
          Jnsnakes,Namanakes,Nmstatuspulang,status_fpk,lmstjklaim,
          Tglbayar,Biaya) %>%
  subset(Kdppklayan == "0204B012") %>%
  subset(Kdppkterdaftar %in% c("13060601","0204B014","0204B016","0204U140")) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagnosa)," - ",
                           as.character(Nmdiagnosa))) %>%
  select(-Kddiagnosa,-Nmdiagnosa) %>%
  subset(Biaya != 0) %>%
  mutate(Klasifikasi = case_when(
    str_detect(Nmtindakan,c("Persalinan|persalinan|keguguran")) ~ "Persalinan",
    str_detect(Nmtindakan,c("pra-rujukan")) ~ "Pra Rujukan",
    str_detect(Nmtindakan,c("ANC")) ~ "ANC",
    str_detect(Nmtindakan,c("PNC")) ~ "PNC",
    str_detect(Nmtindakan,c("Rawat Inap")) ~ "Rawat Inap",
    str_detect(Nmtindakan,c("Kolesterol")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HbA1c")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Mikroalbumin|Microalbumin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("LDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Trigliserida")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Asam Urat|Ureum")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Kreatinin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("KB")) ~ "KB",
    str_detect(Nmtindakan,c("Ambulans")) ~ "Ambulans",
    str_detect(Nmtindakan,c("Skrining DM")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Papsmear")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("IVA")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Krio")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("PRB/Prolanis")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Protesa Gigi")) ~ "Protesa Gigi",
    TRUE ~ "Non Kapitasi lain-lain")) %>%
  subset(Klasifikasi == "Rawat Inap")

nonkap02 <- nonkap02 %>%
  dplyr::group_by(Nosjp) %>%
  dplyr::summarise(Nokapst=paste(Nokapst, collapse = ";"),
                   Jkpst=paste(Jkpst, collapse = ";"),
                   Umur=max(Umur),
                   Rangeumur=paste(Rangeumur, collapse = ";"),
                   Segmen=paste(Segmen, collapse = ";"),
                   Nmpks=paste(Nmpks, collapse = ";"),
                   Nmtkp=paste(Nmtkp, collapse = ";"),
                   Nmdati2Layan=paste(Nmdati2Layan, collapse = ";"),
                   Jenisppklayan=paste(Jenisppklayan, collapse = ";"),
                   Typeppklayan=paste(Typeppklayan, collapse = ";"),
                   Nmppklayan=paste(Nmppklayan, collapse = ";"),
                   Diagnosa=paste(Diagnosa, collapse = ";"),
                   Tglpelayanan=paste(Tglpelayanan, collapse = ";"),
                   Tglkunjungan=paste(Tglkunjungan, collapse = ";"),
                   Tglpulang=paste(Tglpulang, collapse = ";"),
                   Tgltindakan=paste(Tgltindakan, collapse = ";"),
                   Nmtindakan=paste(Nmtindakan, collapse = ";"),
                   Jnsnakes=paste(Jnsnakes, collapse = ";"),
                   Namanakes=paste(Namanakes, collapse = ";"),
                   Nmstatuspulang=paste(Nmstatuspulang, collapse = ";"),
                   status_fpk=paste(status_fpk, collapse = ";"),
                   lmstjklaim=min(lmstjklaim),
                   Tglbayar=paste(Tglbayar, collapse = ";"),
                   Klasifikasi=paste(Klasifikasi, collapse = ";"),
                   Biaya=sum(Biaya))

nonkap02$Nokapst <- sapply(strsplit(nonkap02$Nokapst, ";"),
                           function(x) paste(unique(x), collapse = ";"))
nonkap02$Jkpst <- sapply(strsplit(nonkap02$Jkpst, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap02$Rangeumur <- sapply(strsplit(nonkap02$Rangeumur, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap02$Segmen <- sapply(strsplit(nonkap02$Segmen, ";"),
                          function(x) paste(unique(x), collapse = ";"))
nonkap02$Nmpks <- sapply(strsplit(nonkap02$Nmpks, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap02$Nmtkp <- sapply(strsplit(nonkap02$Nmtkp, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap02$Nmdati2Layan <- sapply(strsplit(nonkap02$Nmdati2Layan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap02$Jenisppklayan <- sapply(strsplit(nonkap02$Jenisppklayan, ";"),
                                 function(x) paste(unique(x), collapse = ";"))
nonkap02$Typeppklayan <- sapply(strsplit(nonkap02$Typeppklayan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap02$Nmppklayan <- sapply(strsplit(nonkap02$Nmppklayan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap02$Diagnosa <- sapply(strsplit(nonkap02$Diagnosa, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap02$Tglpelayanan <- sapply(strsplit(nonkap02$Tglpelayanan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap02$Tglkunjungan <- sapply(strsplit(nonkap02$Tglkunjungan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap02$Tglpulang <- sapply(strsplit(nonkap02$Tglpulang, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap02$Tgltindakan <- sapply(strsplit(nonkap02$Tgltindakan, ";"),
                               function(x) paste(unique(x), collapse = ";"))
nonkap02$Nmtindakan <- sapply(strsplit(nonkap02$Nmtindakan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap02$Jnsnakes <- sapply(strsplit(nonkap02$Jnsnakes, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap02$Namanakes <- sapply(strsplit(nonkap02$Namanakes, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap02$Nmstatuspulang <- sapply(strsplit(nonkap02$Nmstatuspulang, ";"),
                                  function(x) paste(unique(x), collapse = ";"))
nonkap02$status_fpk <- sapply(strsplit(nonkap02$status_fpk, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap02$Tglbayar <- sapply(strsplit(nonkap02$Tglbayar, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap02$Klasifikasi <- sapply(strsplit(nonkap02$Klasifikasi, ";"),
                               function(x) paste(unique(x), collapse = ";"))

nonkap03 <- read_csv("Sheet 1_Full Data_nonkap jan_apr24.csv") %>%
  select (Nokapst,Jkpst,Umur,Rangeumur,Segmen,Nmpks,Nosjp,Nmtkp,
          Kdppkterdaftar,Nmppkterdaftar,Nmdati2Layan,Jenisppklayan,
          Typeppklayan,Kdppklayan,Nmppklayan,Kddiagnosa,Nmdiagnosa,
          Tglpelayanan,Tglkunjungan,Tglpulang,Tgltindakan,Nmtindakan,
          Jnsnakes,Namanakes,Nmstatuspulang,status_fpk,lmstjklaim,
          Tglbayar,Biaya) %>%
  subset(Kdppklayan == "0204B012") %>%
  subset(Kdppkterdaftar %in% c("13060601","0204B014","0204B016","0204U140")) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagnosa)," - ",
                           as.character(Nmdiagnosa))) %>%
  select(-Kddiagnosa,-Nmdiagnosa) %>%
  subset(Biaya != 0) %>%
  mutate(Klasifikasi = case_when(
    str_detect(Nmtindakan,c("Persalinan|persalinan|keguguran")) ~ "Persalinan",
    str_detect(Nmtindakan,c("pra-rujukan")) ~ "Pra Rujukan",
    str_detect(Nmtindakan,c("ANC")) ~ "ANC",
    str_detect(Nmtindakan,c("PNC")) ~ "PNC",
    str_detect(Nmtindakan,c("Rawat Inap")) ~ "Rawat Inap",
    str_detect(Nmtindakan,c("Kolesterol")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HbA1c")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Mikroalbumin|Microalbumin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("LDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Trigliserida")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Asam Urat|Ureum")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Kreatinin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("KB")) ~ "KB",
    str_detect(Nmtindakan,c("Ambulans")) ~ "Ambulans",
    str_detect(Nmtindakan,c("Skrining DM")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Papsmear")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("IVA")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Krio")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("PRB/Prolanis")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Protesa Gigi")) ~ "Protesa Gigi",
    TRUE ~ "Non Kapitasi lain-lain")) %>%
  subset(Klasifikasi == "Rawat Inap")

nonkap03 <- nonkap03 %>%
  dplyr::group_by(Nosjp) %>%
  dplyr::summarise(Nokapst=paste(Nokapst, collapse = ";"),
                   Jkpst=paste(Jkpst, collapse = ";"),
                   Umur=max(Umur),
                   Rangeumur=paste(Rangeumur, collapse = ";"),
                   Segmen=paste(Segmen, collapse = ";"),
                   Nmpks=paste(Nmpks, collapse = ";"),
                   Nmtkp=paste(Nmtkp, collapse = ";"),
                   Nmdati2Layan=paste(Nmdati2Layan, collapse = ";"),
                   Jenisppklayan=paste(Jenisppklayan, collapse = ";"),
                   Typeppklayan=paste(Typeppklayan, collapse = ";"),
                   Nmppklayan=paste(Nmppklayan, collapse = ";"),
                   Diagnosa=paste(Diagnosa, collapse = ";"),
                   Tglpelayanan=paste(Tglpelayanan, collapse = ";"),
                   Tglkunjungan=paste(Tglkunjungan, collapse = ";"),
                   Tglpulang=paste(Tglpulang, collapse = ";"),
                   Tgltindakan=paste(Tgltindakan, collapse = ";"),
                   Nmtindakan=paste(Nmtindakan, collapse = ";"),
                   Jnsnakes=paste(Jnsnakes, collapse = ";"),
                   Namanakes=paste(Namanakes, collapse = ";"),
                   Nmstatuspulang=paste(Nmstatuspulang, collapse = ";"),
                   status_fpk=paste(status_fpk, collapse = ";"),
                   lmstjklaim=min(lmstjklaim),
                   Tglbayar=paste(Tglbayar, collapse = ";"),
                   Klasifikasi=paste(Klasifikasi, collapse = ";"),
                   Biaya=sum(Biaya))

nonkap03$Nokapst <- sapply(strsplit(nonkap03$Nokapst, ";"),
                           function(x) paste(unique(x), collapse = ";"))
nonkap03$Jkpst <- sapply(strsplit(nonkap03$Jkpst, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap03$Rangeumur <- sapply(strsplit(nonkap03$Rangeumur, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap03$Segmen <- sapply(strsplit(nonkap03$Segmen, ";"),
                          function(x) paste(unique(x), collapse = ";"))
nonkap03$Nmpks <- sapply(strsplit(nonkap03$Nmpks, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap03$Nmtkp <- sapply(strsplit(nonkap03$Nmtkp, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap03$Nmdati2Layan <- sapply(strsplit(nonkap03$Nmdati2Layan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap03$Jenisppklayan <- sapply(strsplit(nonkap03$Jenisppklayan, ";"),
                                 function(x) paste(unique(x), collapse = ";"))
nonkap03$Typeppklayan <- sapply(strsplit(nonkap03$Typeppklayan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap03$Nmppklayan <- sapply(strsplit(nonkap03$Nmppklayan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap03$Diagnosa <- sapply(strsplit(nonkap03$Diagnosa, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap03$Tglpelayanan <- sapply(strsplit(nonkap03$Tglpelayanan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap03$Tglkunjungan <- sapply(strsplit(nonkap03$Tglkunjungan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap03$Tglpulang <- sapply(strsplit(nonkap03$Tglpulang, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap03$Tgltindakan <- sapply(strsplit(nonkap03$Tgltindakan, ";"),
                               function(x) paste(unique(x), collapse = ";"))
nonkap03$Nmtindakan <- sapply(strsplit(nonkap03$Nmtindakan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap03$Jnsnakes <- sapply(strsplit(nonkap03$Jnsnakes, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap03$Namanakes <- sapply(strsplit(nonkap03$Namanakes, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap03$Nmstatuspulang <- sapply(strsplit(nonkap03$Nmstatuspulang, ";"),
                                  function(x) paste(unique(x), collapse = ";"))
nonkap03$status_fpk <- sapply(strsplit(nonkap03$status_fpk, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap03$Tglbayar <- sapply(strsplit(nonkap03$Tglbayar, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap03$Klasifikasi <- sapply(strsplit(nonkap03$Klasifikasi, ";"),
                               function(x) paste(unique(x), collapse = ";"))

nonkap04 <- read_csv("Sheet 1_Full Data_nonkap mei_jul24.csv") %>%
  select (Nokapst,Jkpst,Umur,Rangeumur,Segmen,Nmpks,Nosjp,Nmtkp,
          Kdppkterdaftar,Nmppkterdaftar,Nmdati2Layan,Jenisppklayan,
          Typeppklayan,Kdppklayan,Nmppklayan,Kddiagnosa,Nmdiagnosa,
          Tglpelayanan,Tglkunjungan,Tglpulang,Tgltindakan,Nmtindakan,
          Jnsnakes,Namanakes,Nmstatuspulang,status_fpk,lmstjklaim,
          Tglbayar,Biaya) %>%
  subset(Kdppklayan == "0204B012") %>%
  subset(Kdppkterdaftar %in% c("13060601","0204B014","0204B016","0204U140")) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagnosa)," - ",
                           as.character(Nmdiagnosa))) %>%
  select(-Kddiagnosa,-Nmdiagnosa) %>%
  subset(Biaya != 0) %>%
  mutate(Klasifikasi = case_when(
    str_detect(Nmtindakan,c("Persalinan|persalinan|keguguran")) ~ "Persalinan",
    str_detect(Nmtindakan,c("pra-rujukan")) ~ "Pra Rujukan",
    str_detect(Nmtindakan,c("ANC")) ~ "ANC",
    str_detect(Nmtindakan,c("PNC")) ~ "PNC",
    str_detect(Nmtindakan,c("Rawat Inap")) ~ "Rawat Inap",
    str_detect(Nmtindakan,c("Kolesterol")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HbA1c")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Mikroalbumin|Microalbumin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("LDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Trigliserida")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Asam Urat|Ureum")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Kreatinin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("KB")) ~ "KB",
    str_detect(Nmtindakan,c("Ambulans")) ~ "Ambulans",
    str_detect(Nmtindakan,c("Skrining DM")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Papsmear")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("IVA")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Krio")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("PRB/Prolanis")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Protesa Gigi")) ~ "Protesa Gigi",
    TRUE ~ "Non Kapitasi lain-lain")) %>%
  subset(Klasifikasi == "Rawat Inap")

nonkap04 <- nonkap04 %>%
  dplyr::group_by(Nosjp) %>%
  dplyr::summarise(Nokapst=paste(Nokapst, collapse = ";"),
                   Jkpst=paste(Jkpst, collapse = ";"),
                   Umur=max(Umur),
                   Rangeumur=paste(Rangeumur, collapse = ";"),
                   Segmen=paste(Segmen, collapse = ";"),
                   Nmpks=paste(Nmpks, collapse = ";"),
                   Nmtkp=paste(Nmtkp, collapse = ";"),
                   Nmdati2Layan=paste(Nmdati2Layan, collapse = ";"),
                   Jenisppklayan=paste(Jenisppklayan, collapse = ";"),
                   Typeppklayan=paste(Typeppklayan, collapse = ";"),
                   Nmppklayan=paste(Nmppklayan, collapse = ";"),
                   Diagnosa=paste(Diagnosa, collapse = ";"),
                   Tglpelayanan=paste(Tglpelayanan, collapse = ";"),
                   Tglkunjungan=paste(Tglkunjungan, collapse = ";"),
                   Tglpulang=paste(Tglpulang, collapse = ";"),
                   Tgltindakan=paste(Tgltindakan, collapse = ";"),
                   Nmtindakan=paste(Nmtindakan, collapse = ";"),
                   Jnsnakes=paste(Jnsnakes, collapse = ";"),
                   Namanakes=paste(Namanakes, collapse = ";"),
                   Nmstatuspulang=paste(Nmstatuspulang, collapse = ";"),
                   status_fpk=paste(status_fpk, collapse = ";"),
                   lmstjklaim=min(lmstjklaim),
                   Tglbayar=paste(Tglbayar, collapse = ";"),
                   Klasifikasi=paste(Klasifikasi, collapse = ";"),
                   Biaya=sum(Biaya))

nonkap04$Nokapst <- sapply(strsplit(nonkap04$Nokapst, ";"),
                           function(x) paste(unique(x), collapse = ";"))
nonkap04$Jkpst <- sapply(strsplit(nonkap04$Jkpst, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap04$Rangeumur <- sapply(strsplit(nonkap04$Rangeumur, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap04$Segmen <- sapply(strsplit(nonkap04$Segmen, ";"),
                          function(x) paste(unique(x), collapse = ";"))
nonkap04$Nmpks <- sapply(strsplit(nonkap04$Nmpks, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap04$Nmtkp <- sapply(strsplit(nonkap04$Nmtkp, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap04$Nmdati2Layan <- sapply(strsplit(nonkap04$Nmdati2Layan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap04$Jenisppklayan <- sapply(strsplit(nonkap04$Jenisppklayan, ";"),
                                 function(x) paste(unique(x), collapse = ";"))
nonkap04$Typeppklayan <- sapply(strsplit(nonkap04$Typeppklayan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap04$Nmppklayan <- sapply(strsplit(nonkap04$Nmppklayan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap04$Diagnosa <- sapply(strsplit(nonkap04$Diagnosa, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap04$Tglpelayanan <- sapply(strsplit(nonkap04$Tglpelayanan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap04$Tglkunjungan <- sapply(strsplit(nonkap04$Tglkunjungan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap04$Tglpulang <- sapply(strsplit(nonkap04$Tglpulang, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap04$Tgltindakan <- sapply(strsplit(nonkap04$Tgltindakan, ";"),
                               function(x) paste(unique(x), collapse = ";"))
nonkap04$Nmtindakan <- sapply(strsplit(nonkap04$Nmtindakan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap04$Jnsnakes <- sapply(strsplit(nonkap04$Jnsnakes, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap04$Namanakes <- sapply(strsplit(nonkap04$Namanakes, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap04$Nmstatuspulang <- sapply(strsplit(nonkap04$Nmstatuspulang, ";"),
                                  function(x) paste(unique(x), collapse = ";"))
nonkap04$status_fpk <- sapply(strsplit(nonkap04$status_fpk, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap04$Tglbayar <- sapply(strsplit(nonkap04$Tglbayar, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap04$Klasifikasi <- sapply(strsplit(nonkap04$Klasifikasi, ";"),
                               function(x) paste(unique(x), collapse = ";"))

nonkap05 <- read_csv("Sheet 1_Full Data_nonkap agu24.csv") %>%
  select (Nokapst,Jkpst,Umur,Rangeumur,Segmen,Nmpks,Nosjp,Nmtkp,
          Kdppkterdaftar,Nmppkterdaftar,Nmdati2Layan,Jenisppklayan,
          Typeppklayan,Kdppklayan,Nmppklayan,Kddiagnosa,Nmdiagnosa,
          Tglpelayanan,Tglkunjungan,Tglpulang,Tgltindakan,Nmtindakan,
          Jnsnakes,Namanakes,Nmstatuspulang,status_fpk,lmstjklaim,
          Tglbayar,Biaya) %>%
  subset(Kdppklayan == "0204B012") %>%
  subset(Kdppkterdaftar %in% c("13060601","0204B014","0204B016","0204U140")) %>%
  mutate(Diagnosa = paste0(as.character(Kddiagnosa)," - ",
                           as.character(Nmdiagnosa))) %>%
  select(-Kddiagnosa,-Nmdiagnosa) %>%
  subset(Biaya != 0) %>%
  mutate(Klasifikasi = case_when(
    str_detect(Nmtindakan,c("Persalinan|persalinan|keguguran")) ~ "Persalinan",
    str_detect(Nmtindakan,c("pra-rujukan")) ~ "Pra Rujukan",
    str_detect(Nmtindakan,c("ANC")) ~ "ANC",
    str_detect(Nmtindakan,c("PNC")) ~ "PNC",
    str_detect(Nmtindakan,c("Rawat Inap")) ~ "Rawat Inap",
    str_detect(Nmtindakan,c("Kolesterol")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HbA1c")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Mikroalbumin|Microalbumin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("LDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("HDL")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Trigliserida")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Asam Urat|Ureum")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Kreatinin")) ~ "Prolanis",
    str_detect(Nmtindakan,c("KB")) ~ "KB",
    str_detect(Nmtindakan,c("Ambulans")) ~ "Ambulans",
    str_detect(Nmtindakan,c("Skrining DM")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Papsmear")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("IVA")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("Krio")) ~ "Skrining Sekunder",
    str_detect(Nmtindakan,c("PRB/Prolanis")) ~ "Prolanis",
    str_detect(Nmtindakan,c("Protesa Gigi")) ~ "Protesa Gigi",
    TRUE ~ "Non Kapitasi lain-lain")) %>%
  subset(Klasifikasi == "Rawat Inap")

nonkap05 <- nonkap05 %>%
  dplyr::group_by(Nosjp) %>%
  dplyr::summarise(Nokapst=paste(Nokapst, collapse = ";"),
                   Jkpst=paste(Jkpst, collapse = ";"),
                   Umur=max(Umur),
                   Rangeumur=paste(Rangeumur, collapse = ";"),
                   Segmen=paste(Segmen, collapse = ";"),
                   Nmpks=paste(Nmpks, collapse = ";"),
                   Nmtkp=paste(Nmtkp, collapse = ";"),
                   Nmdati2Layan=paste(Nmdati2Layan, collapse = ";"),
                   Jenisppklayan=paste(Jenisppklayan, collapse = ";"),
                   Typeppklayan=paste(Typeppklayan, collapse = ";"),
                   Nmppklayan=paste(Nmppklayan, collapse = ";"),
                   Diagnosa=paste(Diagnosa, collapse = ";"),
                   Tglpelayanan=paste(Tglpelayanan, collapse = ";"),
                   Tglkunjungan=paste(Tglkunjungan, collapse = ";"),
                   Tglpulang=paste(Tglpulang, collapse = ";"),
                   Tgltindakan=paste(Tgltindakan, collapse = ";"),
                   Nmtindakan=paste(Nmtindakan, collapse = ";"),
                   Jnsnakes=paste(Jnsnakes, collapse = ";"),
                   Namanakes=paste(Namanakes, collapse = ";"),
                   Nmstatuspulang=paste(Nmstatuspulang, collapse = ";"),
                   status_fpk=paste(status_fpk, collapse = ";"),
                   lmstjklaim=min(lmstjklaim),
                   Tglbayar=paste(Tglbayar, collapse = ";"),
                   Klasifikasi=paste(Klasifikasi, collapse = ";"),
                   Biaya=sum(Biaya))

nonkap05$Nokapst <- sapply(strsplit(nonkap05$Nokapst, ";"),
                           function(x) paste(unique(x), collapse = ";"))
nonkap05$Jkpst <- sapply(strsplit(nonkap05$Jkpst, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap05$Rangeumur <- sapply(strsplit(nonkap05$Rangeumur, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap05$Segmen <- sapply(strsplit(nonkap05$Segmen, ";"),
                          function(x) paste(unique(x), collapse = ";"))
nonkap05$Nmpks <- sapply(strsplit(nonkap05$Nmpks, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap05$Nmtkp <- sapply(strsplit(nonkap05$Nmtkp, ";"),
                         function(x) paste(unique(x), collapse = ";"))
nonkap05$Nmdati2Layan <- sapply(strsplit(nonkap05$Nmdati2Layan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap05$Jenisppklayan <- sapply(strsplit(nonkap05$Jenisppklayan, ";"),
                                 function(x) paste(unique(x), collapse = ";"))
nonkap05$Typeppklayan <- sapply(strsplit(nonkap05$Typeppklayan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap05$Nmppklayan <- sapply(strsplit(nonkap05$Nmppklayan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap05$Diagnosa <- sapply(strsplit(nonkap05$Diagnosa, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap05$Tglpelayanan <- sapply(strsplit(nonkap05$Tglpelayanan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap05$Tglkunjungan <- sapply(strsplit(nonkap05$Tglkunjungan, ";"),
                                function(x) paste(unique(x), collapse = ";"))
nonkap05$Tglpulang <- sapply(strsplit(nonkap05$Tglpulang, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap05$Tgltindakan <- sapply(strsplit(nonkap05$Tgltindakan, ";"),
                               function(x) paste(unique(x), collapse = ";"))
nonkap05$Nmtindakan <- sapply(strsplit(nonkap05$Nmtindakan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap05$Jnsnakes <- sapply(strsplit(nonkap05$Jnsnakes, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap05$Namanakes <- sapply(strsplit(nonkap05$Namanakes, ";"),
                             function(x) paste(unique(x), collapse = ";"))
nonkap05$Nmstatuspulang <- sapply(strsplit(nonkap05$Nmstatuspulang, ";"),
                                  function(x) paste(unique(x), collapse = ";"))
nonkap05$status_fpk <- sapply(strsplit(nonkap05$status_fpk, ";"),
                              function(x) paste(unique(x), collapse = ";"))
nonkap05$Tglbayar <- sapply(strsplit(nonkap05$Tglbayar, ";"),
                            function(x) paste(unique(x), collapse = ";"))
nonkap05$Klasifikasi <- sapply(strsplit(nonkap05$Klasifikasi, ";"),
                               function(x) paste(unique(x), collapse = ";"))    

nonkap <- rbind(nonkap01,nonkap02,nonkap03,nonkap04,nonkap05)
rm(nonkap01,nonkap02,nonkap03,nonkap04,nonkap05)

nonkap$Tglpelayanan <- as.Date(nonkap$Tglpelayanan, format = "%m/%d/%Y")
nonkap$Tglkunjungan <- as.Date(nonkap$Tglkunjungan, format = "%m/%d/%Y")
nonkap$Tglpulang <- as.Date(nonkap$Tglpulang, format = "%m/%d/%Y")
nonkap$Tgltindakan <- as.Date(nonkap$Tgltindakan, format = "%m/%d/%Y")
nonkap$Tglbayar <- as.Date(nonkap$Tglbayar, format = "%m/%d/%Y")
nonkap$LOS <- as.integer(nonkap$Tglpulang) - as.integer(nonkap$Tglkunjungan)

nonkap <- nonkap[order(nonkap$Nokapst, nonkap$Tglpulang),]
nonkap <- nonkap %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglpulang))
nonkap$interval <- as.integer(nonkap$Tglpulang) - as.integer(nonkap$tgl_before)

#==============================================================================
nonkap23 <- nonkap %>% subset(Tglkunjungan >= "2020-09-28")

ritp_i634 <- nonkap23 %>%
  subset(Klasifikasi == "Rawat Inap") %>%
  filter(str_detect(Diagnosa,c("I63|I64")))
ritp_i634 <- ritp_i634[order(ritp_i634$Nokapst,ritp_i634$Tgltindakan),]
write.xlsx(ritp_i634, file = "ritp_i634.xlsx")
#=============================================
pnc_1 <- nonkap23 %>%
  subset(Klasifikasi == "PNC") %>%
  filter(str_detect(Nmtindakan,"1"))
pnc_1 <- pnc_1[order(pnc_1$Nokapst,pnc_1$Tgltindakan),]
pnc_1 <- pnc_1 %>%
  group_by(Nokapst) %>%
  mutate(selisih = Tgltindakan - lag(Tgltindakan))
pnc_1$keterangan <- with(pnc_1,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
pnc_1 <- pnc_1 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
pnc_1 <- pnc_1[order(pnc_1$Nokapst,pnc_1$Tgltindakan),]

anc_2 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"2"))
anc_2 <- anc_2[order(anc_2$Nokapst,anc_2$Tgltindakan),]
anc_2$keterangan <- with(anc_2,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_2 <- anc_2 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_2 <- anc_2[order(anc_2$Nokapst,anc_2$Tgltindakan),]

anc_3 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"3"))
anc_3 <- anc_3[order(anc_3$Nokapst,anc_3$Tgltindakan),]
anc_3$keterangan <- with(anc_3,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_3 <- anc_3 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_3 <- anc_3[order(anc_3$Nokapst,anc_3$Tgltindakan),]

anc_4 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"4"))
anc_4 <- anc_4[order(anc_4$Nokapst,anc_4$Tgltindakan),]
anc_4$keterangan <- with(anc_4,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_4 <- anc_4 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_4 <- anc_4[order(anc_4$Nokapst,anc_4$Tgltindakan),]

anc_5 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"5"))
anc_5 <- anc_5[order(anc_5$Nokapst,anc_5$Tgltindakan),]
anc_5$keterangan <- with(anc_5,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_5 <- anc_5 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_5 <- anc_5[order(anc_5$Nokapst,anc_5$Tgltindakan),]

anc_6 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"6"))
anc_6 <- anc_6[order(anc_6$Nokapst,anc_6$Tgltindakan),]
anc_6$keterangan <- with(anc_6,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_6 <- anc_6 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_6 <- anc_6[order(anc_6$Nokapst,anc_6$Tgltindakan),]

#========================================================================
write.xlsx(anc_1, file = "anc_1.xlsx")
write.xlsx(anc_2, file = "anc_2.xlsx")
write.xlsx(anc_3, file = "anc_3.xlsx")
write.xlsx(anc_4, file = "anc_4.xlsx")
write.xlsx(anc_5, file = "anc_5.xlsx")
write.xlsx(anc_6, file = "anc_6.xlsx")
#============================================
anc_1 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"1"))
anc_1 <- anc_1[order(anc_1$Nokapst,anc_1$Tgltindakan),]
anc_1$keterangan <- with(anc_1,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_1 <- anc_1 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_1 <- anc_1[order(anc_1$Nokapst,anc_1$Tgltindakan),]

anc_2 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"2"))
anc_2 <- anc_2[order(anc_2$Nokapst,anc_2$Tgltindakan),]
anc_2$keterangan <- with(anc_2,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_2 <- anc_2 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_2 <- anc_2[order(anc_2$Nokapst,anc_2$Tgltindakan),]

anc_3 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"3"))
anc_3 <- anc_3[order(anc_3$Nokapst,anc_3$Tgltindakan),]
anc_3$keterangan <- with(anc_3,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_3 <- anc_3 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_3 <- anc_3[order(anc_3$Nokapst,anc_3$Tgltindakan),]

anc_4 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"4"))
anc_4 <- anc_4[order(anc_4$Nokapst,anc_4$Tgltindakan),]
anc_4$keterangan <- with(anc_4,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_4 <- anc_4 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_4 <- anc_4[order(anc_4$Nokapst,anc_4$Tgltindakan),]

anc_5 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"5"))
anc_5 <- anc_5[order(anc_5$Nokapst,anc_5$Tgltindakan),]
anc_5$keterangan <- with(anc_5,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_5 <- anc_5 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_5 <- anc_5[order(anc_5$Nokapst,anc_5$Tgltindakan),]

anc_6 <- nonkap23 %>%
  subset(Klasifikasi == "ANC") %>%
  filter(str_detect(Nmtindakan,"6"))
anc_6 <- anc_6[order(anc_6$Nokapst,anc_6$Tgltindakan),]
anc_6$keterangan <- with(anc_6,
                         ifelse(Nokapst == lag(Nokapst)|
                                  Nokapst == lead(Nokapst),"double",
                                "Single"))
anc_6 <- anc_6 %>% subset(keterangan == "double") %>%
  select(-Segmen,-Nmpks,-Jenisppklayan,-Typeppklayan,-Tglpulang,-Jnsnakes,
         -Namanakes,-Nmstatuspulang,-Rangeumur,-Klasifikasi,-LOS,
         -tgl_before,-interval,-keterangan)
anc_6 <- anc_6[order(anc_6$Nokapst,anc_6$Tgltindakan),]

#========================================================================
write.xlsx(anc_1, file = "anc_1.xlsx")
write.xlsx(anc_2, file = "anc_2.xlsx")
write.xlsx(anc_3, file = "anc_3.xlsx")
write.xlsx(anc_4, file = "anc_4.xlsx")
write.xlsx(anc_5, file = "anc_5.xlsx")
write.xlsx(anc_6, file = "anc_6.xlsx")
#========================================================================


#==============================================================================
write.csv(nonkap, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_nonkap.csv",
          na="", row.names = FALSE)
#==============================================================================
nonkap0 <- nonkap[!(duplicated(nonkap$Nosjp) | 
                      duplicated(nonkap$Nosjp, fromLast = TRUE)), ]
rm(nonkap0)