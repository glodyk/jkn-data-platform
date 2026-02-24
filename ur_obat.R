setwd("D:/data gresik/FactObat_HV/obat")

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
obat <- read_csv("Sheet_1_Full_Data_data obat 24.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,katastropik,
          nmdati2layan,nmdati2resep,kdppklayan,nmjnsppkresep,nmppklayan,
          refasalsjp,nosjp_apotek,kdppkresep,nmppkresep,flag_iterasi,nmtkp,
          politujsjp,tglpelayanan,tglrsp,tglsjp,tglstjkeu,jenisobat,kdobat,
          obat,generik,noresep,jmlobt,sediaan,jenis_obat_luarpaket,
          obatluarpaket,kdinacbgs,kd_diagprimer,nm_diagprimer,diagsekunder,
          nmdokter,biaya) %>%
  select(nmdati2layan,nmtkp,nmppklayan,biaya,tglstjkeu) %>%
  filter(str_detect(nmtkp,c("RJTL|RITL")))
obat$tglstjkeu <- as.Date(obat$tglstjkeu, format = "%m/%d/%Y")
obat <- obat %>% subset(tglstjkeu >= "2024-01-01")
rekap <- obat %>%
  group_by(nmdati2layan,nmppklayan) %>%
  summarise(biaya)
write.xlsx(rekap, file = "rekapobat2.xlsx")
#=============================data nadyah=============================
obat1417 <- read_csv("Sheet_1_Full_Data_data obat null_14_17.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)


%>%
  subset(nmtkp %in% c("RJTL","RITL"))

obat1820 <- read_csv("Sheet_1_Full_Data_data obat 18_19.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL"))

obat21 <- read_csv("Sheet_1_Full_Data_data obat 21.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL"))

obat2223 <- read_csv("Sheet_1_Full_Data_data obat 22_23.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL"))

obat24 <- read_csv("Sheet 1_Full Data_obat 24.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL"))

obatjul25 <- read_csv("Sheet 1_Full Data_obat 25.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL"))


%>%
  subset(pstprb != "TRUE") %>%
  subset(kdinacbgs == "Q-5-44-0") %>%
  subset(jenisobat == "Non Kemoterapi") %>%
  subset(katastropik != "NON KATASTROFIK") %>%
  subset(jenis_obat_luarpaket == "Kronis")%>%
  filter(str_detect(politujsjp, c("SAR|JAN|INT|IPD|PAR"))) %>%
  select(-pstprb,-pstprolanis,-katastropik,-nmdati2layan,-nmjnsppkresep,
         -nmppklayan,-nmtkp,-jenisobat,-jenis_obat_luarpaket,-obatluarpaket)

obat1417 <- read_csv("Sheet_1_Full_Data_data obat null_14_17.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya) %>%
  subset(nmtkp %in% c("RJTL","RITL")) %>%
  subset(flag_iterasi == "1") %>%
  subset(nmtkp == "RJTL") %>%
  subset(pstprb != "TRUE") %>%
  subset(kdinacbgs == "Q-5-44-0") %>%
  subset(jenisobat == "Non Kemoterapi") %>%
  subset(katastropik != "NON KATASTROFIK") %>%
  subset(jenis_obat_luarpaket == "Kronis")%>%
  filter(str_detect(politujsjp, c("SAR|JAN|INT|IPD|PAR"))) %>%
  select(-pstprb,-pstprolanis,-katastropik,-nmdati2layan,-kdppklayan,
         -nmjnsppkresep,-nmppklayan,-nmtkp,-jenisobat,-jenis_obat_luarpaket,
         -obatluarpaket)

obat1820 <- read_csv("Sheet_1_Full_Data_data obat 18_19.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL")) %>%
  subset(flag_iterasi == "1") %>%
  subset(nmtkp == "RJTL") %>%
  subset(pstprb != "TRUE") %>%
  subset(kdinacbgs == "Q-5-44-0") %>%
  subset(jenisobat == "Non Kemoterapi") %>%
  subset(katastropik != "NON KATASTROFIK") %>%
  subset(jenis_obat_luarpaket == "Kronis")%>%
  filter(str_detect(politujsjp, c("SAR|JAN|INT|IPD|PAR"))) %>%
  select(-pstprb,-pstprolanis,-katastropik,-nmdati2layan,-kdppklayan,
         -nmjnsppkresep,-nmppklayan,-nmtkp,-jenisobat,-jenis_obat_luarpaket,
         -obatluarpaket)

obat21 <- read_csv("Sheet_1_Full_Data_data obat 21.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL")) %>%
  subset(flag_iterasi == "1") %>%
  subset(nmtkp == "RJTL") %>%
  subset(pstprb != "TRUE") %>%
  subset(kdinacbgs == "Q-5-44-0") %>%
  subset(jenisobat == "Non Kemoterapi") %>%
  subset(katastropik != "NON KATASTROFIK") %>%
  subset(jenis_obat_luarpaket == "Kronis")%>%
  filter(str_detect(politujsjp, c("SAR|JAN|INT|IPD|PAR"))) %>%
  select(-pstprb,-pstprolanis,-katastropik,-nmdati2layan,-kdppklayan,
         -nmjnsppkresep,-nmppklayan,-nmtkp,-jenisobat,-jenis_obat_luarpaket,
         -obatluarpaket)

obat2223 <- read_csv("Sheet_1_Full_Data_data obat 22_23.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL")) %>%
  subset(flag_iterasi == "1") %>%
  subset(nmtkp == "RJTL") %>%
  subset(pstprb != "TRUE") %>%
  subset(kdinacbgs == "Q-5-44-0") %>%
  subset(jenisobat == "Non Kemoterapi") %>%
  subset(katastropik != "NON KATASTROFIK") %>%
  subset(jenis_obat_luarpaket == "Kronis")%>%
  filter(str_detect(politujsjp, c("SAR|JAN|INT|IPD|PAR"))) %>%
  select(-pstprb,-pstprolanis,-katastropik,-nmdati2layan,-kdppklayan,
         -nmjnsppkresep,-nmppklayan,-nmtkp,-jenisobat,-jenis_obat_luarpaket,
         -obatluarpaket)

obat24 <- read_csv("Sheet 1_Full Data_obat 24.csv") %>%
  select (nokapst,nmpst,jkpst,umur,pstprb,pstprolanis,nmdati2layan,
          nmdati2resep,nmjnsppkresep,nmppklayan,refasalsjp,nosjp_apotek,
          kdppkresep,nmppkresep,flag_iterasi,nmtkp,politujsjp,tglpelayanan,
          tglrsp,tglsjp,tglstjkeu,jenisobat,katastropik,obat,generik,
          noresep,jmlobt,sediaan,jenis_obat_luarpaket,obatluarpaket,kdinacbgs,
          kd_diagprimer,nm_diagprimer,diagsekunder,nmdokter,biaya)

%>%
  subset(nmtkp %in% c("RJTL","RITL")) %>%
  subset(flag_iterasi == "1") %>%
  subset(nmtkp == "RJTL") %>%
  subset(pstprb != "TRUE") %>%
  subset(kdinacbgs == "Q-5-44-0") %>%
  subset(jenisobat == "Non Kemoterapi") %>%
  subset(katastropik != "NON KATASTROFIK") %>%
  subset(jenis_obat_luarpaket == "Kronis")%>%
  filter(str_detect(politujsjp, c("SAR|JAN|INT|IPD|PAR"))) %>%
  select(-pstprb,-pstprolanis,-katastropik,-nmdati2layan,-kdppklayan,
         -nmjnsppkresep,-nmppklayan,-nmtkp,-jenisobat,-jenis_obat_luarpaket,
         -obatluarpaket)

data <- rbind(obat1417,obat1820,obat21,obat2223,obat24,obatjul25)
rm(obat1417,obat1820,obat21,obat2223,obat24,obatjul25)

data$tglpelayanan <- as.Date(data$tglpelayanan, format = "%m/%d/%Y")
data$tglrsp <- as.Date(data$tglrsp, format = "%m/%d/%Y")
data$tglsjp <- as.Date(data$tglsjp, format = "%m/%d/%Y")
data$tglstjkeu <- as.Date(data$tglstjkeu, format = "%m/%d/%Y")

data <- data[order(data$nmppkresep, data$tglsjp),]
data <- data %>% subset(tglsjp >= "2020-01-01")
data <- data %>% 
  mutate(
    Nmppkresep = case_when(
      kdppkresep == "0204R003" ~ "RSUD Ngimbang",
      kdppkresep == "0204R004" ~ "RS Suyudi Paciran (JST)",
      kdppkresep == "0204R009" ~ "RSI Nashrul Ummah",
      kdppkresep == "0204R010" ~ "RS Muhammadiyah Lamongan",
      kdppkresep == "0204R012" ~ "RS Muhammadiyah Babat",
      kdppkresep == "0204R019" ~ "RSU Muhammadiyah Babat",
      kdppkresep == "0204R013" ~ "RS Fatimah",
      kdppkresep == "0204R014" ~ "RS Intan Medika",
      kdppkresep == "0204R015" ~ "RS Arsy Paciran",
      kdppkresep == "0204R017" ~ "RS Citra Medika",
      kdppkresep == "0204R018" ~ "RS Bedah Mitra Sehat",
      kdppkresep == "0204S001" ~ "Klinik Mata Utama Lamongan",
      kdppkresep == "0205R009" ~ "RS Denisa",
      kdppkresep == "0205R011" ~ "RS Muhammadiyah Gresik",
      kdppkresep == "0205R012" ~ "RS Semen Gresik",
      kdppkresep == "0205R013" ~ "RS PKG Grha Husada",
      kdppkresep == "0205R014" ~ "RS Petrokimia Driyorejo",
      kdppkresep == "0205R019" ~ "RS Wali Songo I",
      kdppkresep == "0205R021" ~ "RS Fathma Medika",
      kdppkresep == "0205R022" ~ "RS Wates Husada",
      kdppkresep == "0205R023" ~ "RS Surya Medika",
      kdppkresep == "0205R024" ~ "RS PKU Muhammadiyah Sekapuk",
      kdppkresep == "0205R025" ~ "RSI Mabarrot MWC NU Bungah",
      kdppkresep == "0205R026" ~ "RS Rachmi Dewi",
      kdppkresep == "0205R027" ~ "RSI Nyai Ageng Pinatih",
      kdppkresep == "0205R028" ~ "RSI Cahaya Giri",
      kdppkresep == "0205R029" ~ "RSUD Umar Mas'ud Bawean",
      kdppkresep == "0205S100" ~ "Klinik Mata Utama",
      kdppkresep == "1302R001" ~ "RSUD Ibnu Sina",
      kdppkresep == "1302R002" ~ "RS Petrokimia Gresik",
      kdppkresep == "1306R001" ~ "RSUD Dr Soegiri",
      kdppkresep == "0205R031" ~ "RS Eka Husada",
      kdppkresep == "0205R030" ~ "RS Randegansari Husada",
      kdppkresep == "0204R020" ~ "RS Permata Bunda",
      kdppkresep == "0204R021" ~ "RSUD Karangkembang",
      kdppkresep == "0204R022" ~ "RS Nahdlatul Ulama Babat",
      kdppkresep == "0204R023" ~ "RS Permata Hati",
      kdppkresep == "0204R024" ~ "RS Muhammadiyah Kalikapas",
      kdppkresep == "0205R032" ~ "RSIA Khodijah",
      kdppkresep == "0205S101" ~ "Klinik Utama Cerme",
      kdppkresep == "0205R033" ~ "RSUD Gresik Sehati",
      TRUE ~ nmppkresep)) %>%
  select(-kdppkresep,nmppkresep)

data0 <- data %>%
  subset(jmlobt > 0)
rm(data)

data0 <- data0 %>%
  mutate(absensi = as.integer(difftime(tglstjkeu,tglrsp,units = 'days')))
data0$nmobat <- paste0(as.character(data0$jmlobt)," ",
                       as.character(data0$generik)," ",
                       as.character(data0$sediaan))

data0 <- data0 %>%
  dplyr::group_by(nosjp_apotek,noresep) %>%
  dplyr::summarise(nokapst=paste(nokapst, collapse = ";"),
                   nmpst=paste(nmpst, collapse = ";"),
                   jkpst=paste(jkpst, collapse = ";"),
                   umur=max(umur, collapse = ";"),
                   pstprb=paste(pstprb, collapse = ";"),
                   pstprolanis=paste(pstprolanis, collapse = ";"),
                   nmdati2layan=paste(nmdati2layan, collapse = ";"),
                   nmdati2resep=paste(nmdati2resep, collapse = ";"),
                   nmjnsppkresep=paste(nmjnsppkresep, collapse = ";"),
                   nmppklayan=paste(nmppklayan, collapse = ";"),
                   Nmppkresep=paste(Nmppkresep, collapse = ";"),
                   refasalsjp=paste(refasalsjp, collapse = ";"),
                   flag_iterasi=paste(flag_iterasi, collapse = ";"),
                   nmtkp=paste(nmtkp, collapse = ";"),
                   absensi=paste(absensi, collapse = ";"),
                   politujsjp=paste(politujsjp, collapse = ";"),
                   tglpelayanan=paste(tglpelayanan, collapse = ";"),
                   tglrsp=paste(tglrsp, collapse = ";"),
                   tglsjp=paste(tglsjp, collapse = ";"),
                   tglstjkeu=paste(tglstjkeu, collapse = ";"),
                   jenisobat=paste(jenisobat, collapse = ";"),
                   katastropik=paste(katastropik, collapse = ";"),
                   obat=paste(obat, collapse = ";"),
                   nmobat=paste(nmobat, collapse = ";"),
                   jenis_obat_luarpaket=paste(jenis_obat_luarpaket, collapse = ";"),
                   kdinacbgs=paste(kdinacbgs, collapse = ";"),
                   kd_diagprimer=paste(kd_diagprimer, collapse = ";"),
                   nm_diagprimer=paste(nm_diagprimer, collapse = ";"),
                   diagsekunder=paste(diagsekunder, collapse = ";"),
                   nmdokter=paste(nmdokter, collapse = ";"),
                   biaya=(sum(biaya)))

data0$nokapst <- sapply(strsplit(data0$nokapst, ";"),
                        function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nokapst = na_if(nokapst, "NA"))
data0$nmpst <- sapply(strsplit(data0$nmpst, ";"),
                      function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nmpst = na_if(nmpst, "NA"))
data0$jkpst <- sapply(strsplit(data0$jkpst, ";"),
                      function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(jkpst = na_if(jkpst, "NA"))
data0$pstprb <- sapply(strsplit(data0$pstprb, ";"),
                      function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(pstprb = na_if(pstprb, "NA"))
data0$pstprolanis <- sapply(strsplit(data0$pstprolanis, ";"),
                      function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(pstprolanis = na_if(pstprolanis, "NA"))
data0$nmdati2layan <- sapply(strsplit(data0$nmdati2layan, ";"),
                             function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nmdati2layan = na_if(nmdati2layan, "NA"))
data0$nmdati2resep <- sapply(strsplit(data0$nmdati2resep, ";"),
                             function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nmdati2resep = na_if(nmdati2resep, "NA"))
data0$nmjnsppkresep <- sapply(strsplit(data0$nmjnsppkresep, ";"),
                             function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nmjnsppkresep = na_if(nmjnsppkresep, "NA"))
data0$nmppklayan <- sapply(strsplit(data0$nmppklayan, ";"),
                              function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nmppklayan = na_if(nmppklayan, "NA"))
data0$Nmppkresep <- sapply(strsplit(data0$Nmppkresep, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(Nmppkresep = na_if(Nmppkresep, "NA"))
data0$refasalsjp <- sapply(strsplit(data0$refasalsjp, ";"),
                        function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(refasalsjp = na_if(refasalsjp, "NA"))
data0$flag_iterasi <- sapply(strsplit(data0$flag_iterasi, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(flag_iterasi = na_if(flag_iterasi, "NA"))
data0$nmtkp <- sapply(strsplit(data0$nmtkp, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nmtkp = na_if(nmtkp, "NA"))
data0$absensi <- sapply(strsplit(data0$absensi, ";"),
                      function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(absensi = na_if(absensi, "NA"))
data0$politujsjp <- sapply(strsplit(data0$politujsjp, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(politujsjp = na_if(politujsjp, "NA"))
data0$tglpelayanan <- sapply(strsplit(data0$tglpelayanan, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(tglpelayanan = na_if(tglpelayanan, "NA"))
data0$tglrsp <- sapply(strsplit(data0$tglrsp, ";"),
                             function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(tglrsp = na_if(tglrsp, "NA"))
data0$tglsjp <- sapply(strsplit(data0$tglsjp, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(tglsjp = na_if(tglsjp, "NA"))
data0$tglstjkeu <- sapply(strsplit(data0$tglstjkeu, ";"),
                       function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(tglstjkeu = na_if(tglstjkeu, "NA"))
data0$jenisobat <- sapply(strsplit(data0$jenisobat, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(jenisobat = na_if(jenisobat, "NA"))
data0$katastropik <- sapply(strsplit(data0$katastropik, ";"),
                          function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(katastropik = na_if(katastropik, "NA"))
data0$obat <- sapply(strsplit(data0$obat, ";"),
                       function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(obat = na_if(obat, "NA"))
data0$nmobat <- sapply(strsplit(data0$nmobat, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nmobat = na_if(nmobat, "NA"))
data0$jenis_obat_luarpaket <- sapply(strsplit(data0$jenis_obat_luarpaket, ";"),
                       function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(jenis_obat_luarpaket = na_if(jenis_obat_luarpaket, "NA"))
data0$kdinacbgs <- sapply(strsplit(data0$kdinacbgs, ";"),
                                     function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(kdinacbgs = na_if(kdinacbgs, "NA"))
data0$kd_diagprimer <- sapply(strsplit(data0$kd_diagprimer, ";"),
                       function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(kd_diagprimer = na_if(kd_diagprimer, "NA"))
data0$nm_diagprimer <- sapply(strsplit(data0$nm_diagprimer, ";"),
                           function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nm_diagprimer = na_if(nm_diagprimer, "NA"))
data0$diagsekunder <- sapply(strsplit(data0$diagsekunder, ";"),
                     function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(diagsekunder = na_if(diagsekunder, "NA"))
data0$nmdokter <- sapply(strsplit(data0$nmdokter, ";"),
                             function(x) paste(unique(x), collapse = ";"))
data0 <- data0 %>% mutate(nmdokter = na_if(nmdokter, "NA"))
#==============================================================================
data0 <- data0 %>%
  mutate(
    kd_diagprimer1 = case_when(
      kd_diagprimer == "Z014" ~ NA,
      kd_diagprimer == "Z016" ~ NA,
      kd_diagprimer == "Z030" ~ NA,
      kd_diagprimer == "Z031" ~ NA,
      kd_diagprimer == "Z038" ~ NA,
      kd_diagprimer == "Z039" ~ NA,
      kd_diagprimer == "Z041" ~ NA,
      kd_diagprimer == "Z048" ~ NA,
      kd_diagprimer == "Z080" ~ NA,
      kd_diagprimer == "Z088" ~ NA,
      kd_diagprimer == "Z089" ~ NA,
      kd_diagprimer == "Z090" ~ NA,
      kd_diagprimer == "Z091" ~ NA,
      kd_diagprimer == "Z092" ~ NA,
      kd_diagprimer == "Z093" ~ NA,
      kd_diagprimer == "Z094" ~ NA,
      kd_diagprimer == "Z097" ~ NA,
      kd_diagprimer == "Z098" ~ NA,
      kd_diagprimer == "Z099" ~ NA,
      kd_diagprimer == "Z209" ~ NA,
      kd_diagprimer == "Z291" ~ NA,
      kd_diagprimer == "Z301" ~ NA,
      kd_diagprimer == "Z340" ~ NA,
      kd_diagprimer == "Z348" ~ NA,
      kd_diagprimer == "Z349" ~ NA,
      kd_diagprimer == "Z351" ~ NA,
      kd_diagprimer == "Z358" ~ NA,
      kd_diagprimer == "Z359" ~ NA,
      kd_diagprimer == "Z370" ~ NA,
      kd_diagprimer == "Z390" ~ NA,
      kd_diagprimer == "Z392" ~ NA,
      kd_diagprimer == "Z419" ~ NA,
      kd_diagprimer == "Z429" ~ NA,
      kd_diagprimer == "Z470" ~ NA,
      kd_diagprimer == "Z478" ~ NA,
      kd_diagprimer == "Z479" ~ NA,
      kd_diagprimer == "Z480" ~ NA,
      kd_diagprimer == "Z488" ~ NA,
      kd_diagprimer == "Z489" ~ NA,
      kd_diagprimer == "Z490" ~ NA,
      kd_diagprimer == "Z491" ~ NA,
      kd_diagprimer == "Z492" ~ NA,
      kd_diagprimer == "Z500" ~ NA,
      kd_diagprimer == "Z501" ~ NA,
      kd_diagprimer == "Z502" ~ NA,
      kd_diagprimer == "Z503" ~ NA,
      kd_diagprimer == "Z504" ~ NA,
      kd_diagprimer == "Z505" ~ NA,
      kd_diagprimer == "Z506" ~ NA,
      kd_diagprimer == "Z507" ~ NA,
      kd_diagprimer == "Z508" ~ NA,
      kd_diagprimer == "Z509" ~ NA,
      kd_diagprimer == "Z510" ~ NA,
      kd_diagprimer == "Z511" ~ NA,
      kd_diagprimer == "Z512" ~ NA,
      kd_diagprimer == "Z513" ~ NA,
      kd_diagprimer == "Z514" ~ NA,
      kd_diagprimer == "Z515" ~ NA,
      kd_diagprimer == "Z516" ~ NA,
      kd_diagprimer == "Z518" ~ NA,
      kd_diagprimer == "Z519" ~ NA,
      kd_diagprimer == "Z549" ~ NA,
      kd_diagprimer == "Z590" ~ NA,
      kd_diagprimer == "Z596" ~ NA,
      kd_diagprimer == "Z719" ~ NA,
      kd_diagprimer == "Z760" ~ NA,
      kd_diagprimer == "Z801" ~ NA,
      kd_diagprimer == "Z898" ~ NA,
      kd_diagprimer == "Z908" ~ NA,
      kd_diagprimer == "Z941" ~ NA,
      kd_diagprimer == "Z955" ~ NA,
      kd_diagprimer == "Z961" ~ NA,
      kd_diagprimer == "Z988" ~ NA,
      TRUE ~ kd_diagprimer))

data0$nm_diagprimer1 <- with(data0,
                             ifelse(is.na(data0$kd_diagprimer1),NA,nm_diagprimer))
data0 <- data0 %>%
  mutate(Diagprimer = paste0(as.character(kd_diagprimer1)," - ",
                             as.character(nm_diagprimer1)))
data0$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,data0$Diagprimer)), "both")
data0 <- data0 %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(diagsekunder)))
data0$Diagnosa <- as.character(trimws(gsub(";NA|NA;|NA","",data0$Diagnosa)), "both")

data0 <- data0 %>% select(-Diagprimer,-kd_diagprimer1,-nm_diagprimer1)
data0 <- data0 %>% mutate(Diagnosa = na_if(Diagnosa, ""))

save(data0, file = "ur_obat.rda")
load("ur_obat.rda")
#==============================================================================


data0$jeda <- as.integer(data$tglstjkeu) - as.integer(data$tglsjp)



necrosispulp1$Diagnosa <- with(necrosispulp1,
                               ifelse(Diagnosa == "K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K052 - Acute periodontitis",
                                      ifelse(Diagnosa == "K045 - Chronic apical periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K045 - Chronic apical periodontitis",
                                             ifelse(Diagnosa == "K011 - Impacted teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth",
                                                    ifelse(Diagnosa == "S025 - Fracture of tooth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;S025 - Fracture of tooth",
                                                           ifelse(Diagnosa == "K040 - Pulpitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K040 - Pulpitis",
                                                                  ifelse(Diagnosa == "K010 - Embedded teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth",
                                                                         ifelse(Diagnosa == "G442 - Tension-type headache;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;G442 - Tension-type headache",
                                                                                ifelse(Diagnosa == "K053 - Chronic periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K053 - Chronic periodontitis",
                                                                                       ifelse(Diagnosa == "J329 - Chronic sinusitis, unspecified;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;J329 - Chronic sinusitis, unspecified",
                                                                                              ifelse(Diagnosa == "K047 - Periapical abscess without sinus;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K047 - Periapical abscess without sinus",
                                                                                                     ifelse(Diagnosa == "K010 - Embedded teeth;K041 - Necrosis of pulp;K052 - Acute periodontitis", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                            ifelse(Diagnosa == "K010 - Embedded teeth;K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                   ifelse(Diagnosa == "K010 - Embedded teeth;K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                          ifelse(Diagnosa == "K041 - Necrosis of pulp;K052 - Acute periodontitis;K010 - Embedded teeth", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                 ifelse(Diagnosa == "K052 - Acute periodontitis;K041 - Necrosis of pulp;K010 - Embedded teeth", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                        ifelse(Diagnosa == "K052 - Acute periodontitis;K010 - Embedded teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                               ifelse(Diagnosa == "K029 - Dental caries, unspecified;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K029 - Dental caries, unspecified",
                                                                                                                                                      ifelse(Diagnosa == "K054 - Periodontosis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K054 - Periodontosis",
                                                                                                                                                             ifelse(Diagnosa == "K011 - Impacted teeth;K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                    ifelse(Diagnosa == "K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                           ifelse(Diagnosa == "K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth;K011 - Impacted teeth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                  ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp;K011 - Impacted teeth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                         ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K011 - Impacted teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                                ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                                       Diagnosa)))))))))))))))))))))))))


write.csv(data0, "D://data gresik//FactObat_HV//obat//ur_obat.csv",
          na="", row.names = FALSE)
#==============================================================================
data0 <- data0[order(data0$nokapst, data0$tglsjp),]
data0 <- data0 %>% subset(tglsjp >= "2023-01-01")

readmisi_1 <- data0[order(data0$nokapst,data0$tglsjp),]
readmisi_1 <- readmisi_1 %>% group_by(nokapst) %>%
  mutate(tgl_before = lag(tglsjp, n=1))
readmisi_1$interval <- as.integer(readmisi_1$tglsjp) - as.integer(readmisi_1$tgl_before)

readmisi_1 <- readmisi_1[order(readmisi_1$nokapst, readmisi_1$tglsjp),]
readmisi_1 <- readmisi_1 %>% subset(tglsjp >= "2023-04-01") %>%
  subset(katastropik != "KANKER" )


readmisi_2 <- readmisi_1 %>%
  subset(!is.na(interval))
readmisi_3 <- readmisi_2 %>%
  subset(interval <= 30) %>% select(Nokapst,Nmppklayan) %>% unique()
readmisi_4 <- left_join(readmisi_3,readmisi_1, by = c("Nokapst","Nmppklayan")) %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_4 <- readmisi_4[order(readmisi_4$Nokapst,readmisi_4$Tglplgsjp),]
readmisi_4 <- readmisi_4 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_4$interval <- as.integer(readmisi_4$Tgldtgsjp) - as.integer(readmisi_4$tgl_before)
readmisi_4$status <- with(readmisi_4,
                          ifelse(interval <= 30 , "readmisi", interval))
readmisi_4 <- readmisi_4 %>% group_by(Nokapst) %>%
  mutate(status1 = lead(status, n=1))

readmisi_5 <- readmisi_4 %>%
  subset(status == "readmisi"|status1 == "readmisi") %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglstjkeu,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_5 <- readmisi_5[order(readmisi_5$Nokapst,readmisi_5$Tglplgsjp),]
readmisi_5 <- readmisi_5 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
readmisi_5$interval <- as.integer(readmisi_5$Tgldtgsjp) - as.integer(readmisi_5$tgl_before)

readmisi_5 <- readmisi_5 %>%
  select(Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmppklayan,Nosjp,
         Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,interval,
         Tglstjkeu,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi)
readmisi_5 <- readmisi_5[order(readmisi_5$Nokapst,readmisi_5$Tglplgsjp),]


setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("ur_all.rda")

data1 <- data %>%
  select(Nosjp,Sumber,Norjkawalsep,Poli_asal,Nminacbgs,Procedure,
         Biayaverifikasi) %>% unique()

data1 <- left_join(data0,data1, by = c("refasalsjp"="Nosjp"))
data1 <- data1 %>% mutate(Procedure = na_if(Procedure, "-"))

data0 <- data0 %>%
  select(Nosjp,Norjkawalsep,Sumber,Flagspesialistik,Flagtacc,Jenisppkperujuk,
         Typeppkperujuk,Kddiagmasuk,Nmdiagmasuk,Typeppklayan,Nokapst,Umur,
         kel_umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Nmtkp,Tglpelayanan,
         tgl_before,Tgldtgsjp,Tglplgsjp,Tglstjkeu,Kdinacbgs,Nminacbgs,
         Diagmasuk,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
         Nmjnspulang,biayars,Biayaverifikasi,Nmppklayan,Poli_asal,
         Politujsjp,Namadpjp)
#===============================SKIP======================================
data0 <- data0 %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data0 <- data0 %>% mutate(Procedure = na_if(Procedure, "-"))

%>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  filter(str_detect(Diagnosa,c("Z961"))) %>%
  filter(str_detect(Procedure,c("8911"))) %>%
  filter(!str_detect(Diagnosa,c("Glaucoma|glaucoma"))) %>%
  select(-Diagnosa)

data20 <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  filter(str_detect(Diagnosa,c("Z961"))) %>%
  filter(str_detect(Procedure,c("8911"))) %>%
  filter(!str_detect(Diagnosa,c("Glaucoma|glaucoma"))) %>%
  select(-Diagnosa)

data21 <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  filter(str_detect(Diagnosa,c("Z961"))) %>%
  filter(str_detect(Procedure,c("8911"))) %>%
  filter(!str_detect(Diagnosa,c("Glaucoma|glaucoma"))) %>%
  select(-Diagnosa)

data22 <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  filter(str_detect(Diagnosa,c("Z961"))) %>%
  filter(str_detect(Procedure,c("8911"))) %>%
  filter(!str_detect(Diagnosa,c("Glaucoma|glaucoma"))) %>%
  select(-Diagnosa)

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  filter(str_detect(Diagnosa,c("Z961"))) %>%
  filter(str_detect(Procedure,c("8911"))) %>%
  filter(!str_detect(Diagnosa,c("Glaucoma|glaucoma"))) %>%
  select(-Diagnosa)

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  filter(str_detect(Diagnosa,c("Z961"))) %>%
  filter(str_detect(Procedure,c("8911"))) %>%
  filter(!str_detect(Diagnosa,c("Glaucoma|glaucoma"))) %>%
  select(-Diagnosa)

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  filter(str_detect(Diagnosa,c("Z961"))) %>%
  filter(str_detect(Procedure,c("8911"))) %>%
  filter(!str_detect(Diagnosa,c("Glaucoma|glaucoma"))) %>%
  select(-Diagnosa)

data <- rbind(data19,data20,data21,data22,data23,data24,dataapr24)
rm(data19,data20,data21,data22,data23,data24,dataapr24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")



data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>% subset(Tglpelayanan >= "2019-01-01")
data <- data %>%
  mutate(Keterangan = "tonometry hanya utk pengecekan setelah diberikan IOL ")
data <- data %>%
  select(Keterangan,Nokapst,Umur,Jkpst,Nosjp,Nmdati2Layan,Nmppklayan,
         Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
         Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
         Nmjnspulang,biayars,Biayaverifikasi)
data <- data[order(data$Nokapst,data$Tglplgsjp),]
#========================================================================
write.xlsx(data, file = "tonometry.xlsx")
#========================================================================


diagnosa <- data %>%
  select(Kddiagprimer,Nmdiagprimer,Diagsekunder) %>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  select(-Kddiagprimer,-Nmdiagprimer,-Diagsekunder)

library(splitstackshape)
diagnosa <- concat.split(diagnosa, "Diagnosa", ";") %>%
  select(-Diagnosa)
colnames(diagnosa)

if
Z961 - Presence of intraocular lens maka ngetest
cataract maka penegakan diagnosa
glaucoma maka penegakan diagnosa
D31 maka penegakan diagnosa

diagnosa <- diagnosa %>%
  mutate(
    diagnosa1 = case_when(
    Diagnosa_1 == "Z098 - Follow-up examination after other treatment for other conditions" ~ "",
    Diagnosa_1 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ "",
    Diagnosa_1 == "Z961 - Presence of intraocular lens" ~ "",
    Diagnosa_1 == "I10 - Essential (primary) hypertension" ~ "",
    Diagnosa_1 == "E114 - Non-insulin-dependent diabetes mellitus with neurological complications" ~ "",
    Diagnosa_1 == "A151 - Tuberculosis of lung, confirmed by culture only" ~ "",
    Diagnosa_1 == "Z090 - Follow-up examination after surgery for other conditions" ~ "",
    Diagnosa_1 == "Z358 - Supervision of other high-risk pregnancies" ~ "",
    Diagnosa_1 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ "",
    Diagnosa_1 == "Z392 - Routine postpartum follow-up" ~ "",
    Diagnosa_1 == "Z491 - Extracorporeal dialysis" ~ "",
    Diagnosa_1 == "Z501 - Other physical therapy" ~ "",
    Diagnosa_1 == "Z530 - Procedure not carried out because of contraindication" ~ "",
    Diagnosa_1 == "Z711 - Person with feared complaint in whom no diagnosis is made" ~ "",
    Diagnosa_1 == "Z969 - Presence of functional implant, unspecified" ~ "",
      TRUE ~ Diagnosa_1)) %>%
  select(-Diagnosa_1)











diagnosa <- diagnosa %>%
  str_replace_all(Diagnosa_1,c("Z098 - Follow-up examination after other treatment for other conditions"="",
                  "Z088 - Follow-up examination after other treatment for malignant neoplasm"=""))

diagnosa <- diagnosa[!grepl("Z098 - Follow-up examination after other treatment for other conditions",
                            diagnosa$Diagnosa_1,diagnosa$Diagnosa_2,
                            diagnosa$Diagnosa_3,diagnosa$Diagnosa_4),]
diagnosa <- select(diagnosa, where(function(x) !all(is.na(x))))
diagnosa <- diagnosa[!grepl("Z088 - Follow-up examination after other treatment for malignant neoplasm",
                            diagnosa$Diagnosa_1,diagnosa$Diagnosa_2,
                            diagnosa$Diagnosa_3,diagnosa$Diagnosa_4),]
diagnosa <- select(diagnosa, where(function(x) !all(is.na(x))))
diagnosa <- diagnosa[!grepl("I10 - Essential (primary) hypertension",
                            diagnosa$Diagnosa_2,diagnosa$Diagnosa_3,
                            diagnosa$Diagnosa_4,diagnosa$Diagnosa_5),]
diagnosa <- select(diagnosa, where(function(x) !all(is.na(x))))
diagnosa <- diagnosa[!grepl("E114 - Non-insulin-dependent diabetes mellitus with neurological complications",
                            diagnosa$Diagnosa_2,diagnosa$Diagnosa_3,
                            diagnosa$Diagnosa_4,diagnosa$Diagnosa_5),]
diagnosa <- select(diagnosa, where(function(x) !all(is.na(x))))
diagnosa <- diagnosa[!grepl("A151 - Tuberculosis of lung, confirmed by culture only",
                            diagnosa$Diagnosa_2,diagnosa$Diagnosa_3,
                            diagnosa$Diagnosa_4,diagnosa$Diagnosa_5),]
diagnosa <- select(diagnosa, where(function(x) !all(is.na(x))))

diagnosa <- diagnosa %>%
  filter_at(vars(Diagnosa,Diagnosa_1,Diagnosa_2,Diagnosa_3),
            all_vars(!. %in%
                       c("Z098 - Follow-up examination after other treatment for other conditions;")))
cols <- c('Diagnosa_1','Diagnosa_2','Diagnosa_3','Diagnosa_4',
          'Diagnosa_5','Diagnosa_6','Diagnosa_7','Diagnosa_8',
          'Diagnosa_9')
diagnosa[rowSums(diagnosa[cols] == "Z098 - Follow-up examination after other treatment for other conditions" | dat[cols] == "") == 0, ]

diagnosa <- diagnosa %>%
  filter(!if_all(Diagnosa:Diagnosa_9, ~ .x %in% c("Z098 - Follow-up examination after other treatment for other conditions;")) |
           if_all(Diagnosa:Diagnosa_9, ~ is.na(.x)))


dat_noka <- data %>%
  select(Nokapst,Kddiagprimer,Nmdiagprimer,Diagsekunder) %>%
  mutate(Diagnosa = paste0(Kddiagprimer," - ", 
                           Nmdiagprimer,"; ",Diagsekunder)) %>%
  select(-Nokapst,-Kddiagprimer,-Nmdiagprimer,-Diagsekunder)

dat_noka <- dat_noka[!(duplicated(dat_noka) | 
                         duplicated(dat_noka, fromLast = TRUE)), ]


#==============================================================================
data19x <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Kdppklayan,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
          Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
          Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Nmtkp == "RJTL") %>%
  subset(!(Procedure %in% c("8911")))
data19x <- left_join(dat_noka, data19x, by = c("Nokapst"="Nokapst",
                                               "Politujsjp"="Politujsjp"),
                     copy = FALSE, keep = NULL)
data19x <- data19x[!is.na(data19x$Nosjp), ]
#==============================================================================
write.csv(data, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_all.csv",
          na="", row.names = FALSE)
#==============================================================================

#8. kasus dirujuk dengan over biaya RS
data <- data %>%
  filter(str_detect(Nmtkp,c("RITL"))) %>%
  filter(str_detect(Nmjnspulang,c("Rujuk")))%>%
  group_by(Nokapst) %>%
  mutate(netbiaya = Biayaverifikasi-biayars)
data$netbiaya <- as.integer(ifelse(data$netbiaya <= 0, 1, 0))

#7. kasus fako pbpu mandiri sebelum satu bulan
%>%
  filter(str_detect(Nmjnspulang,c("Rujuk"))) %>%
  filter(str_detect(Procedure,c("1341|1371|992")))

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$LOS <- as.integer(data$Tglplgsjp) - as.integer(data$Tgldtgsjp) +1

#Transform Nmtkp menjadi 1&2 --> 1= berarti RITL, 2= berarti RJTL
data$Nmtkp[data$Nmtkp=="RITL"] <- "1"
data$Nmtkp[data$Nmtkp=="RJTL"] <- "2"
data$Nmtkp <- as.factor(data$Nmtkp)

#Transform Jkpst menjadi 1&2 --> 1= berarti Laki-laki, 2= berarti Perempuan
data$Jkpst[data$Jkpst=="Laki-laki"] <- "1"
data$Jkpst[data$Jkpst=="Perempuan"] <- "2"
data$Jkpst <- as.factor(data$Jkpst)

#Transform Kelashak menjadi 1,2,3
data$Kelashak[data$Kelashak=="Kelas I"] <- "1"
data$Kelashak[data$Kelashak=="Kelas II"] <- "2"
data$Kelashak[data$Kelashak=="Kelas III"] <- "3"
data$Kelashak <- as.factor(data$Kelashak)

#Transform Klsrawat menjadi 1,2,3
data$Klsrawat[data$Klsrawat=="Kelas I"] <- "1"
data$Klsrawat[data$Klsrawat=="Kelas II"] <- "2"
data$Klsrawat[data$Klsrawat=="Kelas III"] <- "3"
data$Klsrawat <- as.factor(data$Klsrawat)
#========================================================================
data <- data %>% rename(DM = Kddiagmasuk,
                        NmDM = Nmdiagmasuk,
                        Poli = Politujsjp,
                        Sumber = Sumberkunjungan,
                        SEP = Nosjp,
                        DU =  Kddiagprimer, 
                        NmDU = Nmdiagprimer,
                        DS = Diagsekunder,
                        Start = Tgldtgsjp,
                        End = Tglplgsjp,
                        Pulang = Nmjnspulang,
                        BiayaRS = biayars,
                        Biaya = Biayaverifikasi)

colnames(data)
glimpse(data)
str(data)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
load("datasep.rda")
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

dfcm <- left_join(data, df, by = c("SEP"="Nosep")) %>%
  select(-Sumber.y) %>%
  rename(Sumber = Sumber.x)
rm(data,df)

data <- data[order(data$Nokapst, data$End),]
dfcm <- dfcm %>%
  group_by(Nokapst) %>%
  mutate(Prvtkp = lag(Nmtkp))

dfcm$Prvtkp <- as.numeric(dfcm$Prvtkp) %>%
  replace(is.na(.), "")
dfcm$Prvtkp <- as.factor(dfcm$Prvtkp)

dfcm <- dfcm[order(dfcm$Nokapst, dfcm$End),]
dfcm <- dfcm %>%
  group_by(Nokapst) %>%
  mutate(interval = End - lag(End))

dfcm$interval <- as.numeric(dfcm$interval) %>%
  replace(is.na(.), "")
dfcm$interval <- as.factor(dfcm$interval)

dfcm <- dfcm %>% subset(Tglpelayanan >= "2021-12-01")

dfcm$Namadpjp01 <- trimws(gsub("dr.", "", tolower(dfcm$Namadpjp)),"both")
dfcm$Namadpjp01 <- trimws(gsub("dr", "", dfcm$Namadpjp01),"both")
dfcm$Namadpjp01 <- str_replace_all(dfcm$Namadpjp01, "[^[:alnum:]]", " ")
dfcm$Namadpjp01 <- trimws(gsub("sp", "sp ", dfcm$Namadpjp01),"both")
dfcm$Namadpjp01 <- gsub("\\s+"," ",dfcm$Namadpjp01)
dfcm <- dfcm %>% rename(DPJP = Namadpjp01)
dfcm$Namadpjp <- NULL
#========================================================================

#CMG, Specific CBG, SEVERITY
dfcm$CMG <- as.character(trimws(substr(dfcm$Kdinacbgs,1,1)), "both")
dfcm$CBG <- as.character(trimws(substr(dfcm$Kdinacbgs,1,6)), "both")
dfcm$Spec <- as.character(trimws(substr(dfcm$Kdinacbgs,5,6)), "both")
dfcm$Kdsevel <- as.character(trimws(substr(dfcm$Kdinacbgs,8,10)), "both")
dfcm <- dfcm %>% 
  mutate(
    Sevel = case_when(
      Kdsevel == "0" ~ "0",
      Kdsevel == "I" ~ "1",
      Kdsevel == "II" ~ "2",
      Kdsevel == "III" ~ "3",
      TRUE ~ Kdsevel)) %>%
  select(-Kdsevel) 

dfcm$tp <- as.character(trimws(substr(dfcm$Kdinacbgs,3,3)), "both")
dfcm <- data.frame(dfcm)
dfcm <- dfcm %>% 
  mutate(
    tipe = case_when(
      tp == "1" ~ "Prosedur Rawat Inap",
      tp == "2" ~ "Prosedur Besar Rawat Jalan",
      tp == "3" ~ "Prosedur Signifikan Rawat Jalan",
      tp == "4" ~ "Rawat Inap Bukan Prosedur",
      tp == "5" ~ "Rawat Jalan Bukan Prosedur",
      tp == "6" ~ "Rawat Inap Kebidanan",
      tp == "7" ~ "Rawat Jalan Kebidanan",
      tp == "8" ~ "Rawat Inap Neonatal",
      tp == "9" ~ "Rawat Jalan Neonatal",
      tp == "0" ~ "Error",
      TRUE ~ tp)) %>%
  select(-tp)

library(splitstackshape)
dfcm <- concat.split(dfcm, "DS", ";")
colnames(dfcm)

dfcm$DS01 <- as.character(trimws(gsub("-","",substr(dfcm$DS_1,1,6)), "both"))
dfcm$DS01 [is.na(dfcm$DS01)] <- ""
dfcm$DS01 <- as.factor(dfcm$DS01)
dfcm$DS02 <- as.character(trimws(gsub("-","",substr(dfcm$DS_2,1,6)), "both"))
dfcm$DS02 [is.na(dfcm$DS02)] <- ""
dfcm$DS02 <- as.factor(dfcm$DS02)
dfcm$DS03 <- as.character(trimws(gsub("-","",substr(dfcm$DS_3,1,6)), "both"))
dfcm$DS03 [is.na(dfcm$DS03)] <- ""
dfcm$DS03 <- as.factor(dfcm$DS03)
dfcm$DS04 <- as.character(trimws(gsub("-","",substr(dfcm$DS_4,1,6)), "both"))
dfcm$DS04 [is.na(dfcm$DS04)] <- ""
dfcm$DS04 <- as.factor(dfcm$DS04)
dfcm$DS05 <- as.character(trimws(gsub("-","",substr(dfcm$DS_5,1,6)), "both"))
dfcm$DS05 [is.na(dfcm$DS05)] <- ""
dfcm$DS05 <- as.factor(dfcm$DS05)
dfcm$DS06 <- as.character(trimws(gsub("-","",substr(dfcm$DS_6,1,6)), "both"))
dfcm$DS06 [is.na(dfcm$DS06)] <- ""
dfcm$DS06 <- as.factor(dfcm$DS06)
dfcm$DS07 <- as.character(trimws(gsub("-","",substr(dfcm$DS_7,1,6)), "both"))
dfcm$DS07 [is.na(dfcm$DS07)] <- ""
dfcm$DS07 <- as.factor(dfcm$DS07)
#======================================================================
dfcm <- concat.split(dfcm, "Procedure", ";")

colnames(dfcm)

dfcm$Proc01 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_1,1,4)), "both"))
dfcm$Proc01 [is.na(dfcm$Proc01)] <- ""
dfcm$Proc01 <- as.factor(dfcm$Proc01)
dfcm$Proc02 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_2,1,4)), "both"))
dfcm$Proc02 [is.na(dfcm$Proc02)] <- ""
dfcm$Proc02 <- as.factor(dfcm$Proc02)
dfcm$Proc03 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_3,1,4)), "both"))
dfcm$Proc03 [is.na(dfcm$Proc03)] <- ""
dfcm$Proc03 <- as.factor(dfcm$Proc03)
dfcm$Proc04 <- as.character(trimws(gsub("-","",substr(dfcm$Procedure_4,1,4)), "both"))
dfcm$Proc04 [is.na(dfcm$Proc04)] <- ""
dfcm$Proc04 <- as.factor(dfcm$Proc04)

##DATA_03 = Gabung DIAG & PROC
#remove empty cell with NA
#============================================================================
library(readxl)
setwd("D:/data gresik/UR/Data Sampel BPJS Kesehatan 2015-2020 v01/Metadata")
DIAGFKTP <- read_excel("Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx", 
                       sheet = "Sheet1", range = NULL, 
                       col_names = TRUE, col_types = NULL, na = "",
                       trim_ws = TRUE, skip = 0)
colnames(dfcm)
dfcm00 <- dfcm %>%
  mutate(DP = as.factor(substr(dfcm$DU,1,3)),
         DS_01 = as.factor(substr(dfcm$DS01,1,3)),
         DS_02 = as.factor(substr(dfcm$DS02,1,3)),
         DS_03 = as.factor(substr(dfcm$DS03,1,3)),
         DS_04 = as.factor(substr(dfcm$DS04,1,3)),
         DS_05 = as.factor(substr(dfcm$DS05,1,3)),
         DS_06 = as.factor(substr(dfcm$DS06,1,3)),
         DS_07 = as.factor(substr(dfcm$DS07,1,3)))
rm(dfcm)

dfcm00$DIAG3 <- paste(dfcm00$DP,dfcm00$DS_01,dfcm00$DS_02,dfcm00$DS_03,dfcm00$DS_04,
                      dfcm00$DS_05,dfcm00$DS0_6,sep=" ")
dfcm00$DIAG3 <- trimws(dfcm00$DIAG3, whitespace = " ")
dfcm00$DIAG3 <- gsub(" ",", ",dfcm00$DIAG3)
dfcm00$DIAG3 <- sapply(dfcm00$DIAG3,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
PROC <- read.csv2("sub chapter proc.csv",
                  colClasses=c("factor", "character", "character"))
glimpse(PROC)

dfcm01 <- dfcm00 %>%
  mutate(Proc_01 = as.factor(substr(dfcm00$Proc01,1,2)),
         Proc_02 = as.factor(substr(dfcm00$Proc02,1,2)),
         Proc_03 = as.factor(substr(dfcm00$Proc03,1,2)),
         Proc_04 = as.factor(substr(dfcm00$Proc04,1,2)))
rm(dfcm00)

dfcm01$PROC2 <- paste(dfcm01$Proc_01,dfcm01$Proc_02,dfcm01$Proc_03,
                      dfcm01$Proc_04,sep=" ")
dfcm01$PROC2 <- trimws(dfcm01$PROC2, whitespace = " ")
dfcm01$PROC2 <- gsub(" ",", ",dfcm01$PROC2)
dfcm01$PROC2 <- sapply(dfcm01$PROC2,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

PROC2 <- read.csv2("Chapter Procedure.csv", 
                   colClasses=c("factor", "character",
                                "character", "character"))

dfcm02 <- dfcm01 %>%
  mutate(Proc_001 = as.factor(substr(dfcm01$Proc01,1,3)),
         Proc_002 = as.factor(substr(dfcm01$Proc02,1,3)),
         Proc_003 = as.factor(substr(dfcm01$Proc03,1,3)),
         Proc_004 = as.factor(substr(dfcm01$Proc04,1,3)))
rm(dfcm01)

dfcm02$PROC3 <- paste(dfcm02$Proc_001,dfcm02$Proc_002,dfcm02$Proc_003,dfcm02$Proc_004,
                      dfcm02$Proc_005,dfcm02$Proc_006,dfcm02$Proc_007,sep=" ")
dfcm02$PROC3 <- trimws(dfcm02$PROC3, whitespace = " ")
dfcm02$PROC3 <- gsub(" ",", ",dfcm02$PROC3)
dfcm02$PROC3 <- sapply(dfcm02$PROC3,
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

dfcm02 <- dfcm02 %>%
  select(-DP,-DS_01,-DS_02,-DS_03,-DS_04,-DS_05,-DS_06,-DS_07,-Proc_01,
         -Proc_02,-Proc_03,-Proc_04,-Proc_001,-Proc_002,-Proc_003,-Proc_004)
dfcm02 <- concat.split(dfcm02, "DIAG3", ", ")
dfcm02 <- concat.split(dfcm02, "PROC2", ", ")
dfcm02 <- concat.split(dfcm02, "PROC3", ", ")
colnames(dfcm02)
#===========================================================================
#Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx

dfcm03 <- left_join(dfcm02, DIAGFKTP, by = c("DIAG3_1"="ICD10_Code")) %>%
  select(-DIAG3_1, -FKP14) %>%
  rename(DIAG3_1 = ICD10_Text)
dfcm03$DIAG3_1 [is.na(dfcm03$DIAG3_1)] <- ""
rm(dfcm02)
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_2"="ICD10_Code")) %>%
  select(-DIAG3_2, -FKP14) %>%
  rename(DIAG3_2 = ICD10_Text)
dfcm03$DIAG3_2 [is.na(dfcm03$DIAG3_2)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_3"="ICD10_Code")) %>%
  select(-DIAG3_3, -FKP14) %>%
  rename(DIAG3_3 = ICD10_Text)
dfcm03$DIAG3_3 [is.na(dfcm03$DIAG3_3)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_4"="ICD10_Code")) %>%
  select(-DIAG3_4, -FKP14) %>%
  rename(DIAG3_4 = ICD10_Text)
dfcm03$DIAG3_4 [is.na(dfcm03$DIAG3_4)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_5"="ICD10_Code")) %>%
  select(-DIAG3_5, -FKP14) %>%
  rename(DIAG3_5 = ICD10_Text)
dfcm03$DIAG3_5 [is.na(dfcm03$DIAG3_5)] <- ""
dfcm03 <- left_join(dfcm03, DIAGFKTP, by = c("DIAG3_6"="ICD10_Code")) %>%
  select(-DIAG3_6, -FKP14) %>%
  rename(DIAG3_6 = ICD10_Text)
dfcm03$DIAG3_6 [is.na(dfcm03$DIAG3_6)] <- ""
#---------------------------------------------------------------------------
#Kode ICD9CM untuk Procedure Sub Chapter

dfcm03$PROC2_1 <- factor(dfcm03$PROC2_1)
dfcm03$PROC2_2 <- factor(dfcm03$PROC2_2)
dfcm03$PROC2_3 <- factor(dfcm03$PROC2_3)

dfcm04 <- left_join(dfcm03, PROC,
                  by = c("PROC2_1"="No.Sub.Chap")) %>%
  select(-PROC2_1, -Chap.Proc) %>%
  rename(PROC2_1 = Sub.Chap.Proc)
dfcm03$Proc_01 [is.na(dfcm03$Proc_01)] <- ""
rm(dfcm03)

dfcm04 <- left_join(dfcm04, PROC,
                    by = c("PROC2_2"="No.Sub.Chap")) %>%
  select(-PROC2_2, -Chap.Proc) %>%
  rename(PROC2_2 = Sub.Chap.Proc)
dfcm04$PROC2_2 [is.na(dfcm04$PROC2_2)] <- ""
dfcm04 <- left_join(dfcm04, PROC,
                    by = c("PROC2_3"="No.Sub.Chap")) %>%
  select(-PROC2_3, -Chap.Proc) %>%
  rename(PROC2_3 = Sub.Chap.Proc)
dfcm04$PROC2_3 [is.na(dfcm04$PROC2_3)] <- ""
#---------------------------------------------------------------------------
#Kode ICD9CM untuk Procedure Chapter

dfcm04$PROC3_1 <- factor(dfcm04$PROC3_1)
dfcm04$PROC3_2 <- factor(dfcm04$PROC3_2)
dfcm04$PROC3_3 <- factor(dfcm04$PROC3_3)
dfcm04$PROC3_4 <- factor(dfcm04$PROC3_4)

dfcm05 <- left_join(dfcm04, PROC2, by = c("PROC3_1"="No.Sub.Chap")) %>%
  select(-PROC3_1, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_1 = Sub.Chap.Proc)
dfcm05$PROC3_1 [is.na(dfcm05$PROC3_1)] <- ""
rm(dfcm04)

dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_2"="No.Sub.Chap")) %>%
  select(-PROC3_2, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_2 = Sub.Chap.Proc)
dfcm05$PROC3_2 [is.na(dfcm05$PROC3_2)] <- ""
dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_3"="No.Sub.Chap")) %>%
  select(-PROC3_3, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_3 = Sub.Chap.Proc)
dfcm05$PROC3_3 [is.na(dfcm05$PROC3_3)] <- ""
dfcm05 <- left_join(dfcm05, PROC2, by = c("PROC3_4"="No.Sub.Chap")) %>%
  select(-PROC3_4, -Prosedur, -Chap.Proc) %>%
  rename(PROC3_4 = Sub.Chap.Proc)
dfcm05$PROC3_4 [is.na(dfcm05$PROC3_4)] <- ""

dfcm05 <- dfcm05 %>%
  mutate(DIAGP = as.factor(substr(dfcm05$DU,1,3)))

rm(DIAGFKTP,PROC,PROC2)

dfcm05 <- dfcm05 %>%
  select(-DS01,-DS02,-DS03,-DS04,-DS05,-DS06,-DS07,-Proc01,-Proc02,
         -Proc03,-Proc04)
dfcm05 <- dfcm05[order(dfcm05$Nokapst, dfcm05$End),]

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
save(dfcm05, file="dfcm05.rda")

write.csv(dfcm05, "D:/data gresik/MTF KC GRESIK/dfcm.csv",
          na="", row.names = FALSE)


data$DIAGSEK <- paste(data$DS01,data$DS02,data$DS03,data$DS04,
                      data$DS05,data$DS06,data$DS07,data$DS08,
                      data$DS09,data$DS10,data$DS11,data$DS12,
                      sep=", ")
data$DIAGSEK <- gsub("NA, ","",data$DIAGSEK)
data$DIAGSEK <- gsub(", NA","",data$DIAGSEK)
data$DIAGSEK <- gsub("NA","",data$DIAGSEK)
data$DIAGSEK <- sapply(data$DIAGSEK, 
                    function(x) paste(unique(unlist(str_split(x,", "))), 
                                      collapse = ", "))
#=================================================================
#--------------------------LEFT JOIN------------------------------
#=================================================================

colnames(data)
data00 <- data %>%
  mutate(DP = as.factor(substr(data$DU,1,3)),
         DS_01 = as.factor(substr(data$DS01,1,3)),
         DS_02 = as.factor(substr(data$DS02,1,3)),
         DS_03 = as.factor(substr(data$DS03,1,3)),
         DS_04 = as.factor(substr(data$DS04,1,3)),
         DS_05 = as.factor(substr(data$DS05,1,3)),
         DS_06 = as.factor(substr(data$DS06,1,3)),
         DS_07 = as.factor(substr(data$DS07,1,3)),
         DS_08 = as.factor(substr(data$DS08,1,3)),
         DS_09 = as.factor(substr(data$DS09,1,3)),
         DS_10 = as.factor(substr(data$DS10,1,3)),
         DS_11 = as.factor(substr(data$DS11,1,3)),
         DS_12 = as.factor(substr(data$DS12,1,3)))

rm(data)

data00$DIAG3 <- paste(data00$DP,data00$DS_01,data00$DS_02,data00$DS_03, 
                   data00$DS_04,data00$DS_05,data00$DS_06,data00$DS_07,
                   data00$DS_08,data00$DS_09,data00$DS_10,data00$DS_11,
                   data00$DS_12,
                   sep=", ")
data00$DIAG3 <- gsub("NA, ","",data00$DIAG3)
data00$DIAG3 <- gsub(", NA","",data00$DIAG3)
data00$DIAG3 <- sapply(data00$DIAG3, 
                    function(x) paste(unique(unlist(str_split(x,", "))), 
                                      collapse = ", "))
data00$DIAGSEK3 <- paste(data00$DS_01,data00$DS_02,data00$DS_03, 
                      data00$DS_04,data00$DS_05,data00$DS_06,
                      data00$DS_07,data00$DS_08,data00$DS_09,
                      data00$DS_10,data00$DS_11,data00$DS_12,
                      sep=", ")
data00$DIAGSEK3 <- gsub("NA, ","",data00$DIAGSEK3)
data00$DIAGSEK3 <- gsub(", NA","",data00$DIAGSEK3)
data00$DIAGSEK3 <- gsub("NA","",data00$DIAGSEK3)
data00$DIAGSEK3 <- sapply(data00$DIAGSEK3, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))

#===========================================================================

setwd("D://UR//ICD 10 & 9-CM//icd102010enMeta")

chapters <- read.delim2('chapters.txt', header = FALSE, sep = ";", dec = ".")

chapters_headings <- c('chapter_number','chapter_title')
names(chapters) <- chapters_headings
chapters$chapters <- paste(chapters$chapter_number, "-", 
                           chapters$chapter_title)
chapters <- chapters %>% select(chapter_number,chapters)

blocks <- read.delim2('blocks.txt', header = FALSE, sep = ";", dec = ".")
blocks_headings <- c('character_31a', 'character_31b', 'chapter_number',
                     'block_title')
names(blocks) <- blocks_headings
blocks$blocks <- paste(blocks$character_31a, "-", blocks$character_31b,
                       " ", blocks$block_title)
blocks <- blocks %>%
  select(character_31a,blocks)

codes <- read.delim2('codes.txt', header = FALSE, sep = ";", dec = ".")
codes_headings <- c('level_classification', 'place_classification_tree',
                    'terminal_node', 'chapter_number', 'character_31a',
                    'code_wo_dagger', 'code_wo_asterisk', 'code_wo_dot',
                    'title', 'reference_mortality_1', 'reference_mortality_2',
                    'reference_mortality_3', 'reference_mortality_4',
                    'reference_morbidity')
names(codes) <- codes_headings

codes <- codes %>%
  select(chapter_number,character_31a,code_wo_dot, title)

codes <- left_join(codes, chapters,
                   by = c("chapter_number"="chapter_number"),
                   all.x = TRUE) %>%
  select(-chapter_number)

codes <- left_join(codes, blocks,
                   by = c("character_31a"="character_31a"),
                   all.x = TRUE) %>%
  select(-character_31a)

data00$NmDU <- NULL
data00$NmDM <- NULL

library(data.table)
library(caret)

data01 <- preProcess(as.data.frame(data01))

data01 <- left_join(data00, codes,
                    by = c("DU"="code_wo_dot"),all.x = TRUE)

data01 <- data01 %>%
  rename(NmDU = title, ChapDU = chapters,
         DP = blocks, DiaU = codes)

data01 <- left_join(data01, codes,
                    by = c("DS01"="code_wo_dot"),all.x = TRUE) %>%
  select(-title) %>%
  rename(ChapDS01 = chapters,
         blocksDS01 = blocks, DS01 = codes)
data01$DS01 [is.na(data01$DS01)] <- ""

data01 <- left_join(data01, codes,
                    by = c("DS02"="code_wo_dot"),all.x = TRUE) %>%
  select(-title) %>%
  rename(ChapDS02 = chapters,
         blocksDS02 = blocks, DS02 = codes)
data01$DS02 [is.na(data01$DS02)] <- ""

#============================================================================

library(readxl)
setwd("D:/data gresik/UR/Data Sampel BPJS Kesehatan 2015-2020 v01/Metadata")

DIAGFKTP <- read_excel("Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx", 
                       sheet = "Sheet1", range = NULL, 
                       col_names = TRUE, col_types = NULL, na = "",
                       trim_ws = TRUE, skip = 0)

#Kode ICD10 untuk diagnosis FKTP Kapitasi.xlsx

data01 <- left_join(data00, DIAGFKTP,
                    by = c("DP"="ICD10_Code"),all.x = TRUE) %>%
  select(-DP, -FKP14) %>%
  rename(DP = ICD10_Text)

rm(data00)

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_01"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_01, -FKP14) %>%
  rename(DS_01 = ICD10_Text)
data01$DS_01 [is.na(data01$DS_01)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_02"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_02, -FKP14) %>%
  rename(DS_02 = ICD10_Text)
data01$DS_02 [is.na(data01$DS_02)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_03"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_03, -FKP14) %>%
  rename(DS_03 = ICD10_Text)
data01$DS_03 [is.na(data01$DS_03)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_04"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_04, -FKP14) %>%
  rename(DS_04 = ICD10_Text)
data01$DS_04 [is.na(data01$DS_04)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_05"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_05, -FKP14) %>%
  rename(DS_05 = ICD10_Text)
data01$DS_05 [is.na(data01$DS_05)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_06"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_06, -FKP14) %>%
  rename(DS_06 = ICD10_Text)
data01$DS_06 [is.na(data01$DS_06)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_07"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_07, -FKP14) %>%
  rename(DS_07 = ICD10_Text)
data01$DS_07 [is.na(data01$DS_07)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_08"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_08, -FKP14) %>%
  rename(DS_08 = ICD10_Text)
data01$DS_08 [is.na(data01$DS_08)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_09"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_09, -FKP14) %>%
  rename(DS_09 = ICD10_Text)
data01$DS_09 [is.na(data01$DS_09)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_10"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_10, -FKP14) %>%
  rename(DS_10 = ICD10_Text)
data01$DS_10 [is.na(data01$DS_10)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_11"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_11, -FKP14) %>%
  rename(DS_11 = ICD10_Text)
data01$DS_11 [is.na(data01$DS_11)] <- ""

data01 <- left_join(data01, DIAGFKTP,
                    by = c("DS_12"="ICD10_Code"),all.x = TRUE) %>%
  select(-DS_12, -FKP14) %>%
  rename(DS_12 = ICD10_Text)
data01$DS_12 [is.na(data01$DS_12)] <- ""

PROC <- read.csv2("sub chapter proc.csv", 
                  colClasses=c("factor", "character", "character"))
glimpse(PROC)

data02 <- data01 %>%
  mutate(Proc_01 = as.factor(substr(data01$Proc01,1,2)),
         Proc_02 = as.factor(substr(data01$Proc02,1,2)),
         Proc_03 = as.factor(substr(data01$Proc03,1,2)),
         Proc_04 = as.factor(substr(data01$Proc04,1,2)),
         Proc_05 = as.factor(substr(data01$Proc05,1,2)),
         Proc_06 = as.factor(substr(data01$Proc06,1,2)),
         Proc_07 = as.factor(substr(data01$Proc07,1,2)),
         Proc_08 = as.factor(substr(data01$Proc08,1,2)),
         Proc_09 = as.factor(substr(data01$Proc09,1,2)),
         Proc_10 = as.factor(substr(data01$Proc10,1,2)),
         Proc_11 = as.factor(substr(data01$Proc11,1,2)))

rm(data01)

data03 <- left_join(data02, PROC,
                    by = c("Proc_01"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_01, -Chap.Proc) %>%
  rename(Proc_01 = Sub.Chap.Proc)

rm(data02)

data03$Proc_01 [is.na(data03$Proc_01)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_02"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_02, -Chap.Proc) %>%
  rename(Proc_02 = Sub.Chap.Proc)
data03$Proc_02 [is.na(data03$Proc_02)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_03"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_03, -Chap.Proc) %>%
  rename(Proc_03 = Sub.Chap.Proc)
data03$Proc_03 [is.na(data03$Proc_03)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_04"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_04, -Chap.Proc) %>%
  rename(Proc_04 = Sub.Chap.Proc)
data03$Proc_04 [is.na(data03$Proc_04)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_05"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_05, -Chap.Proc) %>%
  rename(Proc_05 = Sub.Chap.Proc)
data03$Proc_05 [is.na(data03$Proc_05)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_06"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_06, -Chap.Proc) %>%
  rename(Proc_06 = Sub.Chap.Proc)
data03$Proc_06 [is.na(data03$Proc_06)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_07"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_07, -Chap.Proc) %>%
  rename(Proc_07 = Sub.Chap.Proc)
data03$Proc_07 [is.na(data03$Proc_07)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_08"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_08, -Chap.Proc) %>%
  rename(Proc_08 = Sub.Chap.Proc)
data03$Proc_08 [is.na(data03$Proc_08)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_09"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_09, -Chap.Proc) %>%
  rename(Proc_09 = Sub.Chap.Proc)
data03$Proc_09 [is.na(data03$Proc_09)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_10"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_10, -Chap.Proc) %>%
  rename(Proc_10 = Sub.Chap.Proc)
data03$Proc_10 [is.na(data03$Proc_10)] <- ""

data03 <- left_join(data03, PROC,
                    by = c("Proc_11"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_11, -Chap.Proc) %>%
  rename(Proc_11 = Sub.Chap.Proc)
data03$Proc_11 [is.na(data03$Proc_11)] <- ""

PROC2 <- read.csv2("Chapter Procedure.csv", 
                  colClasses=c("factor", "character",
                               "character", "character"))

data04 <- data03 %>%
  mutate(Proc_001 = as.factor(substr(data03$Proc01,1,3)),
         Proc_002 = as.factor(substr(data03$Proc02,1,3)),
         Proc_003 = as.factor(substr(data03$Proc03,1,3)),
         Proc_004 = as.factor(substr(data03$Proc04,1,3)),
         Proc_005 = as.factor(substr(data03$Proc05,1,3)),
         Proc_006 = as.factor(substr(data03$Proc06,1,3)),
         Proc_007 = as.factor(substr(data03$Proc07,1,3)),
         Proc_008 = as.factor(substr(data03$Proc08,1,3)),
         Proc_009 = as.factor(substr(data03$Proc09,1,3)),
         Proc_010 = as.factor(substr(data03$Proc10,1,3)),
         Proc_011 = as.factor(substr(data03$Proc11,1,3)))

rm(data03)

data04$PROC3 <- paste(data04$Proc_001,data04$Proc_002,data04$Proc_003,data04$Proc_004,
                      data04$Proc_005,data04$Proc_006,data04$Proc_007,data04$Proc_008,
                      data04$Proc_009,data04$Proc_010,data04$Proc_011,sep=", ")
data04$PROC3 <- gsub("NA, ","",data04$PROC3)
data04$PROC3 <- gsub(", NA","",data04$PROC3)
data04$PROC3 <- gsub("NA","",data04$PROC3)
data04$PROC3 <- sapply(data04$PROC3, 
                     function(x) paste(unique(unlist(str_split(x,", "))), 
                                       collapse = ", "))

data05 <- left_join(data04, PROC2,
                    by = c("Proc_001"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_001, -Prosedur, -Chap.Proc) %>%
  rename(Proc_001 = Sub.Chap.Proc)

rm(data04)

data05$Proc_001 [is.na(data05$Proc_001)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_002"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_002, -Prosedur, -Chap.Proc) %>%
  rename(Proc_002 = Sub.Chap.Proc)
data05$Proc_002 [is.na(data05$Proc_002)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_003"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_003, -Prosedur, -Chap.Proc) %>%
  rename(Proc_003 = Sub.Chap.Proc)
data05$Proc_003 [is.na(data05$Proc_003)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_004"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_004, -Prosedur, -Chap.Proc) %>%
  rename(Proc_004 = Sub.Chap.Proc)
data05$Proc_004 [is.na(data05$Proc_004)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_005"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_005, -Prosedur, -Chap.Proc) %>%
  rename(Proc_005 = Sub.Chap.Proc)
data05$Proc_005 [is.na(data05$Proc_005)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_006"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_006, -Prosedur, -Chap.Proc) %>%
  rename(Proc_006 = Sub.Chap.Proc)
data05$Proc_006 [is.na(data05$Proc_006)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_007"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_007, -Prosedur, -Chap.Proc) %>%
  rename(Proc_007 = Sub.Chap.Proc)
data05$Proc_007 [is.na(data05$Proc_007)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_008"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_008, -Prosedur, -Chap.Proc) %>%
  rename(Proc_008 = Sub.Chap.Proc)
data05$Proc_008 [is.na(data05$Proc_008)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_009"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_009, -Prosedur, -Chap.Proc) %>%
  rename(Proc_009 = Sub.Chap.Proc)
data05$Proc_009 [is.na(data05$Proc_009)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_010"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_010, -Prosedur, -Chap.Proc) %>%
  rename(Proc_010 = Sub.Chap.Proc)
data05$Proc_010 [is.na(data05$Proc_010)] <- ""

data05 <- left_join(data05, PROC2,
                    by = c("Proc_011"="No.Sub.Chap"),all.x = TRUE) %>%
  select(-Proc_011, -Prosedur, -Chap.Proc) %>%
  rename(Proc_011 = Sub.Chap.Proc)
data05$Proc_011 [is.na(data05$Proc_011)] <- ""

#LALI
data05 <- data05 %>%
  mutate(DIAGP = as.factor(substr(data05$DU,1,3)))

#==========================================================================
dataklaim <- data05 %>%
  select(Nokapst,Jkpst,Segmen,kdpst,pisa,Kelashak,Umur,
         Rangeumur,Nmdati2Terdaftar,Kdppkterdaftar,Nmppkterdaftar,
         Kdpks,Nmpks,prb,Nmdati2Perujuk,Kdppkperujuk,Nmppkperujuk,
         SEP,Nmdati2Layan,Kdppk,NmFKRTL,Pemilik,Kelasrsmenkes,Klsrawat,
         Nmtkp,Prvtkp,interval,Poli,DM,NmDM,Tglpelayanan,Start,End,
         Tglreg,Tglstjkeu,CBG,Kdinacbgs,CMG,Nmcmg,Nminacbgs,tipe,Spec,
         Sevel,DIAG,PROC,DU,DP,NmDU,DS,Noreg,DS01,DS_01,DS02,DS_02,DS03,
         DS_03,DS04,DS_04,DS05,DS_05,DS06,DS_06,DS07,DS_07,DS08,DS_08,
         DS09,DS_09,DS10,DS_10,DS11,DS_11,DS12,DS_12,DIAGP,DIAG3,DIAGSEK,
         DIAGSEK3,Jmlproc,PROC3,Procedure,Proc01,Proc_01,Proc02,Proc_02,
         Proc03,Proc_03,Proc04,Proc_04,Proc05,Proc_05,Proc06,Proc_06,
         Proc07,Proc_07,Proc08,Proc_08,Proc09,Proc_09,Proc10,Proc_10,
         Proc11,Proc_11,Proc_001,Proc_002,Proc_003,Proc_004,Proc_005,
         Proc_006,Proc_007,Proc_008,Proc_009,Proc_010,Proc_011,DPJP,
         Nmkronis,Nmkatastrofik,Kdjnsplg,LOS,Tarif,BiayaRS,Biaya,
         maxcase,maxuc)

dataklaim <- dataklaim[order(dataklaim$Nokapst, dataklaim$End),]
rm(data05)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
save(dataklaim, file="dataklaim.rda")

#=================================================================
#--------------------------LEFT JOIN------------------------------
#=================================================================

#4470 - AV shunt RJTL
## SUBSET
data_Q5 <- data %>%
  subset(Nmtkp == "RITL")


data_01 <- data_Q5 %>% 
  arrange(Nokapst, Start, End) %>% 
  group_by(Nokapst) %>%
  mutate(LOS = 1 + difftime(End, Start, units = 'days')) %>%
  mutate(laggedTimeElapsed = difftime(Start, lag(End),
                                      units = 'days'))

data_01 = data_01 %>% rename(DPJP = Namadpjp01)

#DATA_02 = koding variabel
data_02 <- data_01 %>%
  select(Nokapst, Tglpelayanan, Start, End, laggedTimeElapsed, Jkpst, Segmen,
         Kelashak, Umur, SEP, NmFKRTL, Kelasrsmenkes, Klsrawat, Nmtkp, Poli,
         CBG, CMG, tipe, Spec, Sevel, DU, NmDU, DS, DS01, DS02, DS03, DS04, 
         DS05, DS06, DS07, DS08, DS09, DS10, Procedure, Proc01, Proc02, 
         Proc03, Proc04, Proc05, Proc06, Proc07, Proc08, Proc09, Proc10, 
         Proc11, DPJP, Nmkatastrofik, Kdjnsplg, BiayaRS, Biaya, LOS)

#Transform Jkpst menjadi 1&0 --> 0= berarti Laki-laki, 1= berarti Perempuan
data_02$Jkpst[data_02$Jkpst=="Laki-laki"] <- "0"
data_02$Jkpst[data_02$Jkpst=="Perempuan"] <- "1"

#Transform Nmtkp menjadi 1&2 --> 1= berarti RITL, 2= berarti RJTL
data_02$Nmtkp[data_02$Nmtkp=="RITL"] <- "1"
data_02$Nmtkp[data_02$Nmtkp=="RJTL"] <- "2"

#Transform Segmen menjadi 1&0 --> 0= berarti PBI, 1= berarti Non-PBI
data_02$Segmen[data_02$Segmen=="PBI APBN"| 
                 data_02$Segmen=="PBI APBD"]<- "0"
data_02$Segmen[data_02$Segmen=="BP" |
                 data_02$Segmen=="PBPU" |
                 data_02$Segmen=="PPU BU" |
                 data_02$Segmen=="PPU PN" ] <- "1"

#Transform Kelashak menjadi 1,2,3
data_02$Kelashak[data_02$Kelashak=="Kelas I"] <- "1"
data_02$Kelashak[data_02$Kelashak=="Kelas II"] <- "2"
data_02$Kelashak[data_02$Kelashak=="Kelas III"] <- "3"

#Transform Klsrawat menjadi 1,2,3
data_02$Klsrawat[data_02$Klsrawat=="Kelas I"] <- "1"
data_02$Klsrawat[data_02$Klsrawat=="Kelas II"] <- "2"
data_02$Klsrawat[data_02$Klsrawat=="Kelas III"] <- "3"

glimpse(data_02)


#DATA_03 = Gabung DIAG & PROC
#remove empty cell with NA
data_03 <- na_if(data_02, "")
data_03$DIAG <- paste(data_03$DU,data_03$DS01,data_03$DS02,data_03$DS03, 
                      data_03$DS04,data_03$DS05,data_03$DS06,data_03$DS07, 
                      data_03$DS08,data_03$DS09, sep=", ")
data_03$DIAG <- gsub("NA, ","",data_03$DIAG)
data_03$DIAG <- gsub(", NA","",data_03$DIAG)
data_03$DIAG <- sapply(data_03$DIAG, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
#nanti dihapus yaaa..
data_03$DIAG3 <- paste(data_03$DP,data_03$DS_01,data_03$DS_02,data_03$DS_03, 
                       data_03$DS_04,data_03$DS_05,data_03$DS_06,data_03$DS_07, 
                       data_03$DS_08,data_03$DS_09, sep=", ")
data_03$DIAG3 <- gsub("NA, ","",data_03$DIAG3)
data_03$DIAG3 <- gsub(", NA","",data_03$DIAG3)
data_03$DIAG3 <- sapply(data_03$DIAG3, 
                        function(x) paste(unique(unlist(str_split(x,", "))), 
                                          collapse = ", "))


data_03$PROC <- paste(data_03$Proc01,data_03$Proc02,data_03$Proc03,data_03$Proc04,
                      data_03$Proc05,sep=", ")
data_03$PROC <- gsub("NA, ","",data_03$PROC)
data_03$PROC <- gsub(", NA","",data_03$PROC)
data_03$PROC <- gsub("NA","",data_03$PROC)
data_03$PROC <- sapply(data_03$PROC, 
                       function(x) paste(unique(unlist(str_split(x,", "))), 
                                         collapse = ", "))
#nanti dihapus yaaa..
data_03$PROC2 <- paste(data_03$Proc_01,data_03$Proc_02,data_03$Proc_03,data_03$Proc_04,
                       data_03$Proc_05,sep=", ")
data_03$PROC2 <- gsub("NA, ","",data_03$PROC2)
data_03$PROC2 <- gsub(", NA","",data_03$PROC2)
data_03$PROC2 <- gsub("NA","",data_03$PROC2)
data_03$PROC2 <- sapply(data_03$PROC2, 
                        function(x) paste(unique(unlist(str_split(x,", "))), 
                                          collapse = ", "))

data_03 <- na_if(data_03, "")

colnames(data_03)

SEP_DIAG <- data_03 %>% 
  select(SEP, DU, DS01, DS02, DS03, DS04, DS05, DS06, DS07, DS08, DS09,
         DP, DS_01, DS_02, DS_03, DS_04, DS_05, DS_06, DS_07, DS_08, DS_09,
         Proc01, Proc02, Proc03, Proc04, Proc05, Proc_01, Proc_02, Proc_03,
         Proc_04, Proc_05)

SEP_DIAG01 <- SEP_DIAG %>% 
  select(SEP, DU, DS01, DP, DS_01) %>% 
  rename(DS = DS01, DS3 = DS_01) %>%
  na.omit()

SEP_DIAG02 <- SEP_DIAG %>% 
  select(SEP, DU, DS02, DP, DS_02) %>% 
  rename(DS = DS02, DS3 = DS_02) %>%
  na.omit()

SEP_DIAG03 <- SEP_DIAG %>% 
  select(SEP, DU, DS03, DP, DS_03) %>% 
  rename(DS = DS03, DS3 = DS_03) %>%
  na.omit()

SEP_DIAG04 <- SEP_DIAG %>% 
  select(SEP, DU, DS04, DP, DS_04) %>% 
  rename(DS = DS04, DS3 = DS_04) %>%
  na.omit()

SEP_DIAG05 <- SEP_DIAG %>% 
  select(SEP, DU, DS05, DP, DS_05) %>% 
  rename(DS = DS05, DS3 = DS_05) %>%
  na.omit()

SEP_DIAG06 <- SEP_DIAG %>% 
  select(SEP, DU, DS06, DP, DS_06) %>% 
  rename(DS = DS06, DS3 = DS_06) %>%
  na.omit()

SEP_DIAG07 <- SEP_DIAG %>% 
  select(SEP, DU, DS07, DP, DS_07) %>% 
  rename(DS = DS07, DS3 = DS_07) %>%
  na.omit()

SEP_DIAG08 <- SEP_DIAG %>% 
  select(SEP, DU, DS08, DP, DS_08) %>% 
  rename(DS = DS08, DS3 = DS_08) %>%
  na.omit()

SEP_DIAG09 <- SEP_DIAG %>% 
  select(SEP, DU, DS09, DP, DS_09) %>% 
  rename(DS = DS09, DS3 = DS_09) %>%
  na.omit()

SEP_PROC01 <- SEP_DIAG %>% 
  select(SEP, DU, Proc01, DP, Proc_01) %>% 
  rename(PROC = Proc01, PROC2 = Proc_01) %>%
  na.omit()

SEP_PROC02 <- SEP_DIAG %>% 
  select(SEP, DU, Proc02, DP, Proc_02) %>% 
  rename(PROC = Proc02, PROC2 = Proc_02) %>%
  na.omit()

SEP_PROC03 <- SEP_DIAG %>% 
  select(SEP, DU, Proc03, DP, Proc_03) %>% 
  rename(PROC = Proc03, PROC2 = Proc_03) %>%
  na.omit()

SEP_PROC04 <- SEP_DIAG %>% 
  select(SEP, DU, Proc04, DP, Proc_04) %>% 
  rename(PROC = Proc04, PROC2 = Proc_04) %>%
  na.omit()

SEP_PROC05 <- SEP_DIAG %>% 
  select(SEP, DU, Proc05, DP, Proc_05) %>% 
  rename(PROC = Proc05, PROC2 = Proc_05) %>%
  na.omit()

SEP_DIAG00 <- rbind(SEP_DIAG01, SEP_DIAG02, SEP_DIAG03, SEP_DIAG04, SEP_DIAG05,
                    SEP_DIAG06, SEP_DIAG07, SEP_DIAG08, SEP_DIAG09) %>%
  arrange(SEP)

SEP_PROC00 <- rbind(SEP_PROC01, SEP_PROC02, SEP_PROC03, SEP_PROC04, SEP_PROC05) %>%
  arrange(SEP)

df <- merge(x = SEP_DIAG00, y = SEP_PROC00, by = c( "SEP","DU", "DP"), 
            all = TRUE) %>%
  arrange(SEP)
save(df, file = "dfdiagproc.rda")

save(data_03, file = "Q455.rda")

data_03 <- data_03 %>%
  select(Nokapst, Tglpelayanan, Start, End, Jkpst, Segmen, Kelashak, SEP, 
         NmFKRTL, Kelasrsmenkes, Klsrawat, CBG, Sevel, DIAG, PROC, DPJP, 
         Kdjnsplg, BiayaRS, Biaya, LOS)

data_03 <- data_03 %>% 
  filter(str_detect(PROC, c("4470")))


%>%
  subset(Nmtkp == 1)

data_03 <- data_03 %>% 
  filter(str_detect(DIAG, c("P030|P031|P032|P033|P034|P035|P036"))) %>%
  subset(Klsrawat != 3)

glimpse(data_03)
colnames(data_03)

data_03 <- data_03 %>%
  filter(str_detect(DIAG, c("B342")))


%>%
  subset(Kdjnsplg == 2)

data_03 <- subset(data_03, data_03$Tglpelayanan >= "2020-03-01")

%>%
  filter(Kdjnsplg == 2)

write.csv(data, "D://dataextractdumai//buban bojonegoro//2014-2021//data_lap_kewil.csv",
          na="", row.names = FALSE)

df_DS = read_csv("data_lap_kewil.csv") %>% select(SEP, DS)

library(splitstackshape)
df_DS <- concat.split(df_DS, "DS", ";")
df_DS$DS01 <- as.character(trimws(df_DS$DS_01, "both"))
df_DS$DS02 <- as.character(trimws(df_DS$DS_02, "both"))
df_DS$DS03 <- as.character(trimws(df_DS$DS_03, "both"))
df_DS$DS04 <- as.character(trimws(df_DS$DS_04, "both"))
df_DS$DS05 <- as.character(trimws(df_DS$DS_05, "both"))
df_DS$DS06 <- as.character(trimws(df_DS$DS_06, "both"))
df_DS$DS07 <- as.character(trimws(df_DS$DS_07, "both"))
df_DS$DS08 <- as.character(trimws(df_DS$DS_08, "both"))
df_DS$DS09 <- as.character(trimws(df_DS$DS_09, "both"))
df_DS$DS10 <- as.character(trimws(df_DS$DS_10, "both"))
df_DS <- df_DS %>% select(-2:-12)

df.DS01 = df_DS %>% select(SEP, DS01)
df.DS01 = df.DS01 %>% rename(DS = DS01)
df.DS01 = na.omit(df.DS01)
df.DS02 = df_DS %>% select(SEP, DS02)
df.DS02 = df.DS02 %>% rename(DS = DS02)
df.DS02 = na.omit(df.DS02)
df.DS03 = df_DS %>% select(SEP, DS03)
df.DS03 = df.DS03 %>% rename(DS = DS03)
df.DS03 = na.omit(df.DS03)
df.DS04 = df_DS %>% select(SEP, DS04)
df.DS04 = df.DS04 %>% rename(DS = DS04)
df.DS04 = na.omit(df.DS04)
df.DS05 = df_DS %>% select(SEP, DS05)
df.DS05 = df.DS05 %>% rename(DS = DS05)
df.DS05 = na.omit(df.DS05)
df.DS06 = df_DS %>% select(SEP, DS06)
df.DS06 = df.DS06 %>% rename(DS = DS06)
df.DS06 = na.omit(df.DS06)
df.DS07 = df_DS %>% select(SEP, DS07)
df.DS07 = df.DS07 %>% rename(DS = DS07)
df.DS07 = na.omit(df.DS07)
df.DS08 = df_DS %>% select(SEP, DS08)
df.DS08 = df.DS08 %>% rename(DS = DS08)
df.DS08 = na.omit(df.DS08)
df.DS09 = df_DS %>% select(SEP, DS09)
df.DS09 = df.DS09 %>% rename(DS = DS09)
df.DS09 = na.omit(df.DS09)
df.DS10 = df_DS %>% select(SEP, DS10)
df.DS10 = df.DS10 %>% rename(DS = DS10)
df.DS10 = na.omit(df.DS10)

df.DS <- rbind(df.DS01, df.DS02, df.DS03, df.DS04, df.DS05, df.DS06, df.DS07, 
               df.DS08, df.DS09, df.DS10)

write.csv(df.DS, "D://dataextractdumai//buban bojonegoro//2014-2021//DS.csv",
          na="", row.names = FALSE)

df_Proc = read_csv("data_lap_kewil.csv") %>% select(SEP, Procedure)
df_Proc <- concat.split(df_Proc, "Procedure", ";")

df_Proc$Proc01 <- as.character(trimws(df_Proc$Procedure_01, "both"))
df_Proc$Proc02 <- as.character(trimws(df_Proc$Procedure_02, "both"))
df_Proc$Proc03 <- as.character(trimws(df_Proc$Procedure_03, "both"))
df_Proc$Proc04 <- as.character(trimws(df_Proc$Procedure_04, "both"))
df_Proc$Proc05 <- as.character(trimws(df_Proc$Procedure_05, "both"))
df_Proc$Proc06 <- as.character(trimws(df_Proc$Procedure_06, "both"))
df_Proc$Proc07 <- as.character(trimws(df_Proc$Procedure_07, "both"))
df_Proc$Proc08 <- as.character(trimws(df_Proc$Procedure_08, "both"))
df_Proc$Proc09 <- as.character(trimws(df_Proc$Procedure_09, "both"))
df_Proc$Proc10 <- as.character(trimws(df_Proc$Procedure_10, "both"))
df_Proc$Proc11 <- as.character(trimws(df_Proc$Procedure_11, "both"))
df_Proc <- df_Proc %>% select(-2:-13)

df.Proc01 = df_Proc %>% select(SEP, Proc01)
df.Proc01 = df.Proc01 %>% rename(Proc = Proc01)
df.Proc01 = na.omit(df.Proc01)
df.Proc02 = df_Proc %>% select(SEP, Proc02)
df.Proc02 = df.Proc02 %>% rename(Proc = Proc02)
df.Proc02 = na.omit(df.Proc02)
df.Proc03 = df_Proc %>% select(SEP, Proc03)
df.Proc03 = df.Proc03 %>% rename(Proc = Proc03)
df.Proc03 = na.omit(df.Proc03)
df.Proc04 = df_Proc %>% select(SEP, Proc04)
df.Proc04 = df.Proc04 %>% rename(Proc = Proc04)
df.Proc04 = na.omit(df.Proc04)
df.Proc05 = df_Proc %>% select(SEP, Proc05)
df.Proc05 = df.Proc05 %>% rename(Proc = Proc05)
df.Proc05 = na.omit(df.Proc05)
df.Proc06 = df_Proc %>% select(SEP, Proc06)
df.Proc06 = df.Proc06 %>% rename(Proc = Proc06)
df.Proc06 = na.omit(df.Proc06)
df.Proc07 = df_Proc %>% select(SEP, Proc07)
df.Proc07 = df.Proc07 %>% rename(Proc = Proc07)
df.Proc07 = na.omit(df.Proc07)
df.Proc08 = df_Proc %>% select(SEP, Proc08)
df.Proc08 = df.Proc08 %>% rename(Proc = Proc08)
df.Proc08 = na.omit(df.Proc08)
df.Proc09 = df_Proc %>% select(SEP, Proc09)
df.Proc09 = df.Proc09 %>% rename(Proc = Proc09)
df.Proc09 = na.omit(df.Proc09)
df.Proc10 = df_Proc %>% select(SEP, Proc10)
df.Proc10 = df.Proc10 %>% rename(Proc = Proc10)
df.Proc10 = na.omit(df.Proc10)
df.Proc11 = df_Proc %>% select(SEP, Proc11)
df.Proc11 = df.Proc11 %>% rename(Proc = Proc11)
df.Proc11 = na.omit(df.Proc11)