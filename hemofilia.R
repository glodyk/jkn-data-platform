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
library(data.table)

data16 <- read_csv("Sheet 1_Full Data gresik 2016.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

data17 <- read_csv("Sheet 1_Full Data gresik 2017.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

data18 <- read_csv("Sheet 1_Full Data gresik 2018.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

data19 <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)
  
data20 <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

data21 <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

data22 <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

datamei24 <- read_csv("Sheet 1_Full Data gresik mei_jul 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

dataagu24 <- read_csv("Sheet 1_Full Data gresik agu_sep 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Nmdati2Layan,Nmppkperujuk,Kdppklayan,Nosjp,
          Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,Tarifgroup,
          biayars,Biayaverifikasi) %>%
  subset(kdsd %in% c("DD-10-II","DD-06-II")) %>%
  subset(Kdppklayan %in% c("0204R012","0204R019","0204R010","0205R012","1302R001","1306R001")) %>%
  mutate(koreksi = Biayaverifikasi - Tarifgroup)

data <- rbind(data16,data17,data18,data19,data20,data21,data22,data23,
              data24,dataapr24,datamei24,dataagu24)
rm(data16,data17,data18,data19,data20,data21,data22,data23,
   data24,dataapr24,datamei24,dataagu24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data <- data %>% subset(Tglpelayanan >= "2018-01-01")

data <- data %>% 
  mutate(
    Nmppklayan = case_when(
      Kdppklayan == "0204R003" ~ "RSUD Ngimbang",
      Kdppklayan == "0204R004" ~ "RS Suyudi Paciran (JST)",
      Kdppklayan == "0204R009" ~ "RSI Nashrul Ummah",
      Kdppklayan == "0204R010" ~ "RS Muhammadiyah Lamongan",
      Kdppklayan == "0204R012" ~ "RS Muhammadiyah Babat",
      Kdppklayan == "0204R019" ~ "RSU Muhammadiyah Babat",
      Kdppklayan == "0204R013" ~ "RS Fatimah",
      Kdppklayan == "0204R014" ~ "RS Intan Medika",
      Kdppklayan == "0204R015" ~ "RS Arsy Paciran",
      Kdppklayan == "0204R017" ~ "RS Citra Medika",
      Kdppklayan == "0204R018" ~ "RS Bedah Mitra Sehat",
      Kdppklayan == "0204S001" ~ "Klinik Mata Utama Lamongan",
      Kdppklayan == "0205R009" ~ "RS Denisa",
      Kdppklayan == "0205R011" ~ "RS Muhammadiyah Gresik",
      Kdppklayan == "0205R012" ~ "RS Semen Gresik",
      Kdppklayan == "0205R013" ~ "RS PKG Grha Husada",
      Kdppklayan == "0205R014" ~ "RS Petrokimia Driyorejo",
      Kdppklayan == "0205R019" ~ "RS Wali Songo I",
      Kdppklayan == "0205R021" ~ "RS Fathma Medika",
      Kdppklayan == "0205R022" ~ "RS Wates Husada",
      Kdppklayan == "0205R023" ~ "RS Surya Medika",
      Kdppklayan == "0205R024" ~ "RS PKU Muhammadiyah Sekapuk",
      Kdppklayan == "0205R025" ~ "RSI Mabarrot MWC NU Bungah",
      Kdppklayan == "0205R026" ~ "RS Rachmi Dewi",
      Kdppklayan == "0205R027" ~ "RSI Nyai Ageng Pinatih",
      Kdppklayan == "0205R028" ~ "RSI Cahaya Giri",
      Kdppklayan == "0205R029" ~ "RSUD Umar Mas'ud Bawean",
      Kdppklayan == "0205S100" ~ "Klinik Mata Utama",
      Kdppklayan == "1302R001" ~ "RSUD Ibnu Sina",
      Kdppklayan == "1302R002" ~ "RS Petrokimia Gresik",
      Kdppklayan == "1306R001" ~ "RSUD Dr Soegiri",
      Kdppklayan == "0205R031" ~ "RS Eka Husada",
      Kdppklayan == "0205R030" ~ "RS Randegansari Husada",
      Kdppklayan == "0204R020" ~ "RS Permata Bunda",
      Kdppklayan == "0204R021" ~ "RSUD Karangkembang",
      Kdppklayan == "0204R022" ~ "RS Nahdlatul Ulama Babat",
      Kdppklayan == "0204R023" ~ "RS Permata Hati",
      Kdppklayan == "0204R024" ~ "RS Muhammadiyah Kalikapas",
      Kdppklayan == "0205R032" ~ "RSIA Khodijah",
      Kdppklayan == "0205S101" ~ "Klinik Utama Gresik",
      TRUE ~ Kdppklayan)) %>%
  select(-Kdppklayan)

data <- data %>%
  select(Nokapst,Umur,Jkpst,Nmppkperujuk,Nmdati2Layan,Nmppklayan,Nosjp,
         Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
         Namadpjp,Nmjnspulang,Tarifgroup,biayars,Biayaverifikasi,koreksi)

noka <- data %>%
  select(Nokapst) %>% unique()

data1 <- data %>% subset(Nmppklayan == "RS Semen Gresik")
data1 <- data1[order(data1$Nokapst, data1$Tglplgsjp),]
data1 <- data1 %>% group_by (Nokapst,Tglpelayanan) %>% 
  dplyr :: mutate (kunjungan =  sequence(n()))

data2 <- data %>% subset(Nmppklayan == "RS Muhammadiyah Lamongan")
data2 <- data2[order(data2$Nokapst, data2$Tglplgsjp),]
data2 <- data2 %>% group_by (Nokapst,Tglpelayanan) %>% 
  dplyr :: mutate (kunjungan =  sequence(n()))

data3 <- data %>% subset(Nmppklayan == "RSUD Ibnu Sina")
data3 <- data3[order(data3$Nokapst, data3$Tglplgsjp),]
data3 <- data3 %>% group_by (Nokapst,Tglpelayanan) %>% 
  dplyr :: mutate (kunjungan =  sequence(n()))

data4 <- data %>% subset(Nmppklayan == "RSUD Dr Soegiri")
data4 <- data4[order(data4$Nokapst, data4$Tglplgsjp),]
data4 <- data4 %>% group_by (Nokapst,Tglpelayanan) %>% 
  dplyr :: mutate (kunjungan =  sequence(n()))

#========================================================================
write.xlsx(data4, file = "Hemofilia RSUD Dr Soegiri.xlsx")
#========================================================================


data$Nokapst <- as.factor(data$Nokapst)
data <- data %>% 
  mutate(
    Noka = case_when(
      Nokapst == "5311710" ~ "0000752555349",
      Nokapst == "12238932" ~ "0000752556126",
      Nokapst == "15662971" ~ "0001046253936",
      Nokapst == "25569126" ~ "0000761487052",
      Nokapst == "32268888" ~ "0002226944496",
      Nokapst == "45880048" ~ "0001290615579",
      Nokapst == "55218362" ~ "0000205878216",
      Nokapst == "55977904" ~ "0001902488703",
      Nokapst == "75545145" ~ "0001261001384",
      Nokapst == "78495593" ~ "0001285398999",
      Nokapst == "99779552" ~ "0001260998256",
      Nokapst == "311674889" ~ "0002935455311",
      Nokapst == "349954526" ~ "0003081959739",
      Nokapst == "398594093" ~ "0002695264716",
      Nokapst == "409537623" ~ "0003085378367",
      Nokapst == "419702420" ~ "0001109148197",
      Nokapst == "455499014" ~ "0003108366955",
      Nokapst == "455499932" ~ "0003108366753",
      Nokapst == "494651867" ~ "0003563589824",
      TRUE ~ Nokapst))

data <- data %>% 
  mutate(
    NamaPst = case_when(
      Nokapst == "5311710" ~ "MUHAMMAD ABIB",
      Nokapst == "12238932" ~ "MOHAMAD SAEFUL MUNIR",
      Nokapst == "15662971" ~ "SUEDI PRAPTO",
      Nokapst == "25569126" ~ "AGUS SUPARDI",
      Nokapst == "32268888" ~ "IDAM RIZKI",
      Nokapst == "45880048" ~ "SHYLVILIO JHONES RANDE",
      Nokapst == "55218362" ~ "MUFTI NAUFAL",
      Nokapst == "55977904" ~ "MUHAMMAD ASY'ARI",
      Nokapst == "75545145" ~ "GHIFFARI ACHMAD FERDIANSYAH",
      Nokapst == "78495593" ~ "DAFFA ISLAMY PUTRANTO",
      Nokapst == "99779552" ~ "A. SYIFA ALFIAN NUR ROHMAN",
      Nokapst == "311674889" ~ "MUH. SYAIFUDIN ZUHRI",
      Nokapst == "349954526" ~ "DANIEL PRAMONO",
      Nokapst == "398594093" ~ "MUHAMMAD FAUZAN RASYID",
      Nokapst == "409537623" ~ "MUHAMMAD FAHRI ALBIANSYAH HADI",
      Nokapst == "419702420" ~ "ANDHIKA PUTRA BAGASKARA",
      Nokapst == "455499014" ~ "FAREL ARDIANSYAH",
      Nokapst == "455499932" ~ "JAYADI SAFARI",
      Nokapst == "494651867" ~ "M SOKHIF AFANDI",
      TRUE ~ Nokapst)) 

data <- data %>% 
  mutate(
    Keterangan = case_when(
      Nokapst == "78495593" ~ "Surat ke RSU Muhammadiyah Babat tgl 06 November 2023",
      Nokapst == "75545145" ~ "Surat ke RSU Muhammadiyah Babat tgl 26 April 2024",
      Nokapst == "55977904" ~ "Surat ke RS Muhammadiyah Babat tgl 27 Mei 2024",
      Nokapst == "5311710" ~ "Surat ke RS Muhammadiyah Babat tgl 14 Maret 2023",
      Nokapst == "494651867" ~ "Surat ke RSU Muhammadiyah Babat tgl 13 Desember 2023",
      Nokapst == "45880048" ~ "Surat ke RSI Sumberrejo tgl 07 Juni 2021",
      Nokapst == "455499932" ~ "Surat ke RSU Muhammadiyah Babat tgl 27 Maret 2023",
      Nokapst == "455499014" ~ "Surat ke RS Muhammadiyah Babat tgl 14 Maret 2023",
      Nokapst == "419702420" ~ "Surat ke RSU Muhammadiyah Babat tgl 02 Mei 2024",
      Nokapst == "409537623" ~ "Surat ke RS Muhammadiyah Babat (ASLI) tgl 04 Juni 2021",
      Nokapst == "15662971" ~ "Surat ke RS Muhammadiyah Babat tgl 04 September 2023",
      Nokapst == "12238932" ~ "Surat ke RS Muhammadiyah Babat tgl 14 Maret 2023",
      Nokapst == "349954526" ~ "Surat ke RSU Muhammadiyah Babat tgl 05 Januari 2024"))

data <- data %>% 
  mutate(
    Protokol = case_when(
      Nokapst == "78495593" ~ "terapi Octanate 1.500 IU 2 kali per minggu",
      Nokapst == "75545145" ~ "terapi Octanine 1.000 IU 2 kali per minggu",
      Nokapst == "55977904" ~ "terapi Octanine 1.000 IU 2 kali per minggu",
      Nokapst == "5311710" ~ "terapi Octanate 1.500 IU 2 kali per minggu",
      Nokapst == "494651867" ~ "terapi Octanate 1.500 IU 2 kali per minggu",
      Nokapst == "45880048" ~ "terapi Octanate 1.000 IU 2 kali per minggu",
      Nokapst == "455499932" ~ "terapi Octanate 1.500 IU 2 kali per minggu",
      Nokapst == "455499014" ~ "terapi Octanate 1.500 IU 2 kali per minggu",
      Nokapst == "419702420" ~ "terapi Octanate 1.500 IU 2 kali per minggu",
      Nokapst == "409537623" ~ "terapi Octanate 750 IU satu kali per minggu",
      Nokapst == "15662971" ~ "terapi Octanate 1.500 IU 2 kali per minggu",
      Nokapst == "12238932" ~ "terapi Octanate 1.500 IU 2 kali per minggu",
      Nokapst == "349954526" ~ "terapi Octanate 1.500 IU 2 kali per minggu"))

data$Flag <- with(data,
                  ifelse(is.na(data$Keterangan), "Belum Ada Protokol Sama Sekali",
                         "Surat Dipalsukan"))
                  
data24 <- data %>% subset(Tglpelayanan >= "2024-05-01")


noka <- data %>%
  select(Nokapst,Nosjp,Tglpelayanan) %>%
  unique()

noka <- noka %>% subset(Tglpelayanan >= "2024-05-01") %>%
  select(Nokapst) %>%
  unique()

cek <- data %>%
  subset(Nokapst %in% c("32268888","12238932","5311710","15662971","311674889",
                        "398594093","455499932","455499014","409537623",
                        "78495593","494651867","349954526","55218362",
                        "25569126","55977904"))


cek <- data %>%
  subset(Nokapst %in% c("45880048","12238932","5311710","75545145","15662971",
                        "55977904","383544131","419702420","78495593",
                        "2984024","349954526","358592832","39051404",
                        "409537623","40577178","444546709","455499014",
                        "448070467","455499932","474736972","494607190",
                        "494651867"))


%>%
  select(-Nokapst)

cek <- data %>%
  select(Noka,NamaPst,Umur,Jkpst,Nmppkperujuk,Nmdati2Layan,Nmppklayan,Nosjp,
         Nmtkp,kdsd,deskripsisd,Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
         Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,
         Namadpjp,Nmjnspulang,Tarifgroup,biayars,Biayaverifikasi,koreksi,
         Keterangan,Protokol,Flag)
cek <- cek[order(cek$Noka, cek$Tglplgsjp),]
cek <- cek %>% group_by (Noka,Tglpelayanan) %>% 
  dplyr :: mutate (kunjungan =  sequence(n()))
#========================================================================
write.xlsx(cek, file = "Phantom Billing Hemofilia.xlsx")
#========================================================================
