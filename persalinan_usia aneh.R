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

data19a <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P"))) %>%
  filter(!str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Persalinan")

data19b <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Kuretase")

data20a <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P"))) %>%
  filter(!str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Persalinan")

data20b <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Kuretase")

data21a <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P"))) %>%
  filter(!str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Persalinan")

data21b <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Kuretase")

data22a <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P"))) %>%
  filter(!str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Persalinan")

data22b <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Kuretase")

data23a <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P"))) %>%
  filter(!str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Persalinan")

data23b <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Kuretase")

data24a <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P"))) %>%
  filter(!str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Persalinan")

data24b <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Kuretase")

dataapr24a <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P"))) %>%
  filter(!str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Persalinan")

dataapr24b <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Kuretase")

datamei24a <- read_csv("Sheet 1_Full Data gresik mei_jul 2024.csv")%>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Diagsekunder,c("Delivery|delivery"))) %>%
  filter(!str_detect(Diagsekunder,c("without delivery"))) %>%
  filter(str_detect(Procedure,c("73|74"))) %>%
  filter(!str_detect(Kdinacbgs,c("P"))) %>%
  filter(!str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Persalinan")

datamei24b <- read_csv("Sheet 1_Full Data gresik mei_jul 2024.csv") %>%
  select (Nokapst,Umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,Nmppkperujuk,
          Kdppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,
          Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
          Procedure,Namadpjp,Nmjnspulang,biayars,Biayaverifikasi) %>%
  subset(Pisapst != 4) %>%
  filter(str_detect(Procedure,c("curettage"))) %>%
  mutate(Keterangan = "Kuretase")

data <- rbind(data19a,data19b,data20a,data20b,data21a,data21b,data22a,
              data22b,data23a,data23b,data24a,data24b,dataapr24a,
              dataapr24b,datamei24a,datamei24b)
rm(data19a,data19b,data20a,data20b,data21a,data21b,data22a,
   data22b,data23a,data23b,data24a,data24b,dataapr24a,
   dataapr24b,datamei24a,datamei24b)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")
data$Tglstjkeu <- as.Date(data$Tglstjkeu, format = "%m/%d/%Y")

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

data1 <- data %>% subset(Tglpelayanan >= "2023-01-01")
data1 <- data1[order(data1$Nokapst,data1$Tglplgsjp),]

data1$Namadpjp01 <- trimws(gsub("dr.", "", tolower(data1$Namadpjp)),"both")
data1$Namadpjp01 <- trimws(gsub("dr", "", data1$Namadpjp01),"both")
data1$Namadpjp01 <- str_replace_all(data1$Namadpjp01, "[^[:alnum:]]", " ")
data1$Namadpjp01 <- trimws(gsub("sp", "sp ", data1$Namadpjp01),"both")
data1$Namadpjp01 <- gsub("\\s+"," ",data1$Namadpjp01)
data1$Namadpjp <- NULL
data1 <- data1 %>% rename(Namadpjp = Namadpjp01)
#========================================================================
data1$kel_umur <- with(data1,
                      ifelse(Umur >= 0 & Umur < 1, "< 1 Tahun",
                             ifelse(Umur >= 1 & Umur < 5, "1-4 Tahun",
                                    ifelse(Umur >= 5 & Umur < 12, "5-11 Tahun",
                                           ifelse(Umur >= 12 & Umur < 17, "12-16 Tahun",
                                                  ifelse(Umur >= 17 & Umur <= 25, "17-25 Tahun",
                                                         ifelse(Umur > 25 & Umur <= 35, "26-35 Tahun",
                                                                ifelse(Umur > 35 & Umur <= 45, "36-45 Tahun",
                                                                       ifelse(Umur > 45 & Umur <= 55, "46-55 Tahun",
                                                                              ifelse(Umur > 55 & Umur <= 65, "56-65 Tahun",
                                                                                     ifelse(Umur > 65 & Umur < 75, "66-74 Tahun",
                                                                                            ifelse(Umur >= 75 & Umur <= 90, "75-90 Tahun",
                                                                                                   ifelse(Umur > 90, "> 90 Tahun",
                                                                                                          "Salah")))))))))))))

data1 <- data1 %>%
  subset(kel_umur %in% c("< 1 Tahun","1-4 Tahun","5-11 Tahun","12-16 Tahun",
                         "56-65 Tahun","66-74 Tahun","75-90 Tahun","> 90 Tahun"))


cek <- data1 %>%
  subset(kel_umur == "Salah")
data1 <- data1 %>%
  select(Keterangan,Nokapst,Umur,kel_umur,Jkpst,Segmen,Pisapst,Nmdati2Layan,
         Nmppklayan,Nosjp,Nmtkp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Kdinacbgs,
         Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
         Nmjnspulang,biayars,Biayaverifikasi)
data1 <- data1[order(data1$Nokapst,data1$Tglplgsjp),]
#========================================================================
write.xlsx(data1, file = "Persalinan Umur Anomali.xlsx")

write.csv(data, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_all.csv",
          na="", row.names = FALSE)
#==============================================================================