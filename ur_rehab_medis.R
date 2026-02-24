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
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

data17 <- read_csv("Sheet 1_Full Data gresik 2017.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

data18 <- read_csv("Sheet 1_Full Data gresik 2018.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

data19 <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))
  
data20 <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

data21 <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

data22 <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

data23 <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

data24 <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

dataapr24 <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0"))

data <- rbind(data16,data17,data18,data19,data20,data21,
              data22,data23,data24,dataapr24)
rm(data16,data17,data18,data19,data20,data21,
   data22,data23,data24,dataapr24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data$Tglpelayanan <- as.Date(data$Tglpelayanan, format = "%m/%d/%Y")

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>% group_by(Nokapst) %>%
  mutate(Keterangan = paste0("fisiotx ke-", sequence(n())))

dat_noka <- data %>% select(Nokapst)
dat_noka <- dat_noka[!(duplicated(dat_noka) | 
                         duplicated(dat_noka, fromLast = TRUE)), ]
#==============================================================================
data16x <- read_csv("Sheet 1_Full Data gresik 2016.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data16x <- left_join(dat_noka, data16x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data16x <- data16x[!is.na(data16x$Nosjp), ]

data17x <- read_csv("Sheet 1_Full Data gresik 2017.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data17x <- left_join(dat_noka, data17x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data17x <- data17x[!is.na(data17x$Nosjp), ]

data18x <- read_csv("Sheet 1_Full Data gresik 2018.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data18x <- left_join(dat_noka, data18x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data18x <- data18x[!is.na(data18x$Nosjp), ]

data19x <- read_csv("Sheet 1_Full Data gresik 2019.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data19x <- left_join(dat_noka, data19x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data19x <- data19x[!is.na(data19x$Nosjp), ]

data20x <- read_csv("Sheet 1_Full Data gresik 2020.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data20x <- left_join(dat_noka, data20x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data20x <- data20x[!is.na(data20x$Nosjp), ]

data21x <- read_csv("Sheet 1_Full Data gresik 2021.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data21x <- left_join(dat_noka, data21x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data21x <- data21x[!is.na(data21x$Nosjp), ]

data22x <- read_csv("Sheet 1_Full Data gresik 2022.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data22x <- left_join(dat_noka, data22x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data22x <- data22x[!is.na(data22x$Nosjp), ]

data23x <- read_csv("Sheet 1_Full Data gresik 2023.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data23x <- left_join(dat_noka, data23x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data23x <- data23x[!is.na(data23x$Nosjp), ]

data24x <- read_csv("Sheet 1_Full Data gresik jan-mar 2024.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
data24x <- left_join(dat_noka, data24x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
data24x <- data24x[!is.na(data24x$Nosjp), ]

dataapr24x <- read_csv("Sheet 1_Full Data gresik apr 2024.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Sumberkunjungan,Jenisppkperujuk,
          Kdppklayan,Nmdati2Layan,Politujsjp,Tglpelayanan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Namadpjp,Nmjnspulang,biayars,
          Biayaverifikasi) %>%
  subset(!(Kdinacbgs %in% c("M-3-16-0","Z-3-12-0")))
dataapr24x <- left_join(dat_noka, dataapr24x, by = c("Nokapst"="Nokapst"),
                     copy = FALSE, keep = NULL)
dataapr24x <- dataapr24x[!is.na(dataapr24x$Nosjp), ]

datax <- rbind(data16x,data17x,data18x,data19x,data20x,data21x,
               data22x,data23x,data24x,dataapr24x)
rm(dat_noka,data16x,data17x,data18x,data19x,data20x,data21x,
   data22x,data23x,data24x,dataapr24x)

datax$Tgldtgsjp <- as.Date(datax$Tgldtgsjp, format = "%m/%d/%Y")
datax$Tglplgsjp <- as.Date(datax$Tglplgsjp, format = "%m/%d/%Y")
datax$Tglpelayanan <- as.Date(datax$Tglpelayanan, format = "%m/%d/%Y")

datax <- datax[order(datax$Nokapst,datax$Tglplgsjp),]
datax <- datax %>% group_by(Nokapst) %>%
  mutate(Keterangan = "Bukan fisiotx")

data <- rbind(data,datax)
rm(datax)

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
      Kdppklayan == "0205S101" ~ "Klinik Utama Cerme",
      TRUE ~ Kdppklayan)) %>%
  select(-Kdppklayan)

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

data <- left_join(data,refpoli, by = c("Politujsjp"="KDPOLI"))
data <- data %>%
  select(-Politujsjp) %>%
  rename(Politujsjp = NMPOLI)
rm(refpoli)

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>% group_by(Nokapst) %>%
  mutate(Poli_before = lag(Politujsjp, n=1))

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>%
  group_by(Nokapst) %>%
  mutate(RiwLayan_ke = sequence(n()))
#=====================================================================================
data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
data$interval <- as.integer(data$Tgldtgsjp) - as.integer(data$tgl_before)
data$interval <- as.numeric(data$interval) %>%
  replace(is.na(.), "")
data$interval <- as.factor(data$interval)

data <- data[order(data$Nokapst,data$Tglplgsjp),]
data <- data %>% group_by(Nokapst) %>%
  mutate(tkp_before = lag(Nmtkp, n=1))

data$Sumber01 <- with(data,
                    ifelse(Nokapst == lag(Nokapst) &
                      Poli_before == Politujsjp, "Kontrol Ulang",
                           Sumberkunjungan))
data$Sumber <- with(data,
                    ifelse(is.na(data$Sumber01),Sumberkunjungan,Sumber01))

data <- data[order(data$Nmppklayan, data$Tglplgsjp),]
data <- data %>% select(-Sumberkunjungan,-Sumber01)
data <- data %>% subset(Tglpelayanan >= "2018-01-01")
#==============================================================================
write.csv(data, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_irm.csv",
          na="", row.names = FALSE)
#==============================================================================