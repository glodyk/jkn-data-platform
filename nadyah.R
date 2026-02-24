setwd("D:/Downloads")

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")

load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()

source("http://bioconductor.org/biocLite.R")
chooseBioCmirror()
biocLite()
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
for (i in 1:length(missing)) biocLite(missing[i])

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.22")

BiocManager::install(c("GenomicFeatures", "AnnotationDbi"))

# Load necessary package (if you want to use ggplot2 for visualization)
install.packages("ggplot2") 
# Uncomment to install ggplot2 if you haven't already
library(ggplot2)

# Step 1: Prepare your data
# Using the built-in 'mtcars' dataset
data <- mtcars

# Step 2: Create the linear regression model
# Let's model 'mpg' as a function of 'wt' (weight of the cars)

model <- lm(mpg ~ wt, data = data)

# Step 3: Summarize the model
summary(model)

# Step 4: Visualize the results
ggplot(data, aes(x = wt, y = mpg)) +
  geom_point() +             
  geom_smooth(method = "lm")

# Add points
# Add regression line



approveka <- xl.read.file("RS EKA HUSADA (4).xlsx",
                             header = FALSE,
                             row.names = NULL,
                             col.names = TRUE,
                             xl.sheet = "APROVAL FP",
                             top.left.cell = "A1",
                             na = "",
                             excel.visible = FALSE) %>%
  select(Nosep,ket_idsource,ket_fp_approval)

valbiomeka <- read_csv("Sheet 1_Full Data_data.csv") %>%
  select(Nokapst,Umur,Jenkel,nmdati2asalpst,nmppkasalpst,ppkasalpst,`Fp Sep`,
         `Fp Approval`,alasan_fp_approval,ket_fp_approval,`First Date Fp Sep`,
         `Last Date  Fp Sep`,`First Date Fp Approval`,`Last Date Fp Approval`,
         idsource,ket_idsource,sumber,jenisrujukaninternal,Nosep,Nmtkp,`Bulan Layan`,
         Tglsep,Tglplgsep,Nmdati2Layan,Kdppklayan,Nmppklayan,Kelasrsmenkes,
         ppkrjkawalsep,nmppkrjkawalsep,Politujsep,kll_kk_pak,jnskecelakaan,
         kddiagmasuk,nmdiagmasuk,spesialistik,tacc,kodediagprimer,nmdiagprimer,
         nmdokter,potensiprb,Flagprsklaimsep,`Ket Flagprsklaimsep`,Tgltagsep,
         Tglversep,Tglstjkasie,Tarifgrup,Biayars,Bytagsep,Byversep,Bystjsep,
         `Until Now`,trigger_valbiom) %>%
  select(Nosep,trigger_valbiom)

dfnew <- left_join(approveka,valbiomeka, by = c("Nosep"="Nosep")) %>%
  subset(!is.na(ket_idsource))

write.xlsx(dfnew, file = "approveka.xlsx")

%>%
  select(Nosep,ket_idsource,ket_fp_approval,trigger_valbiom)

dataeka <- read_csv("Sheet 1_Full Data_data (ekah).csv")  %>%
  select (Nokapst,Umur,Jkpst,,Kelashak,Klsrawat,Nmdati2Layan,Jenisppkperujuk,
          Typeppkperujuk,Nmppkperujuk,Kdppklayan,Kelasrsmenkes,Nosjp,Nmtkp,
          Politujsjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Tglreg,Tglstjkeu,Kdinacbgs,
          Nminacbgs,kdsa,kdsd,kdsi,kdsp,kdsr,deskripsisd,deskripsisi,deskripsisp,
          deskripsisr,Kddiagprimer,Nmdiagprimer,Diagsekunder,Procedure,Namadpjp,
          Nmjnspulang,tarifsa,tarifsd,tarifsi,tarifsp,tarifsr,biayars,Biayaverifikasi)
dataeka <- dataeka[order(dataeka$Nokapst,dataeka$Tglplgsjp),]

dfnew <- left_join(dataeka,approveka, by = c("Nosjp"="Nosep"))
dfnew$Tglplgsjp <- as.Date(dfnew$Tglplgsjp, format = "%m/%d/%Y")
dfnew <- dfnew[order(dfnew$Nokapst,dfnew$Tglplgsjp),]

dfnew <- dfnew %>%
  group_by(Nokapst) %>% 
  mutate(keterangan = lead(ket_fp_approval))
dfnew$alasan <- with(dfnew,
                     keterangan == "FINGERPRINT BY APPROVAL" | ket_fp_approval == "FINGERPRINT BY APPROVAL",keterangan,NA)
dfnew <- dfnew %>% subset(alasan == TRUE)


colnames(approveka)

ranapjul25_1 <- xl.read.file("KONFIRMASI PENDING (RW INAP + RW JLN) JULI 2025.xlsx",
                       header = FALSE,
                       row.names = NULL,
                       col.names = TRUE,
                       xl.sheet = "Konf Pending RwInap Juli 2025",
                       top.left.cell = "A2",
                       na = "",
                       excel.visible = FALSE)
colnames(ranapjul25_1)
write.csv(ranapjul25_1, "D://Downloads//ranapjul25_1.csv",
          na="", row.names = FALSE)

rajaljul25_1 <- xl.read.file("KONFIRMASI PENDING (RW INAP + RW JLN) JULI 2025.xlsx",
                       header = FALSE,
                       row.names = NULL,
                       col.names = TRUE,
                       xl.sheet = "Konf Pending RwJaln Juli 2025",
                       top.left.cell = "A2",
                       na = "",
                       excel.visible = FALSE)
colnames(rajaljul25_1)
write.csv(rajaljul25_1, "D://Downloads//rajaljul25_1.csv",
          na="", row.names = FALSE)

rajaljun25w <- xl.read.file("REKPdanKONF PEND RW INP+JLN JUNI 25 - RSUD WLINGI.xlsx",
                       header = FALSE,
                       row.names = NULL,
                       col.names = TRUE,
                       xl.sheet = "RKP PEND RwJln JUNI25",
                       top.left.cell = "A5",
                       na = "",
                       excel.visible = FALSE)
colnames(rajaljun25w)
write.csv(rajaljun25w, "D://Downloads//rajaljun25w.csv",
          na="", row.names = FALSE)


rajaljun25 <- xl.read.file("REKPdanKONF PEND RW INP+JLN JUNI 25 - RSUD WLINGI.xlsx",
                       header = FALSE,
                       row.names = NULL,
                       col.names = TRUE,
                       xl.sheet = "KONFIRMASI PEND RwJln JUNI25",
                       top.left.cell = "A2",
                       na = "",
                       excel.visible = FALSE)
colnames(rajaljun25)
write.csv(rajaljun25, "D://Downloads//rajaljun25.csv",
          na="", row.names = FALSE)

ranapjun25 <- xl.read.file("REKPdanKONF PEND RW INP+JLN JUNI 25 - RSUD WLINGI.xlsx",
                       header = FALSE,
                       row.names = NULL,
                       col.names = TRUE,
                       xl.sheet = "KONFIRMASI PEND RwInp JUNI25",
                       top.left.cell = "A3",
                       na = "",
                       excel.visible = FALSE)
colnames(ranapjun25)
write.csv(ranapjun25, "D://Downloads//ranapjun25.csv",
          na="", row.names = FALSE)

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")

load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()

source("http://bioconductor.org/biocLite.R")
chooseBioCmirror()
biocLite()
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
for (i in 1:length(missing)) biocLite(missing[i])


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
library(nycflights13)
library(xlsx)


data1 <- read_csv("Sheet 3_data (1).csv")
data1$Tglpelayanan <- as.Date(data1$Tglpelayanan, format = "%m/%d/%Y")
write.csv(data1, "D://Downloads//KKK.csv",
          na="", row.names = FALSE)


data1 <- read_csv("Sheet 1_Full Data_data (kesesuaian iterasi).csv") %>%
  subset(Nmkclayan == "GRESIK")

data1 <- read_csv("Sheet 1_Full Data_data (1).csv")
data2 <- read_csv("Sheet 1_Full Data_data (2).csv")

data2 <- read_csv("Sheet 1_Full Data_data (2).csv")

data2 <- read_csv("Sheet 1_Full Data_data (2).csv") %>%
  select(Nokapst,Nmpst,Jkpst,Pisat,Umur,Segmen,Kelashak,Nmpks,Refasalsjp,
         `Flag Iterasi`,`Flag Potensiiterasi`,`Flag Potensiprb`,Nmdati2Terdaftar,
         Nmppkterdaftar,Nmdati2Layan,Nmdati2Resep,Nmjnsppkresep,Nmtypeppkresep,
         Nmppklayan,Kelasrsmenkes,Nmppkresep,Nmtkp,Politujsjp,`Nosjp Apotek`,Kdinacbgs,
         Nminacbgs,`Kd Diagprimer`,`Nm Diagprimer`,Diagsekunder,Katastropik,Pstprb,
         Nmprogprb,Pstprolanis,Nmprogprolanis,Nmdokter,Noresep,`Id Obat`,Jenisobat,
         `Jenis Obat Luarpaket`,Obat,Obatluarpaket,Pabrik,Sediaan,Dagang,Generik,
         Jmlobt,Tgldtgsjp,Tglplgsjp,Tglpelayanan,`Tglpelayanan Rjtl`,Tglsjp,Tglrsp,
         Tglstjkeu,Biaya,`Biaya Rjtl`)

data2$Tgldtgsjp <- as.Date(data2$Tgldtgsjp, format = "%m/%d/%Y")
data2$Tglplgsjp <- as.Date(data2$Tglplgsjp, format = "%m/%d/%Y")
data2$Tglpelayanan <- as.Date(data2$Tglpelayanan, format = "%m/%d/%Y")
data2$`Tglpelayanan Rjtl` <- as.Date(data2$`Tglpelayanan Rjtl`, format = "%m/%d/%Y")
data2$Tglsjp <- as.Date(data2$Tglsjp, format = "%m/%d/%Y")
data2$Tglrsp <- as.Date(data2$Tglrsp, format = "%m/%d/%Y")
data2$Tglstjkeu <- as.Date(data2$Tglstjkeu, format = "%m/%d/%Y")

write.csv(data2, "D://data gresik//MTF KC GRESIK//gresik_2020//factkronis.csv",
          na="", row.names = FALSE)

%>%
  subset(Kdppklayan == "0205R013")

data2 <- read_csv("Sheet 1_Full Data_data (factpesertapotensiprb_new).csv") %>%
  select(`Status Kunjungan`,Nokapst,Jkpst,Umur,Pisapst,Segmen,Sumberkunjungan,
         Pstprb,`Status Prb`,Tmtpstprb,`Status Obat Prb`,Pstprolanis,`Status Prolanis`,
         Tmtpstprolanis,Nmtkp,Nosjp,Politujsjp,Tglpelayanan,statusobatpeserta,
         Nmkronis,Nmkatastrofik,Flagspesialistik,Flagtacc,Nmdokter,Nmdati2Layan,
         Typeppklayan,Kdppklayan,Nmppklayan,kelasrsmenkes,Nmdati2Terdaftar,
         Nmppkterdaftar,Nmdati2Perujuk,Typeppkperujuk,Nmppkperujuk,Kddiagmasuk,
         Nmdiagmasuk,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,Diagsekunder,
         Prosedur,Namadpjp,Nmjnspulang,Tglstjkeu) %>%
  subset(Pstprb != "TRUE")

%>%
  subset(`Status Obat Prb` == "Ada Obat PRB 3 Bulan berturut") %>%
  subset(statusobatpeserta == "PRB")
colnames(data2)

data2$Tmtpstprb <- as.Date(data2$Tmtpstprb, format = "%m/%d/%Y")
data2$Tmtpstprolanis <- as.Date(data2$Tmtpstprolanis, format = "%m/%d/%Y")
data2$Tglpelayanan <- as.Date(data2$Tglpelayanan, format = "%m/%d/%Y")
data2$Tglstjkeu <- as.Date(data2$Tglstjkeu, format = "%m/%d/%Y")

data2 <- data2[order(data2$Tglpelayanan),]
grhu <- data2 %>%
  subset(Kdppklayan == "0205R013") %>%
  select(Nokapst,Jkpst,Umur,Nmtkp,Nosjp,Politujsjp,Tglpelayanan,Nmppklayan,
         Nmdati2Terdaftar,Nmppkterdaftar,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Prosedur,Namadpjp,Nmjnspulang)
grhu <- grhu[order(grhu$Nokapst, grhu$Tglpelayanan),]

rahmi <- data2 %>%
  subset(Kdppklayan == "0205R026") %>%
  select(Nokapst,Jkpst,Umur,Nmtkp,Nosjp,Politujsjp,Tglpelayanan,Nmppklayan,
         Nmdati2Terdaftar,Nmppkterdaftar,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Prosedur,Namadpjp,Nmjnspulang)
rahmi <- rahmi[order(rahmi$Nokapst, rahmi$Tglpelayanan),]

write.xlsx(grhu, file = "Data Potensi PRB RS PKG Grha Husada.xlsx")
write.xlsx(rahmi, file = "Data Potensi PRB RS Rachmi Dewi.xlsx")

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
      Kdppklayan == "0205R033" ~ "RSUD Gresik Sehati",
      TRUE ~ Kdppklayan)) %>%
  select(-Kdppklayan)

colnames(data2)


data1 <- read_csv("Sheet 1_Full Data_data (dimpesertaprb).csv") %>%
  select(Nokapst,Nama,Tgllahir,Pisat,Segmen,Nmstatuspst,Alamat,Email,Nohp,
         Nmdati2Terdaftar,Nmjnsfktpterdaftar,Nmtypefktpterdaftar,Nmfktpterdaftar,
         Pstprbid,Nmprogprb,Nmprogprolanis,Withprb,Statusprb,Nmalasanprb,
         `Periode Layanan`,Nmtkp,Nmdati2Mendaftar,Nmjnsfaskesmendaftar,
         Nmtypefaskesmendaftar,Nmfaskesmendaftar,Nmdokter,Tglmulai,Tglakhir,
         Ketmulai,Ketakhir,Jenisfaskes,`Tglmulai Prolanis`,`Tglakhir Prolanis`,
         Namaclub,Nmalasanprolanis,Nmdati2Prolanis,Nmjnsfktpprolanis,
         Nmtypefktpprolanis,Nmfktpprolanis)
write.csv(data1, "D://data gresik//MTF KC GRESIK//gresik_2020//dimprb.csv",
          na="", row.names = FALSE)

colnames(data1)

fraud1 <- xl.read.file("RAWAT INAP AL MUSTOFA cek MF.xlsx",
                       header = TRUE,
                       row.names = NULL,
                       col.names = NULL,
                       xl.sheet = "DATA",
                       top.left.cell = "A1",
                       na = "",
                       excel.visible = FALSE)

fraud2 <- xl.read.file("RAWAT INAP AL MUSTOFA cek MF.xlsx",
                       header = FALSE,
                       row.names = NULL,
                       col.names = NULL,
                       xl.sheet = "Sheet34",
                       top.left.cell = "A1",
                       na = "",
                       excel.visible = FALSE)

setwd("D:/Downloads/FPK implant dan IUD Data BKKBN")

data1 <- read_csv("Sheet 1_Full Data_data (1).csv")
colnames(data1)
data2 <- read_csv("Sheet 1_Full Data_data (2).csv")
colnames(data2)
data3 <- read_csv("Sheet 1_Full Data_data (3).csv")
colnames(data3)
data4 <- read_csv("Sheet 1_Full Data_data (4).csv")
colnames(data4)

bkkbn <- xl.read.file("2025052313012620250522160415Surat Pelayanan KB NON BPJS.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE)
colnames(bkkbn)

bkkbn$NAMA <- str_replace_all(bkkbn$NAMA, "[^[:alnum:]]", " ")
bkkbn$NAMA <- gsub("\\s+"," ",bkkbn$NAMA)
bkkbn$NAMA <- trimws(tolower(bkkbn$NAMA),"both")
bkkbn$NAMA <- trimws(gsub("  ", " ", bkkbn$NAMA),"both")

library(splitstackshape)
bkkbn <- concat.split(bkkbn, "Tanggal Pelayanan", " ")

bkkbn <- bkkbn %>%
  mutate(
    `Tanggal Pelayanan_4` = case_when(
      `Tanggal Pelayanan_2` == "Januari" ~ "01",
      `Tanggal Pelayanan_2` == "Februari" ~ "02",
      `Tanggal Pelayanan_2` == "Maret" ~ "03",
      `Tanggal Pelayanan_2` == "April" ~ "04",
      `Tanggal Pelayanan_2` == "Mei" ~ "05",
      `Tanggal Pelayanan_2` == "Juni" ~ "06",
      `Tanggal Pelayanan_2` == "Juli" ~ "07",
      `Tanggal Pelayanan_2` == "Agustus" ~ "08",
      `Tanggal Pelayanan_2` == "September" ~ "09",
      `Tanggal Pelayanan_2` == "Oktober" ~ "10",
      `Tanggal Pelayanan_2` == "November" ~ "11",
      `Tanggal Pelayanan_2` == "Desember" ~ "12",
      TRUE ~ `Tanggal Pelayanan_2`))

bkkbn <- bkkbn %>%
  mutate(Tglpelayanan = paste0(as.character(`Tanggal Pelayanan_1`),"/",
                               as.character(`Tanggal Pelayanan_4`),"/",
                               as.character(`Tanggal Pelayanan_3`)))
bkkbn$Tglpelayanan <- as.Date(bkkbn$Tglpelayanan, format = "%d/%m/%Y")
bkkbn <- bkkbn %>%
  select(NAMA,NIK,Tglpelayanan,`Tempat Pelayanan`,`Metode MKJP`)

library("tabulapdf")

file_pdf <- "Combined_OCR.pdf"
file_pdf <- extract_tables(file_pdf, encoding="UTF-8")[[1]]


[[1]]  ## comes as a character matrix
> ## use first row as column names
  > dat <- setNames(type.convert(as.data.frame(m[-1, ]), as.is=TRUE), m[1, ])
> ## example-specific date conversion
  > dat$Date <- as.POSIXlt(dat$Date, format="%m/%d/%y")
> dat <- within(dat, Date$year <- ifelse(Date$year > 120, Date$year - 100, Date$year))
> dat  ## voilà

setwd("D:/Downloads")

potprb <- read_csv("Sheet 1_Full factpotensiprb.csv") %>%
  select(Nokapst,Umur,Jkpst,Segmen,Nmdati2Terdaftar,Nmppkterdaftar,Nosjp,
         `Status Kunjungan`,Sumberkunjungan,Tglpelayanan,Nmdati2Layan,Kdppklayan,
         Nmtkp,Politujsjp,Namadpjp,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Prosedur,Nmkatastrofik,Nmkronis,Pstprb,Tmtpstprb,
         `Status Prb`,`Status Obat Prb`,statusobatpeserta,`Tanggal Obat Prb`,
         Pstprolanis,Tmtpstprolanis,`Status Prolanis`) %>%
  subset(`Status Prb` == FALSE) %>%
  subset(`Status Prolanis` == FALSE)

potprb$Tglpelayanan <- as.Date(potprb$Tglpelayanan, format = "%m/%d/%Y")
potprb$Tmtpstprb <- as.Date(potprb$Tmtpstprb, format = "%m/%d/%Y")
potprb$`Tanggal Obat Prb` <- as.Date(potprb$`Tanggal Obat Prb`, format = "%m/%d/%Y")
potprb$Tmtpstprolanis <- as.Date(potprb$Tmtpstprolanis, format = "%m/%d/%Y")

potprb <- potprb %>%
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
      Kdppklayan == "0205R033" ~ "RSUD Gresik Sehati",
      TRUE ~ Kdppklayan))

ngimbang <- potprb %>%
  subset(Kdppklayan == "0204R003") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

pinatih <- potprb %>%
  subset(Kdppklayan == "0205R027") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

pinatih <- pinatih[order(pinatih$Tglpelayanan),]

walisongo <- potprb %>%
  subset(Kdppklayan == "0205R019") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

walisongo <- walisongo[order(walisongo$Tglpelayanan),]

karangkembang <- potprb %>%
  subset(Kdppklayan == "0204R021") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

karangkembang <- karangkembang[order(karangkembang$Tglpelayanan),]


semen <- potprb %>%
  subset(Kdppklayan == "0205R012") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

semen <- semen[order(semen$Tglpelayanan),]

intan <- potprb %>%
  subset(Kdppklayan == "0204R014") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

intan <- intan[order(intan$Tglpelayanan),]

suyudi <- potprb %>%
  subset(Kdppklayan == "0204R004") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

suyudi <- suyudi[order(suyudi$Tglpelayanan),]

mabarot <- potprb %>%
  subset(Kdppklayan == "0205R025") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

rsumb <- potprb %>%
  subset(Kdppklayan == "0204R012") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)
rsumb <- rsumb[order(rsumb$Tglpelayanan),]

soegiri <- potprb %>%
  subset(Kdppklayan == "1306R001") %>%
  select(Nokapst,Segmen,Jkpst,Umur,Kdppklayan,Nmppklayan,Nmppkterdaftar,Nosjp,
         Tglpelayanan,Politujsjp,Kdinacbgs,Nminacbgs,Kddiagprimer,Nmdiagprimer,
         Diagsekunder,Prosedur,Namadpjp) %>%
  select(-Kdppklayan)

#========================================================================
write.xlsx(ngimbang, file = "Data Potensi PRB RSUD Ngimbang.xlsx")
write.xlsx(pinatih, file = "Data Potensi PRB RSI Nyai Ageng Pinatih.xlsx")
write.xlsx(walisongo, file = "Data Potensi PRB RS Wali Songo I.xlsx")
write.xlsx(karangkembang, file = "Data Potensi PRB RSUD Karangkembang.xlsx")
write.xlsx(intan, file = "Data Potensi PRB RS Intan Medika.xlsx")
write.xlsx(semen, file = "Data Potensi PRB RS Semen Gresik.xlsx")
write.xlsx(mabarot, file = "Data Potensi PRB RSI Mabarrot MWC NU Bungah.xlsx")
write.xlsx(rsumb, file = "Data Potensi PRB RSU Muhammadiyah Babat(2).xlsx")
write.xlsx(soegiri, file = "Data Potensi PRB RSUD Dr Soegiri.xlsx")
write.xlsx(suyudi, file = "Data Potensi PRB RS Suyudi Paciran (JST).xlsx")
#========================================================================
potprb <- potprb %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
potprb <- potprb %>% mutate(Prosedur = na_if(Prosedur, "-"))

potprb <- potprb %>%
  mutate(
    Kddiagprimer1 = case_when(
      Kddiagprimer == "Z014" ~ NA,
      Kddiagprimer == "Z016" ~ NA,
      Kddiagprimer == "Z030" ~ NA,
      Kddiagprimer == "Z031" ~ NA,
      Kddiagprimer == "Z136" ~ NA,
      Kddiagprimer == "Z355" ~ NA,
      Kddiagprimer == "Z530" ~ NA,
      Kddiagprimer == "Z038" ~ NA,
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
      Kddiagprimer == "Z048" ~ NA,
      Kddiagprimer == "Z080" ~ NA,
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
      Kddiagprimer == "Z209" ~ NA,
      Kddiagprimer == "Z301" ~ NA,
      Kddiagprimer == "Z348" ~ NA,
      Kddiagprimer == "Z351" ~ NA,
      Kddiagprimer == "Z470" ~ NA,
      Kddiagprimer == "Z478" ~ NA,
      Kddiagprimer == "Z479" ~ NA,
      Kddiagprimer == "Z480" ~ NA,
      Kddiagprimer == "Z500" ~ NA,
      Kddiagprimer == "Z502" ~ NA,
      Kddiagprimer == "Z506" ~ NA,
      Kddiagprimer == "Z508" ~ NA,
      Kddiagprimer == "Z510" ~ NA,
      Kddiagprimer == "Z516" ~ NA,
      Kddiagprimer == "Z590" ~ NA,
      Kddiagprimer == "Z596" ~ NA,
      Kddiagprimer == "Z760" ~ NA,
      Kddiagprimer == "Z801" ~ NA,
      Kddiagprimer == "Z898" ~ NA,
      Kddiagprimer == "Z908" ~ NA,
      Kddiagprimer == "Z955" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z988" ~ NA,
      Kddiagprimer == "Q246" ~ NA,
      Kddiagprimer == "Q248" ~ NA,
      Kddiagprimer == "Q249" ~ NA,
      Kddiagprimer == "R008" ~ NA,
      Kddiagprimer == "Z034" ~ NA,
      Kddiagprimer == "Z035" ~ NA,
      Kddiagprimer == "Z049" ~ NA,
      Kddiagprimer == "Z352" ~ NA,
      Kddiagprimer == "Z354" ~ NA,
      Kddiagprimer == "Z356" ~ NA,
      Kddiagprimer == "Z357" ~ NA,
      Kddiagprimer == "Z369" ~ NA,
      Kddiagprimer == "Z436" ~ NA,
      Kddiagprimer == "Z458" ~ NA,
      Kddiagprimer == "Z519" ~ NA,
      Kddiagprimer == "Z900" ~ NA,
      Kddiagprimer == "Z950" ~ NA,
      Kddiagprimer == "Z952" ~ NA,
      Kddiagprimer == "Z954" ~ NA,
      Kddiagprimer == "Z959" ~ NA,
      Kddiagprimer == "Z960" ~ NA,
      TRUE ~ Kddiagprimer))

potprb$Nmdiagprimer1 <- with(potprb,
                             ifelse(is.na(potprb$Kddiagprimer1),NA,Nmdiagprimer))
potprb <- potprb %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
potprb$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,potprb$Diagprimer)), "both")
potprb <- potprb %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
potprb$Diagnosa <- as.character(trimws(gsub(";NA|NA;|NA","",potprb$Diagnosa)), "both")
potprb <- potprb %>% select(-Diagprimer,-Kddiagprimer1,-Nmdiagprimer1)
potprb <- potprb %>% mutate(Diagnosa = na_if(Diagnosa, ""))

diag <- potprb %>%
  select(Diagnosa) %>%
  unique()

write.xlsx(diag, file = "Diagnosa.xlsx")

write.csv(potprb, "D:/Downloads/potprb.csv",na="", row.names = FALSE)

data_apa <- read_csv("Sheet 1_Full Data_data (4).csv") 

%>%
  subset(flagprolanisht == "Ya")

datahis23 <- read_csv("Sheet 1_Full Data_data.csv") %>%
  select(Noka,Nokapst,Jkpst,Umur,Kelashak,Pisapst,Segmen,Nmdati2Layan,Kdppklayan,
         Tgldtgsjp,Tglplgsjp,Tglpelayanan,Kddiagmasuk,Nmdiagmasuk,Sumberpintumasuk,
         Nmtkp,Politujsep,Nosjp,Waktuterbitsep,Klsrawat,Nmdokter,Nmjnspulang,
         `Kdinacbgs Awal`,`Nminacbgs Awal`,`Kdinacbgs Akhir`,`Nminacbgs Akhir`,
         `Kdgrupdiagnosis Awal`,`Kdgrupdiagnosis Akhir`,`Grupdiagnosis Awal`,
         `Grupdiagnosis Akhir`,`Kdgrupkategori Awal`,`Kdgrupkategori Akhir`,
         `Grupkategori Awal`,`Grupkategori Akhir`,`Kodediagprimer Awal`,
         `Kodediagprimer Akhir`,`Nmdiagprimer Awal`,`Nmdiagprimer Akhir`,
         `Diagsekunder Awal`,`Diagsekunder Akhir`,`Procedure Awal`,
         `Procedure Akhir`,Kdsa,Kdsd,Kdsi,Kdsp,Kdsr,Deskripsisd,Deskripsisi,
         Deskripsisp,Deskripsisr,Tglbast,Tglbahv,`Statusklaim Awal`,
         `Statusklaim Akhir`,Naikkelas,`Flag Iterasi`,`Flag Kk Pak Kll`,
         Flagalgoritma,Nmlogicalgoritma,Flagdiva,Nmlogicdiva,Jmldiagsekunder,
         Jmlprosedur,Jmldokterperpoli,Jmlpoliperfaskes,Jmltempattidur,
         `Biayars Awal`,`Biayars Akhir`,`Biayatagih Awal`,`Biayatagih Akhir`,
         `Biayaverifikasi Awal`,`Biayaverifikasi Akhir`)


setwd("D:/Downloads")
directory <- 'Data Antrian Operasi Phacoemulsifikasi KC Gresik'
files <- list.files(path = directory)
files1 <- data.frame(matrix(unlist(files), ncol = max(lengths(files)), byrow = TRUE))

setwd("D:/Downloads/Data Antrian Operasi Phacoemulsifikasi KC Gresik")

list1 <- xl.read.file("ANTRIAN PHACO RSMG.xlsx",
                           header = TRUE,
                           row.names = NULL,
                           col.names = NULL,
                           xl.sheet = "Sheet1",
                           top.left.cell = "A1",
                           na = "",
                           excel.visible = FALSE) %>% select(-1)
list1$NmFKRTL <- "RS Muhammadiyah Gresik"
list1$`JADWAL OPERASI` <- as.Date(list1$`JADWAL OPERASI`)
list1$`JADWAL OPERASI` <- as.character(list1$`JADWAL OPERASI`)

list2 <- xl.read.file("DAFTAR ANTRI KATARAK petrokimia Gresik.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list2$NmFKRTL <- "RS Petrokimia Gresik"
list2$`JADWAL OPERASI` <- as.Date(list2$`JADWAL OPERASI`)
list2$`JADWAL OPERASI` <- as.character(list2$`JADWAL OPERASI`)

list3 <- xl.read.file("KLINIK MATA UTAMA.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "April",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list3$NmFKRTL <- "Klinik Mata Utama Gresik"
list3$`JADWAL OPERASI` <- as.Date(list3$`JADWAL OPERASI`)
list3$`JADWAL OPERASI` <- as.character(list3$`JADWAL OPERASI`)

list4 <- xl.read.file("Data Operasi Phaco Cahaya Giri.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list4$NmFKRTL <- "RSI Cahaya Giri"
list4$`JADWAL OPERASI` <- as.Date(list4$`JADWAL OPERASI`)
list4$`JADWAL OPERASI` <- as.character(list4$`JADWAL OPERASI`)

list5 <- xl.read.file("KLINIK MATA UTAMA.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Maret",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list5$NmFKRTL <- "Klinik Mata Utama Gresik"
list5$`JADWAL OPERASI` <- as.Date(list5$`JADWAL OPERASI`)
list5$`JADWAL OPERASI` <- as.character(list5$`JADWAL OPERASI`)

list6 <- xl.read.file("KLINIK UTAMA GRESIK.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list6$NmFKRTL <- "Klinik Utama Gresik"
list6$`JADWAL OPERASI` <- as.Date(list6$`JADWAL OPERASI`)
list6$`JADWAL OPERASI` <- as.character(list6$`JADWAL OPERASI`)

list7 <- xl.read.file("KMU LAMONGAN PASIEN PHACO 2025.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list7$NmFKRTL <- "Klinik Mata Utama Lamongan"
list7$`JADWAL OPERASI` <- as.Date(list7$`JADWAL OPERASI`)
list7$`JADWAL OPERASI` <- as.character(list7$`JADWAL OPERASI`)

list8 <- xl.read.file("LIST OPERASI FACO 2025 RS Petro Driyo.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "MARET ",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1,-`DIAGNOSA `,-`RENCANA `,-`NA`)
list8$NmFKRTL <- "RS Petrokimia Driyorejo"
list8$`JADWAL OPERASI` <- as.Date(list8$`JADWAL OPERASI`)
list8$`JADWAL OPERASI` <- as.character(list8$`JADWAL OPERASI`)

list81 <- xl.read.file("LIST OPERASI FACO 2025 RS Petro Driyo.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "APRIL ",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1,-`DIAGNOSA `,-`RENCANA `,-`NA`)
list81$NmFKRTL <- "RS Petrokimia Driyorejo"
list81$`JADWAL OPERASI` <- as.Date(list81$`JADWAL OPERASI`)
list81$`JADWAL OPERASI` <- as.character(list81$`JADWAL OPERASI`)

list9 <- xl.read.file("list rencana operasi RSUD Soegiri.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1,-2)
list9$NmFKRTL <- "RSUD Dr Soegiri Lamongan"

list10 <- xl.read.file("PHACO JAN-FEB 2025 RS Permata Hati.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list10$NmFKRTL <- "RS Permata Hati"
list10$`JADWAL OPERASI` <- as.Date(list10$`JADWAL OPERASI`)
list10$`JADWAL OPERASI` <- as.character(list10$`JADWAL OPERASI`)

list11 <- xl.read.file("REKAP RS RANDEGANSARI TH 2025.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "PHACO 2025",
                      top.left.cell = "A3",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list11$NmFKRTL <- "RSU Randegansari Husada"
list11$`JADWAL OPERASI` <- as.Date(list11$`JADWAL OPERASI`)
list11$`JADWAL OPERASI` <- as.character(list11$`JADWAL OPERASI`)

list12 <- xl.read.file("RS ARSY DAFTAR PASIEN PHACO 2025.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1,-2)
list12$NmFKRTL <- "RS Arsy Paciran"

list13 <- xl.read.file("RS DENISA.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list13$NmFKRTL <- "RS Denisa"

list14 <- xl.read.file("RS Eka Husada.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list14$NmFKRTL <- "RSU Eka Husada"

list15 <- xl.read.file("RS Fathma medika.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Rencana Operasi Phaco",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list15$NmFKRTL <- "RS Fathma medika"

list16 <- xl.read.file("RS Grahu.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list16$NmFKRTL <- "RS PKG Grha Husada"

list17 <- xl.read.file("RS Intan Medika.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "RSIM",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list17$NmFKRTL <- "RS Intan Medika"

list18 <- xl.read.file("RS Muhammadiyah Babat.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list18$NmFKRTL <- "RS Muhammadiyah Babat"

list19 <- xl.read.file("RS Muhammadiyah Lamongan.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1,-3)
list19$NmFKRTL <- "RS Muhammadiyah Lamongan"

list20 <- xl.read.file("RS Muhammadiyah Sekapuk.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list20$NmFKRTL <- "RS PKU Muhammadiyah Sekapuk"

list21 <- xl.read.file("RS NU BABAT DAFTAR PASIEN PHACO 2025.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list21$NmFKRTL <- "RS NU Babat"

list22 <- xl.read.file("RS Semen Gresik 2025.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list22$NmFKRTL <- "RS Semen Gresik"

list23 <- xl.read.file("RS Surya Medika.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list23$NmFKRTL <- "RS Surya Medika"

list24 <- xl.read.file("RS Suyudi.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list24$NmFKRTL <- "RSU Suyudi Paciran (JST)"

list25 <- xl.read.file("RS Wates Husada.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE) %>% select(-1)
list25$NmFKRTL <- "RS Wates Husada"

list26 <- xl.read.file("RSI Nasrul Ummah.xlsx",
                       header = TRUE,
                       row.names = NULL,
                       col.names = NULL,
                       xl.sheet = "Sheet1",
                       top.left.cell = "A1",
                       na = "",
                       excel.visible = FALSE) %>% select(-1)
list26$NmFKRTL <- "RSI Nashrul Ummah"

list27 <- xl.read.file("RSI NYAI AGENG PINATIH.xlsx",
                       header = TRUE,
                       row.names = NULL,
                       col.names = NULL,
                       xl.sheet = "Sheet1",
                       top.left.cell = "A1",
                       na = "",
                       excel.visible = FALSE) %>% select(-1)
list27$NmFKRTL <- "RSI Nyai Ageng Pinatih"

list28 <- xl.read.file("RSUD IBNU SINA KAB GRESIK.xlsx",
                       header = TRUE,
                       row.names = NULL,
                       col.names = NULL,
                       xl.sheet = "Sheet1",
                       top.left.cell = "A4",
                       na = "",
                       excel.visible = FALSE) %>% select(-1)
list28$NmFKRTL <- "RSUD Ibnu Sina Gresik"

list29 <- xl.read.file("RSUD Karang Kembang.xlsx",
                       header = TRUE,
                       row.names = NULL,
                       col.names = NULL,
                       xl.sheet = "Sheet1",
                       top.left.cell = "A1",
                       na = "",
                       excel.visible = FALSE) %>% select(-1)
list29$NmFKRTL <- "RSUD Karang Kembang"

list30 <- xl.read.file("RSUD NGIMBANG.xlsx",
                       header = TRUE,
                       row.names = NULL,
                       col.names = NULL,
                       xl.sheet = "Sheet1",
                       top.left.cell = "A1",
                       na = "",
                       excel.visible = FALSE) %>% select(-1,-3)
list30$NmFKRTL <- "RSUD Ngimbang"

gabungan <- rbind(list1,list2,list3,list4,list5,list6,list7,list8,list81,list9,list10,
                  list11,list12,list13,list14,list15,list16,list17,list18,list19,list20,
                  list21,list22,list23,list24,list25,list26,list27,list28,list29,list30)

gabungan <- gabungan %>% select (NmFKRTL,`NAMA PASIEN`,`NOMOR KARTU`,ALAMAT,`NOMOR HP`,
                                 `JADWAL OPERASI`)

#========================================================================
write.xlsx(gabungan, file = "Data Antrian Operasi Phacoemulsifikasi KC Gresik.xlsx")
#========================================================================

petro <- read_delim("all petro.csv") %>%
  select (Flag,Nokapst,Umur,Jkpst,Segmenpeserta,Nmbu,Nmppkpeserta,`Nmppk Fktp`,
          Nofktp,`Waktu Kunjungan Fktp`,`Waktu Kunjungan Fkrtl`,Selisih,`User Fkrtl`,
          `User Pcare`,Spesialistik,`Grup Jam RS`,Sumber,`Tgldatang Fktp`,`Tglpulang Fktp`,`Tgl Datang Fkrtl`,
          `Tgl Pulang Fkrtl`,Nmtkp,Politujsjp,Nosjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Diagmasuk,
          Diagprimer,Diagsekunder,Procedure,Kdinacbgs,Nminacbgs,Namadpjp,
          Nmjnspulang,Tglstjkeu,Biayars,Biayaverifikasi)

pengurang_1 <- read_delim("pengurang 1.csv") %>%
  select (Flag,Nokapst,Umur,Jkpst,Segmenpeserta,Nmbu,Nmppkpeserta,`Nmppk Fktp`,
          Nofktp,`Waktu Kunjungan Fktp`,`Waktu Kunjungan Fkrtl`,Selisih,`User Fkrtl`,
          `User Pcare`,Spesialistik,`Grup Jam RS`,Sumber,`Tgldatang Fktp`,`Tglpulang Fktp`,`Tgl Datang Fkrtl`,
          `Tgl Pulang Fkrtl`,Nmtkp,Politujsjp,Nosjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Diagmasuk,
          Diagprimer,Diagsekunder,Procedure,Kdinacbgs,Nminacbgs,Namadpjp,
          Nmjnspulang,Tglstjkeu,Biayars,Biayaverifikasi)

pengurang_2 <- read_delim("pengurang 2.csv") %>%
  select (Flag,Nokapst,Umur,Jkpst,Segmenpeserta,Nmbu,Nmppkpeserta,`Nmppk Fktp`,
          Nofktp,`Waktu Kunjungan Fktp`,`Waktu Kunjungan Fkrtl`,Selisih,`User Fkrtl`,
          `User Pcare`,Spesialistik,`Grup Jam RS`,Sumber,`Tgldatang Fktp`,`Tglpulang Fktp`,`Tgl Datang Fkrtl`,
          `Tgl Pulang Fkrtl`,Nmtkp,Politujsjp,Nosjp,Tglpelayanan,Tgldtgsjp,Tglplgsjp,Diagmasuk,
          Diagprimer,Diagsekunder,Procedure,Kdinacbgs,Nminacbgs,Namadpjp,
          Nmjnspulang,Tglstjkeu,Biayars,Biayaverifikasi)

data <- data[order(data$Tglplgsjp),]

#========================================================================
write.xlsx(data, file = "Petrokimia Anomali.xlsx")
#========================================================================


data <- rbind(petro,pengurang_1,pengurang_2)



#=================================================#
#============   4004263   ========================#
#=================================================#
parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2021"
ext <- c(
"0205R0120121V000457|0205R0120121V001884|0205R0120121V002543|0205R0120121V004008|0205R0120121V004657|0205R0120121V006139|0205R0120121V006800|0205R0120221V000147|0205R0120221V001521|0205R0120221V002220|0205R0120221V003290|0205R0120221V004210|0205R0120221V005792|0205R0120221V006517|0205R0120221V007990|0205R0120321V000271|0205R0120321V001806|0205R0120321V002352|0205R0120321V003657|0205R0120321V004512|0205R0120321V005979|0205R0120321V006807|0205R0120321V008354|0205R0120321V008994|0205R0120421V000143|0205R0120421V001017|0205R0120421V002624|0205R0120421V003573|0205R0120421V004876|0205R0120421V005596|0205R0120421V007015|0205R0120421V007788|0205R0120421V009303|0205R0120521V000141|0205R0120521V001769|0205R0120521V002536|0205R0120521V003569|0205R0120521V003873|0205R0120521V005498|0205R0120521V006317|0205R0120521V007781|0205R0120521V008561|0205R0120621V000973|0205R0120621V001667|0205R0120621V003278|0205R0120621V003977|0205R0120621V005298|0205R0120621V006137|0205R0120621V007468|0205R0120621V008295|0205R0120721V000351|0205R0120721V000839|0205R0120721V001692|0205R0120721V002033|0205R0120721V002733|0205R0120721V003419|0205R0120721V003829|0205R0120721V004290|0205R0120821V000108|0205R0120821V001013|0205R0120821V001567|0205R0120821V002382|0205R0120821V002902|0205R0120821V003756|0205R0120821V004357|0205R0120821V005451|0205R0120821V006107|0205R0120921V000651|0205R0120921V001270|0205R0120921V002486|0205R0120921V003002|0205R0120921V004236|0205R0120921V004836|0205R0120921V006094|0205R0120921V006827|0205R0121021V000140|0205R0121021V000711|0205R0121021V002080|0205R0121021V002607|0205R0121021V003943|0205R0121021V004635|0205R0121021V005710|0205R0121021V006437|0205R0121021V007870|0205R0121121V000187|0205R0121121V001418|0205R0121121V002113|0205R0121121V003485|0205R0121121V004142|0205R0121121V005554|0205R0121121V006119|0205R0121121V007620|0205R0121121V008243|0205R0121221V000898|0205R0121221V001549|0205R0121221V003012|0205R0121221V003593|0205R0121221V005054|0205R0121221V005741|0205R0121221V007088|0205R0121221V007706|0205R0121221V009290")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2021/4004263", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2021/4004263/"))

parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2022"
ext <- c(
"0205R0120122V000259|0205R0120122V001734|0205R0120122V002338|0205R0120122V003661|0205R0120122V004511|0205R0120122V005946|0205R0120122V006655|0205R0120122V008048|0205R0120122V008820|0205R0120222V000910|0205R0120222V001536|0205R0120222V002745|0205R0120222V003431|0205R0120222V004666|0205R0120222V005168|0205R0120222V006425|0205R0120322V000177|0205R0120322V001011|0205R0120322V001780|0205R0120322V003112|0205R0120322V003736|0205R0120322V005115|0205R0120322V005657|0205R0120322V006941|0205R0120322V007635|0205R0120422V000055|0205R0120422V000814|0205R0120422V002108|0205R0120422V002631|0205R0120422V003663|0205R0120422V004445|0205R0120422V005755|0205R0120422V006354|0205R0120422V007768|0205R0120522V000479|0205R0120522V001061|0205R0120522V002708|0205R0120522V003444|0205R0120522V005532|0205R0120522V006764|0205R0120522V007399|0205R0120622V000545|0205R0120622V001192|0205R0120622V002586|0205R0120622V003345|0205R0120622V004806|0205R0120622V005433|0205R0120622V006921|0205R0120622V007648|0205R0120722V000142|0205R0120722V000846|0205R0120722V002120|0205R0120722V002494|0205R0120722V004184|0205R0120722V004849|0205R0120722V006304|0205R0120722V006938|0205R0120722V008512|0205R0120822V000164|0205R0120822V001547|0205R0120822V002338|0205R0120822V003675|0205R0120822V004525|0205R0120822V005765|0205R0120822V006546|0205R0120822V008002|0205R0120822V008668|0205R0120922V000583|0205R0120922V001208|0205R0120922V002695|0205R0120922V003315|0205R0120922V004924|0205R0120922V005539|0205R0120922V007229|0205R0120922V007964|0205R0120922V009568|0205R0121022V000718|0205R0121022V002048|0205R0121022V002538|0205R0121022V004182|0205R0121022V004936|0205R0121022V006526|0205R0121022V007192|0205R0121022V008716|0205R0121022V009455|0205R0121122V001259|0205R0121122V002057|0205R0121122V003607|0205R0121122V004259|0205R0121122V005777|0205R0121122V006567|0205R0121122V008233|0205R0121122V008909|0205R0121222V000559|0205R0121222V001203|0205R0121222V002938|0205R0121222V003668|0205R0121222V005357|0205R0121222V006097|0205R0121222V007788|0205R0121222V008585|0205R0121222V010184")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2022/4004263", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2022/4004263/"))


parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2023"
ext <- c(
  "0205R0120123V000242|0205R0120123V001860|0205R0120123V002630|0205R0120123V004394|0205R0120123V005109|0205R0120123V006693|0205R0120123V007469|0205R0120123V009100|0205R0120123V009890|0205R0120223V000872|0205R0120223V001677|0205R0120223V003333|0205R0120223V003969|0205R0120223V005825|0205R0120223V006260|0205R0120223V008067|0205R0120223V008891|0205R0120323V000960|0205R0120323V001609|0205R0120323V003541|0205R0120323V004376|0205R0120323V006125|0205R0120323V006817|0205R0120323V008435|0205R0120323V009205|0205R0120323V010851|0205R0120423V000546|0205R0120423V001762|0205R0120423V002832|0205R0120423V004631|0205R0120423V005489|0205R0120423V006807|0205R0120423V008443|0205R0120523V000551|0205R0120523V002115|0205R0120523V002931|0205R0120523V004872|0205R0120523V005651|0205R0120523V007344|0205R0120523V008226|0205R0120523V009928|0205R0120523V010853|0205R0120623V000139|0205R0120623V001187|0205R0120623V003066|0205R0120623V003935|0205R0120623V005795|0205R0120623V006870|0205R0120623V008622|0205R0120623V009326|0205R0120623V010766|0205R0120723V000520|0205R0120723V002572|0205R0120723V003434|0205R0120723V005213|0205R0120723V006126|0205R0120723V007625|0205R0120723V008796|0205R0120723V010651|0205R0120723V011250|0205R0120823V001413|0205R0120823V002270|0205R0120823V004279|0205R0120823V005243|0205R0120823V006852|0205R0120823V007486|0205R0120823V009619|0205R0120823V010453|0205R0120923V000192|0205R0120923V000911|0205R0120923V002895|0205R0120923V003857|0205R0120923V005781|0205R0120923V006574|0205R0120923V008477|0205R0120923V009670|0205R0120923V011201|0205R0121023V000143|0205R0121023V002275|0205R0121023V003020|0205R0121023V005232|0205R0121023V006255|0205R0121023V008205|0205R0121023V009067|0205R0121023V011167|0205R0121023V012165|0205R0121123V000971|0205R0121123V001896|0205R0121123V003979|0205R0121123V004977|0205R0121123V007124|0205R0121123V008029|0205R0121123V010145|0205R0121123V011067|0205R0121223V000233|0205R0121223V001253|0205R0121223V003167|0205R0121223V004187|0205R0121223V006387|0205R0121223V007171|0205R0121223V009426|0205R0121223V010522|0205R0121223V012277")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2023/4004263", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2023/4004263/"))

parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2024"
ext <- c(
  "0205R0120124V000382|0205R0120124V001886|0205R0120124V003130|0205R0120124V004904|0205R0120124V005996|0205R0120124V008184|0205R0120124V009018|0205R0120124V011102|0205R0120124V012305|0205R0120224V000561|0205R0120224V001667|0205R0120224V003600|0205R0120224V004209|0205R0120224V006067|0205R0120224V007466|0205R0120224V009601|0205R0120224V010843|0205R0120324V000256|0205R0120324V001157|0205R0120324V003602|0205R0120324V004983|0205R0120324V006314|0205R0120324V007490|0205R0120324V009554|0205R0120324V010584|0205R0120324V012297|0205R0120424V000269|0205R0120424V002131|0205R0120424V003186|0205R0120424V004007|0205R0120424V004553|0205R0120424V006685|0205R0120424V008091|0205R0120424V010335|0205R0120424V011551|0205R0120524V001019|0205R0120524V002195|0205R0120524V004230|0205R0120524V005280|0205R0120524V007241|0205R0120524V008578|0205R0120524V010509|0205R0120524V011638|0205R0120524V014063|0205R0120624V001057|0205R0120624V002660|0205R0120624V003923|0205R0120624V005829|0205R0120624V006833|0205R0120624V008560|0205R0120624V009671|0205R0120624V011919|0205R0120724V000437|0205R0120724V002401|0205R0120724V003569|0205R0120724V005859|0205R0120724V006798|0205R0120724V008902|0205R0120724V009918|0205R0120724V012509|0205R0120724V013732")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2024/4004263", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2024/4004263/"))
#=================================================#
#============   6571995   ========================#
#=================================================#
parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2021"
ext <- c(
  "0205R0120121V000456|0205R0120121V001879|0205R0120121V002541|0205R0120121V004003|0205R0120121V004762|0205R0120121V006141|0205R0120121V006809|0205R0120121V008301|0205R0120221V000138|0205R0120221V001511|0205R0120221V002216|0205R0120221V003271|0205R0120221V004208|0205R0120221V005779|0205R0120321V000276|0205R0120321V001822|0205R0120321V002344|0205R0120321V003649|0205R0120321V004508|0205R0120321V005988|0205R0120321V006812|0205R0120321V008394|0205R0120321V009002|0205R0120421V000150|0205R0120421V001033|0205R0120421V002691|0205R0120421V003580|0205R0120421V004874|0205R0120421V005593|0205R0120421V007006|0205R0120421V007839|0205R0120421V009314|0205R0120521V000148|0205R0120521V001773|0205R0120521V002543|0205R0120521V003568|0205R0120521V003872|0205R0120521V005495|0205R0120521V006321|0205R0120521V007762|0205R0120521V008509|0205R0120621V000978")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2021/6571995", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2021/6571995/"))
#==================================================#
#============   25569126   ========================#
#==================================================#
parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2021"
ext <- c(
  "0205R0120121V000464|0205R0120121V001882|0205R0120121V002538|0205R0120121V004002|0205R0120121V004766|0205R0120121V006132|0205R0120121V006812|0205R0120121V008298|0205R0120221V000126|0205R0120221V003426|0205R0120221V004192|0205R0120221V005785|0205R0120221V006564|0205R0120221V007993|0205R0120321V000270|0205R0120321V001794|0205R0120321V002358|0205R0120321V003641|0205R0120321V004519|0205R0120321V005991|0205R0120321V006818|0205R0120321V008360|0205R0120321V008991|0205R0120421V000141|0205R0120421V001005|0205R0120421V002618|0205R0120421V003570|0205R0120421V004873|0205R0120421V005592|0205R0120421V007005|0205R0120421V007853|0205R0120421V009319|0205R0120521V000165|0205R0120521V001784|0205R0120521V002558|0205R0120521V003573|0205R0120521V003875|0205R0120521V005462|0205R0120521V006313|0205R0120521V007769|0205R0120521V008478|0205R0120621V000980|0205R0120621V001672|0205R0120621V003274|0205R0120621V003926|0205R0120621V005292|0205R0120621V006134|0205R0120621V007476|0205R0120621V008288|0205R0120721V000350|0205R0120721V000845|0205R0120721V001697|0205R0120721V002026|0205R0120721V002734|0205R0120721V003121|0205R0120721V003831|0205R0120721V004283|0205R0120721V005228|0205R0120821V000140|0205R0120821V001073|0205R0120821V001581|0205R0120821V002384|0205R0120821V002893|0205R0120821V003751|0205R0120821V004356|0205R0120821V005454|0205R0120821V006114|0205R0120921V000662|0205R0120921V001276|0205R0120921V002514|0205R0120921V003023|0205R0120921V004152|0205R0120921V004832|0205R0120921V006086|0205R0120921V006830|0205R0121021V000135|0205R0121021V000764|0205R0121021V002081|0205R0121021V002575|0205R0121021V003970|0205R0121021V004624|0205R0121021V005660|0205R0121021V006445|0205R0121021V007879|0205R0121121V000079|0205R0121121V001425|0205R0121121V002105|0205R0121121V003475|0205R0121121V004120|0205R0121121V005552|0205R0121121V006122|0205R0121121V007602|0205R0121121V008247|0205R0121221V000896|0205R0121221V001550|0205R0121221V003008|0205R0121221V003580|0205R0121221V005046|0205R0121221V005737|0205R0121221V007087|0205R0121221V007707|0205R0121221V009278")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2021/25569126", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2021/25569126/"))

parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2022"
ext <- c(
  "0205R0120122V000268|0205R0120122V001728|0205R0120122V002331|0205R0120122V003658|0205R0120122V004502|0205R0120122V005938|0205R0120122V006663|0205R0120122V008025|0205R0120122V008752|0205R0120222V000915|0205R0120222V001542|0205R0120222V002734|0205R0120222V003429|0205R0120222V004665|0205R0120222V005177|0205R0120222V006436|0205R0120322V000176|0205R0120322V001029|0205R0120322V001761|0205R0120322V003108|0205R0120322V003717|0205R0120322V005114|0205R0120322V005647|0205R0120322V006942|0205R0120322V007592|0205R0120422V000056|0205R0120422V000820|0205R0120422V002121|0205R0120422V002627|0205R0120422V003657|0205R0120422V004446|0205R0120422V005757|0205R0120422V006356|0205R0120422V007771|0205R0120522V001077|0205R0120522V002726|0205R0120522V003447|0205R0120522V006751|0205R0120522V007462|0205R0120622V000544|0205R0120622V001190|0205R0120622V002585|0205R0120622V003329|0205R0120622V004819|0205R0120622V005476|0205R0120622V006919|0205R0120622V007639|0205R0120722V000122|0205R0120722V000842|0205R0120722V002118|0205R0120722V002492|0205R0120722V004187|0205R0120722V004847|0205R0120722V006299|0205R0120722V006927|0205R0120722V008520|0205R0120822V000180|0205R0120822V001554|0205R0120822V002330|0205R0120822V003721|0205R0120822V004523|0205R0120822V005744|0205R0120822V006536|0205R0120822V008008|0205R0120822V008659|0205R0120922V000579|0205R0120922V001246|0205R0120922V002692|0205R0120922V003265|0205R0120922V004933|0205R0120922V005529|0205R0120922V007276|0205R0120922V007954|0205R0120922V009571|0205R0121022V000707|0205R0121022V002049|0205R0121022V002539|0205R0121022V004187|0205R0121022V004940|0205R0121022V006537|0205R0121022V007141|0205R0121022V008729|0205R0121022V009451|0205R0121122V001255|0205R0121122V002061|0205R0121122V003608|0205R0121122V004252|0205R0121122V005774|0205R0121122V006572|0205R0121122V008248|0205R0121122V008926|0205R0121222V000565|0205R0121222V001219|0205R0121222V002933|0205R0121222V003645|0205R0121222V005373|0205R0121222V006074|0205R0121222V007798|0205R0121222V008586|0205R0121222V010188")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2022/25569126", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2022/25569126/"))

parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2023"
ext <- c(
  "0205R0120123V000230|0205R0120123V001845|0205R0120123V002620|0205R0120123V004395|0205R0120123V005105|0205R0120123V006686|0205R0120123V007453|0205R0120123V009121|0205R0120123V009903|0205R0120223V000868|0205R0120223V001673|0205R0120223V003347|0205R0120223V003985|0205R0120223V005823|0205R0120223V006226|0205R0120223V008069|0205R0120223V008885|0205R0120323V000982|0205R0120323V001595|0205R0120323V003563|0205R0120323V004383|0205R0120323V006119|0205R0120323V006823|0205R0120323V008440|0205R0120323V009196|0205R0120323V010852|0205R0120423V000525|0205R0120423V001759|0205R0120423V002808|0205R0120423V004654|0205R0120423V005427|0205R0120423V006820|0205R0120423V008390|0205R0120523V000425|0205R0120523V002094|0205R0120523V002916|0205R0120523V004868|0205R0120523V005696|0205R0120523V007338|0205R0120523V008247|0205R0120523V009933|0205R0120523V010872|0205R0120623V000144|0205R0120623V001186|0205R0120623V003055|0205R0120623V003910|0205R0120623V005811|0205R0120623V006857|0205R0120623V008628|0205R0120623V009333|0205R0120623V010770|0205R0120723V000535|0205R0120723V002578|0205R0120723V003421|0205R0120723V005203|0205R0120723V006130|0205R0120723V007629|0205R0120723V008802|0205R0120723V010485|0205R0120723V011256|0205R0120823V001411|0205R0120823V002335|0205R0120823V004290|0205R0120823V005238|0205R0120823V006843|0205R0120823V007585|0205R0120823V009601|0205R0120823V010474|0205R0120923V000179|0205R0120923V000909|0205R0120923V002888|0205R0120923V003860|0205R0120923V005776|0205R0120923V006569|0205R0120923V008471|0205R0120923V009665|0205R0120923V011223|0205R0121023V000164|0205R0121023V002257|0205R0121023V003032|0205R0121023V005230|0205R0121023V006250|0205R0121023V008206|0205R0121023V009096|0205R0121023V011160|0205R0121023V012170|0205R0121123V000987|0205R0121123V001971|0205R0121123V003995|0205R0121123V005027|0205R0121123V007120|0205R0121123V008067|0205R0121123V010110|0205R0121123V011085|0205R0121223V000195|0205R0121223V001286|0205R0121223V003166|0205R0121223V004190|0205R0121223V006431|0205R0121223V007326|0205R0121223V009438|0205R0121223V010526|0205R0121223V012230")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2023/25569126", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2023/25569126/"))


parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2024"
ext <- c(
  "0205R0120124V000369|0205R0120124V001916|0205R0120124V003133|0205R0120124V005002|0205R0120124V006010|0205R0120124V008189|0205R0120124V009052|0205R0120124V011109|0205R0120124V012309|0205R0120224V000663|0205R0120224V001679|0205R0120224V003585|0205R0120224V004232|0205R0120224V006076|0205R0120224V007457|0205R0120224V009654|0205R0120224V010846|0205R0120324V000294|0205R0120324V001184|0205R0120324V003630|0205R0120324V004977|0205R0120324V006330|0205R0120324V007500|0205R0120324V009568|0205R0120324V010741|0205R0120324V012371|0205R0120424V000256|0205R0120424V002114|0205R0120424V003193|0205R0120424V004011|0205R0120424V004580|0205R0120424V006701|0205R0120424V008112|0205R0120424V010314|0205R0120424V011709|0205R0120524V001015|0205R0120524V004221|0205R0120524V005286|0205R0120524V007512")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2024/25569126", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2024/25569126/"))

#==================================================#
#============   28087534   ========================#
#==================================================#
parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2021"
ext <- c(
  "0205R0120121V000461|0205R0120121V001886|0205R0120121V002548|0205R0120121V004014|0205R0120121V004764|0205R0120121V006067|0205R0120121V006841|0205R0120121V008289|0205R0120221V000133|0205R0120221V001519|0205R0120221V002218|0205R0120221V003428|0205R0120221V004187|0205R0120221V005794|0205R0120221V006528|0205R0120221V007998|0205R0120321V000273|0205R0120321V001805|0205R0120321V002359|0205R0120321V003667|0205R0120321V004526|0205R0120321V005987|0205R0120321V006822|0205R0120321V008357|0205R0120321V008989|0205R0120421V000153|0205R0120421V001030|0205R0120421V002689|0205R0120421V003591|0205R0120421V004878|0205R0120421V005597|0205R0120421V007017|0205R0120421V007830|0205R0120421V009316|0205R0120521V000151|0205R0120521V001779|0205R0120521V002549|0205R0120521V003575|0205R0120521V003869|0205R0120521V005500|0205R0120521V006300|0205R0120521V007783|0205R0120521V008484|0205R0120621V000981|0205R0120621V001670|0205R0120621V003268|0205R0120621V003884|0205R0120621V005299|0205R0120621V006135|0205R0120621V007481|0205R0120621V008300|0205R0120721V000349|0205R0120721V000849|0205R0120721V001694|0205R0120721V002021|0205R0120721V002736|0205R0120721V003426|0205R0120721V003847|0205R0120721V004287|0205R0120721V005230|0205R0120821V000143|0205R0120821V001078|0205R0120821V001583|0205R0120821V002386|0205R0120821V002887|0205R0120821V003742|0205R0120821V004359|0205R0120821V005453|0205R0120821V006106|0205R0120921V000650|0205R0120921V001273|0205R0120921V002480|0205R0120921V003008|0205R0120921V004228|0205R0120921V004827|0205R0120921V006085|0205R0120921V006821|0205R0121021V000116|0205R0121021V000760|0205R0121021V002079|0205R0121021V002583|0205R0121021V003940|0205R0121021V004627|0205R0121021V005665|0205R0121021V006412|0205R0121021V007844|0205R0121121V000077|0205R0121121V001414|0205R0121121V002133|0205R0121121V003482|0205R0121121V004171|0205R0121121V005544|0205R0121121V006117|0205R0121121V007629|0205R0121121V008250|0205R0121221V000890|0205R0121221V001545|0205R0121221V003014|0205R0121221V003588|0205R0121221V005058|0205R0121221V005733|0205R0121221V007146|0205R0121221V007709|0205R0121221V009279")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2021/28087534", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2021/28087534/"))

parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2022"
ext <- c(
  "0205R0120122V000262|0205R0120122V001735|0205R0120122V002337|0205R0120122V003657|0205R0120122V004509|0205R0120122V005943|0205R0120122V006640|0205R0120122V008068|0205R0120122V008749|0205R0120222V000909|0205R0120222V001548|0205R0120222V002733|0205R0120222V003430|0205R0120222V004667|0205R0120222V005171|0205R0120222V006439|0205R0120322V000175|0205R0120322V001027|0205R0120322V001756|0205R0120322V003115|0205R0120322V003719|0205R0120322V005109|0205R0120322V005641|0205R0120322V006943|0205R0120322V007573|0205R0120422V000059|0205R0120422V000819|0205R0120422V002118|0205R0120422V002618|0205R0120422V003655|0205R0120422V004449|0205R0120422V005763|0205R0120422V006358|0205R0120422V007775|0205R0120522V001144|0205R0120522V002728|0205R0120522V003449|0205R0120522V005471|0205R0120522V006748|0205R0120522V007422|0205R0120622V000546|0205R0120622V001193|0205R0120622V002584|0205R0120622V003332|0205R0120622V004823|0205R0120622V005436|0205R0120622V006926|0205R0120622V007649|0205R0120722V000147|0205R0120722V000838|0205R0120722V002122|0205R0120722V002486|0205R0120722V004190|0205R0120722V004839|0205R0120722V006302|0205R0120722V006920|0205R0120722V008533|0205R0120822V000201|0205R0120822V001558|0205R0120822V002294|0205R0120822V003684|0205R0120822V004552|0205R0120822V005762|0205R0120822V006518|0205R0120822V007992|0205R0120822V008664|0205R0120922V000581|0205R0120922V001242|0205R0120922V002688|0205R0120922V003263|0205R0120922V004934|0205R0120922V005538|0205R0120922V007279|0205R0120922V007949|0205R0120922V009593|0205R0121022V000697|0205R0121022V002050|0205R0121022V002537|0205R0121022V004202|0205R0121022V004938|0205R0121022V006530|0205R0121022V007159|0205R0121022V008721|0205R0121022V009459|0205R0121122V001265|0205R0121122V002060|0205R0121122V003612|0205R0121122V004282|0205R0121122V005769|0205R0121122V006549|0205R0121122V008256|0205R0121122V008933|0205R0121222V000569|0205R0121222V001221|0205R0121222V002934|0205R0121222V003647|0205R0121222V005363|0205R0121222V006072|0205R0121222V007803|0205R0121222V008583|0205R0121222V010194")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2022/28087534", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2022/28087534/"))

parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2023"
ext <- c(
  "0205R0120123V000230|0205R0120123V001845|0205R0120123V002620|0205R0120123V004395|0205R0120123V005105|0205R0120123V006686|0205R0120123V007453|0205R0120123V009121|0205R0120123V009903|0205R0120223V000868|0205R0120223V001673|0205R0120223V003347|0205R0120223V003985|0205R0120223V005823|0205R0120223V006226|0205R0120223V008069|0205R0120223V008885|0205R0120323V000982|0205R0120323V001595|0205R0120323V003563|0205R0120323V004383|0205R0120323V006119|0205R0120323V006823|0205R0120323V008440|0205R0120323V009196|0205R0120323V010852|0205R0120423V000525|0205R0120423V001759|0205R0120423V002808|0205R0120423V004654|0205R0120423V005427|0205R0120423V006820|0205R0120423V008390|0205R0120523V000425|0205R0120523V002094|0205R0120523V002916|0205R0120523V004868|0205R0120523V005696|0205R0120523V007338|0205R0120523V008247|0205R0120523V009933|0205R0120523V010872|0205R0120623V000144|0205R0120623V001186|0205R0120623V003055|0205R0120623V003910|0205R0120623V005811|0205R0120623V006857|0205R0120623V008628|0205R0120623V009333|0205R0120623V010770|0205R0120723V000535|0205R0120723V002578|0205R0120723V003421|0205R0120723V005203|0205R0120723V006130|0205R0120723V007629|0205R0120723V008802|0205R0120723V010485|0205R0120723V011256|0205R0120823V001411|0205R0120823V002335|0205R0120823V004290|0205R0120823V005238|0205R0120823V006843|0205R0120823V007585|0205R0120823V009601|0205R0120823V010474|0205R0120923V000179|0205R0120923V000909|0205R0120923V002888|0205R0120923V003860|0205R0120923V005776|0205R0120923V006569|0205R0120923V008471|0205R0120923V009665|0205R0120923V011223|0205R0121023V000164|0205R0121023V002257|0205R0121023V003032|0205R0121023V005230|0205R0121023V006250|0205R0121023V008206|0205R0121023V009096|0205R0121023V011160|0205R0121023V012170|0205R0121123V000987|0205R0121123V001971|0205R0121123V003995|0205R0121123V005027|0205R0121123V007120|0205R0121123V008067|0205R0121123V010110|0205R0121123V011085|0205R0121223V000195|0205R0121223V001286|0205R0121223V003166|0205R0121223V004190|0205R0121223V006431|0205R0121223V007326|0205R0121223V009438|0205R0121223V010526|0205R0121223V012230")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2023/25569126", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2023/25569126/"))


parent.folder <- "EVIDEN HEMOFILIA RSSG 2021-2024/2024"
ext <- c(
  "0205R0120124V000369|0205R0120124V001916|0205R0120124V003133|0205R0120124V005002|0205R0120124V006010|0205R0120124V008189|0205R0120124V009052|0205R0120124V011109|0205R0120124V012309|0205R0120224V000663|0205R0120224V001679|0205R0120224V003585|0205R0120224V004232|0205R0120224V006076|0205R0120224V007457|0205R0120224V009654|0205R0120224V010846|0205R0120324V000294|0205R0120324V001184|0205R0120324V003630|0205R0120324V004977|0205R0120324V006330|0205R0120324V007500|0205R0120324V009568|0205R0120324V010741|0205R0120324V012371|0205R0120424V000256|0205R0120424V002114|0205R0120424V003193|0205R0120424V004011|0205R0120424V004580|0205R0120424V006701|0205R0120424V008112|0205R0120424V010314|0205R0120424V011709|0205R0120524V001015|0205R0120524V004221|0205R0120524V005286|0205R0120524V007512")

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("EVIDEN HEMOFILIA RSSG 2021-2024/2024/25569126", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "EVIDEN HEMOFILIA RSSG 2021-2024/2024/25569126/"))





origindir <- c("D:/Downloads/EVIDEN HEMOFILIA RSSG 2021-2024")
targetdir <- c("D:/Downloads/EVIDEN HEMOFILIA RSSG 2021-2024/M.ANANG NAFI'UZZAKI")
filestocopy <- c("1306R0010124V001852.pdf",
                 "1306R0010124V006949.pdf",
                 "1306R0010424V011003.pdf",
                 "1306R0010524V001530.pdf",
                 "1306R0010524V006396.pdf",
                 "1306R0010524V012025.pdf",
                 "1306R0010524V016389.pdf")

file.copy(from=filestocopy, to=targetdir, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

flist <- list.files(filestocopy, ".csv", full.names = TRUE)
file.copy(flist, targetdir)

parent.folder <- "Data RSUD Soegiri"
ext <- c("1306R0010124V001852|1306R0010124V006949|1306R0010424V011003|1306R0010524V001530|1306R0010524V006396|1306R0010524V012025|1306R0010524V016389")                 # Wanted file extension

my_dirs <- list.files(path = parent.folder, 
                      full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

dir.create("Data RSUD Soegiri/M.ANANG NAFI'UZZAKI", recursive = TRUE)

n <- sapply(my_dirs[grep(ext, my_dirs)], 
            FUN=function(x) file.copy(from = x, to = "Data RSUD Soegiri/M.ANANG NAFI'UZZAKI/"))

message(paste("Number of files in", parent.folder, "with", ext, ":", length(n), 
              "(successully copied:", round(sum(n)/length(n)*100, 0), "%).")) 

setwd("D:/data gresik/VPK")

surkon <- read_csv("Sheet 1_Full Data_data (surkon).csv") %>%
  select(`Status Surat Kontrol`,`Nama Faskes`,`Jenis Pelayanan`,
         `Nomor Surat Kontrol`,`Tgl Rencana Kontrol`,`Tgl Terbit Kontrol`,
         `Nama Poli Tujuan SEP`,`Nomor SEP`,`Tgl SEP`,`Tgl SEP Sebelum`,
         `Nomor SEP Asal Kontrol`,Sumber)


%>%
  subset(`Status Surat Kontrol` %in% c("Tanggal Terbit Kontrol Sama Dengan Tanggal SEP","Tanggal Terbit Kontrol Lebih Besar Tanggal SEP")) %>%
  select(-`Status Surat Kontrol`)

surkon1 <- read_csv("Sheet 1_Full Data_data_surkon.csv") %>%
  select(`Status Surat Kontrol`,`Nama Faskes`,`Jenis Pelayanan`,
         `Nomor Surat Kontrol`,`Tgl Rencana Kontrol`,`Tgl Terbit Kontrol`,
         `Nama Poli Tujuan SEP`,`Nomor SEP`,`Tgl SEP`,`Tgl SEP Sebelum`,
         `Nomor SEP Asal Kontrol`,Sumber) %>%
  subset(`Status Surat Kontrol` %in% c("Tanggal Terbit Kontrol Sama Dengan Tanggal SEP","Tanggal Terbit Kontrol Lebih Besar Tanggal SEP")) %>%
  select(-`Status Surat Kontrol`)

surkon2 <- read_csv("Sheet 1_Full Data_data_surkon.csv") %>%
  select(`Status Surat Kontrol`,`Nama Faskes`,`Jenis Pelayanan`,
         `Nomor Surat Kontrol`,`Tgl Rencana Kontrol`,`Tgl Terbit Kontrol`,
         `Nama Poli Tujuan SEP`,`Nomor SEP`,`Tgl SEP`,`Tgl SEP Sebelum`,
         `Nomor SEP Asal Kontrol`,Sumber) %>%
  subset(`Status Surat Kontrol` %in% c("Tanggal Terbit Kontrol Sama Dengan Tanggal SEP","Tanggal Terbit Kontrol Lebih Besar Tanggal SEP")) %>%
  select(-`Status Surat Kontrol`)

surkon3 <- read_csv("Sheet 1_Full Data_data_surkon feb25.csv") %>%
  select(`Status Surat Kontrol`,`Nama Faskes`,`Jenis Pelayanan`,
         `Nomor Surat Kontrol`,`Tgl Rencana Kontrol`,`Tgl Terbit Kontrol`,
         `Nama Poli Tujuan SEP`,`Nomor SEP`,`Tgl SEP`,`Tgl SEP Sebelum`,
         `Nomor SEP Asal Kontrol`,Sumber) %>%
  subset(`Status Surat Kontrol` %in% c("Tanggal Terbit Kontrol Sama Dengan Tanggal SEP","Tanggal Terbit Kontrol Lebih Besar Tanggal SEP")) %>%
  select(-`Status Surat Kontrol`)

surkon <- rbind(surkon,surkon1,surkon2,surkon3)
surkon <- surkon %>%
  arrange(desc(`Tgl SEP`)) %>% 
  distinct(`Nomor SEP`,.keep_all = T)

surkon$`Tgl Rencana Kontrol` <- as.Date(surkon$`Tgl Rencana Kontrol`, format = "%m/%d/%Y")
surkon$`Tgl Terbit Kontrol` <- as.Date(surkon$`Tgl Terbit Kontrol`, format = "%m/%d/%Y")
surkon$`Tgl SEP` <- as.Date(surkon$`Tgl SEP`, format = "%m/%d/%Y")
surkon$`Tgl SEP Sebelum` <- as.Date(surkon$`Tgl SEP Sebelum`, format = "%m/%d/%Y")

surkon <- surkon[order(surkon$`Nama Faskes`,surkon$`Tgl SEP`),]
surkon <- surkon[order(surkon$`Nama Faskes`,surkon$`Tgl SEP`),]
surkon <- surkon %>% subset(`Tgl SEP` >= "2025-06-01")

save(surkon, file = "surkon.rda")

write.xlsx(surkon, file = "Tanggal Terbit Kontrol Sama Dengan Tanggal SEP.xlsx")
write.csv(surkon,
          "D://data gresik//VPK//surkon.csv",
          na="", row.names = FALSE)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
rm(surkon1,surkon2)
load("ur_all.rda")
data <- data %>%
  select(-CMG,-CBG,-Spec,-tipe)

df <- left_join(surkon,data, by = c("Nomor SEP"="Nosjp")) %>%
  subset(!is.na(Biayaverifikasi)) %>%
  subset(Nmtkp != "RITL") %>%
  select(-Sevel,-Sumber.x,-Diagmasuk,-Poli_asal,-tgl_before,-jeda,-kel_umur,
         -Diagnosa,-Diagflag,-Flag,-`Nama Faskes`,-`Nama Poli Tujuan SEP`,
         -`Jenis Pelayanan`) %>%
  rename(Sumber = Sumber.y) %>%
  select(Nokapst,`Nomor SEP`,Sumber,`Nomor Surat Kontrol`,
         `Nomor SEP Asal Kontrol`,`Tgl Rencana Kontrol`,`Tgl Terbit Kontrol`,
         Norjkawalsep,Nmppklayan,Nmtkp,Politujsjp,Tglpelayanan,Tgldtgsjp,
         Tglplgsjp,`Tgl SEP Sebelum`,Kdinacbgs,Nminacbgs,Kddiagprimer,
         Nmdiagprimer,Diagsekunder,Procedure,Tglstjkeu,Namadpjp,Nmjnspulang,
         biayars,Biayaverifikasi)

write.csv(df,
          "D://data gresik//VPK//aak_surkon.csv",
          na="", row.names = FALSE)

df <- df[order(df$Nmppklayan,df$Tgldtgsjp),]
write.xlsx(df, file = "AAK_Tanggal Terbit Kontrol Sama Dengan Tanggal SEP.xlsx")


setwd("D:/data gresik/VPK")

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

pending <- read_csv("Sheet 1_Full Data_data.csv") %>%
  select(Nosep,alasan_pending,Bupel,Bytagsep,
         Flagprsklaimsep,Kdcbgs,Kdppklayan,Nmdati2Layan,Nminacbgs,
         Nmtkp,Statusklaim,Tglplgsep,Tglsep)

pending <- pending %>%
  mutate(
    Nmppklayan = case_when(
      Kdppklayan == "0185R003" ~ "RSU An Nisaa",
      Kdppklayan == "0185R004" ~ "RSU Aulia",
      Kdppklayan == "0185R005" ~ "RSU Ananda",
      Kdppklayan == "0185R008" ~ "RSU Al Ittihad",
      Kdppklayan == "0185R009" ~ "RS Medika Utama",
      Kdppklayan == "0185R010" ~ "RSUD Srengat",
      Kdppklayan == "0185R011" ~ "RS Wava Husada Kesamben",
      Kdppklayan == "0185S001" ~ "KU Puspa Husada",
      Kdppklayan == "0186R002" ~ "RSU Aura Syifa",
      Kdppklayan == "0186R003" ~ "RS Muhammadiyah Siti Khodijah",
      Kdppklayan == "0186R004" ~ "IHC RS Toeloengredjo (HVA)",
      Kdppklayan == "0186R005" ~ "RS Arga Husada",
      Kdppklayan == "0186R007" ~ "RSU Muhammadiyah Surya Melati",
      Kdppklayan == "0186R012" ~ "RSU Wilujeng",
      Kdppklayan == "0186R013" ~ "RS Amelia",
      Kdppklayan == "0186R016" ~ "RSUD Simpang Lima Gumul Kediri",
      Kdppklayan == "0186S001" ~ "KU Rawat Inap Medika Utama",
      Kdppklayan == "0186S002" ~ "KU Rawat Inap Dia Husada",
      Kdppklayan == "0198R004" ~ "RSI Aisyiyah",
      Kdppklayan == "0198S001" ~ "Klinik Mata EDC Warujayeng",
      Kdppklayan == "0198S003" ~ "Klinik Mata Ayu Siwi",
      Kdppklayan == "0210R005" ~ "RS Daha Husada",
      Kdppklayan == "0210R006" ~ "RSU TNI AD Tk. IV DKT",
      Kdppklayan == "0210R007" ~ "RSU Ratih",
      Kdppklayan == "0210R008" ~ "RS Muhammadiyah Ahmad Dahlan",
      Kdppklayan == "0210R009" ~ "RSIA Melinda",
      Kdppklayan == "0210R010" ~ "RS Baptis",
      Kdppklayan == "0210R016" ~ "RSU Lirboyo",
      Kdppklayan == "0210R017" ~ "RSIA Citra Keluarga",
      Kdppklayan == "0210R018" ~ "RSIA Nirmala Kediri",
      Kdppklayan == "0210R019" ~ "RSUD Kilisuci",
      Kdppklayan == "0210R021" ~ "RSGM IIK Bhakti Wiyata",
      Kdppklayan == "0211R004" ~ "RSU Aminah",
      Kdppklayan == "0211R006" ~ "RS Katolik Budi Rahayu",
      Kdppklayan == "0211R008" ~ "RS Syuhada' Haji",
      Kdppklayan == "0211R009" ~ "RSI Aminah",
      Kdppklayan == "1313R001" ~ "RSUD Mardi Waluyo",
      Kdppklayan == "1314R001" ~ "RSUD Ngudi Waluyo",
      Kdppklayan == "1317R001" ~ "RSUD Gambiran",
      Kdppklayan == "1317R003" ~ "RS Bhayangkara Kediri",
      Kdppklayan == "1318R001" ~ "RSUD Kabupaten Kediri",
      Kdppklayan == "1319R001" ~ "RSUD Nganjuk",
      Kdppklayan == "1319R002" ~ "RSD Kertosono",
      Kdppklayan == "1319R003" ~ "RS Bhayangkara Nganjuk",
      TRUE ~ Kdppklayan)) %>%
  select(-Kdppklayan)

pending$Tglsep <- as.Date(pending$Tglsep, format = "%m/%d/%Y")
pending$Tglplgsep <- as.Date(pending$Tglplgsep, format = "%m/%d/%Y")


pending <- read_csv("Sheet 1_Full Data_pending3.csv") %>%
  select(Nosep,Statusklaim,Flagprsklaimsep,alasan_pending,`Bulan Layan`,
         `Bulan Proses`,Bupel,Tglsep,Tglplgsep,Kdcbgs,Nminacbgs,
         Nmdati2Layan,Nmppklayan,Nmtkp,Bytagsep)

pending <- read_csv("Sheet 1_Full Data_pending2.csv") %>%
  select(Nosep,Statusklaim,Flagprsklaimsep,alasan_pending,`Bulan Layan`,
         `Bulan Proses`,Bupel,Tglsep,Tglplgsep,Kdcbgs,Nminacbgs,
         Nmdati2Layan,Nmppklayan,Nmtkp,Bytagsep)

write.csv(pending,
          "D://data gresik//VPK//pending_tab2.csv",
          na="", row.names = FALSE)


feb23 <- read_delim("Kepwil_data_feb23.csv") %>%
  mutate(Bulan = "1/2/2023")

file2 <- read_csv("Sheet 1_Full Data_data.csv")
file3 <- read_csv("Sheet 1_Full Data_data (3).csv")

anc_dobel <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                          password = "Anomali7",
                          header = TRUE,
                          row.names = NULL,
                          col.names = NULL,
                          xl.sheet = "1. Klaim ANC Dobel",
                          top.left.cell = "A1",
                          na = "",
                          excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

setwd("D:/data gresik/monthlyreviu")
rjn1 <- read_csv("SEP Terbit Detail_buban nov23 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban nov23 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban nov23 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban nov23 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban nov23 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban nov23 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban nov23 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban nov23 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban nov23 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban nov23 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban nov23 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban nov23 ri n6.csv")

bubannov23 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubannov23 <- bubannov23 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "11/1/2023")

bubannov23$Bupel <- with(bubannov23,
                         ifelse(Flag == "N-1", "10/1/2023",
                                ifelse(Flag == "N-2", "9/1/2023",
                                       ifelse(Flag == "N-3", "8/1/2023",
                                              ifelse(Flag == "N-4", "7/1/2023",
                                                     ifelse(Flag == "N-5", "6/1/2023",
                                                            ifelse(Flag == "N-6", "5/1/2023",
                                                                   "Salah")))))))



rjn1 <- read_csv("SEP Terbit Detail_buban des23 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban des23 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban des23 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban des23 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban des23 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban des23 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban des23 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban des23 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban des23 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban des23 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban des23 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban des23 ri n6.csv")

bubandes23 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubandes23 <- bubandes23 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "12/1/2023")

bubandes23$Bupel <- with(bubandes23,
                         ifelse(Flag == "N-1", "11/1/2023",
                                ifelse(Flag == "N-2", "10/1/2023",
                                       ifelse(Flag == "N-3", "9/1/2023",
                                              ifelse(Flag == "N-4", "8/1/2023",
                                                     ifelse(Flag == "N-5", "7/1/2023",
                                                            ifelse(Flag == "N-6", "6/1/2023",
                                                                   "Salah")))))))




rjn1 <- read_csv("SEP Terbit Detail_buban jan24 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban jan24 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban jan24 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban jan24 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban jan24 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban jan24 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban jan24 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban jan24 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban jan24 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban jan24 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban jan24 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban jan24 ri n6.csv")

bubanjan24 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubanjan24 <- bubanjan24 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "1/1/2024")

bubanjan24$Bupel <- with(bubanjan24,
                         ifelse(Flag == "N-1", "12/1/2023",
                                ifelse(Flag == "N-2", "11/1/2023",
                                       ifelse(Flag == "N-3", "10/1/2023",
                                              ifelse(Flag == "N-4", "9/1/2023",
                                                     ifelse(Flag == "N-5", "8/1/2023",
                                                            ifelse(Flag == "N-6", "7/1/2023",
                                                                   "Salah")))))))

rjn1 <- read_csv("SEP Terbit Detail_buban feb24 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban feb24 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban feb24 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban feb24 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban feb24 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban feb24 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban feb24 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban feb24 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban feb24 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban feb24 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban feb24 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban feb24 ri n6.csv")

bubanfeb24 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubanfeb24 <- bubanfeb24 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "2/1/2024")

bubanfeb24$Bupel <- with(bubanfeb24,
                         ifelse(Flag == "N-1", "1/1/2024",
                                ifelse(Flag == "N-2", "12/1/2023",
                                       ifelse(Flag == "N-3", "11/1/2023",
                                              ifelse(Flag == "N-4", "10/1/2023",
                                                     ifelse(Flag == "N-5", "9/1/2023",
                                                            ifelse(Flag == "N-6", "8/1/2023",
                                                                   "Salah")))))))





rjn1 <- read_csv("SEP Terbit Detail_buban mar24 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban mar24 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban mar24 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban mar24 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban mar24 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban mar24 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban mar24 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban mar24 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban mar24 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban mar24 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban mar24 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban mar24 ri n6.csv")

bubanmar24 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubanmar24 <- bubanmar24 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "3/1/2024")

bubanmar24$Bupel <- with(bubanmar24,
                         ifelse(Flag == "N-1", "2/1/2024",
                                ifelse(Flag == "N-2", "1/1/2024",
                                       ifelse(Flag == "N-3", "12/1/2023",
                                              ifelse(Flag == "N-4", "11/1/2023",
                                                     ifelse(Flag == "N-5", "10/1/2023",
                                                            ifelse(Flag == "N-6", "9/1/2023",
                                                                   "Salah")))))))





rjn1 <- read_csv("SEP Terbit Detail_buban apr24 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban apr24 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban apr24 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban apr24 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban apr24 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban apr24 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban apr24 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban apr24 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban apr24 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban apr24 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban apr24 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban apr24 ri n6.csv")

bubanapr24 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubanapr24 <- bubanapr24 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "4/1/2024")

bubanapr24$Bupel <- with(bubanapr24,
                         ifelse(Flag == "N-1", "3/1/2024",
                                ifelse(Flag == "N-2", "2/1/2024",
                                       ifelse(Flag == "N-3", "1/1/2024",
                                              ifelse(Flag == "N-4", "12/1/2023",
                                                     ifelse(Flag == "N-5", "11/1/2023",
                                                            ifelse(Flag == "N-6", "10/1/2023",
                                                                   "Salah")))))))





rjn1 <- read_csv("SEP Terbit Detail_buban mei24 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban mei24 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban mei24 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban mei24 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban mei24 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban mei24 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban mei24 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban mei24 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban mei24 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban mei24 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban mei24 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban mei24 ri n6.csv")

bubanmei24 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubanmei24 <- bubanmei24 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "5/1/2024")

bubanmei24$Bupel <- with(bubanmei24,
                         ifelse(Flag == "N-1", "4/1/2024",
                                ifelse(Flag == "N-2", "3/1/2024",
                                       ifelse(Flag == "N-3", "2/1/2024",
                                              ifelse(Flag == "N-4", "1/1/2024",
                                                     ifelse(Flag == "N-5", "12/1/2023",
                                                            ifelse(Flag == "N-6", "11/1/2023",
                                                                   "Salah")))))))





rjn1 <- read_csv("SEP Terbit Detail_buban jun24 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban jun24 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban jun24 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban jun24 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban jun24 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban jun24 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban jun24 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban jun24 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban jun24 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban jun24 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban jun24 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban jun24 ri n6.csv")

bubanjun24 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubanjun24 <- bubanjun24 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "6/1/2024")

bubanjun24$Bupel <- with(bubanjun24,
                         ifelse(Flag == "N-1", "5/1/2024",
                                ifelse(Flag == "N-2", "4/1/2024",
                                       ifelse(Flag == "N-3", "3/1/2024",
                                              ifelse(Flag == "N-4", "2/1/2024",
                                                     ifelse(Flag == "N-5", "1/1/2024",
                                                            ifelse(Flag == "N-6", "12/1/2023",
                                                                   "Salah")))))))





rjn1 <- read_csv("SEP Terbit Detail_buban jul24 rj n1.csv")
rin1 <- read_csv("SEP Terbit Detail_buban jul24 ri n1.csv")
rjn2 <- read_csv("SEP Terbit Detail_buban jul24 rj n2.csv")
rin2 <- read_csv("SEP Terbit Detail_buban jul24 ri n2.csv")
rjn3 <- read_csv("SEP Terbit Detail_buban jul24 rj n3.csv")
rin3 <- read_csv("SEP Terbit Detail_buban jul24 ri n3.csv")
rjn4 <- read_csv("SEP Terbit Detail_buban jul24 rj n4.csv")
rin4 <- read_csv("SEP Terbit Detail_buban jul24 ri n4.csv")
rjn5 <- read_csv("SEP Terbit Detail_buban jul24 rj n5.csv")
rin5 <- read_csv("SEP Terbit Detail_buban jul24 ri n5.csv")
rjn6 <- read_csv("SEP Terbit Detail_buban jul24 rj n6.csv")
rin6 <- read_csv("SEP Terbit Detail_buban jul24 ri n6.csv")

bubanjul24 <- rbind(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
                    rin1,rin2,rin3,rin4,rin5,rin6)
rm(rjn1,rjn2,rjn3,rjn4,rjn5,rjn6,
   rin1,rin2,rin3,rin4,rin5,rin6)

bubanjul24 <- bubanjul24 %>%
  subset(`Measure Names` %in% c("Terbit Tanpa Anomali","Layak Proses Verifikasi",
                                "Dispute","Pending","Tidak Layak",
                                "Total Diajukan","Sudah Dibebankan")) %>%
  select(-Insertdate,-Cabang) %>%
  mutate(Buban = "7/1/2024")

bubanjul24$Bupel <- with(bubanjul24,
                         ifelse(Flag == "N-1", "6/1/2024",
                                ifelse(Flag == "N-2", "5/1/2024",
                                       ifelse(Flag == "N-3", "4/1/2024",
                                              ifelse(Flag == "N-4", "3/1/2024",
                                                     ifelse(Flag == "N-5", "2/1/2024",
                                                            ifelse(Flag == "N-6", "1/1/2024",
                                                                   "Salah")))))))
                                
monthlyreviu <- rbind(bubannov23,bubandes23,bubanjan24,bubanfeb24,
                      bubanmar24,bubanapr24,bubanmei24,bubanjun24,
                      bubanjul24)
rm(bubannov23,bubandes23,bubanjan24,bubanfeb24,
   bubanmar24,bubanapr24,bubanmei24,bubanjun24,
   bubanjul24)

monthlyreviu$Buban <- as.Date(monthlyreviu$Buban, format = "%m/%d/%Y")
monthlyreviu$Bupel <- as.Date(monthlyreviu$Bupel, format = "%m/%d/%Y")

write.csv(monthlyreviu,
          "D://data gresik//monthlyreviu//monthlyreviu.csv",
          na="", row.names = FALSE)

data_4 <- read_csv("Sheet 1_Full Data_data (4).csv")
data_5 <- read_csv("Sheet 1_Full Data_data (5).csv")
data_6 <- read_csv("Sheet 1_Full Data_data (6).csv") 

anc_dobel <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                          password = "Anomali7",
                          header = TRUE,
                          row.names = NULL,
                          col.names = NULL,
                          xl.sheet = "1. Klaim ANC Dobel",
                          top.left.cell = "A1",
                          na = "",
                          excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

pnc_dobel <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                          password = "Anomali7",
                          header = TRUE,
                          row.names = NULL,
                          col.names = NULL,
                          xl.sheet = "2. Klaim PNC Dobel",
                          top.left.cell = "A1",
                          na = "",
                          excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

persalinan_dobel <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                                 password = "Anomali7",
                                 header = TRUE,
                                 row.names = NULL,
                                 col.names = NULL,
                                 xl.sheet = "3. Klaim Persalinan Dobel",
                                 top.left.cell = "A1",
                                 na = "",
                                 excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

ritp_dobel <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                           password = "Anomali7",
                           header = TRUE,
                           row.names = NULL,
                           col.names = NULL,
                           xl.sheet = "4. Klaim Inap FKTP Dobel",
                           top.left.cell = "A1",
                           na = "",
                           excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK") %>%
  mutate(Nmdiagnosa = paste0(as.character(Kddiagnosa)," - ",
                           as.character(Nmdiagnosa))) %>%
  select(-Kddiagnosa)
  
mri_dobel <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                          password = "Anomali7",
                          header = TRUE,
                          row.names = NULL,
                          col.names = NULL,
                          xl.sheet = "5. dobel klaim mri",
                          top.left.cell = "A1",
                          na = "",
                          excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

ctscan_dobel <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                             password = "Anomali7",
                             header = TRUE,
                             row.names = NULL,
                             col.names = NULL,
                             xl.sheet = "6. dobel klaim ct scan",
                             top.left.cell = "A1",
                             na = "",
                             excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

usg_lain <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                         password = "Anomali7",
                         header = TRUE,
                         row.names = NULL,
                         col.names = NULL,
                         xl.sheet = "7.usg kehamilan dikode usg lain",
                         top.left.cell = "A1",
                         na = "",
                         excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

kraniotomi <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                           password = "Anomali7",
                           header = TRUE,
                           row.names = NULL,
                           col.names = NULL,
                           xl.sheet = "8. kraniotomi hidrosefalus",
                           top.left.cell = "A1",
                           na = "",
                           excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

inj_ia_rinap <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                             password = "Anomali7",
                             header = TRUE,
                             row.names = NULL,
                             col.names = NULL,
                             xl.sheet = "9.injeksi intraartikular inap",
                             top.left.cell = "A1",
                             na = "",
                             excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

bayinonkelas <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                             password = "Anomali7",
                             header = TRUE,
                             row.names = NULL,
                             col.names = NULL,
                             xl.sheet = "10. Bayi rawat nonkelas",
                             top.left.cell = "A1",
                             na = "",
                             excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

gea_los_12hr <- xl.read.file("Copy of Data Feedback Anomali Tahap 7 - Copy.xlsx",
                             password = "Anomali7",
                             header = TRUE,
                             row.names = NULL,
                             col.names = NULL,
                             xl.sheet = "11. GEA tnp dehidrasi LOS 1-2hr",
                             top.left.cell = "A1",
                             na = "",
                             excel.visible = FALSE) %>%
  subset(Nmkclayan == "GRESIK")

anc_pnc <- rbind(anc_dobel,pnc_dobel)
ritp <- rbind(persalinan_dobel,ritp_dobel)
fkrtl <- rbind(mri_dobel,ctscan_dobel,usg_lain,kraniotomi,inj_ia_rinap,
               bayinonkelas,gea_los_12hr)

anc_pnc <- anc_pnc[order(anc_pnc$Nokapst,anc_pnc$Tglkunjungan),]
ritp <- ritp[order(ritp$Nokapst,ritp$Tglkunjungan),]
fkrtl <- fkrtl[order(fkrtl$Nokapst,fkrtl$Tglplgsjp),]

rm(anc_dobel,pnc_dobel,persalinan_dobel,ritp_dobel,mri_dobel,ctscan_dobel,
   usg_lain,kraniotomi,inj_ia_rinap,bayinonkelas,gea_los_12hr)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

lapaudit <- read.xlsx(xlsxFile = "lapaudit.xlsx",
                      sheet = "Sheet1",
                      colNames = TRUE,
                      rowNames = FALSE,
                      detectDates = TRUE,
                      fillMergedCells = TRUE)
df <- left_join(lapaudit,df_data, by = c("Nosjp"="Nosjp"))
df <- df %>%
  select(-koreksi)
write.xlsx(df, file = "lapaudit_tab.xlsx")

hasil5 <- read.xlsx(xlsxFile = "REKAP HASIL AUDIT BULANAN.xlsx",
                 sheet = "Sheet5",
                 colNames = TRUE,
                 rowNames = FALSE,
                 detectDates = TRUE,
                 fillMergedCells = TRUE) %>%
  select(Nosjp,KETERANGAN,BIAYA.INEFISIENSI) %>%
  rename(Keterangan = KETERANGAN,
         Efisiensi = BIAYA.INEFISIENSI)
auditkepwil4 <- rbind(hasil2,hasil3,hasil4,hasil5)

lapaudit <- rbind(audithppk,auditkepwil3,auditkepwil4,auditketidaksesuaian1,
                  auditmku3,auditmku4)

write.xlsx(lapaudit, file = "lapaudit.xlsx")

auditmku3 <- auditmku3 %>%
  rename(Efisiensi = INEFISIENSI,
         Keterangan = KETERANGAN)

task$nosjp <- as.integer(task$nosjp)

task <- task %>%
  group_by(Faskes) %>%
  dplyr::summarise(kasus = sum(nosjp))

task1 <- read_csv("Bar RS_data (1).csv") %>%
  select(-3,-4) %>%
  rename(Faskes = `Nama +  Kode Faskes`,
         aproval = `COUNTD NOSEP`)
task2 <- left_join(task,task1, by = "Faskes")
write.xlsx(task2, file = "task2.xlsx")


task <- xl.read.file("Detail (1).xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "Sheet1",
                      top.left.cell = "A1",
                      na = "",
                      excel.visible = FALSE)

ranap <- xl.read.file("Ranap Juni 2024.xlsx",
                     header = TRUE,
                     row.names = NULL,
                     col.names = NULL,
                     xl.sheet = "sheet1",
                     top.left.cell = "A4",
                     na = "",
                     excel.visible = FALSE) %>%
  subset(!is.na(`No. Klaim / SEP`)) %>%
  select(-`No.`) %>%
  rename(Tgldtgsjp = `Tgl. Masuk`,
         Tglplgsjp = `Tgl. Pulang`,
         Nosjp = `No. Klaim / SEP`,
         NoRM = `No. RM`,
         Nmtkp1 = Jenis,
         Nmpst = `Nama Pasien`,
         Topup = `Top Up`,
         Tarif = `Total Tarif`,
         biayars = `Tarif RS`)

rajal <- xl.read.file("RAJAL Juni 2024.xlsx",
                      header = TRUE,
                      row.names = NULL,
                      col.names = NULL,
                      xl.sheet = "sheet1",
                      top.left.cell = "A4",
                      na = "",
                      excel.visible = FALSE) %>%
  subset(!is.na(`No. Klaim / SEP`)) %>%
  select(-`No.`) %>%
  rename(Tgldtgsjp = `Tgl. Masuk`,
         Tglplgsjp = `Tgl. Pulang`,
         Nosjp = `No. Klaim / SEP`,
         NoRM = `No. RM`,
         Nmtkp1 = Jenis,
         Nmpst = `Nama Pasien`,
         Topup = `Top Up`,
         Tarif = `Total Tarif`,
         biayars = `Tarif RS`)
klaim <- rbind(rajal,ranap)
rm(rajal,ranap)

klaim <- klaim %>%
  mutate(Nmtkp = case_when(
    str_detect(Nmtkp1, "RJ") ~ "RJTL",
    str_detect(Nmtkp1, "RI") ~ "RITL",
    TRUE ~ Nmtkp1))

klaim <- concat.split(klaim,"Tgldtgsjp"," ")
klaim <- klaim %>%
  mutate(Tgldtgsjp_4 = case_when(
    str_detect(Tgldtgsjp_2, "Mei") ~ "05",
    str_detect(Tgldtgsjp_2, "Jun") ~ "06",
    TRUE ~ Tgldtgsjp_2))

klaim$Tgldtgsjp_1 <- as.numeric(klaim$Tgldtgsjp_1)
klaim$Tgldtgsjp_3 <- as.numeric(klaim$Tgldtgsjp_3)

klaim$Tgldtgsjp_5 <- with(klaim,
                          ifelse(Tgldtgsjp_1 < 10,
                                 paste0("0",Tgldtgsjp_1), Tgldtgsjp_1))
klaim$Tgldtgsjp_6 <- paste0(as.character(klaim$Tgldtgsjp_5 ),"/",
                            as.character(klaim$Tgldtgsjp_4),"/",
                            as.character(klaim$Tgldtgsjp_3 ))
klaim$Tgldtgsjp_6 <- as.Date(klaim$Tgldtgsjp_6, format = "%d/%m/%Y")

klaim <- concat.split(klaim,"Tglplgsjp"," ")
klaim <- klaim %>%
  mutate(Tglplgsjp_4 = case_when(
    str_detect(Tglplgsjp_2, "Mei") ~ "05",
    str_detect(Tglplgsjp_2, "Jun") ~ "06",
    TRUE ~ Tglplgsjp_2))

klaim$Tglplgsjp_1 <- as.numeric(klaim$Tglplgsjp_1)
klaim$Tglplgsjp_3 <- as.numeric(klaim$Tglplgsjp_3)

klaim$Tglplgsjp_5 <- with(klaim,
                          ifelse(Tglplgsjp_1 < 10,
                                 paste0("0",Tglplgsjp_1), Tglplgsjp_1))
klaim$Tglplgsjp_6 <- paste0(as.character(klaim$Tglplgsjp_5 ),"/",
                            as.character(klaim$Tglplgsjp_4),"/",
                            as.character(klaim$Tglplgsjp_3 ))
klaim$Tglplgsjp_6 <- as.Date(klaim$Tglplgsjp_6, format = "%d/%m/%Y")

klaim <- klaim %>%
  select(-Tgldtgsjp,-Tgldtgsjp_1,-Tgldtgsjp_2,-Tgldtgsjp_3,-Tgldtgsjp_4,
         -Tgldtgsjp_5,-Tglplgsjp,-Tglplgsjp_1,-Tglplgsjp_2,-Tglplgsjp_3,
         -Tglplgsjp_4,-Tglplgsjp_5,-Nmtkp1) %>%
  rename(Tgldtgsjp = Tgldtgsjp_6,
         Tglplgsjp = Tglplgsjp_6)
klaim <- klaim[order(klaim$NoRM, klaim$Tglplgsjp),]
klaim <- klaim %>%
  group_by(NoRM) %>%
  mutate(kunjung_ke = sequence(n()))

klaim <- klaim[order(klaim$NoRM,klaim$Tglplgsjp),]
klaim <- klaim %>% group_by(NoRM,Nmpst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
klaim$interval <- as.integer(klaim$Tglplgsjp) - as.integer(klaim$tgl_before)

df_klaim <- left_join(klaim,df, by = c("Nosjp"="Nosjp"))
df_klaim <- df_klaim %>%
  select(NoRM,Nokapst,Nmpst,Nosjp,Norjkawalsep,Sumber,Nmppk,Kelasrsmenkes,
         Nmppkperujuk,Kelasrsperujuk,Nmtkp,Tgldtgsjp,Tglplgsjp,tgl_before.x,
         interval,Politujsjp,Poli_asal,Tglpelayanan,tgl_before.y,jeda,INACBG,
         Kddiag,Nmdiag,Topup,Tarif,biayars,kunjung_ke.x,Potensiprb)
df_klaim <- df_klaim[order(df_klaim$NoRM, klaim$Tglplgsjp),]
write.xlsx(df_klaim, file = "RSI NU Jun24.xlsx")
#========================================================================


data14$Diagprimer <- paste0(as.character(data$Kddiagprimer)," - ",
                            as.character(data$Nmdiagprimer))


ranap$`Tgl. Masuk` <- anytime::anydate(ranap$`Tgl. Masuk`)

%>%
  mutate(Tgldtgsjp = match(`Tgl. Masuk`, tolower(month.abb)))

library(splitstackshape)
ranap$`Tgl. Masuk` <- concat.split(ranap$`Tgl. Masuk`, " ")



parse_date_time(ranap$`Tgl. Masuk`,c("mdy"))
ranap$`Tgl. Masuk` <- as.Date(ranap$`Tgl. Masuk`,format = "%m/%d/%Y")

%>%
  subset(kantor_kap == "GRESIK")

bpk <- xl.read.file("kap_pbijk.xlsx",
                     password = "K4ppbijk",
                     header = TRUE,
                     row.names = NULL,
                     col.names = NULL,
                     xl.sheet = "BPK",
                     top.left.cell = "B1",
                     na = "",
                     excel.visible = FALSE) %>%
  subset(`Nmkc Kap` == "GRESIK")

write.xlsx(bpkp, file = "bpkp.xlsx")
write.xlsx(bpk, file = "bpk.xlsx")

sip <- read.xlsx2()


(xlsxFile = "SIP Dokter Sejatim- 27 Juni 2024.xlsx",
                 sheet = "SIP Dokter RS_Full Data_data",
                 colNames = TRUE,
                 rowNames = FALSE,
                 detectDates = TRUE,
                 fillMergedCells = TRUE) %>%
  subset(`NMKC` == "GRESIK") %>%
  select(NIK)
sip1 <- sip %>% distinct()
sip <- read.xlsx(xlsxFile = "SIP Dokter Sejatim- 27 Juni 2024.xlsx",
                 sheet = "SIP Dokter RS_Full Data_data",
                 colNames = TRUE,
                 rowNames = FALSE,
                 detectDates = TRUE,
                 fillMergedCells = TRUE)

sip <- sip %>%
  mutate(
    hari = case_when(
      HARI == "SENIN" ~ "1_SENIN",
      HARI == "SELASA" ~ "2_SELASA",
      HARI == "RABU" ~ "3_RABU",
      HARI == "KAMIS" ~ "4_KAMIS",
      HARI == "JUMAT" ~ "5_JUMAT",
      HARI == "SABTU" ~ "6_SABTU",
      HARI == "MINGGU" ~ "7_MINGGU",
      HARI == "LIBUR NASIONAL" ~ "8_LIBUR NASIONAL",
      TRUE ~ HARI))
sip <- sip %>% select(-HARI) %>%
  rename(HARI = hari)

sip2 <- sip %>%
  mutate(JAMPRAKTEK = na_if(JAMPRAKTEK, "-"))
sip2$NomorSIP <- trimws((sip2$NomorSIP),"both")
sip2$TAT.SIP <- as.Date(sip2$TAT.SIP)
sip2$TMT.SIP <- as.Date(sip2$TMT.SIP)

sip3 <- left_join(sip1,sip2, by = c("NIK"="NIK"))
write.xlsx(sip3, file = "SIP Dokter Gresik Sejatim- 27 Juni 2024.xlsx")

data23 <- read.xlsx(xlsxFile = "PIN-F koreksi23.xlsx",
                    sheet = "Sheet 1",
                    colNames = TRUE,
                    rowNames = FALSE,
                    detectDates = TRUE,
                    fillMergedCells = TRUE) %>%
  select(NOSEP,TGLSEP,Tingkat.Pelayanan,KDCBGS,NMCBGS,KDCBGSVER,
         NMCBGSVER,Efisiensi.Biaya,BYTAGSEP,BYVERSEP) %>%
  rename(Nosjp = NOSEP,
         Nmtkp = Tingkat.Pelayanan,
         Tglsep = TGLSEP,
         Kdinacbgs = KDCBGS,
         Nminacbgs = NMCBGS,
         Efisiensi = Efisiensi.Biaya)
data24 <- read.xlsx(xlsxFile = "PIN-F koreksi24.xlsx",
                    sheet = "Sheet 1",
                    colNames = TRUE,
                    rowNames = FALSE,
                    detectDates = TRUE,
                    fillMergedCells = TRUE) %>%
  select(NOSEP,TGLSEP,Tingkat.Pelayanan,KDCBGS,NMCBGS,KDCBGSVER,
         NMCBGSVER,Efisiensi.Biaya,BYTAGSEP,BYVERSEP) %>%
  rename(Nosjp = NOSEP,
         Nmtkp = Tingkat.Pelayanan,
         Tglsep = TGLSEP,
         Kdinacbgs = KDCBGS,
         Nminacbgs = NMCBGS,
         Efisiensi = Efisiensi.Biaya)
datkor <- rbind(data23,data24)
rm(data23,data24)

datkor$Tglsep <- as.Date(datkor$Tglsep, format = "%m/%d/%Y")
datkor <- datkor[order(datkor$Tglsep, datkor$Nosjp),]
datkor$Nminacbgs <- as.character(trimws(datkor$Nminacbgs), "both")
datkor$NMCBGSVER <- as.character(trimws(datkor$NMCBGSVER), "both")

#agar bisa SUBSET
datkor$Kdppklayan <- as.character(trimws(substr(datkor$Nosjp,1,8)), "both")
datkor <- datkor %>%
  mutate(koreksi = paste0(as.character(Kdppklayan),"; ",
                           as.character(Kdinacbgs)))

%>%
  select(-Kdppklayan,-Tglsep,-BYTAGSEP)

df <- left_join(df_ruj,data, by = c("Norjkawalsep"="no_kunjungan"))

write.csv(datkor, "D://data gresik//MTF KC GRESIK//gresik_2020//pot_koreksi.csv",
          na="", row.names = FALSE)

#===============================SKIP======================================
datkor <- datkor %>%
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





data <- xl.read.file("PIN-F.xlsx",
                     header = TRUE,
                     row.names = NULL,
                     col.names = NULL,
                     xl.sheet = "Sheet",
                     top.left.cell = "B1",
                     na = "",
                     excel.visible = FALSE)

data <- data %>% select(`NOSJP Asal`,`KDINACBGS ASAL`,`KDINACBGS KOR`,
                        `BY TAGIHAN`,`BY KOREKSI`) %>%
  rename(Nosjp = `NOSJP Asal`,
         Kdinacbgs = `KDINACBGS ASAL`,
         Kdinakor = `KDINACBGS KOR`,
         Biayaverifikasi = `BY TAGIHAN`,
         bykoreksi = `BY KOREKSI`) %>%
  subset(bykoreksi > 0) %>%
  subset(!is.na(Nosjp))

data <- xl.read.file("Jadwal Tenaga Medis FKTP.xlsx",
                        header = TRUE,
                        row.names = NULL,
                        col.names = NULL,
                        xl.sheet = "Sheet 1",
                        top.left.cell = "A2",
                        na = "",
                        excel.visible = FALSE)

data <- read.xls

falseEMG <- read.xlsx(xlsxFile = "7.AAK Kepwil tahap 4 Klaim UGD.xlsx",
                      sheet = "1.Potensi False emergency",
                      colNames = TRUE,
                      rowNames = FALSE,
                      detectDates = TRUE,
                      fillMergedCells = TRUE) %>%
  subset(Nmkclayan == "GRESIK") %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer))) %>%
  select(Nosjp)

New_DF <- falseEMG %>% distinct()
rm(falseEMG)

igdfrag <- read.xlsx(xlsxFile = "7.AAK Kepwil tahap 4 Klaim UGD.xlsx",
                     sheet = "2.Fragmentasi UGD",
                     colNames = TRUE,
                     rowNames = FALSE,
                     detectDates = TRUE,
                     fillMergedCells = TRUE) %>%
  subset(Nmkclayan == "GRESIK")

potfrag <- read.xlsx(xlsxFile = "7.AAK Kepwil tahap 4 Klaim UGD.xlsx",
                     sheet = "3.Potensi fragmentasi ugd",
                     colNames = TRUE,
                     rowNames = FALSE,
                     detectDates = TRUE,
                     fillMergedCells = TRUE) %>%
  subset(Nmkclayan == "GRESIK")




write.xlsx(data, file = "Jadwal Tenaga Medis FKTP_edit.xlsx")



tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")

load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()

source("http://bioconductor.org/biocLite.R")
chooseBioCmirror()
biocLite()
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
for (i in 1:length(missing)) biocLite(missing[i])



data <- read_csv("SIP Dokter RS_data.csv", sep = "\t", dec = ".", header = TRUE,
                   encoding = "UTF-8", stringsAsFactors = FALSE, quote = "") 

df_des20 <- read_csv("SIP Dokter RS_data.csv")
library(readr)
data <- read.csv(file = "MedisFKTP_data.csv", head = TRUE, sep=";")
data$TAT.SIP <- as.Date(data$TAT.SIP, format = "%m/%d/%Y")
data$TMT.SIP <- as.Date(data$TMT.SIP, format = "%m/%d/%Y")

data$NIK <- as.character(data$NIK)

write.xlsx(data, file = "MedisFKTP_data.xlsx")


x2 <- read.csv(file = "MedisFKTP.csv", head = TRUE, sep=";")

str_detect(Procedure,c("470")))

df_jan19 <- read_csv("Sheet_1_Full_Data_data gresik jan 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_feb19 <- read_csv("Sheet_1_Full_Data_data gresik feb 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mar19 <- read_csv("Sheet_1_Full_Data_data gresik mar 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_apr19 <- read_csv("Sheet_1_Full_Data_data gresik apr 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mei19 <- read_csv("Sheet_1_Full_Data_data gresik mei 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jun19 <- read_csv("Sheet_1_Full_Data_data gresik jun 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jul19 <- read_csv("Sheet_1_Full_Data_data gresik jul 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_agu19 <- read_csv("Sheet_1_Full_Data_data gresik agu 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_sep19 <- read_csv("Sheet_1_Full_Data_data gresik sep 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_okt19 <- read_csv("Sheet_1_Full_Data_data gresik okt 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_nov19 <- read_csv("Sheet_1_Full_Data_data gresik nov 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_des19 <- read_csv("Sheet_1_Full_Data_data gresik des 19.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jan20 <- read_csv("Sheet_1_Full_Data_data gresik jan 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_feb20 <- read_csv("Sheet_1_Full_Data_data gresik feb 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mar20 <- read_csv("Sheet_1_Full_Data_data gresik mar 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_apr20 <- read_csv("Sheet_1_Full_Data_data gresik apr 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mei20 <- read_csv("Sheet_1_Full_Data_data gresik mei 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jun20 <- read_csv("Sheet_1_Full_Data_data gresik jun 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jul20 <- read_csv("Sheet_1_Full_Data_data gresik jul 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_agu20 <- read_csv("Sheet_1_Full_Data_data gresik agu 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_sep20 <- read_csv("Sheet_1_Full_Data_data gresik sep 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_okt20 <- read_csv("Sheet_1_Full_Data_data gresik okt 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_nov20 <- read_csv("Sheet_1_Full_Data_data gresik nov 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_des20 <- read_csv("Sheet_1_Full_Data_data gresik des 20.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jan21 <- read_csv("Sheet_1_Full_Data_data gresik jan 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_feb21 <- read_csv("Sheet_1_Full_Data_data gresik feb 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mar21 <- read_csv("Sheet_1_Full_Data_data gresik mar 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_apr21 <- read_csv("Sheet_1_Full_Data_data gresik apr 21.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mei21 <- read_csv("Sheet_1_Full_Data_data gresik mei 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jun21 <- read_csv("Sheet_1_Full_Data_data gresik jun 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jul21 <- read_csv("Sheet_1_Full_Data_data gresik jul 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_agu21 <- read_csv("Sheet_1_Full_Data_data gresik agu 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_sep21 <- read_csv("Sheet_1_Full_Data_data gresik sep 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_okt21 <- read_csv("Sheet_1_Full_Data_data gresik okt 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_nov21 <- read_csv("Sheet_1_Full_Data_data gresik nov 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_des21 <- read_csv("Sheet_1_Full_Data_data gresik des 21.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jan22 <- read_csv("Sheet_1_Full_Data_data gresik jan 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_feb22 <- read_csv("Sheet_1_Full_Data_data gresik feb 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mar22 <- read_csv("Sheet_1_Full_Data_data gresik mar 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_apr22 <- read_csv("Sheet_1_Full_Data_data gresik apr 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mei22 <- read_csv("Sheet_1_Full_Data_data gresik mei 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jun22 <- read_csv("Sheet_1_Full_Data_data gresik jun 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jul22 <- read_csv("Sheet_1_Full_Data_data gresik jul 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_agu22 <- read_csv("Sheet_1_Full_Data_data gresik agu 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_sep22 <- read_csv("Sheet 1_Full Data_data gresik sep 22.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_okt22 <- read_csv("Sheet 1_Full Data_data gresik okt 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_nov22 <- read_csv("Sheet 1_Full Data_data gresik nov 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_des22 <- read_csv("Sheet 1_Full Data_data gresik des 22.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jan23 <- read_csv("Sheet_1_Full_Data_data gresik jan 23.csv") %>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_feb23 <- read_csv("Sheet_1_Full_Data_data gresik feb 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mar23 <- read_csv("Sheet_1_Full_Data_data gresik mar 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_apr23 <- read_csv("Sheet_1_Full_Data_data gresik apr 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mei23 <- read_csv("Sheet_1_Full_Data_data gresik mei 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jun23 <- read_csv("Sheet_1_Full_Data_data gresik jun 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jul23 <- read_csv("Sheet_1_Full_Data_data gresik jul 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_agu23 <- read_csv("Sheet_1_Full_Data_data gresik agu 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_sep23 <- read_csv("Sheet_1_Full_Data_data gresik sep 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_okt23 <- read_csv("Sheet_1_Full_Data_data gresik okt 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_nov23 <- read_csv("Sheet 1_Full Data_data gresik nov 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_des23 <- read_csv("Sheet 1_Full Data_data gresik des 23.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_jan24 <- read_csv("Sheet 1_Full Data_data gresik jan 24.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_feb24 <- read_csv("Sheet 1_Full Data_data gresik feb 24.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

df_mar24 <- read_csv("Sheet 1_Full Data_data gresik mar 24.csv")%>%
  select (Nokapst,Nosjp,Jkpst,Umur,Nmtkp,Nmppklayan,Tgldtgsjp,
          Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
          Diagsekunder,Procedure,Tarifgroup,biayars,
          Biayaverifikasi) %>% 
  mutate (selisih = Biayaverifikasi - Tarifgroup) %>%
  filter(str_detect(Procedure,c("1341")))

data <- rbind(df_jan19, df_feb19, df_mar19, df_apr19, df_mei19, df_jun19,
              df_jul19, df_agu19, df_sep19, df_okt19, df_nov19, df_des19,
              df_jan20, df_feb20, df_mar20, df_apr20, df_mei20, df_jun20,
              df_jul20, df_agu20, df_sep20, df_okt20, df_nov20, df_des20,
              df_jan21, df_feb21, df_mar21, df_apr21, df_mei21, df_jun21,
              df_jul21, df_agu21, df_sep21, df_okt21, df_nov21, df_des21,
              df_jan22, df_feb22, df_mar22, df_apr22, df_mei22, df_jun22,
              df_jul22, df_agu22, df_sep22, df_okt22, df_nov22, df_des22,
              df_jan23, df_feb23, df_mar23, df_apr23, df_mei23, df_jun23,
              df_jul23, df_agu23, df_sep23, df_okt23, df_nov23, df_des23,
              df_jan24, df_feb24, df_mar24)

rm(df_jan19, df_feb19, df_mar19, df_apr19, df_mei19, df_jun19,
   df_jul19, df_agu19, df_sep19, df_okt19, df_nov19, df_des19,
   df_jan20, df_feb20, df_mar20, df_apr20, df_mei20, df_jun20,
   df_jul20, df_agu20, df_sep20, df_okt20, df_nov20, df_des20,
   df_jan21, df_feb21, df_mar21, df_apr21, df_mei21, df_jun21,
   df_jul21, df_agu21, df_sep21, df_okt21, df_nov21, df_des21,
   df_jan22, df_feb22, df_mar22, df_apr22, df_mei22, df_jun22,
   df_jul22, df_agu22, df_sep22, df_okt22, df_nov22, df_des22,
   df_jan23, df_feb23, df_mar23, df_apr23, df_mei23, df_jun23,
   df_jul23, df_agu23, df_sep23, df_okt23, df_nov23, df_des23,
   df_jan24, df_feb24, df_mar24)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")

data$Kdppk <- as.character(trimws(substr(data$Nosjp,1,8)), "both")
#data <- data %>% subset(Nmtkp == "RJTL") %>%
#  subset(selisih == 0) 

data <- data %>% 
  mutate(
    NmFKRTL = case_when(
      Kdppk == "0204R003" ~ "RSUD Ngimbang",
      Kdppk == "0204R004" ~ "RS Suyudi Paciran (JST)",
      Kdppk == "0204R009" ~ "RSI Nashrul Ummah",
      Kdppk == "0204R010" ~ "RS Muhammadiyah Lamongan",
      Kdppk == "0204R012" ~ "RS Muhammadiyah Babat",
      Kdppk == "0204R019" ~ "RSU Muhammadiyah Babat",
      Kdppk == "0204R013" ~ "RS Fatimah",
      Kdppk == "0204R014" ~ "RS Intan Medika",
      Kdppk == "0204R015" ~ "RS Arsy Paciran",
      Kdppk == "0204R017" ~ "RS Citra Medika",
      Kdppk == "0204R018" ~ "RS Bedah Mitra Sehat",
      Kdppk == "0204S001" ~ "Klinik Mata Utama Lamongan",
      Kdppk == "0205R009" ~ "RS Denisa",
      Kdppk == "0205R011" ~ "RS Muhammadiyah Gresik",
      Kdppk == "0205R012" ~ "RS Semen Gresik",
      Kdppk == "0205R013" ~ "RS PKG Grha Husada",
      Kdppk == "0205R014" ~ "RS Petrokimia Driyorejo",
      Kdppk == "0205R019" ~ "RS Wali Songo I",
      Kdppk == "0205R021" ~ "RS Fathma Medika",
      Kdppk == "0205R022" ~ "RS Wates Husada",
      Kdppk == "0205R023" ~ "RS Surya Medika",
      Kdppk == "0205R024" ~ "RS PKU Muhammadiyah Sekapuk",
      Kdppk == "0205R025" ~ "RSI Mabarrot MWC NU Bungah",
      Kdppk == "0205R026" ~ "RS Rachmi Dewi",
      Kdppk == "0205R027" ~ "RSI Nyai Ageng Pinatih",
      Kdppk == "0205R028" ~ "RSI Cahaya Giri",
      Kdppk == "0205R029" ~ "RSUD Umar Mas'ud Bawean",
      Kdppk == "0205S100" ~ "Klinik Mata Utama",
      Kdppk == "1302R001" ~ "RSUD Ibnu Sina",
      Kdppk == "1302R002" ~ "RS Petrokimia Gresik",
      Kdppk == "1306R001" ~ "RSUD Dr Soegiri",
      Kdppk == "0205R031" ~ "RS Eka Husada",
      Kdppk == "0205R030" ~ "RS Randegansari Husada",
      Kdppk == "0204R020" ~ "RS Permata Bunda",
      Kdppk == "0204R021" ~ "RSUD Karangkembang",
      Kdppk == "0204R022" ~ "RS Nahdlatul Ulama Babat",
      Kdppk == "0204R023" ~ "RS Permata Hati",
      Kdppk == "0204R024" ~ "RS Muhammadiyah Kalikapas",
      Kdppk == "0205R032" ~	"RSIA Khodijah",
      Kdppk == "0205S101" ~	"Klinik Utama Cerme",
      TRUE ~ Kdppk))

data <- data %>% select(Nokapst,Nosjp,Jkpst,Umur,NmFKRTL,Nmtkp,Tgldtgsjp,
                        Tglplgsjp,Kdinacbgs,Kddiagprimer,Nmdiagprimer,
                        Diagsekunder,Procedure,Tarifgroup,Biayaverifikasi,
                        biayars,selisih)

data$Tgldtgsjp <- as.Date(data$Tgldtgsjp, format = "%m/%d/%Y")
data$Tglplgsjp <- as.Date(data$Tglplgsjp, format = "%m/%d/%Y")
data <- data[order(data$NmFKRTL, data$Tglplgsjp),]

data <- data %>%
  subset(Tglplgsjp >= "2019-01-01" & Tglplgsjp <= "2024-01-31")

write.xlsx(data, file = "db katarak.xlsx")

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