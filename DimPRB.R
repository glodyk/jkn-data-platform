install.packages(c("survival","survminer","data.table","lubridate","ggplot2"))

setwd("D:/data gresik/Prolanis")

library(data.table)
library(lubridate)
library(survival)
library(survminer)
library(ggplot2)

#========================================================================
# Data cleaning & cohort building
#========================================================================
prb <- fread("Sheet 1_Full Data_data (dimprb).csv", encoding="UTF-8")
prb[trimws(tglmulai) %in% c("", "NA", "NULL", "-", "."), tglmulai := NA]
prb[trimws(tglakhir) %in% c("", "NA", "NULL", "-", "."), tglakhir := NA]
prb[trimws(tgllahir) %in% c("", "NA", "NULL", "-", "."), tgllahir := NA]

prb[, tglmulai := mdy(tglmulai)]
prb[, tglakhir := mdy(tglakhir)]
prb[, tgllahir := mdy(tgllahir)]

prb <- prb[kdstatuspst == "0"]      # peserta aktif
prb[, umur := as.numeric(floor((tglmulai - tgllahir)/365.25))]
prb <- prb[umur >= 15] # (opsional — biasanya PRB hanya ≥ 15 tahun)
#========================================================================
# 1️⃣ Survival Analysis (INI PALING PENTING)
#========================================================================
# EPISODE PRB
setorder(prb, nokapst, tglmulai)

prb[, episode := rleid(nokapst)]

# tanggal akhir observasi (tanggal extract data)
cutoff <- as.Date("2026-02-10")

prb[, event := ifelse(is.na(tglakhir), 0, 1)]
prb[, enddate := fifelse(is.na(tglakhir), cutoff, tglakhir)]

prb[, survival_days := as.numeric(enddate - tglmulai)]     #lama bertahan PRB
prb <- prb[survival_days > 0]

# Membuat objek survival
surv_obj <- Surv(time = prb$survival_days,
                 event = prb$event)

#========================================================================
# Kaplan-Meier Survival Curve (analisis utama PRB)
#========================================================================
km_fit <- survfit(surv_obj ~ 1, data = prb)

ggsurvplot(km_fit,
           conf.int = TRUE,
           xlab = "Hari dalam PRB",
           ylab = "Probabilitas tetap di PRB",
           risk.table = TRUE)

# Survival berdasarkan jenis FKTP
km_fktp <- survfit(surv_obj ~ nmjnsfktpterdaftar, data = prb)

ggsurvplot(km_fktp,
           pval = TRUE,
           risk.table = TRUE)

#========================================================================
# Cox Regression (Faktor risiko keluar PRB)
#========================================================================
cox_model <- coxph(
  Surv(survival_days, event) ~ umur + segmen + jenisfaskes + nmjnsfktpterdaftar,
  data = prb
)

summary(cox_model)

#========================================================================
# Analisis dropout kategori (mudah dipahami pimpinan)
# indikator fungsi gatekeeper JKN
#========================================================================
prb[, prb_kategori :=
      fifelse(survival_days < 90, "Gagal <3 bulan",
              fifelse(survival_days < 365, "Stabil sementara",
                      "Stabil >1 tahun"))]

table(prb$prb_kategori)

#========================================================================
# Rehospitalization Outcome Study (continuity of care evaluation)
#========================================================================
# time to rehospitalization
# 30-day readmission rate

# Siapkan data klaim INA-CBGs
library(data.table)
library(lubridate)

inacbg <- fread("klaim_inacbg.csv", encoding="UTF-8")
# parsing tanggal
inacbg[, tglmasuk := as.Date(tglmasuk)]
inacbg[, tglpulang := as.Date(tglpulang)]
# hanya rawat inap
inacbg <- inacbg[jenisrawat == "RITL"]
# hanya pasien yang ada di PRB
inacbg <- inacbg[nokapst %in% prb$nokapst]

# Fokus pada pasien yang KELUAR PRB (event saja)
prb_exit <- prb[event == 1]

# Linking PRB dengan rawat inap (cari rawat inap pertama setelah tanggal keluar PRB)
setkey(prb_exit, nokapst)
setkey(inacbg, nokapst)
rehosp <- inacbg[prb_exit, allow.cartesian=TRUE]

rehosp <- rehosp[tglmasuk > tglakhir]

# Ambil rawat inap pertama:
  rehosp <- rehosp[
    order(nokapst, tglakhir, tglmasuk),
    .SD[1],
    by = .(nokapst, tglakhir)
  ]

#  Hitung waktu ke rehospitalisasi
rehosp[, days_to_readmit := as.numeric(tglmasuk - tglakhir)]
#  Gabungkan kembali ke dataset PRB:
prb_exit <- merge(prb_exit, rehosp[,.(nokapst,tglakhir,days_to_readmit)],
                  by=c("nokapst","tglakhir"),
                  all.x=TRUE)

# Buat indikator 30-day readmission
prb_exit[, readmit30 := ifelse(days_to_readmit <= 30, 1, 0)]
prb_exit[, readmit90 := ifelse(days_to_readmit <= 90, 1, 0)]
prb_exit[, readmit180 := ifelse(days_to_readmit <= 180, 1, 0)]

#========================================================================
# Analisis Utama
#========================================================================
# Readmission Rate
mean(prb_exit$readmit30, na.rm=TRUE)
mean(prb_exit$readmit90, na.rm=TRUE)

# Survival ke Rehospitalisasi / Event = masuk RS lagi
prb_exit[, rehosp_event := ifelse(is.na(days_to_readmit),0,1)]
prb_exit[, rehosp_time := fifelse(is.na(days_to_readmit),365,days_to_readmit)]
library(survival)
library(survminer)
rehosp_surv <- Surv(prb_exit$rehosp_time,
                    prb_exit$rehosp_event)
fit_rehosp <- survfit(rehosp_surv ~ 1)
ggsurvplot(fit_rehosp,
           xlab="Hari setelah keluar PRB",
           ylab="Bebas rawat inap")

# Faktor Risiko Rehospitalisasi
cox_rehosp <- coxph(
  Surv(rehosp_time, rehosp_event) ~ umur + segmen + nmjnsfktpterdaftar,
  data = prb_exit
)
summary(cox_rehosp)

#========================================================================
# Biaya rawat inap akibat kegagalan PRB
# Avoidable Hospitalization Cost (AHC)
#========================================================================

# Siapkan data keluar PRB
prb_exit <- prb[event == 1]
prb_exit <- prb_exit[, .(
  nokapst,
  tgl_keluar = tglakhir,
  diag_prb = kdpoli # atau diagnosis kronis utama jika ada
)]

# Siapkan klaim rawat inap INA-CBGs
# (nokapst,tglmasuk,kddiagprimer,biaya)
ri <- fread("inacbg_rawatinap.csv")
ri[, tglmasuk := as.Date(tglmasuk)]
ri <- ri[nokapst %in% prb_exit$nokapst]

# Hubungkan keluar PRB dengan rawat inap
setkey(prb_exit, nokapst)
setkey(ri, nokapst)
link <- ri[prb_exit, allow.cartesian=TRUE]
# Filter hanya rawat inap setelah keluar PRB:
link <- link[tglmasuk > tgl_keluar]
# Hitung selisih hari:
link[, hari_ke_ri := as.numeric(tglmasuk - tgl_keluar)]
# Ambil hanya ≤ 180 hari: Dalam studi chronic care internasional,
# 3–6 bulan adalah periode kegagalan manajemen kronis
link <- link[hari_ke_ri <= 180]

# Tentukan “diagnosis terkait PRB”
prb_icd <- c("^I10","^I11","^I12","^I13", # hipertensi
             "^E11","^E10", # diabetes
             "^I25","^I50", # jantung
             "^J44","^J45", # PPOK/asma
             "^G40","^I64") # epilepsi/stroke stabil

link[, kronis_terkait :=
       Reduce(`|`, lapply(prb_icd, function(x)
         grepl(x, kddiagprimer)))]
# Ambil hanya yang relevan:
avoidable <- link[kronis_terkait == TRUE]

# Ambil rawat inap pertama saja
avoidable <- avoidable[
  order(nokapst, hari_ke_ri),
  .SD[1],
  by=nokapst
]

# Hitung biaya yang bisa dicegah
total_avoidable_cost <- sum(avoidable$biaya, na.rm=TRUE)
total_avoidable_cost
#Biaya rata-rata:
mean_cost <- mean(avoidable$biaya, na.rm=TRUE)
# Jumlah kasus:
nrow(avoidable)

# indikator penting (yang sangat kuat di rapat): Avoidable hospitalization rate
rate <- nrow(avoidable) / nrow(prb_exit)
rate

# biaya per FKTP (ini biasanya “eye opening”)
cost_fktp <- merge(avoidable, prb_exit, by="nokapst")
cost_fktp <- cost_fktp[, .(
  kasus = .N,
  biaya = sum(biaya, na.rm=TRUE)
), by=nmfktpterdaftar]
cost_fktp[order(-biaya)]
# menunjukkan FKTP mana yang PRB-nya gagal menjaga stabilitas pasien

#========================================================================
# Policy Simulation (What-If Analysis)
#========================================================================
# Hitung risiko rawat inap pada pasien PRB (baseline)
# jumlah pasien PRB keluar
n_prb <- nrow(prb_exit)
# readmission kronis
n_readmit_prb <- nrow(avoidable)
risk_prb <- n_readmit_prb / n_prb
risk_prb

# Risiko rawat inap pasien NON-PRB (potensi PRB)
# Ambil pasien potensi_final (yang kita deteksi dari RS tapi belum PRB),
# lalu link ke rawat inap:
ri_potensi <- ri[nokapst %in% potensi_final$Nokapst]
# rawat inap kronis
ri_potensi[, kronis :=
             Reduce(`|`, lapply(prb_icd,
                                function(x) grepl(x, kddiagprimer)))]
potensi_ri <- unique(ri_potensi[kronis == TRUE, nokapst])
risk_nonprb <- length(potensi_ri) / nrow(potensi_final)
risk_nonprb

# Hitung biaya rata-rata rawat inap kronis
mean_ri_cost <- mean(avoidable$biaya, na.rm=TRUE)
mean_ri_cost

# Jumlah pasien potensi PRB
n_potensi <- nrow(potensi_final)

# Kita akan simulasikan: 50% berhasil dimasukkan PRB
# enrolled <- 0.5 * n_potensi
#========================================================================
#========================================================================
# Hitung rawat inap yang bisa dicegah
#========================================================================
#  Tanpa PRB:
#  rawat inap = risk_nonprb × enrolled
#  Dengan PRB:
#  rawat inap = risk_prb × enrolled
#  Selisih = rawat inap yang dapat dicegah.
#  prevented_cases <- (risk_nonprb - risk_prb) * enrolled
#  prevented_cases

#========================================================================
# Hitung penghematan biaya
#========================================================================
cost_saving <- prevented_cases * mean_ri_cost
cost_saving

# estimasi penghematan JKN
# Misal hasilnya:
# prevented_cases = 214 pasien
# mean_ri_cost = Rp 7.200.000
# maka:
# cost_saving ≈ Rp 1,54 miliar / tahun

# simulasi 3 skenario Cakupan PRB
# konservatif 25%
# moderat 50%
# optimal 75%

scenario <- c(0.25,0.5,0.75)
simulation <- data.table(
  coverage = scenario,
  prevented_cases = scenario*n_potensi*(risk_nonprb-risk_prb),
  saving = scenario*n_potensi*(risk_nonprb-risk_prb)*mean_ri_cost
)
simulation


# (opsional terbaik) linkage ke klaim RS → rehospitalisasi



# Determinan keluar PRB (Risk Factor Analysis)

# Logistic Regression / Random Forest

# Continuity of Care Analysis

# Analisis Kinerja FKTP (Facility Performance)



setwd("D:/data gresik/Prolanis")

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

prbpasif_aktif <- read_csv("dimprb.csv") %>%
  select(nama,tgllahir,nokapst,pstprbid,nohp,email,pisat,segmen,alamat,
         kdstatuspst,nmstatuspst,nmdati2terdaftar,nmjnsfktpterdaftar,
         nmtypefktpterdaftar,nmfktpterdaftar,nmdokter,jenisfaskes,
         nmdati2mendaftar,nmjnsfaskesmendaftar,
         nmtypefaskesmendaftar,nmfaskesmendaftar,nmtkp,kdpoli,
         statusprb,nmprogprb,nmalasanprb,tglmulai,ketmulai,tglakhir,ketakhir,
         nmdati2prolanis,nmjnsfktpprolanis,nmtypefktpprolanis,
         nmfktpprolanis,nmprogprolanis,namaclub,nmalasanprolanis,withprb,
         tglmulai_prolanis,tglakhir_prolanis) %>%
  subset(kdstatuspst == "0") %>%
  select(-kdstatuspst) %>%
  subset(statusprb == "PASIF") %>%
  subset(nmalasanprb == "Aktif")

prbaktif_pasif <- read_csv("Sheet 1_Full Data_data (DimPRB).csv") %>%
  select(nama,tgllahir,nokapst,pstprbid,nohp,email,pisat,segmen,alamat,
         kdstatuspst,nmstatuspst,nmdati2terdaftar,nmjnsfktpterdaftar,
         nmtypefktpterdaftar,nmfktpterdaftar,nmdokter,jenisfaskes,
         nmdati2mendaftar,nmjnsfaskesmendaftar,
         nmtypefaskesmendaftar,nmfaskesmendaftar,nmtkp,kdpoli,
         statusprb,nmprogprb,nmalasanprb,tglmulai,ketmulai,tglakhir,ketakhir,
         nmdati2prolanis,nmjnsfktpprolanis,nmtypefktpprolanis,
         nmfktpprolanis,nmprogprolanis,namaclub,nmalasanprolanis,withprb,
         tglmulai_prolanis,tglakhir_prolanis) %>%
  subset(kdstatuspst == "0") %>%
  select(-kdstatuspst) %>%
  subset(statusprb == "AKTIF") %>%
  subset(nmalasanprb == "Tidak Aktif") 
