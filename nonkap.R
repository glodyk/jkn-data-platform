library(data.table)
library(stringi)
library(lubridate)
library(openxlsx)

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri")
setDTthreads(0)   # pakai semua core CPU

read_nonkap <- function(file){
  
  dt <- fread(file, showProgress = TRUE)
  
  dt <- dt[, .(
    Nokapst,Jkpst,Umur,Rangeumur,Segmen,Nmpks,Nosjp,Nmtkp,Nmdati2Layan,
    Jenisppklayan,Typeppklayan,Nmppklayan,Kddiagnosa,Nmdiagnosa,
    Tglpelayanan,Tglkunjungan,Tglpulang,Tgltindakan,Nmtindakan,
    Jnsnakes,Namanakes,Nmstatuspulang,status_fpk,lmstjklaim,
    Tglbayar,Biaya
  )]
  
  # buang biaya nol
  dt <- dt[Biaya != 0]
  
  # buat diagnosa
  dt[, Diagnosa := paste(Kddiagnosa, Nmdiagnosa, sep=" - ")]
  dt[, c("Kddiagnosa","Nmdiagnosa") := NULL]
  
  # klasifikasi cepat (regex vectorized)
  dt[, Klasifikasi :=
       fcase(
         stri_detect_regex(Nmtindakan,"Persalinan|keguguran",case_insensitive=TRUE),"Persalinan",
         stri_detect_regex(Nmtindakan,"pra-rujukan",case_insensitive=TRUE),"Pra Rujukan",
         stri_detect_regex(Nmtindakan,"ANC",case_insensitive=TRUE),"ANC",
         stri_detect_regex(Nmtindakan,"PNC",case_insensitive=TRUE),"PNC",
         stri_detect_regex(Nmtindakan,"Rawat Inap",case_insensitive=TRUE),"Rawat Inap",
         stri_detect_regex(Nmtindakan,"Kolesterol|HbA1c|LDL|HDL|Trigliserida|Kreatinin|Microalbumin|Ureum|Asam Urat",case_insensitive=TRUE),"Prolanis",
         stri_detect_regex(Nmtindakan,"KB",case_insensitive=TRUE),"KB",
         stri_detect_regex(Nmtindakan,"Ambulans",case_insensitive=TRUE),"Ambulans",
         stri_detect_regex(Nmtindakan,"Skrining DM|Papsmear|IVA|Krio",case_insensitive=TRUE),"Skrining Sekunder",
         stri_detect_regex(Nmtindakan,"Protesa Gigi",case_insensitive=TRUE),"Protesa Gigi",
         default="Non Kapitasi lain-lain"
       )]
  
  return(dt)
}

files <- c(
  "Sheet 1_Full Data_data (nonkap25)_.csv"
)

nonkap <- rbindlist(lapply(files, read_nonkap), use.names=TRUE)

collapse_unique <- function(x)
  paste(unique(na.omit(x)), collapse=";")

setkey(nonkap, Nosjp)

nonkap <- nonkap[, .(
  Nokapst = collapse_unique(Nokapst),
  Jkpst = collapse_unique(Jkpst),
  Umur = max(Umur, na.rm=TRUE),
  Rangeumur = collapse_unique(Rangeumur),
  Segmen = collapse_unique(Segmen),
  Nmpks = collapse_unique(Nmpks),
  Nmtkp = collapse_unique(Nmtkp),
  Nmdati2Layan = collapse_unique(Nmdati2Layan),
  Jenisppklayan = collapse_unique(Jenisppklayan),
  Typeppklayan = collapse_unique(Typeppklayan),
  Nmppklayan = collapse_unique(Nmppklayan),
  Diagnosa = collapse_unique(Diagnosa),
  Tglpelayanan = min(Tglpelayanan),
  Tglkunjungan = min(Tglkunjungan),
  Tglpulang = max(Tglpulang),
  Tgltindakan = min(Tgltindakan),
  Nmtindakan = collapse_unique(Nmtindakan),
  Jnsnakes = collapse_unique(Jnsnakes),
  Namanakes = collapse_unique(Namanakes),
  Nmstatuspulang = collapse_unique(Nmstatuspulang),
  status_fpk = collapse_unique(status_fpk),
  lmstjklaim = min(lmstjklaim, na.rm=TRUE),
  Tglbayar = min(Tglbayar),
  Klasifikasi = collapse_unique(Klasifikasi),
  Biaya = sum(Biaya, na.rm=TRUE)
), by = Nosjp]

datecols <- c("Tglpelayanan","Tglkunjungan","Tglpulang","Tgltindakan","Tglbayar")
nonkap[, (datecols) := lapply(.SD, mdy), .SDcols=datecols]

nonkap[, LOS := as.integer(Tglpulang - Tglkunjungan)]

setorder(nonkap, Nokapst, Tglpulang)
nonkap[, tgl_before := shift(Tglpulang), by=Nokapst]
nonkap[, interval := as.integer(Tglpulang - tgl_before)]

nonkap[, Klasifikasi :=
         stri_replace_all_fixed(Klasifikasi,
                                c("Ambulans;Pra Rujukan",
                                  "Ambulans;Rawat Inap",
                                  "Persalinan;Rawat Inap",
                                  "Pra Rujukan;Rawat Inap",
                                  "Prolanis;Skrining Sekunder"),
                                c("Pra Rujukan;Ambulans",
                                  "Rawat Inap;Ambulans",
                                  "Rawat Inap;Persalinan",
                                  "Rawat Inap;Pra Rujukan",
                                  "Skrining Sekunder;Prolanis"),
                                vectorize_all=FALSE)]
