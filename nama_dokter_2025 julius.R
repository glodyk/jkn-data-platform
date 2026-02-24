getwd()


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

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri")

load("kamus_dpjp2025_nosjp.rda")


# ============================================================
# TOP OFFENDER REPORT: "Seharusnya digabung tapi masih beda ID"
# Sumber utama: kamus_dpjp2025_nosjp.rda (object: dok_long_final)
# Tambahan: mapping_nama.xlsx + kamus_nama_to_id_fix.csv
# Output: laporan_top_offender_beda_id_from_sumber.xlsx
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
  library(stringr)
  library(readxl)
  library(openxlsx)
})

# -----------------------------
# 1) Load sumber utama RDA
# -----------------------------
load("kamus_dpjp2025_nosjp.rda")  # memuat object dok_long_final

dt_src <- as.data.table(dok_long_final)
rm(dok_long_final)

# Minimal kolom yang dipakai
dt_src <- dt_src[, .(baris, Namadpjp, id_dokter_final)]

# -----------------------------
# 2) Normalisasi nama (konsisten, aman)
# -----------------------------
normalize_nama <- function(x_chr) {
  x_chr <- ifelse(is.na(x_chr), "", x_chr)
  x_chr <- stri_trans_general(x_chr, "Latin-ASCII")
  x_chr <- str_to_lower(x_chr)
  
  # rapikan pemisah
  x_chr <- str_replace_all(x_chr, "[^a-z ]", " ")
  x_chr <- str_replace_all(x_chr, "\\s+", " ")
  x_chr <- str_trim(x_chr)
  
  # buang gelar umum di awal/akhir
  # catatan: 'dr' sebagai gelar biasanya di awal; nanti kita deteksi 'dr' di tengah sebagai indikasi 2 orang
  x_chr <- str_replace_all(x_chr, "\\bdrg\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bprof\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bdokter\\b", " ")
  
  # buang pola spesialis/pendidikan yang sering nempel (dibuat konservatif)
  x_chr <- str_replace_all(x_chr, "\\bsp\\s*[a-z]+\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bm\\s*kes\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bskm\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bmkm\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bm\\s*sc\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bmsc\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bphd\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bmm\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bmba\\b", " ")
  x_chr <- str_replace_all(x_chr, "\\bmars\\b", " ")
  
  # normalisasi ejaan indonesia yang umum (konservatif, biar tidak over-merge)
  x_chr <- str_replace_all(x_chr, "\\bachmad\\b", "ahmad")
  x_chr <- str_replace_all(x_chr, "oe", "u")
  x_chr <- str_replace_all(x_chr, "dj", "j")
  x_chr <- str_replace_all(x_chr, "ch", "c")
  x_chr <- str_replace_all(x_chr, "dh", "d")
  x_chr <- str_replace_all(x_chr, "th", "t")
  x_chr <- str_replace_all(x_chr, "kh", "h")
  x_chr <- str_replace_all(x_chr, "q", "k")
  
  # satukan token putra/putri compound (supaya stabil)
  x_chr <- str_replace_all(x_chr, "\\bdwi\\s+putra\\b", "dwiputra")
  x_chr <- str_replace_all(x_chr, "\\bdwi\\s+putri\\b", "dwiputri")
  x_chr <- str_replace_all(x_chr, "\\btri\\s+putra\\b", "triputra")
  x_chr <- str_replace_all(x_chr, "\\btri\\s+putri\\b", "triputri")
  x_chr <- str_replace_all(x_chr, "\\badi\\s+putra\\b", "adiputra")
  x_chr <- str_replace_all(x_chr, "\\badi\\s+putri\\b", "adiputri")
  
  # final clean
  x_chr <- str_replace_all(x_chr, "\\s+", " ")
  x_chr <- str_trim(x_chr)
  
  x_chr
}

# -----------------------------
# 3) Deteksi indikasi "2 orang dalam 1 baris"
#    aturan kamu:
#    - setelah dibersihkan, kata > 4 => kemungkinan 2 orang (exclude dari kandidat merge)
#    - ada token 'dr' di tengah nama => hampir pasti 2 orang
# -----------------------------
word_count <- function(x_chr) {
  x_chr <- str_trim(x_chr)
  ifelse(x_chr == "", 0L, str_count(x_chr, "\\S+"))
}

has_dr_in_middle <- function(x_chr) {
  # 'dr' muncul dan tidak di awal
  # contoh: "indra ... dr surya ..." => TRUE
  str_detect(x_chr, "\\bdr\\b") & !str_detect(x_chr, "^dr\\b")
}

# Normalisasi sumber utama
dt_src[, nama_norm := normalize_nama(Namadpjp)]
dt_src[, n_kata := word_count(nama_norm)]
dt_src[, flag_dua_orang := (n_kata > 4) | has_dr_in_middle(nama_norm)]

# dataset kandidat merge (exclude dua orang)
dt_src_ok <- dt_src[flag_dua_orang == FALSE & nama_norm != ""]

# -----------------------------
# 4) Baca mapping_nama.xlsx dan kamus_nama_to_id_fix.csv
# -----------------------------
dt_map <- as.data.table(read_excel("mapping_nama.xlsx"))
setnames(dt_map, c("Namadpjp","id_dokter"), c("Namadpjp","id_dokter_map"))
dt_map[, nama_norm := normalize_nama(Namadpjp)]
dt_map[, n_kata := word_count(nama_norm)]
dt_map[, flag_dua_orang := (n_kata > 4) | has_dr_in_middle(nama_norm)]
dt_map_ok <- dt_map[flag_dua_orang == FALSE & nama_norm != ""]

library(data.table)

dt_kamus_fix <- fread(
  "kamus_nama_to_id_fix.csv",
  sep = ";",
  encoding = "Latin-1"
)

# pastikan kolom ada
# kolom id_dokter di file ini bukan id_final, tapi tetap kita bawa untuk perbandingan
dt_kamus_fix[, nama_norm2 := normalize_nama(Namadpjp)]
dt_kamus_fix[, n_kata := word_count(nama_norm2)]
dt_kamus_fix[, flag_dua_orang := (n_kata > 4) | has_dr_in_middle(nama_norm2)]
dt_kamus_fix_ok <- dt_kamus_fix[flag_dua_orang == FALSE & nama_norm2 != ""]

# -----------------------------
# 5) Top offenders dari sumber utama: nama_norm sama punya banyak id_dokter_final
#    Ini yang paling penting untuk perbaikan "dari sumber ini"
# -----------------------------
off_src <- dt_src_ok[, .(
  n_rows = .N,
  n_unique_name = uniqueN(Namadpjp),
  n_ids_final = uniqueN(id_dokter_final)
), by = .(nama_norm)]

off_src <- off_src[n_ids_final > 1][order(-n_ids_final, -n_rows, -n_unique_name)]

# detail contoh nama dan daftar id_dokter_final
detail_src <- dt_src_ok[off_src, on = "nama_norm", nomatch = 0]
detail_src <- unique(detail_src[, .(nama_norm, id_dokter_final, Namadpjp)])
setorder(detail_src, nama_norm, id_dokter_final, Namadpjp)

detail_src2 <- detail_src[, .(
  contoh_nama = paste(head(unique(Namadpjp), 10), collapse = " | "),
  n_contoh = uniqueN(Namadpjp)
), by = .(nama_norm, id_dokter_final)]
setorder(detail_src2, -n_contoh)

# -----------------------------
# 6) Cross-check: nama_norm yang sama tapi ID beda antar sumber (opsional tapi berguna)
# -----------------------------
# satukan tiga sumber untuk lihat "nama_norm sama, id berbeda"
comb <- rbindlist(list(
  dt_src_ok[, .(source = "sumber_rda", nama_norm, id = as.character(id_dokter_final), Namadpjp)],
  dt_map_ok[, .(source = "mapping_xlsx", nama_norm, id = as.character(id_dokter_map), Namadpjp)],
  dt_kamus_fix_ok[, .(source = "kamus_fix_csv", nama_norm = nama_norm2, id = as.character(id_dokter), Namadpjp)]
), use.names = TRUE, fill = TRUE)

comb_sum <- comb[, .(
  n_rows = .N,
  n_unique_name = uniqueN(Namadpjp),
  n_ids = uniqueN(id),
  n_sources = uniqueN(source)
), by = .(nama_norm)]

comb_sum <- comb_sum[n_ids > 1][order(-n_ids, -n_rows, -n_unique_name)]

comb_detail <- unique(comb[comb_sum, on = "nama_norm", nomatch = 0][, .(nama_norm, source, id, Namadpjp)])
setorder(comb_detail, nama_norm, source, id, Namadpjp)

comb_detail2 <- comb_detail[, .(
  contoh_nama = paste(head(unique(Namadpjp), 10), collapse = " | "),
  n_contoh = uniqueN(Namadpjp)
), by = .(nama_norm, source, id)]

# -----------------------------
# 7) Export laporan
# -----------------------------
out_xlsx <- "laporan_top_offender_beda_id_from_sumber.xlsx"
wb <- createWorkbook()

addWorksheet(wb, "src_ringkas")
writeDataTable(wb, "src_ringkas", off_src)

addWorksheet(wb, "src_detail_ids")
writeDataTable(wb, "src_detail_ids", detail_src2)

addWorksheet(wb, "cross_ringkas")
writeDataTable(wb, "cross_ringkas", comb_sum)

addWorksheet(wb, "cross_detail")
writeDataTable(wb, "cross_detail", comb_detail2)

addWorksheet(wb, "catatan_filter_dua_orang")
note_dt <- data.table(
  aturan = c(
    "exclude_kandidat_merge",
    "exclude_kandidat_merge"
  ),
  deskripsi = c(
    "n_kata > 4 (asumsi nama Indonesia max 4 kata)",
    "token dr muncul di tengah nama (bukan di awal) => hampir pasti 2 orang"
  )
)
writeDataTable(wb, "catatan_filter_dua_orang", note_dt)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

print(head(off_src, 20))
print(out_xlsx)









dt <- dok_long_final
names(dt)

# ============================================================
# LAPORAN TOP OFFENDER: "seharusnya digabung tapi beda id"
# Input:
#   - mapping_nama.xlsx (kolom: Namadpjp, id_dokter)
#   - kamus_nama_to_id_fix.csv (delimiter ;)
# Output:
#   - laporan_top_offender_beda_id.xlsx
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringi)
  library(stringr)
  library(openxlsx)
})

# -----------------------------
# 1) Normalisasi nama (gabungan rules kamu + rapihin tempelan gelar)
# -----------------------------
normalize_nama <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- tolower(x)
  
  # buang gelar/imbuhan umum
  x <- gsub("\\bsp\\s*[a-z]+\\b", " ", x)
  x <- gsub("\\bdr\\b", " ", x)
  x <- gsub("sp\\s*[a-z]+", " ", x)
  x <- gsub("biomed\\s*[a-z]+", " ", x)
  x <- gsub("m med klin\\s*[a-z]+", " ", x)
  x <- gsub("m kes\\s*[a-z]+", " ", x)
  x <- gsub("prof|dokter|fics|khom", " ", x)
  
  # spelling normalization
  x <- gsub("achmad", "ahmad", x)
  x <- gsub("ch", "c", x)
  x <- gsub("dj", "j", x)
  x <- gsub("oe", "u", x)
  x <- gsub("sy|sh", "s", x)
  x <- gsub("dh", "d", x)
  x <- gsub("th", "t", x)
  x <- gsub("kh", "h", x)
  x <- gsub("q", "k", x)
  x <- gsub("y", "i", x)
  x <- gsub("ie", "i", x)
  x <- gsub("iy", "i", x)
  x <- gsub("aa", "a", x)
  x <- gsub("ee", "e", x)
  x <- gsub("oo", "o", x)
  x <- gsub("ni am", "niam", x)
  x <- gsub("ulloh", "ullah", x)
  
  # compound names
  x <- gsub("\\bdwi\\s+putra\\b", "dwiputra", x)
  x <- gsub("\\bdwi\\s+putri\\b", "dwiputri", x)
  x <- gsub("\\btri\\s+putra\\b", "triputra", x)
  x <- gsub("\\btri\\s+putri\\b", "triputri", x)
  x <- gsub("\\badi\\s+putra\\b", "adiputra", x)
  x <- gsub("\\badi\\s+putri\\b", "adiputri", x)
  x <- gsub("\\bek\\w?\\s+putra\\b", "ekaputra", x)
  x <- gsub("\\bek\\w?\\s+putri\\b", "ekaputri", x)
  x <- gsub("\\bmuhamad\\b|\\bmohamad\\b|\\bmuhammad\\b", "muhamad", x)
  x <- gsub("\\bmuh\\s+", "muhamad ", x)
  
  # buang gelar akademik yg sering nempel
  x <- gsub("\\b(m|s|dr|drg|prof)\\s*(ked|kedg|biomed|biomedik|med|medik|klin|klinik|kes|kesehatan|km|skm|mph|msc|mm|mba|phd|spog|spd|spb|spa)\\b", " ", x)
  x <- gsub("\\b(biomed|biomedik|med|klin|klinik|kes|kesehatan)\\b", " ", x)
  x <- gsub("\\b(mars|cmc|qhia|pol|kol|mkes|sked)\\b", " ", x)
  
  # paksa pisahin kata yang suka nempel
  x <- gsub("putri", " putri", x)
  x <- gsub("putra", " putra", x)
  
  # keep letters only
  x <- gsub("[^a-z ]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x
}

# -----------------------------
# 2) Baca data
# -----------------------------
df_map <- as.data.table(read_excel("mapping_nama.xlsx"))
setnames(df_map, c("Namadpjp","id_dokter"))

df_kamus <- fread("kamus_nama_to_id_fix.csv", sep = ";")
# kolom minimal: Namadpjp, id_dokter ada di file ini

# -----------------------------
# 3) Hitung nama_norm
# -----------------------------
df_map[, source := "mapping_xlsx"]
df_kamus[, source := "kamus_fix"]

df_map[, nama_norm := normalize_nama(Namadpjp)]
df_kamus[, nama_norm := normalize_nama(Namadpjp)]

comb <- rbindlist(list(
  df_map[, .(Namadpjp, id_dokter, nama_norm, source)],
  df_kamus[, .(Namadpjp, id_dokter, nama_norm, source)]
), use.names = TRUE, fill = TRUE)

comb <- comb[!is.na(nama_norm) & nama_norm != ""]

# -----------------------------
# 4) Ringkas: nama_norm yang punya >1 id_dokter
# -----------------------------
ringkas <- comb[, .(
  n_rows = .N,
  n_unique_name = uniqueN(Namadpjp),
  n_ids = uniqueN(id_dokter)
), by = .(nama_norm)]

ringkas <- ringkas[n_ids > 1][order(-n_ids, -n_rows, -n_unique_name)]

# -----------------------------
# 5) Detail per nama_norm: list id dan contoh nama per source
# -----------------------------
detail <- comb[ringkas, on = "nama_norm", nomatch = 0]
setorder(detail, nama_norm, source, id_dokter, Namadpjp)
detail <- unique(detail, by = c("nama_norm","source","id_dokter","Namadpjp"))

detail2 <- detail[, .(
  contoh_nama = paste(head(unique(Namadpjp), 10), collapse = " | "),
  n_contoh = uniqueN(Namadpjp)
), by = .(nama_norm, source, id_dokter)]

setorder(detail2, -n_contoh)

# -----------------------------
# 6) Export laporan
# -----------------------------
out_xlsx <- "laporan_top_offender_beda_id.xlsx"
wb <- createWorkbook()

addWorksheet(wb, "ringkas_top")
writeDataTable(wb, "ringkas_top", ringkas)

addWorksheet(wb, "detail_ids")
writeDataTable(wb, "detail_ids", detail2)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

# print quick view
print(head(ringkas, 20))
print(out_xlsx)





fwrite(ringkas[order(id_dokter)], "kamus_ringkas.csv")

































library(data.table)
library(stringi)
library(stringdist)

# =========================
# 1) Fungsi normalisasi nama (ringan tapi efektif untuk Indonesia)
# =========================
norm_nama <- function(x) {
  x <- toupper(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")           # buang aksen kalau ada
  x <- gsub("[^A-Z0-9 ]", " ", x)                              # buang tanda baca
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  # normalisasi token gelar/imbuhan umum (opsional; sesuaikan jika mau lebih agresif)
  # ide: kita buang gelar yang sering bikin variasi tapi bukan inti nama
  x <- gsub("\\bDR\\b", " ", x)
  x <- gsub("\\bDOKTER\\b", " ", x)
  x <- gsub("\\bSP\\b", " ", x)
  x <- gsub("\\bSPOG\\b|\\bSP\\.OG\\b", " SPOG ", x)
  x <- gsub("\\bSPPD\\b|\\bSP\\.PD\\b", " SPPD ", x)
  x <- gsub("\\bM\\.KED\\b|\\bMKED\\b", " ", x)
  x <- gsub("\\bM\\.SI\\b|\\bMSI\\b", " ", x)
  x <- gsub("\\bSE\\b|\\bSH\\b|\\bMH\\b|\\bMM\\b", " ", x)
  
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x
}

# Blocking key agar matching cuma terjadi di grup kecil:
# contoh: gabungan beberapa huruf awal token1+token2 + panjang string
block_key <- function(x_norm) {
  toks <- strsplit(x_norm, " ", fixed = TRUE)
  vapply(toks, function(tt) {
    tt <- tt[nzchar(tt)]
    if (length(tt) == 0) return("BLK_EMPTY")
    t1 <- tt[1]
    t2 <- if (length(tt) >= 2) tt[2] else ""
    p1 <- substr(t1, 1, 2)
    p2 <- substr(t2, 1, 2)
    paste0(p1, p2, "_L", nchar(paste(tt, collapse="")))
  }, character(1))
}

# =========================
# 2) Build kamus dari data besar (dt) tapi unik dulu biar ringan
# =========================
# Asumsi: dt adalah data.table kamu yang punya kolom Namadpjp
# setDT(dt)

dt[, Namadpjp := trimws(Namadpjp)]
dt[is.na(Namadpjp) | Namadpjp == "", Namadpjp := "NA_NAMADPJP"]

# unikkan dulu (ini kunci performa untuk 100+ juta baris)
kamus <- unique(dt[, .(Namadpjp)])
kamus[, nama_norm := norm_nama(Namadpjp)]
kamus[, blk := block_key(nama_norm)]

# =========================
# 3) Deduplikasi dalam setiap block menggunakan stringdist (Jaro-Winkler)
#    Strategi aman:
#    - hanya match dalam block
#    - hanya gabung kalau similarity tinggi
# =========================
# threshold: makin besar makin ketat. 0.92-0.95 biasanya aman untuk nama.
sim_threshold <- 0.93

setkey(kamus, blk)

# helper untuk clustering sederhana per block (greedy)
cluster_block <- function(nm_vec, thr) {
  n <- length(nm_vec)
  grp <- integer(n)
  g <- 0L
  
  for (i in seq_len(n)) {
    if (grp[i] != 0L) next
    g <- g + 1L
    grp[i] <- g
    
    if (i < n) {
      # hitung similarity Jaro-Winkler terhadap sisa
      sim <- 1 - stringdist::stringdist(nm_vec[i], nm_vec[(i+1):n], method = "jw", p = 0.1)
      hit <- which(sim >= thr)
      if (length(hit) > 0) {
        grp[(i+1):n][hit] <- g
      }
    }
  }
  grp
}

# proses per block (cepat karena unik nama cuma 1-2 ribu; pada data kamu mungkin puluhan ribu)
kamus[, grp_in_blk := cluster_block(nama_norm, sim_threshold), by = blk]

# =========================
# 4) Tentukan "nama kanonik" per grup
#    aturan: pilih nama_norm terpanjang (biasanya paling lengkap) atau yang paling sering (butuh count)
# =========================
kamus[, nama_kanonik := nama_norm[which.max(nchar(nama_norm))], by = .(blk, grp_in_blk)]

# buat ID final: 1 kanonik = 1 id
kamus_kanonik <- unique(kamus[, .(nama_kanonik)])
kamus_kanonik[, id_dokter := .I]
setkey(kamus_kanonik, nama_kanonik)

# map balik ke kamus
setkey(kamus, nama_kanonik)
kamus[kamus_kanonik, id_dokter := i.id_dokter]

# VALIDASI 1-1 untuk kamus kanonik (wajib)
stopifnot(!anyNA(kamus$id_dokter))
stopifnot(kamus[, uniqueN(nama_kanonik), by = id_dokter][, max(V1)] == 1)

# =========================
# 5) Join ke tabel besar (dt) lewat Namadpjp (cepat)
# =========================
setkey(kamus, Namadpjp)
setkey(dt, Namadpjp)
dt[kamus, id_dokter_fix := i.id_dokter]

stopifnot(!anyNA(dt$id_dokter_fix))

# =========================
# 6) Optional: export kamus untuk audit manual
# =========================
fwrite(kamus[order(id_dokter)], "kamus_nama_to_id_fix.csv")
fwrite(unique(kamus[, .(id_dokter, nama_kanonik)][order(id_dokter)]), "kamus_kanonik.csv")

































# dt = data kamu (data.table). Kalau kamu masih data.frame, convert dulu:
# setDT(dt)

# 1) pastikan Namadpjp tidak NA / kosong (opsional tapi bagus untuk menjaga kualitas)
dt[, Namadpjp := trimws(Namadpjp)]
dt[is.na(Namadpjp) | Namadpjp == "", Namadpjp := "NA_NAMADPJP"]

# 2) bikin mapping unik: 1 Namadpjp -> 1 id
# data.table::unique lebih hemat memori daripada distinct dplyr
map_nama <- unique(dt[, .(Namadpjp)])

# 3) buat ID deterministik dan cepat (integer)
# kalau mau ada prefix biar jelas itu ID dokter, pakai paste0
map_nama[, id_dokter := .I]                     # integer 1..N (paling cepat)
# map_nama[, id_dokter := paste0("DPJP", .I)]   # kalau kamu mau string DPJP1, DPJP2, ...

setkey(map_nama, Namadpjp)

# 4) join balik ke tabel besar super cepat
setkey(dt, Namadpjp)
dt[map_nama, id_dokter := i.id_dokter]

# 5) VALIDASI WAJIB: harus 1-1 (tidak ketimpa)
# a) tidak boleh ada NA id_dokter
stopifnot(!anyNA(dt$id_dokter))

# b) 1 nama hanya boleh punya 1 id
cek_nama_ke_id <- dt[, .(n_id = uniqueN(id_dokter)), by = Namadpjp][n_id != 1]
stopifnot(nrow(cek_nama_ke_id) == 0)

# c) 1 id hanya boleh untuk 1 nama
cek_id_ke_nama <- dt[, .(n_nama = uniqueN(Namadpjp)), by = id_dokter][n_nama != 1]
stopifnot(nrow(cek_id_ke_nama) == 0)

# kalau mau lihat size mapping
print(map_nama[, .N])

write.xlsx(map_nama, "mapping_nama.xlsx", overwrite = TRUE)
