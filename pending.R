########################################################
# 0. LIBRARY
########################################################
library(data.table)
library(stringr)
library(dplyr)
library(openxlsx)

########################################################
# 1. PENDING
########################################################
setwd("D:/data gresik/VPK")

pending <- fread(
  "Sheet 1_Full Data_pending0226.csv",
  select = c("Nosep","alasan_pending","Flagprsklaimsep",
             "Statusklaim","Kdppklayan","Tglplgsep","Tglsep"),
  colClasses = list(
    character = c("Nosep","Kdppklayan","Statusklaim","alasan_pending"),
    integer = c("Flagprsklaimsep")
  )
)

setnames(pending,"Nosep","Nosjp")

pending[, `:=`(
  Tglsep = as.IDate(Tglsep, "%m/%d/%Y"),
  Tglplgsep = as.IDate(Tglplgsep, "%m/%d/%Y")
)]

pending <- pending[,.(Nosjp,alasan_pending,Flagprsklaimsep,Statusklaim)]
pending[, Statusklaim := toupper(Statusklaim)]

pending <- pending %>% distinct(Nosjp, .keep_all = TRUE)
pending <- as.data.table(pending)

########################################################
# 2. METAFISIK (LOAD DATA)
########################################################
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp/kediri")

files <- c(
  "Sheet 1_Full Data_data (metafisik25).csv",
  "Sheet 1_Full Data_metafisik fdate_janfeb26.csv",
  "Sheet 1_Full Data_metafisik fdate_des25.csv",
  "Sheet 1_Full Data_metafisik fdate_nov25.csv",
  "Sheet 1_Full Data_metafisik fdate_okt25.csv",
  "Sheet 1_Full Data_metafisik fdate_sep25.csv",
  "Sheet 1_Full Data_metafisik fdate_agu25.csv",
  "Sheet 1_Full Data_metafisik fdate junjul25.csv",
  "Sheet 1_Full Data_metafisik fdate mei25.csv",
  "Sheet 1_Full Data_metafisik fdate marapr25.csv",
  "Sheet 1_Full Data_metafisik fdate janfeb25.csv"
)

read_meta <- function(file){
  
  dt <- fread(file, showProgress = TRUE)
  
  na_cols_none <- c("Kdsa","Kdsd","Kdsi","Kdsp","Kdsr")
  for(col in na_cols_none){
    if(col %in% names(dt)){
      dt[get(col) == "None", (col) := NA_character_]
    }
  }
  
  na_cols_dash <- c("Deskripsisd","Deskripsisi","Deskripsisp","Deskripsisr")
  for(col in na_cols_dash){
    if(col %in% names(dt)){
      dt[get(col) == "-", (col) := NA_character_]
    }
  }
  
  date_cols <- c("Tgl Bast","Tglpelayanan","Tgldtgsep","Tglplgsep")
  for(dc in date_cols){
    if(dc %in% names(dt)){
      dt[, (dc) := as.IDate(get(dc), "%m/%d/%Y")]
    }
  }
  
  return(dt)
}

metafisik <- rbindlist(lapply(files, read_meta), fill=TRUE, use.names=TRUE)

########################################################
# 2.1 DISTINCT SEP (FIX data.table version)
########################################################

setDT(metafisik)
metafisik[, Nosjp := as.character(Nosjp)]

# ---- kolom prioritas (penentu header SEP) ----
metafisik[, pr_diag := !is.na(Kddiagprimer)]
metafisik[, pr_tgl  := !is.na(Tglpelayanan)]

# jika kolom tanggal tidak ada di sebagian file
if(!"Tgldtgsep" %in% names(metafisik)) metafisik[, Tgldtgsep := as.IDate(NA)]
if(!"Tglplgsep" %in% names(metafisik)) metafisik[, Tglplgsep := as.IDate(NA)]

# urutkan: baris "header SEP" akan berada paling atas
setorder(metafisik,
         Nosjp,
         -pr_diag,
         -pr_tgl,
         -Tgldtgsep,
         -Tglplgsep)

# ambil 1 baris per SEP
metafisik <- metafisik[!duplicated(Nosjp)]

# hapus kolom bantu
metafisik[, c("pr_diag","pr_tgl") := NULL]

cat("BARIS METAFISIK:", nrow(metafisik), "\n")
cat("SEP UNIK:", uniqueN(metafisik$Nosjp), "\n")

# ---- mapping RS (SUPER CEPAT) ----
rs_map <- c(
  "0185R003"="RSU An Nisaa","0185R004"="RSU Aulia","0185R005"="RSU Ananda",
  "0185R008"="RSU Al Ittihad","0185R009"="RS Medika Utama","0185R010"="RSUD Srengat",
  "0185R011"="RS Wava Husada Kesamben","0185S001"="KU Puspa Husada",
  "0186R002"="RSU Aura Syifa","0186R003"="RS Muhammadiyah Siti Khodijah",
  "0186R004"="IHC RS Toeloengredjo (HVA)","0186R005"="RS Arga Husada",
  "0186R007"="RSU Muhammadiyah Surya Melati","0186R012"="RSU Wilujeng",
  "0186R013"="RS Amelia","0186R016"="RSUD Simpang Lima Gumul Kediri",
  "0186R014"="RSUD Simpang Lima Gumul Kediri (IGD)","0186S001"="KU Rawat Inap Medika Utama",
  "0186S002"="KU Rawat Inap Dian Husada","0198R004"="RSI Aisyiyah",
  "0198S001"="Klinik Mata EDC Warujayeng","0198S003"="Klinik Mata Ayu Siwi",
  "0210R005"="RS Daha Husada","0210R006"="RSU TNI AD Tk. IV DKT",
  "0210R007"="RSU Ratih","0210R008"="RS Muhammadiyah Ahmad Dahlan",
  "0210R009"="RSIA Melinda","0210R010"="RS Baptis","0210R016"="RSU Lirboyo",
  "0210R017"="RSIA Citra Keluarga","0210R018"="RSIA Nirmala Kediri",
  "0210R019"="RSUD Kilisuci","0210R021"="RSGM IIK Bhakti Wiyata",
  "0211R004"="RSU Aminah","0211R006"="RS Katolik Budi Rahayu",
  "0211R008"="RS Syuhada' Haji","0211R009"="RSI Aminah",
  "1313R001"="RSUD Mardi Waluyo","1314R001"="RSUD Ngudi Waluyo",
  "1317R001"="RSUD Gambiran","1317R003"="RS Bhayangkara Kediri",
  "1318R001"="RSUD Kabupaten Kediri","1319R001"="RSUD Nganjuk",
  "1319R002"="RSUD Kertosono","1319R003"="RS Bhayangkara Nganjuk"
)

metafisik[, Nmppklayan := rs_map[Kdppklayan]]
metafisik[is.na(Nmppklayan), Nmppklayan := Kdppklayan]
########################################################
# 3. JOIN
########################################################
setkey(metafisik, Nosjp)
setkey(pending, Nosjp)

df1 <- metafisik[pending]
setDT(df1)

df1[, .(
  Nosjp_na = sum(is.na(Nosjp)),
  No_bast_na = sum(is.na(`No Bast`)),
  Nokapst_na = sum(is.na(Nokapst)),
  alasan_pending_na = sum(is.na(alasan_pending))
)]

na_profile <- data.frame(
  kolom = names(df1),
  jumlah_NA = colSums(is.na(df1)),
  persen_NA = round(colMeans(is.na(df1))*100,2)
)

na_profile[order(-na_profile$persen_NA), ]

df_na <- df1[is.na(Nokapst)]
df_clean <- df1[!is.na(Nokapst)]

########################################################
# 4. LOGIC
########################################################
df_clean[, kadaluarsa := Tglplgsep + 180L]
setorder(df_clean,Tglplgsep)

if(!"nama_peserta" %in% names(df_clean)){
  df_clean[, nama_peserta := NA_character_]
}

########################################################
# 5. RENAME KOLOM
########################################################
old_names <- c("Nosjp","alasan_pending","Statusklaim","No Bast","Tgl Bast",
               "No Surat Bast","Nokapst","nama_peserta","Umur Tahun","jenis_kelamin",
               "Kdppklayan","Nmppklayan","Nmdati2Layan","Nmtkp","Tglpelayanan",
               "Tgldtgsep","Tglplgsep","politujsep","Kdinacbgs","Nminacbgs",
               "Severity Level","Kddiagprimer","Nmdiagprimer","Diagsekunder",
               "Prosedur","Kdsa","Kdsd","Kdsi","Kdsp","Kdsr","Deskripsisd",
               "Deskripsisi","Deskripsisp","Deskripsisr","jenisppkperujuk",
               "nmppkperujuk","nmdati2perujuk","Tarifgrup","Tarifsa","Tarifsd",
               "Tarifsi","Tarifsp","Tarifsr","Nmdokter","Nmjnspulang","biayars",
               "Bytagsep","Byversep","flag_biometrik","flag_iterasi","kadaluarsa")

new_names <- c("nosjp","alasan_pending","statusklaim","no_bast","tgl_bast",
               "no_surat_bast","nokapst","nama_peserta","umur_tahun","jenis_kelamin",
               "kd_instansi","nmppklayan","nmdati2Layan","nm_tkp","tgl_pelayanan",
               "tgl_dtgsep","tgl_plgsep","poli_tuj_sep","kdinacbgs","nminacbgs",
               "severity_level","kddiagprimer","nmdiagprimer","diagsekunder",
               "prosedur","kdsa","kdsd","kdsi","kdsp","kdsr","deskripsisd",
               "deskripsisi","deskripsisp","deskripsisr","jenisppkperujuk",
               "nmppkperujuk","nmdati2perujuk","tarif_grup","tarifsa","tarifsd",
               "tarifsi","tarifsp","tarifsr","nm_dokter","nm_jns_pulang","biaya_rs",
               "bys_tag_sep","byversep","flag_biometrik","flag_iterasi","kadaluarsa")

########################################################
# 5. STANDARDISASI NAMA KOLOM
########################################################

# pasangan nama lama -> baru
name_map <- data.table(old = old_names, new = new_names)

# hanya rename yang benar-benar ada di data
name_map_exist <- name_map[old %in% names(df_clean)]

# lakukan rename
setnames(df_clean, old = name_map_exist$old, new = name_map_exist$new)

########################################################
# 5.1 PERTAHANKAN HANYA KOLOM STANDAR
########################################################

# sekarang new_names SUDAH ADA
keep_cols <- intersect(new_names, names(df_clean))

df_clean <- df_clean[, ..keep_cols]

########################################################
# 5.3 STANDARDISASI JENIS PERAWATAN
########################################################

# pastikan karakter
df_clean[, nm_tkp := as.character(nm_tkp)]

# mapping
df_clean[nm_tkp == "RJTL", nm_tkp := "Rawat Jalan"]
df_clean[nm_tkp == "RITL", nm_tkp := "Rawat Inap"]

# jika ada spasi tersembunyi dari RS
df_clean[, nm_tkp := trimws(nm_tkp)]

# cek hasil
print(df_clean[, .N, by = nm_tkp])

########################################################
# 5.2 CEK KUALITAS HEADER
########################################################

cat("Kolom tersedia:", length(names(df_clean)), "\n")

missing_cols <- setdiff(new_names, names(df_clean))
cat("Kolom tidak ditemukan:\n")
print(missing_cols)

########################################################
# 5.4 STANDARDISASI STATUS KLAIM
########################################################

# pastikan karakter & bersihkan spasi
df_clean[, statusklaim := trimws(as.character(statusklaim))]

# samakan kapital dulu (penting!)
df_clean[, statusklaim := toupper(statusklaim)]

# ubah label
df_clean[statusklaim == "PENDING", statusklaim := "BELUM DISETUJUI"]

# cek hasil
print(df_clean[, .N, by = statusklaim])


########################################################
# 6. EXPORT
########################################################
if(nrow(df_clean) > 800000){
  fwrite(df_clean,"pending.csv")
} else {
  write.xlsx(df_clean,"pending.xlsx",overwrite=TRUE)
}

cat("SELESAI - DATA SUDAH TERANONIMISASI & FILE DIBUAT")