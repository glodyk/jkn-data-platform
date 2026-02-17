folder_data <- ("D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri/New Folder")

library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(stringr)

read_cbgs <- function(path){
  
  # --- 1. baca raw text
  raw <- read_lines(path, n_max = 200)
  
  # cari header sebenarnya
  header_row <- which(str_detect(raw, "Nosjp"))[1]
  
  if(is.na(header_row)){
    cat("SKIP:", basename(path), "\n")
    return(NULL)
  }
  
  # --- 2. baca HANYA header untuk tahu posisi kolom
  header <- fread(path, skip = header_row - 1, nrows = 0, showProgress = FALSE)
  
  # nama kolom target
  target_cols <- c("Nosjp", "Kdppklayan", "Namadpjp")
  
  # cek kolom ada
  if(!all(target_cols %in% names(header))){
    cat("TIDAK ADA KOLOM:", basename(path), "\n")
    return(NULL)
  }
  
  # ambil index kolom
  idx <- which(names(header) %in% target_cols)
  
  # --- 3. baca ulang file HANYA 3 kolom
  dt <- fread(
    path,
    skip = header_row - 1,
    select = idx,
    showProgress = FALSE
  )
  
  return(dt)
}

# ==== 1. BACA DATA ====
files <- list.files("D:/data gresik/MTF KC GRESIK/gresik_2020/CBG_HV_Kediri/New Folder",
                    pattern="\\.csv$",
                    full.names=TRUE)

dt_list <- lapply(files, read_cbgs)
dt_list <- dt_list[!sapply(dt_list, is.null)]
dt <- rbindlist(dt_list, fill=TRUE)
rm(dt_list); gc()

#remove duplicate
table(duplicated(dt$Nosjp))
#ambil yang Namadpjp kosong
dt <- dt[order(Nosjp, -!is.na(Namadpjp))]
dt_unique <- dt[!duplicated(Nosjp)]

dt2 <- dt_unique[, .(Namadpjp,Nosjp)]
rm(dt_unique); gc()

# misal nama kolomnya Namadpjp
# (kalau beda, ganti di bawah)

# ==== 2. SAMAKAN PEMISAH ====

split_dpjp_real <- function(x){
  
  if(is.na(x) || x=="") return(NA_character_)
  
  # samakan huruf
  x <- toupper(x)
  x <- str_replace_all(x, "\\u00A0", " ")
  x <- str_replace_all(x, "&|/|,|\\+| dan | DAN ", ";")
  x <- str_replace_all(x, ";+", ";")
  x <- str_replace_all(x, " ; ", ";")
  
  # perbaiki format: ;DR → DR
  x <- str_replace_all(x, ";\\s*DR", " DR")
  
  # pecah setiap kemunculan DR sebagai awal dokter
  parts <- str_split(x, "(?=\\bDR\\b)")[[1]]
  
  parts <- str_trim(parts)
  parts <- parts[parts!=""]
  
  return(parts)
}

dt2 <- dt2 %>%
  mutate(dpjp_list = map(Namadpjp, split_dpjp_real)) %>%
  unnest(dpjp_list)

# ==== 3. BERSIHKAN & PECAH KE BANYAK BARIS ====
dt2$dokter_bersih <- dt2$Namadpjp %>%
  str_to_upper() %>%
  str_replace_all("\\.", " ") %>%
  str_replace_all(",", " ") %>%
  str_replace_all(";", " ") %>%
  str_replace_all("&", " DAN ") %>%
  str_squish()

library(stringr)

dt2 <- dt2 %>%
  mutate(
    dokter_bersih2 = str_replace_all(dokter_bersih, "\\|?\\s*DR\\s+", "|DR ")
  )

dt2 <- dt2 %>%
  mutate(
    dokter_bersih2 = iconv(dokter_bersih2, from = "", to = "UTF-8"),
    dokter_bersih2 = str_replace_all(dokter_bersih2, "\\\\'", "'"),
    dokter_bersih2 = str_squish(dokter_bersih2),
    dokter_list = str_split(dokter_bersih2, "\\|")
  )

split_dpjp <- function(x){
  
  if(is.na(x) || x=="") return(NA)
  
  # setiap kemunculan DR dianggap dokter baru
  parts <- str_split(x, "(?=\\bDR\\b)", simplify = FALSE)[[1]]
  
  parts <- str_trim(parts)
  parts <- parts[parts!=""]
  
  return(parts)
}

dokter_long <- dt2 %>%
  mutate(dpjp_list = map(dokter_bersih2, split_dpjp)) %>%
  unnest(dpjp_list)

dokter_long %>% filter(str_count(dpjp_list,"DR")>1)

nrow(dt2)
nrow(dokter_long)

dokter_long <- dokter_long %>%
  filter(dokter_list != "")

remove_medical_block <- function(x){
  
  x <- str_to_upper(x)
  
  # hapus semua mulai dari SP sampai akhir
  x <- str_replace(x, "\\bSP[\\s\\.A-Z\\(\\)]+$", "")
  
  # hapus fellowship/kolegium setelahnya
  x <- str_replace(x, "\\b(FINASIM|KHOM|FICS|FACS|FIHA|KIC|KGH|KGEH)\\b.*$", "")
  
  return(x)
}
remove_prefix_titles <- function(x){
  x <- str_remove(x, "^DRG?\\.?\\s*")
  x <- str_remove(x, "^PROF\\.?\\s*")
  x
}
remove_academic_titles <- function(x){
  str_remove_all(x,
                 "\\b(S\\.KED|M\\.KES|MARS|MBIOMED|MEDKLIN|MBOIMED|MPH|PH\\.D|DR\\.PH)\\b"
  )
}

dokter_long <- dokter_long %>%
  mutate(
    nama_dokter = dokter_list,
    nama_dokter = remove_medical_block(nama_dokter),
    nama_dokter = remove_prefix_titles(nama_dokter),
    nama_dokter = remove_academic_titles(nama_dokter),
    nama_dokter = str_replace_all(nama_dokter, "[^A-Z' ]", " "),
    nama_dokter = str_squish(nama_dokter),
    nama_dokter = str_to_upper(nama_dokter)
  ) %>%
  filter(nama_dokter != "")

split_sisa_dr <- function(x){
  
  # kalau ada DR di tengah nama → pecah
  if(str_detect(x, "\\sDR\\s")){
    return(unlist(str_split(x, "\\s+DR\\s+")))
  } else {
    return(x)
  }
}

dokter_long <- dokter_long %>%
  mutate(nama_dokter = map(nama_dokter, split_sisa_dr)) %>%
  unnest(nama_dokter)

dokter_long$flag_single_token <-
  str_count(dokter_long$nama_dokter, " ") == 0

dokter_multiword <- dokter_long %>%
  filter(!flag_single_token)

dokter_singleword <- dokter_long %>%
  filter(flag_single_token)

kamus_dokter <- dokter_long %>%
  distinct(nama_dokter) %>%
  arrange(nama_dokter)

nrow(kamus_dokter)

clean_kamus_nama <- function(x){
  
  x <- toupper(x)
  
  # hilangkan tanda baca
  x <- str_replace_all(x, "[^A-Z' ]", " ")
  
  # pecah kata
  tokens <- unlist(str_split(x, " "))
  tokens <- tokens[tokens!=""]
  
  # daftar kata yang hampir pasti bukan nama orang
  bad_tokens <- c(
    "SP","PD","OT","OG","B","S","A","T","U","N","M","MS",
    "K","KHOM","FINASIM","FICS","FACS","MSC","MARS",
    "MPH","MBA","MKES","KLIN","BIOMED","ONK","KED"
  )
  
  # ambil hanya kata yang valid sebagai nama
  tokens_valid <- tokens[
    !(tokens %in% bad_tokens) &
      nchar(tokens) >= 3
  ]
  
  # gabungkan kembali
  nama <- paste(tokens_valid, collapse=" ")
  
  str_squish(nama)
}
kamus_dokter$nama_dokter <- 
  sapply(kamus_dokter$nama_dokter, clean_kamus_nama)

kamus_dokter %>% count(nama_dokter, sort=TRUE) %>% head(20)

phonetic_id <- function(x){
  
  x <- toupper(x)
  
  # normalisasi variasi Arab-Indonesia
  x <- str_replace_all(x, "DJ", "J")
  x <- str_replace_all(x, "KH", "H")
  x <- str_replace_all(x, "DH", "D")
  x <- str_replace_all(x, "CH", "H")
  x <- str_replace_all(x, "SY", "S")
  x <- str_replace_all(x, "SH", "S")
  x <- str_replace_all(x, "DH", "D")
  x <- str_replace_all(x, "TH", "T")
  x <- str_replace_all(x, "IE", "I")
  x <- str_replace_all(x, "IY", "I")
  x <- str_replace_all(x, "OE", "U")
  x <- str_replace_all(x, "Q", "K")
  x <- str_replace_all(x, "Z", "S")
  x <- str_replace_all(x, "Y", "I")
  
  # double consonant
  x <- str_replace_all(x, "LL", "L")
  x <- str_replace_all(x, "NN", "N")
  x <- str_replace_all(x, "MM", "M")
  x <- str_replace_all(x, "RR", "R")
  x <- str_replace_all(x, "DD", "D")
  x <- str_replace_all(x, "LLOH", "LLAH")
  x <- str_replace_all(x, "PUROHITO ", "PURUHITO ")
  
  x
}
kamus_dokter$nama_phonetic <- phonetic_id(kamus_dokter$nama_dokter)

library(dplyr)
library(stringr)

kamus_dokter <- kamus_dokter %>%
  mutate(
    nama_split = str_split(nama_dokter, " "),
    kata1 = map_chr(nama_split, 1, .default = ""),
    kata2 = map_chr(nama_split, 2, .default = ""),
    kata_last = map_chr(nama_split, ~tail(.x,1))
  )

install.packages("fuzzyjoin")

library(fuzzyjoin)

candidate_pairs <- kamus_dokter %>%
  inner_join(kamus_dokter, by=c("kata1","kata2"), suffix=c("_1","_2")) %>%
  filter(nama_dokter_1 < nama_dokter_2)


is_truncated_lastname <- function(a,b){
  
  # salah satu adalah prefix dari yang lain
  startsWith(a,b) | startsWith(b,a)
}

candidate_pairs <- candidate_pairs %>%
  rowwise() %>%
  mutate(
    lastname_match =
      is_truncated_lastname(kata_last_1, kata_last_2)
  ) %>%
  ungroup()

library(stringdist)

candidate_pairs <- candidate_pairs %>%
  mutate(
    phonetic_dist = stringdist(nama_phonetic_1,
                               nama_phonetic_2,
                               method="jw")
  )

calon_merge <- candidate_pairs %>%
  filter(
    lastname_match == TRUE |
      phonetic_dist < 0.08
  )


library(stringdist)

candidate_pairs <- candidate_pairs %>%
  mutate(
    phonetic_dist = stringdist(nama_phonetic_1,
                               nama_phonetic_2,
                               method="jw")
  )

calon_merge <- candidate_pairs %>%
  filter(
    lastname_match == TRUE |
      phonetic_dist < 0.08
  )

calon_merge %>% select(nama_dokter_1, nama_dokter_2) %>% head(30)

library(igraph)

edges <- calon_merge %>%
  select(nama_dokter_1, nama_dokter_2)

g <- graph_from_data_frame(edges, directed=FALSE)

cluster_id <- components(g)$membership

cluster_tbl <- tibble(
  nama_dokter = names(cluster_id),
  cluster = cluster_id
)

canonical_name <- kamus_dokter %>%
  left_join(cluster_tbl, by="nama_dokter") %>%
  group_by(cluster) %>%
  slice_max(nchar(nama_dokter), n=1) %>%
  ungroup() %>%
  select(cluster, nama_canonical=nama_dokter)

kamus_dokter <- kamus_dokter %>%
  left_join(cluster_tbl, by="nama_dokter") %>%
  left_join(canonical_name, by="cluster")

kamus_dokter <- dokter_long %>%
  distinct(nama_dokter) %>%
  arrange(nama_dokter)



dokter_long %>%
  filter(str_detect(nama_dokter, "DR")) %>%
  select(nama_dokter) %>%
  head(20)





dokter_singleword_match <- dokter_singleword %>%
  left_join(kamus_dokter, by="nama_dokter")





dokter_long <- dokter_long %>%
  mutate(
    nama_dokter = dokter_list,
    
    # hilangkan DR / DRG
    nama_dokter = str_remove(nama_dokter, "^DRG?\\.?\\s*"),
    
    # hilangkan semua gelar spesialis
    nama_dokter = str_remove_all(nama_dokter,
                                 "\\bSP(ESIALIS)?\\.?\\s?[A-Z]+(\\([A-Z]+\\))?\\b"),
    
    # hapus gelar bertingkat setelah SP
    nama_dokter = str_remove_all(nama_dokter, "SP\\.[A-Z\\.]+"),
    
    # hilangkan gelar akademik umum
    nama_dokter = str_remove_all(nama_dokter,
                                 "\\b(S\\.KED|M\\.KES|MARS|MBIOMED|MEDKLIN|MBOIMED|MPH|PH\\.D|DR\\.PH)\\b"),
    
    # normalisasi apostrophe
    nama_dokter = str_replace_all(nama_dokter, "[`´’]", "'"),
    
    # bersihkan spasi
    nama_dokter = str_squish(nama_dokter),
    
    # huruf besar semua (penting untuk matching)
    nama_dokter = str_to_upper(nama_dokter)
  ) %>%
  filter(nama_dokter != "")

dokter_long <- dokter_long %>%
  select(-dokter_bersih,-dokter_bersih2,-dokter_list)


normalize_spesialis <- function(x){
  
  x <- str_to_upper(x)
  
  # semua bentuk SP.xxx -> SP_xxx
  x <- str_replace_all(x, "SP[\\.-]?", "SP ")
  
  # gabungkan huruf spesialis yang dipisah titik/spasi
  x <- str_replace_all(x, "SP\\s*T\\s*H\\s*T\\s*K\\s*L", "SP THTKL")
  x <- str_replace_all(x, "SP\\s*K\\s*F\\s*R", "SP KFR")
  x <- str_replace_all(x, "SP\\s*D\\s*V\\s*E", "SP DVE")
  x <- str_replace_all(x, "SP\\s*B\\s*S\\s*Y", "SP BSY")
  x <- str_replace_all(x, "SP\\s*O\\s*G", "SP OG")
  x <- str_replace_all(x, "SP\\s*P\\s*D", "SP PD")
  x <- str_replace_all(x, "SP\\s*O\\s*T", "SP OT")
  x <- str_replace_all(x, "SP\\s*A\\s*N", "SP AN")
  x <- str_replace_all(x, "SP\\s*B\\s*S", "SP BS")
  x <- str_replace_all(x, "SP\\s*B\\s*M", "SP BM")
  x <- str_replace_all(x, "SP\\s*B", "SP B")
  x <- str_replace_all(x, "SP\\s*S", "SP S")
  
  # hapus semua spesialis
  x <- str_remove_all(x, "\\bSP\\s*[A-Z]{1,6}\\b")
  
  return(x)
}
dokter_long$nama_dokter <- normalize_spesialis(dokter_long$nama_dokter)






dokter_long %>% summarise(n_unique = n_distinct(nama_dokter))





clean_nama_dokter <- function(x){
  
  x <- str_to_upper(x)
  
  # hapus DR / DRG
  x <- str_remove(x, "^DRG?\\.?\\s+")
  
  # hapus konsultan (K) atau (K-XXX)
  x <- str_remove_all(x, "\\(K[^\\)]*\\)")
  
  # hapus semua SP.xxx
  x <- str_remove_all(x, "\\bSP\\.?\\s?[A-Z]+(\\([A-Z]+\\))?\\b")
  
  # hapus gelar akademik umum
  x <- str_remove_all(x,
                      "\\b(MARS|MPH|MSC|PHD|DRPH|MBA|MH|MKM|MKES|M\\.KES|M\\.KED\\.KLIN)\\b")
  
  # hapus kode konsultan lepas (HTKL, KPARU, dll)
  x <- str_remove_all(x, "\\b[K]?[- ]?[A-Z]{3,5}$")
  
  # normalisasi apostrophe
  x <- str_replace_all(x, "[`´’]", "'")
  
  # sisakan hanya huruf, spasi, apostrophe
  x <- str_replace_all(x, "[^A-Z' ]", " ")
  
  # hapus spasi berlebih
  x <- str_squish(x)
  
  return(x)
}
dokter_long$nama_dokter <- clean_nama_dokter(dokter_long$nama_dokter)


dokter_long$flag_single_token <- str_count(dokter_long$nama_dokter, " ")==0



# -----------------------------
# 2) Normalisasi nama (konsisten, aman)
# -----------------------------
normalize_nama <- function(x){
  
  x <- ifelse(is.na(x), "", x)
  
  # jangan ke lower, pakai upper lebih stabil
  x <- str_to_upper(x)
  
  # normalisasi apostrophe
  x <- str_replace_all(x, "[`´’]", "'")
  
  # hapus gelar depan
  x <- str_remove(x, "^DRG?\\.?\\s+")
  
  # hapus spesialis
  x <- str_remove_all(x, "\\bSP\\.?\\s?[A-Z]+(\\([A-Z]+\\))?\\b")
  
  # hapus gelar akademik umum
  x <- str_remove_all(x, "\\b(MARS|MPH|MEDKLIN|MBOIMED|MBIOMED|MSC|PHD|MBA|MH|MKM|MKES|M\\.KES)\\b")
  
  # hapus (K) konsultan
  x <- str_remove_all(x, "\\(K[^\\)]*\\)")
  
  # sisakan hanya huruf + apostrophe + spasi
  x <- str_replace_all(x, "[^A-Z' ]", " ")
  
  # rapikan
  x <- str_squish(x)
  
  x
}

dokter_long$nama_dokter <- normalize_nama(dokter_long$nama_dokter)

is_multi_dokter <- function(x){
  
  if(is.na(x) || x=="") return(FALSE)
  
  tokens <- unlist(str_split(x, " "))
  tokens <- tokens[tokens!=""]
  
  # terlalu pendek -> bukan multi
  if(length(tokens) <= 4) return(FALSE)
  
  # hitung token yang panjangnya >=5 huruf
  long_tokens <- nchar(tokens) >= 5
  
  # jika ada 3 atau lebih token panjang -> kemungkinan 2 nama orang
  if(sum(long_tokens) >= 3) return(TRUE)
  
  return(FALSE)
}
dokter_long$flag_multi <- sapply(dokter_long$nama_dokter, is_multi_dokter)

# ===============================
# VALIDASI NAMA MANUSIA
# ===============================
dokter_long <- dokter_long %>%
  filter(
    nama_bersih != "",
    str_count(nama_bersih, " ") >= 1,   # minimal 2 kata
    nchar(nama_bersih) >= 5,            # terlalu pendek bukan nama
    !str_detect(nama_bersih, "^[a-z]{1,3}$")  # bukan inisial
  )

# ==== 6. REMOVE DUPLICATE ====
dokter_unique <- dokter_long %>%
  group_by(nama_bersih) %>%
  summarise(
    baris = paste(sort(unique(baris)), collapse=";"),
    .groups="drop"
  ) %>%
  arrange(nama_bersih)

# 1️⃣ Fungsi pembersih awal
clean_text <- function(x){
  
  x %>%
    str_to_lower() %>%
    str_replace_all("\\u00A0", " ") %>%        # spasi excel
    str_replace_all("[,;/()]", " ") %>%        # tanda baca
    str_replace_all("\\s+", " ") %>%           # multi spasi
    str_squish()
}

# 2️⃣ Standarisasi semua gelar Sp
standardize_sp <- function(x){
  
  str_replace_all(
    x,
    "\\bsp[\\.\\- ]*([a-z]{1,4})\\b",
    "sp_\\1"
  )
}

# 3️⃣ Fungsi pemisah dokter
split_dokter <- function(text){
  
  kata <- unlist(str_split(text, " "))
  hasil <- character()
  buffer <- character()
  
  for(i in seq_along(kata)){
    
    buffer <- c(buffer, kata[i])
    
    # setiap gelar menutup 1 dokter
    if(str_detect(kata[i], "^sp_")){
      hasil <- c(hasil, paste(buffer, collapse=" "))
      buffer <- character()
    }
  }
  
  # sisa tanpa gelar (dokter umum)
  if(length(buffer) >= 2){
    hasil <- c(hasil, paste(buffer, collapse=" "))
  }
  
  hasil
}

# 4️⃣ Pipeline utama
dok_rapi <- dokter_long %>%
  select(Namadpjp) %>%
  distinct() %>%
  rename(nama_dokter = Namadpjp)

library(purrr)
dok_rapi <- dok_rapi %>%
  mutate(
    nama_dokter = clean_text(nama_dokter),
    nama_dokter = standardize_sp(nama_dokter),
    nama_dokter = map(nama_dokter, split_dokter)
  ) %>%
  unnest(nama_dokter) %>%
  mutate(
    nama_dokter = str_squish(nama_dokter)
  ) %>%
  filter(nama_dokter != "")

# 5️⃣ Hapus gelar
dok_rapi <- dok_rapi %>%
  mutate(
    nama_clean = nama_dokter %>%
      str_remove("\\bsp_.*$") %>%    # hapus gelar sampai akhir
      str_remove_all("\\bdr\\.?\\b") %>%
      str_replace_all("[^a-z ]", " ") %>%
      str_squish()
  )

# 6️⃣ Filter nama tidak valid
dok_rapi <- dok_rapi %>%
  mutate(
    # hapus hanya huruf tunggal (inisial), tapi JANGAN hapus kata asli
    nama_clean = str_remove_all(nama_clean, "\\b[a-z]\\b"),
    nama_clean = str_squish(nama_clean)
  ) %>%
  filter(
    !is.na(nama_clean),
    nama_clean != "",
    nchar(nama_clean) >= 3
  )

normalize_nama <- function(x){
  
  x <- str_to_lower(x)
  
  # hapus spesialis
  x <- str_remove_all(x, "\\bsp\\s*[a-z]+\\b")
  
  # hapus gelar
  x <- str_remove_all(x, "\\bdr\\b")
  x <- str_remove_all(x, "sp\\s*[a-z]+")
  x <- str_remove_all(x, "biomed\\s*[a-z]+")
  x <- str_remove_all(x, "m biomed\\s*[a-z]+")
  x <- str_remove_all(x, "m med klin\\s*[a-z]+")
  x <- str_remove_all(x, "m kes\\s*[a-z]+")
  x <- str_remove_all(x, "prof|dokter|fics|khom")
  x <- str_replace_all(x, "achmad", "ahmad")
  x <- str_replace_all(x, "ch", "c")
  x <- str_replace_all(x, "dj", "j")
  x <- str_replace_all(x, "oe", "u")
  x <- str_replace_all(x, "sy|sh", "s")
  x <- str_replace_all(x, "dh", "d")
  x <- str_replace_all(x, "th", "t")
  x <- str_replace_all(x, "kh", "h")
  x <- str_replace_all(x, "q", "k")
  x <- str_replace_all(x, "y", "i")
  x <- str_replace_all(x, "ie", "i")
  x <- str_replace_all(x, "iy", "i")
  x <- str_replace_all(x, "aa", "a")
  x <- str_replace_all(x, "ee", "e")
  x <- str_replace_all(x, "oo", "o")
  x <- str_replace_all(x, "ni am", "niam")
  x <- str_replace_all(x, "ulloh", "ullah")
  
  # satukan nama majemuk umum
  x <- str_replace_all(x, "\\bdwi\\s+putra\\b", "dwiputra")
  x <- str_replace_all(x, "\\bdwi\\s+putri\\b", "dwiputri")
  x <- str_replace_all(x, "\\btri\\s+putra\\b", "triputra")
  x <- str_replace_all(x, "\\btri\\s+putri\\b", "triputri")
  x <- str_replace_all(x, "\\badi\\s+putra\\b", "adiputra")
  x <- str_replace_all(x, "\\badi\\s+putri\\b", "adiputri")
  x <- str_replace_all(x, "\\bek\\w?\\s+putra\\b", "ekaputra")
  x <- str_replace_all(x, "\\bek\\w?\\s+putri\\b", "ekaputri")
  x <- str_replace_all(x, "\\bmuhamad\\b|\\bmohamad\\b|\\bmuhammad\\b", "muhamad")
  x <- str_replace_all(x, "\\bmuh\\s+", "muhamad ")
  
  # hapus gelar akademik & profesi umum
  x <- str_remove_all(
    x,
    "\\b(m|s|dr|drg|prof)\\s*(ked|kedg|biomed|biomedik|med|medik|klin|klinik|kes|kesehatan|km|skm|mph|msc|mm|mba|phd|spog|spd|spb|spa)\\b"
  )
  x <- str_remove_all(x, "\\b(biomed|biomedik|med|klin|klinik|kes|kesehatan)\\b")
  x <- str_remove_all(x, "\\b(mars|cmc|qhia|pol|kol|mkes|sked)\\b")
  
  # pisahkan kata nempel
  x <- str_replace_all(x, "putri", " putri")
  x <- str_replace_all(x, "putra", " putra")
  
  # huruf saja
  x <- str_replace_all(x, "[^a-z ]", " ")
  
  str_squish(x)
  
}

dok_rapi <- dok_rapi %>%
  mutate(
    nama_clean = normalize_nama(nama_clean)
  )


library(dplyr)
library(stringr)
library(tidyr)
library(stringdist)
library(purrr)
library(janitor)
library(igraph)

suffix_nama <- c(
  "putra","putri","pratama","utama","permana","saputra",
  "wijaya","maulana","ramadhan","ramadhani","syahputra",
  "syahputri","kurniawan","kurniawati","hidayat","hidayati",
  "maharani","maharana","firdaus","firdauz","rahmawati",
  "rahmawan","setiawan","setiawati","lesmana","gunawan"
)

remove_suffix <- function(x){
  kata <- unlist(str_split(x," "))
  
  # jika kata terakhir adalah suffix → buang
  if(length(kata) > 2 && tail(kata,1) %in% suffix_nama){
    kata <- kata[-length(kata)]
  }
  
  paste(kata, collapse=" ")
}

dok_rapi <- dok_rapi %>%
  mutate(
    nama_clean = map_chr(nama_clean, remove_suffix)
  )

same_person <- function(buffer, nextword){
  
  if(length(buffer) < 2) return(FALSE)
  
  core <- paste(buffer[1:2], collapse=" ")
  
  # jika nama masih variasi orang yang sama
  test1 <- str_detect(paste(buffer, collapse=" "), core)
  test2 <- nchar(nextword) >= 3
  
  # kata tambahan umum
  suffix <- c("putra","putri","tanjaya","pratama","permana",
              "wijaya","kurniawan","ramadhan","maulana",
              "syahputra","saputra","setiawan")
  
  if(nextword %in% suffix) return(TRUE)
  
  # jika masih satu rangkaian nama
  if(test1 && test2) return(TRUE)
  
  FALSE
}

repair_sp <- function(x){
  
  x <- str_to_lower(x)
  
  # bonus
  x <- str_replace_all(x, "\\bpd\\b", "sp_pd")
  x <- str_replace_all(x, "\\bjp\\b", "sp_jp")
  x <- str_replace_all(x, "\\bog\\b", "sp_og")
  x <- str_replace_all(x, "\\bu\\b", "sp_u")
  
  # typo paling umum: ap pd → sp pd
  x <- str_replace_all(x, "\\bap\\s*pd\\b", "sp pd")
  x <- str_replace_all(x, "\\bap\\s*([a-z]{1,3})\\b", "sp \\1")
  
  # dp pd, xp pd, dll
  x <- str_replace_all(x, "\\b[a-z]p\\s*([a-z]{1,3})\\b", "sp \\1")
  
  # spdp / sppd / sp pd / sp-pd / sp.pd
  x <- str_replace_all(x, "\\bsp\\s*([a-z]{1})\\s*([a-z]{1})\\b", "sp \\1\\2")
  x <- str_replace_all(x, "\\bsp[\\.-]?([a-z]{1,4})\\b", "sp_\\1")
  
  # hilangkan spasi aneh
  x <- str_squish(x)
  
  x
}

split_dokter <- function(text){
  
  kata <- unlist(str_split(text, " "))
  n <- length(kata)
  
  hasil <- list()
  buffer <- c()
  
  is_sp <- function(w){
    str_detect(w, "^sp_[a-z]+$")
  }
  
  i <- 1
  while(i <= n){
    
    buffer <- c(buffer, kata[i])
    
    # 1️⃣ jika ketemu gelar → satu dokter selesai
    if(is_sp(kata[i])){
      hasil <- append(hasil, paste(buffer, collapse=" "))
      buffer <- c()
      i <- i + 1
      next
    }
    
    # 2️⃣ cek apakah kata berikutnya masih nama orang yang sama
    if(length(buffer) >= 2 && i < n){
      
      nextword <- kata[i+1]
      
      # jika BUKAN orang yang sama → potong
      if(!same_person(buffer, nextword) && nchar(nextword) >= 3){
        hasil <- append(hasil, paste(buffer, collapse=" "))
        buffer <- c()
      }
    }
    
    i <- i + 1
  }
  
  if(length(buffer) >= 2){
    hasil <- append(hasil, paste(buffer, collapse=" "))
  }
  
  unlist(hasil)
}

dok_rapi <- dok_rapi %>%
  mutate(
    nama_clean = clean_text(nama_clean),
    nama_clean = repair_sp(nama_clean),      # <---- TAMBAHAN PENTING
    nama_clean = standardize_sp(nama_clean),
    nama_clean = map(nama_clean, split_dokter)
  ) %>%
  unnest(nama_clean) %>%
  mutate(nama_clean = str_squish(nama_clean))

normalize_anchor <- function(x){
  
  x <- str_to_lower(x)
  
  # hapus gelar
  x <- str_remove(x, "\\bsp_.*$")
  
  # samakan huruf sering tertukar
  x <- str_replace_all(x, "v", "w")
  x <- str_replace_all(x, "f", "p")
  x <- str_replace_all(x, "z", "s")
  x <- str_replace_all(x, "c", "k")
  
  # hilangkan huruf ganda
  x <- str_replace_all(x, "(.)\\1+", "\\1")
  
  # hilangkan vokal
  x <- str_replace_all(x, "[aeiou]", "")
  
  # pecah kata
  words <- str_split(x, " ")
  
  # ambil 4 konsonan pertama tiap kata
  words <- lapply(words, function(w){
    w <- w[nchar(w) > 0]
    sapply(w, function(k){
      substr(k, 1, min(4, nchar(k)))
    })
  })
  
  # pakai hanya 2 kata pertama (identitas orang)
  words <- sapply(words, function(w){
    paste(head(w,2), collapse=" ")
  })
  
  str_squish(words)
}

# ===============================
# BUAT ANCHOR DULU (INI YANG HILANG)
# ===============================
dok_rapi <- dok_rapi %>%
  mutate(anchor_key = normalize_anchor(nama_clean))

dok_rapi <- dok_rapi %>%
  mutate(
    anchor_key = ifelse(
      is.na(anchor_key) | anchor_key == "",
      nama_clean,
      anchor_key
    )
  )

dok_rapi <- dok_rapi %>%
  filter(!is.na(nama_clean), nama_clean != "")

dok_rapi <- dok_rapi %>%
  mutate(
    anchor = word(anchor_key, 1, 2)
  )

freq_anchor <- dok_rapi %>%
  count(anchor, sort=TRUE)

pisah_anchor <- function(x, anchor_list){
  
  kata <- unlist(str_split(x," "))
  
  if(length(kata) <= 3) return(x)
  
  first2 <- paste(kata[1:2], collapse=" ")
  
  # kalau termasuk anchor sering muncul
  if(first2 %in% anchor_list){
    
    # dokter utama
    dokter1 <- first2
    
    # dokter kedua = sisa kata
    dokter2 <- paste(kata[-c(1,2)], collapse=" ")
    
    return(c(dokter1, dokter2))
  }
  
  return(x)
}

anchor_umum <- freq_anchor %>%
  filter(n >= 4) %>%
  pull(anchor)

dok_rapi <- dok_rapi %>%
  mutate(nama_clean = map(nama_clean, pisah_anchor, anchor_umum)) %>%
  unnest(nama_clean) %>%
  mutate(nama_clean = str_squish(nama_clean))

dok_rapi <- dok_rapi %>%
  mutate(raw_name = nama_dokter)

# FINAL CLEAN NAME
dok_rapi <- dok_rapi %>%
  mutate(nama_clean = normalize_nama(nama_dokter))

# BARU BUAT ANCHOR (INI POSISI YANG BENAR)
dok_rapi <- dok_rapi %>%
  mutate(anchor_key = normalize_anchor(nama_clean))

dok_rapi <- dok_rapi %>%
  mutate(anchor_key = ifelse(anchor_key=="" | is.na(anchor_key), nama_clean, anchor_key))

# FINAL CLEAN NAME (INI SUMBER IDENTITAS DOKTER)
dok_rapi <- dok_rapi %>%
  mutate(nama_clean = normalize_nama(nama_dokter))

# BARU BUAT ANCHOR
dok_rapi <- dok_rapi %>%
  mutate(anchor_key = normalize_anchor(nama_clean))

# ===============================
# FINAL IDENTITAS DOKTER
# ===============================

# 1. nama_clean final
dok_rapi <- dok_rapi %>%
  mutate(nama_clean = nama_dokter %>%
           remove_medical_titles() %>%
           normalize_nama())

# 2. buat anchor_key (INI YANG BELUM ADA TADI)
fix_suffix_initial <- function(x){
  
  x <- toupper(x)
  x <- trimws(gsub("\\s+"," ",x))
  
  parts <- unlist(strsplit(x," "))
  
  if(length(parts) <= 1) return(x)
  
  last <- parts[length(parts)]
  
  # jika kata terakhir hanya 1–3 huruf kapital → buang
  if(grepl("^[A-Z]{1,3}$", last)){
    parts <- parts[-length(parts)]
  }
  
  paste(parts, collapse=" ")
}
dok_rapi$nama_clean <- sapply(dok_rapi$nama_clean, fix_suffix_initial)

dok_rapi <- dok_rapi %>%
  mutate(anchor_key = normalize_anchor(nama_clean))

# 3. fallback jika anchor kosong
dok_rapi <- dok_rapi %>%
  mutate(
    anchor_key = dplyr::if_else(
      is.na(anchor_key) | anchor_key == "",
      nama_clean,
      anchor_key
    )
  )

master_dokter <- dok_rapi %>%
  group_by(anchor_key) %>%
  summarise(
    # pilih nama paling informatif
    nama_baku  = nama_clean[which.max(nchar(nama_clean))],
    
    # simpan semua variasi
    variasi_nama_asli = paste(unique(raw_name), collapse = ";"),
    variasi_nama_split = paste(unique(nama_dokter), collapse = ";"),
    variasi_nama_clean = paste(unique(nama_clean), collapse = ";"),
    
    .groups = "drop"
  ) %>%
  arrange(nama_baku) %>%
  mutate(id_dokter = row_number())

mapping_dokter <- dok_rapi %>%
  select(raw_name, nama_dokter, nama_clean, anchor_key) %>%
  distinct() %>%
  left_join(master_dokter, by="anchor_key")

rescue_id <- function(x){
  
  x <- toupper(x)
  x <- gsub("[^A-Z ]"," ",x)
  x <- trimws(gsub("\\s+"," ",x))
  
  parts <- unlist(strsplit(x," "))
  if(length(parts)==0) return(NA_character_)
  
  # ambil kata terakhir yang bukan inisial
  last <- parts[length(parts)]
  if(grepl("^[A-Z]{1,3}$", last) && length(parts)>=2){
    last <- parts[length(parts)-1]
  }
  
  first <- substr(parts[1],1,1)
  
  paste(first,last,sep="_")
}

mapping_dokter$rescue_key <- sapply(mapping_dokter$nama_clean, rescue_id)

rescue_master <- mapping_dokter %>%
  filter(is.na(id_dokter)) %>%
  distinct(rescue_key) %>%
  mutate(id_rescue = row_number() + max(master_dokter$id_dokter, na.rm=TRUE))

mapping_dokter <- mapping_dokter %>%
  left_join(rescue_master, by="rescue_key") %>%
  mutate(id_dokter_final = coalesce(id_dokter, id_rescue))

# ambil nama depan + inisial belakang
extract_initial <- function(x){
  
  x <- tolower(x)
  x <- gsub("[^a-z ]"," ",x)
  x <- trimws(gsub("\\s+"," ",x))
  
  parts <- unlist(strsplit(x," "))
  
  # minimal: nama depan + 1 huruf
  if(length(parts) < 2) return(NA_character_)
  
  first  <- parts[1]
  second <- substr(parts[2],1,1)
  
  paste(first, second)
}

master_initial <- master_dokter %>%
  mutate(init = sapply(nama_baku, extract_initial)) %>%
  filter(!is.na(init))

dok_rapi_final <- dok_rapi %>%
  left_join(mapping_dokter,
            by = c("nama_dokter" = "nama_dokter")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~str_remove(.x, "\\.x$"), ends_with(".x"))

dokter_long <- dokter_long %>%
  mutate(
    nama_clean = normalize_nama(nama_bersih),
    anchor_key = normalize_anchor(nama_clean))

# QC
mapping_dokter %>% count(id_dokter_final, sort=TRUE) %>% head(10)
sum(is.na(mapping_dokter$id_dokter_final))
# ===============================
# PERBAIKI OVER-MERGE NAMA UMUM
# ===============================

# ambil kata ke-3 (kata pembeda setelah nama inti)
mapping_dokter <- mapping_dokter %>%
  mutate(word2 = word(nama_clean, 3))

# cari dokter yang terlalu banyak variasinya
big_id <- mapping_dokter %>%
  count(id_dokter_final) %>%
  filter(n >= 20) %>%        # ambang collision
  pull(id_dokter_final)

# pecah hanya id yang terlalu besar
mapping_dokter <- mapping_dokter %>%
  mutate(
    id_dokter_final =
      ifelse(id_dokter_final %in% big_id,
             paste(id_dokter_final, word2, sep="_"),
             as.character(id_dokter_final))
  )

# QC ULANG
mapping_dokter %>%
  count(id_dokter_final) %>%
  summarise(max=max(n), p95=quantile(n,0.95))

# FINAL TEMPEL KE KLAIM
dok_long_final <- dokter_long %>%
  left_join(
    mapping_dokter %>% select(anchor_key, id_dokter_final),
    by = "anchor_key"
  )

# CEK
mean(is.na(dok_long_final$id_dokter_final))
dok_long_final %>%
  summarise(
    n_dokter = n_distinct(id_dokter_final),
    n_kasus = n(),
    kasus_per_dokter = n_kasus / n_dokter
  )
dok_long_final %>%
  count(id_dokter_final, sort = TRUE) %>%
  head(20)
# ========================================================================================
save(dok_long_final, file = "kamus_dpjp2025_nosjp.rda")


dok_long_final %>%
  group_by(Kdppklayan, id_dokter_final) %>%
  summarise(kasus=n(), .groups="drop") %>%
  arrange(desc(kasus)) %>%
  head(20)




dok_long_final %>%
  summarise(
    dokter = n_distinct(id_dokter_final),
    kasus = n(),
    rasio = kasus/dokter
  )



# ===============================
# BUANG YANG BUKAN DOKTER
# ===============================
dokter_long <- dokter_long %>%
  filter(
    !is.na(anchor_key),
    anchor_key != ""
  )

dok_long_final <- dokter_long %>%
  left_join(
    mapping_dokter %>% select(anchor_key, id_dokter_final),
    by = "anchor_key"
  )

dok_long_final %>% 
  summarise(kosong = sum(is.na(id_dokter)),
            total = n(),
            persen = mean(is.na(id_dokter))*100)

dok_long_final %>%
  summarise(
    dokter_unik = n_distinct(id_dokter_final),
    kasus = n(),
    rata2_kasus_per_dokter = kasus/dokter_unik
  )

#===============================================================================
dok_long_final %>% 
  filter(is.na(id_dokter)) %>%
  count(Namadpjp, sort=TRUE) %>%
  head(30)


dok_na <- dok_long_final %>%
  filter(is.na(id_dokter)) %>%
  mutate(init = sapply(Namadpjp, extract_initial)) %>%
  left_join(master_initial, by="init") %>%
  mutate(id_dokter = id_dokter.y)


dok_na_fix <- dok_na %>%
  filter(!is.na(id_dokter)) %>%
  group_by(baris) %>%
  filter(n_distinct(id_dokter)==1) %>%
  distinct(baris, id_dokter) %>%
  ungroup()

dok_long_final <- dok_long_final %>%
  left_join(dok_na_fix, by="baris", suffix=c("",".new")) %>%
  mutate(id_dokter = coalesce(id_dokter, id_dokter.new)) %>%
  select(-id_dokter.new)

dok_long_final %>% 
  summarise(persen = mean(is.na(id_dokter))*100)

dok_long_final <- dok_long_final %>%
  mutate(status_identifikasi =
           case_when(
             !is.na(id_dokter) ~ "teridentifikasi",
             str_detect(Namadpjp, "SP\\.|SP") ~ "spesialis_tanpa_nama",
             str_count(Namadpjp, " ") == 0 ~ "nama_tunggal",
             str_detect(Namadpjp, "^[A-Za-z]+\\s[A-Za-z]\\.?$") ~ "inisial",
             TRUE ~ "dokter_baru"
           ))

dok_long_final %>%
  filter(status_identifikasi=="spesialis_tanpa_nama") %>%
  count(baris, sort=TRUE)

# BONUS
dok_long_final <- dok_long_final %>%
  left_join(
    dt %>% select(baris, Kdppklayan, Nmppklayan),
    by="baris"
  )

dok_long_final %>%
  filter(status_identifikasi=="spesialis_tanpa_nama") %>%
  count(Kdppklayan, Nmppklayan, sort=TRUE)

mean(is.na(dok_long_final$id_dokter))*100

unmapped <- dok_long_final %>%
  filter(is.na(id_dokter)) %>%
  count(Namadpjp, sort=TRUE)

head(unmapped, 30)

unmapped %>%
  mutate(kategori =
           case_when(
             str_count(Namadpjp, " ") == 0 ~ "1 kata",
             str_detect(Namadpjp, "spesialis|igd|jaga|tim|operator|konsulen") ~ "bukan nama",
             nchar(Namadpjp) <= 4 ~ "inisial",
             TRUE ~ "kemungkinan dokter baru"
           )) %>%
  count(kategori, sort=TRUE)


#QUALITY CONTROL
mapping_dokter %>%
  filter(id_dokter_final == 844) %>%
  select(nama_clean) %>%
  arrange(nama_clean) %>%
  head(50)

mapping_dokter %>%
  count(id_dokter_final) %>%
  summarise(
    p95 = quantile(n,0.95),
    max = max(n)
  )




dt_final <- dok_long_final %>%
  group_by(baris) %>%
  summarise(
    id_dokter = paste(sort(unique(na.omit(id_dokter))), collapse=";"),
    .groups="drop"
  ) %>%
  mutate(baris = as.numeric(baris)) %>%
  arrange(baris)

dt <- dt %>%
  mutate(baris = as.numeric(baris))

dt_final <- dt_final %>%
  mutate(baris = as.numeric(baris))

dt_final2 <- dt %>%
  left_join(dt_final, by="baris")


library(openxlsx)

mapping_dokter2 <- mapping_dokter %>%
  select(id_dokter,anchor_key,nama_clean,raw_name)
write.xlsx(mapping_dokter2, "mapping_dpjp_final.xlsx", overwrite = TRUE)
# ==== 7. SIMPAN ====
write.csv(dokter_unique, "dokter_unique.csv", row.names=FALSE)