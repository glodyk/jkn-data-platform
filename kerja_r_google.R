.rs.restartR()
library(ggmap)
register_google(key="REMOVED_GOOGLE_KEYA-BZefkU26N4nD2BWvV5hGESiix9UDs4Y")

geocode("RSUD Gambiran Kediri")


remove.packages(c("curl","openssl","httr","httr2","ggmap"))
install.packages("curl")
install.packages("openssl")
install.packages("httr2")
install.packages("httr")
install.packages("ggmap")
.rs.restartR()

library(httr)
GET("https://www.google.com")

library(httr)
GET("https://maps.googleapis.com/maps/api/geocode/json?address=jakarta&key=REMOVED_GOOGLE_KEYA-BZefkU26N4nD2BWvV5hGESiix9UDs4Y")

library(ggmap)

register_google(key = "REMOVED_GOOGLE_KEYA-BZefkU26N4nD2BWvV5hGESiix9UDs4Y")
geocode("Jakarta, Indonesia")


library(ggmap)
register_google(key="REMOVED_GOOGLE_KEYA-BZefkU26N4nD2BWvV5hGESiix9UDs4Y")
geocode("Kediri, Indonesia")


library(gargle)

gargle::credentials_service_account(
  path = "D:/Rcloud/bpjs-kediri-82616a5b1041.json"
)





library(googleCloudStorageR)
library(googleAuthR)

# 1. autentikasi robot
gar_auth_service("D:/Rcloud/bpjs-kediri-82616a5b1041.json")

# 2. set project ID
Sys.setenv(GOOGLE_CLOUD_PROJECT = "bpjs-kediri")

# 3. set bucket default
gcs_global_bucket("bpjs-kediri")
gcs_list_objects()

gcs_auth("D:/Rcloud/bpjs-kediri-82616a5b1041.json")
gcs_global_bucket("bpjs-kediri")
gcs_list_objects()


#=============================================================================
files_cloud <- gcs_list_objects(prefix = "cbg_hv_layan")
files_cloud$name

library(data.table)
library(stringr)

pilih <- files_cloud$name[
  str_detect(files_cloud$name, "jan2026")
]

pilih

cols_keep <- c("Nosjp","Nokapst","Tgldtgsjp","Tglplgsjp","Kelasrsmenkes","Kdppklayan",
               "Politujsjp","Kdinacbgs","Nminacbgs","Kddiagprimer","Nmdiagprimer",
               "Diagsekunder","Procedure","Jenisppkperujuk","Kdppkperujuk","Namadpjp",
               "Nmjnspulang","biayars","Biayaverifikasi","Tglpelayanan")

baca_cloud <- function(path_file, cols_keep){
  
  # buat nama file sementara unik
  tf <- tempfile(fileext = ".csv")
  
  gcs_get_object(
    object_name = path_file,
    saveToDisk = tf,
    overwrite = TRUE
  )
  
  dt <- fread(
    tf,
    select = cols_keep,
    encoding = "UTF-8",
    showProgress = FALSE
  )
  
  unlink(tf)
  
  return(dt)
}

library(pbapply)

klaim <- rbindlist(
  pblapply(pilih, baca_cloud, cols_keep = cols_keep),
  fill = TRUE
)

# Tambahkan identitas file asal
klaim <- rbindlist(
  pblapply(pilih, function(x){
    dt <- baca_cloud_select(x, cols_keep)
    dt[, sumber_file := basename(x)]
    return(dt)
  }),
  fill = TRUE
)

dim(klaim)
head(klaim)