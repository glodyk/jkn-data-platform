library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
library(purrr)
library(data.table)

setwd("D:/Downloads/")   # sesuaikan

tema_raw <- read_excel("TEMA AAK.xlsx") %>%
  clean_names()

glimpse(tema_raw)
names(tema_raw)

# extract ICD-10 from Diagsekunder and ICD-9 procedure codes from Procedure using stringi (vectorized)
icd_regex <- "\\b[A-Z]\\d{2,4}(?:\\.\\d+)?\\b"
diag_vec <- as.character(dt$Diagsekunder)
diag_vec[is.na(diag_vec) | trimws(diag_vec) == ""] <- NA_character_
diag_matches <- stri_extract_all_regex(diag_vec, icd_regex, omit_no_match = TRUE)
dt[, Diagsekunder_codes := vapply(diag_matches, function(x) if(length(x)==0) NA_character_ else paste(unique(x), collapse=";"), FUN.VALUE=character(1))]

icd_proc_regex <- "\\b\\d{3,4}\\b"
proc_matches <- stri_extract_all_regex(as.character(dt$Procedure), icd_proc_regex, omit_no_match = TRUE)
dt[, kdprosedure := vapply(proc_matches, function(x) if(length(x)==0) NA_character_ else paste(unique(x), collapse=";"), FUN.VALUE=character(1))]

# replace "NA" strings if any created
dt[, Diagsekunder_codes := na_if(Diagsekunder_codes, "NA")]
dt[, kdprosedure := na_if(kdprosedure, "NA")]

tema_audit <- tema_raw %>%
  mutate(
    icd = str_replace_all(icd, " ", ""),
    procedure = str_replace_all(procedure, " ", "")
  ) %>%
  separate_rows(icd, sep = ";|,") %>%
  separate_rows(procedure, sep = ";|,") %>%
  mutate(
    icd = toupper(icd),
    procedure = toupper(procedure)
  )
