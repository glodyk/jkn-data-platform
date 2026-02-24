# ============================================================
# ICD REFERENCE BUILDER FOR BIGQUERY (FINAL PRODUCTION SAFE)
# ============================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(data.table)
library(stringr)
library(stringi)

# ============================================================
# FUNCTION: CLEAN ICD
# ============================================================

clean_icd <- function(x){
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    str_trim() %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("\\.", "")
}

# ============================================================
# PATH ICD10 WHO
# ============================================================

setwd("D:/data gresik/UR/ICD 10 & 9-CM/icd102010enMeta")

chapters <- fread('chapters.txt', sep=";", encoding="UTF-8", header = FALSE, quote="")
blocks   <- fread('blocks.txt',   sep=";", encoding="UTF-8", header = FALSE, quote="")
codes    <- fread('codes.txt',    sep=";", encoding="UTF-8", header = FALSE, quote="")

names(chapters) <- c('chapter_number','chapter_title')

names(blocks) <- c(
  'block_start',
  'block_end',
  'chapter_number',
  'block_title'
)

names(codes) <- c(
  'level_classification',
  'place_classification_tree',
  'terminal_node',
  'chapter_number',
  'character3_block',
  'code_wo_dagger',
  'code_wo_asterisk',
  'code_wo_dot',
  'title',
  'reference_mortality_1',
  'reference_mortality_2',
  'reference_mortality_3',
  'reference_mortality_4',
  'reference_morbidity'
)

# ============================================================
# CLEAN ICD10
# ============================================================

icd10 <- codes %>%
  mutate(
    icd10 = clean_icd(code_wo_dot),
    icd10_3 = substr(icd10,1,3),
    is_terminal = as.integer(terminal_node)
  ) %>%
  filter(nchar(icd10)>=3)

# ============================================================
# FIX BLOCK RANGE MATCH (SAFE NUMERIC ICD COMPARISON)
# ============================================================

convert_icd_to_numeric <- function(code){
  letter <- substr(code,1,1)
  number <- as.numeric(substr(code,2,3))
  return(match(letter, LETTERS)*1000 + number)
}

blocks <- blocks %>%
  mutate(
    block_start3 = substr(block_start,1,3),
    block_end3   = substr(block_end,1,3),
    start_num = sapply(block_start3, convert_icd_to_numeric),
    end_num   = sapply(block_end3, convert_icd_to_numeric)
  )

icd10$num_code3 <- sapply(icd10$icd10_3, convert_icd_to_numeric)

icd10 <- icd10 %>%
  rowwise() %>%
  mutate(
    block_title = blocks$block_title[
      which(blocks$start_num <= num_code3 & blocks$end_num >= num_code3)[1]
    ]
  ) %>%
  ungroup()

# ============================================================
# ADD CHAPTER DESCRIPTION
# ============================================================

icd10 <- icd10 %>%
  left_join(chapters, by="chapter_number")

# ============================================================
# KEEP VALID DIAGNOSIS (IMPORTANT FIX)
# ============================================================

icd10_terminal <- icd10 %>%
  filter(nchar(icd10)>=3) %>%
  select(
    icd10,
    icd10_3,
    title,
    chapter_number,
    chapter_title,
    block_title
  ) %>%
  distinct()

# ============================================================
# ICD9CM PROCEDURE
# ============================================================

setwd("D:/data gresik/UR/ICD 10 & 9-CM/ICD-9-CM-v32-master-descriptions")

icd9cm <- read_excel("CMS32_DESC_LONG_SHORT_SG.xlsx")

icd9cm <- icd9cm %>%
  mutate(
    icd9cm = clean_icd(`PROCEDURE CODE`)
  ) %>%
  select(
    icd9cm,
    `SHORT DESCRIPTION`,
    `LONG DESCRIPTION`
  ) %>%
  distinct()

# ============================================================
# EXPORT (BIGQUERY SAFE UTF8)
# ============================================================

dir.create("output", showWarnings = FALSE)

fwrite(icd10_terminal,
       "output/icd10_reference.csv",
       na="",
       quote=TRUE,
       bom=FALSE)

fwrite(icd9cm,
       "output/icd9cm_reference.csv",
       na="",
       quote=TRUE,
       bom=FALSE)

print("SELESAI. FILE SIAP DIUPLOAD KE BIGQUERY.")