library(stats)
library(dplyr)
library(tidyr)
library(tidyverse)
library(openxlsx)


potensi = read.csv2(file.choose())
potensi$No_Ka_Pst <- as.factor(potensi$No_Ka_Pst)
potensi$No_SJP <- as.factor(potensi$No_SJP)
potensi$KDPPK_Layan <- as.factor(potensi$KDPPK_Layan)

potensi$Nmdati2 <- as.character(trimws(substr(potensi$KDPPK_Layan,1,4)), "both")
potensi$Bupel <- as.character(trimws(substr(potensi$No_SJP,9,12)), "both")

potensi$Nmdati2 <- as.character(potensi$Nmdati2)
potensi <- data.frame(potensi)

str(potensi)

potensi <- potensi %>% 
  mutate(
    Dati2 = case_when(
      Nmdati2 == "1304" ~ "Bojonegoro",
      Nmdati2 == "0202" ~ "Bojonegoro",
      Nmdati2 == "1305" ~ "Tuban",
      Nmdati2 == "0203" ~ "Tuban",
      Nmdati2 == "1302" ~ "Tuban",
      TRUE ~ Nmdati2))

View(potensi)

write.xlsx(potensi, "C:/Users/Lenovo/Downloads/potensi.xlsx",
           sheetName = "Sheet1", col.names = TRUE, row.names = FALSE, 
           showNA = FALSE, password = NULL)
