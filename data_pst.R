setwd("D:/Downloads")

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

okt22 <- read_delim("Kepwil_data_okt22.csv") %>%
  mutate(Bulan = "1/10/2022")
nov22 <- read_delim("Kepwil_data_nov22.csv") %>%
  mutate(Bulan = "1/11/2022")
des22 <- read_delim("Kepwil_data_des22.csv") %>%
  mutate(Bulan = "1/12/2022")
jan23 <- read_delim("Kepwil_data_jan23.csv") %>%
  mutate(Bulan = "1/1/2023")
feb23 <- read_delim("Kepwil_data_feb23.csv") %>%
  mutate(Bulan = "1/2/2023")
mar23 <- read_delim("Kepwil_data_mar23.csv") %>%
  mutate(Bulan = "1/3/2023")
apr23 <- read_delim("Kepwil_data_apr23.csv") %>%
  mutate(Bulan = "1/4/2023")
mei23 <- read_delim("Kepwil_data_mei23.csv") %>%
  mutate(Bulan = "1/5/2023")
jun23 <- read_delim("Kepwil_data_jun23.csv") %>%
  mutate(Bulan = "1/6/2023")
jul23 <- read_delim("Kepwil_data_jul23.csv") %>%
  mutate(Bulan = "1/7/2023")
agu23 <- read_delim("Kepwil_data_agu23.csv") %>%
  mutate(Bulan = "1/8/2023")
sep23 <- read_delim("Kepwil_data_sep23.csv") %>%
  mutate(Bulan = "1/9/2023")
okt23 <- read_delim("Kepwil_data_okt23.csv") %>%
  mutate(Bulan = "1/10/2023")
nov23 <- read_delim("Kepwil_data_nov23.csv") %>%
  mutate(Bulan = "1/11/2023")
des23 <- read_delim("Kepwil_data_des23.csv") %>%
  mutate(Bulan = "1/12/2023")
jan24 <- read_delim("Kepwil_data_jan24.csv") %>%
  mutate(Bulan = "1/1/2024")
feb24 <- read_delim("Kepwil_data_feb24.csv") %>%
  mutate(Bulan = "1/2/2024")
mar24 <- read_delim("Kepwil_data_mar24.csv") %>%
  mutate(Bulan = "1/3/2024")
apr24 <- read_delim("Kepwil_data_apr24.csv") %>%
  mutate(Bulan = "1/4/2024")
mei24 <- read_delim("Kepwil_data_mei24.csv") %>%
  mutate(Bulan = "1/5/2024")
jun24 <- read_delim("Kepwil_data_jun24.csv") %>%
  mutate(Bulan = "1/6/2024")
jul24 <- read_delim("Kepwil_data_jul24.csv") %>%
  mutate(Bulan = "1/7/2024")
agu24 <- read_delim("Kepwil_data_agu24.csv") %>%
  mutate(Bulan = "1/8/2024")
sep24 <- read_delim("Kepwil_data_sep24.csv") %>%
  mutate(Bulan = "1/9/2024")
okt24 <- read_delim("Kepwil_data_okt24.csv") %>%
  mutate(Bulan = "1/10/2024")
data <- rbind(okt22,nov22,des22,
              jan23,feb23,mar23,apr23,mei23,jun23,jul23,agu23,sep23,okt23,nov23,des23,
              jan24,feb24,mar24,apr24,mei24,jun24,jul24,agu24,sep24,okt24)
rm(okt22,nov22,des22,
   jan23,feb23,mar23,apr23,mei23,jun23,jul23,agu23,sep23,okt23,nov23,des23,
   jan24,feb24,mar24,apr24,mei24,jun24,jul24,agu24,sep24,okt24)

data$Bulan <- as.Date(data$Bulan, format = "%d/%m/%Y")
write.csv(data, "D://Downloads//data_peserta.csv",
          na="", row.names = FALSE)
