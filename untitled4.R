library(dplyr)

diagnosa = read.csv(file.choose())
str(diagnosa)

colnames(diagnosa)

# Core packages
library(tidyverse)
library(tidyquant)

# date manipulation
library(timetk)
library(lubridate)

# Modeling packages
library(forecast)

buban$Buban <- as.Date(buban$Buban, format = "%m/%d/%Y")
str(buban)


buban %>% 
  tk_summary_diagnostics(
    .date_var = Buban
  )

buban %>% 
  plot_acf_diagnostics(
    .date_var = month,
    .value    = Rajal,
    .show_white_noise_bars = TRUE
  )

buban %>% 
  plot_seasonal_diagnostics(
    .date_var = month,
    .value    = Rajal,
    .geom     = "boxplot"
  )
