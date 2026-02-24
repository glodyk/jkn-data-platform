library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(DT)
library(stringr)
library(tidyr)
library(scales)
library(DBI)
library(duckdb)

# --- fungsi clean_data (sama seperti Anda) ---
clean_data <- function(df){
  want <- c("Nosjp","Nokapst","Umur","Jkpst","Biayatagih","Klsrawat","Nmdati2Layan",
            "Kdppklayan","Kelasrsmenkes","Nmtkp","Politujsjp","Tglpelayanan",
            "Tgldtgsjp","Tglplgsjp","Tglversjp","Tglstjkeu","Kdinacbgs","Nminacbgs",
            "kdsa","kdsd","kdsi","kdsp","kdsr","Kddiagprimer","Diagsekunder","Procedure",
            "Namadpjp","Nmjnspulang","biayars","Biayaverifikasi","Biayatagih")
  present <- intersect(want, names(df))
  df <- df %>% select(all_of(present))
  topup_cols <- intersect(c("kdsa","kdsd","kdsi","kdsp","kdsr"), names(df))
  if(length(topup_cols)) df <- df %>% mutate(across(all_of(topup_cols),
                                                    ~ na_if(., "None") %>% na_if("NONE")))
  if("Politujsjp" %in% names(df)) df$Politujsjp <- toupper(trimws(df$Politujsjp))
  if("Nminacbgs" %in% names(df)) df$Nminacbgs <- trimws(as.character(df$Nminacbgs))
  date_cols <- intersect(c("Tgldtgsjp","Tglplgsjp","Tglpelayanan","Tglversjp","Tglstjkeu"), names(df))
  for (cname in date_cols) df[[cname]] <- as.Date(df[[cname]], format = "%m/%d/%Y")
  if("Kdinacbgs" %in% names(df)){
    df$CBG <- str_trim(substr(df$Kdinacbgs,1,6))
    tp <- substr(df$Kdinacbgs,3,3)
    df$tipecbg <- case_when(
      tp=="1" ~ "Prosedur Rawat Inap",
      tp=="2" ~ "Prosedur Besar Rawat Jalan",
      tp=="3" ~ "Prosedur Signifikan Rawat Jalan",
      tp=="4" ~ "Rawat Inap Bukan Prosedur",
      tp=="5" ~ "Rawat Jalan Bukan Prosedur",
      tp=="6" ~ "Rawat Inap Kebidanan",
      tp=="7" ~ "Rawat Jalan Kebidanan",
      tp=="8" ~ "Rawat Inap Neonatal",
      tp=="9" ~ "Rawat Jalan Neonatal",
      TRUE ~ NA_character_)
  }
  if("Diagsekunder" %in% names(df)){
    icd_regex <- "\\b[A-Z]\\d{2,4}(?:\\.\\d+)?\\b"
    df$icd10_kdsekunder <- str_extract_all(as.character(df$Diagsekunder), icd_regex) %>%
      lapply(unique) %>% sapply(function(x) if(length(x)==0) NA_character_ else paste(x, collapse=";"))
  }
  if("Procedure" %in% names(df)){
    proc_regex <- "\\b\\d{3,4}\\b"
    df$icd9cm_kdprocedure <- str_extract_all(as.character(df$Procedure), proc_regex) %>%
      lapply(unique) %>% sapply(function(x) if(length(x)==0) NA_character_ else paste(x, collapse=";"))
  }
  rename_map <- c(Nosjp="claim_id", Nokapst="patient_id", Umur="patient_age", Jkpst="patient_gender",
                  Nmdati2Layan="region_kabkota", Kdppklayan="facility_id", Kelasrsmenkes="facility_class",
                  Nmtkp="service_type", Politujsjp="poli", Klsrawat="care_class",
                  Namadpjp="doctor_name", Nmjnspulang="discharge_type", Kdinacbgs="ina_cbg_code",
                  Nminacbgs="ina_cbg_name", biayars="claim_amount", Biayatagih="claim_tariff",
                  Biayaverifikasi="claim_cost")
  common_names <- intersect(names(rename_map), names(df))
  if(length(common_names)) df <- df %>% rename( !!!setNames(rename_map[common_names], common_names) )
  if(all(c("Tgldtgsjp","Tglplgsjp") %in% names(df))){
    df$los <- as.integer(difftime(df$Tglplgsjp, df$Tgldtgsjp, units="days")) + 1
    df$los[is.na(df$los)] <- NA
  }
  if("Tglpelayanan" %in% names(df)) df$month_layan <- as.Date(cut(df$Tglpelayanan, "month"))
  if("Tglstjkeu" %in% names(df)) df$month_bayar <- as.Date(cut(df$Tglstjkeu, "month"))
  df
}

# --- DuckDB connection global (one per R process) ---
con <- dbConnect(duckdb::duckdb(), dbdir = "data.duckdb", read_only = FALSE)

ui <- fluidPage(
  titlePanel("Ina CBG Claims - DuckDB Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV (will be written to data.duckdb)", accept = c(".csv")),
      checkboxInput("use_rda", "Load ur_data.rda and write to DuckDB (if exists)", value = FALSE),
      uiOutput("facility_ui"),
      dateRangeInput("daterange", "Date range (Tglpelayanan)", start = Sys.Date()-365, end = Sys.Date()),
      numericInput("nrows", "Max rows to fetch", value = 500, min = 10),
      actionButton("refresh", "Refresh / Recompute")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Time Series", plotOutput("ts_plot", height = "400px")),
        tabPanel("Table", DTOutput("table"))
      )
    )
  )
)

server <- function(input, output, session){
  # helper: write uploaded CSV or in-memory data to DuckDB table "mytable"
  observeEvent(input$file, {
    req(input$file)
    tmp <- input$file$datapath
    # use dbWriteTable with readr::read_csv for streaming
    df <- read_csv(tmp, show_col_types = FALSE)
    dbWriteTable(con, "mytable", df, overwrite = TRUE)
    showNotification("CSV written to data.duckdb as 'mytable'", type = "message")
  })
  
  observeEvent(input$use_rda, {
    if(input$use_rda && file.exists("ur_data.rda")){
      # load expected object name; adapt if different
      e <- new.env()
      load("ur_data.rda", envir = e)
      # try to find first data.frame-like object
      objs <- ls(e)
      df_obj <- NULL
      for(nm in objs){
        if(is.data.frame(e[[nm]])){
          df_obj <- e[[nm]]; break
        }
      }
      if(is.null(df_obj)) {
        showNotification("No data.frame found inside ur_data.rda", type="error")
      } else {
        dbWriteTable(con, "mytable", df_obj, overwrite = TRUE)
        showNotification("ur_data.rda content written to data.duckdb as 'mytable'", type = "message")
      }
    }
  })
  
  # helper to query subset from DuckDB, collect to memory, then clean
  fetch_subset <- function(daterange = NULL, facility = NULL, n = 500){
    if(!dbExistsTable(con, "mytable")) return(NULL)
    q <- tbl(con, "mytable")
    # filter by date column name variants if present on table
    tbl_cols <- colnames(q)
    # prefer Tglpelayanan if exists
    if("Tglpelayanan" %in% tbl_cols && !is.null(daterange)){
      # DuckDB stores CSV date as text; try casting; we pull rows using SQL BETWEEN on string if format m/d/Y,
      # safer to collect a slightly larger sample by date range converted to character in m/d/Y
      start_chr <- format(daterange[1], "%m/%d/%Y")
      end_chr <- format(daterange[2], "%m/%d/%Y")
      q <- filter(q, Tglpelayanan >= start_chr & Tglpelayanan <= end_chr)
    } else if("Tglpelayanan" %in% tbl_cols && is.null(daterange)){
      # no date filter
    } else {
      # no Tglpelayanan column: do nothing
    }
    if(!is.null(facility) && facility != "All"){
      if("facility_name" %in% tbl_cols) q <- filter(q, facility_name == facility
                                                    