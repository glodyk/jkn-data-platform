# memanggil koneksi BigQuery

source("config/bigquery_auth.R")

wait_for_table <- function(table_id){
  
  cat("Checking table:", table_id, "\n")
  
  repeat {
    
    exists <- tryCatch({
      bigrquery::bq_table_exists(table_id)
    }, error = function(e) FALSE)
    
    if (exists) break
    
    cat("  waiting table metadata...\n")
    Sys.sleep(5)
  }
}

run_sql <- function(file){
  
  cat("Running:", file, "\n")
  
  sql <- paste(readLines(file, warn = FALSE), collapse="\n")
  
  job <- bigrquery::bq_perform_query(
    query = sql,
    billing = project_id,
    use_legacy_sql = FALSE
  )
  
  bigrquery::bq_job_wait(job, quiet = FALSE)
  
  # beri waktu BigQuery menulis metadata tabel
  Sys.sleep(8)
}

folders <- c(
  "sql_warehouse/00_raw",
  "sql_warehouse/01_staging",
  "sql_warehouse/fragmentasi",
  "sql_warehouse/02_mart"
)

for (f in folders){
  if (dir.exists(f)){
    files <- sort(list.files(f, pattern="sql$", full.names=TRUE, recursive=TRUE, ignore.case=TRUE))
    cat("Found", length(files), "SQL files in", f, "\n")
    for (file in files){
      run_sql(file)
    }
  }
}

cat("=== DATA PLATFORM BUILD COMPLETE ===\n")
