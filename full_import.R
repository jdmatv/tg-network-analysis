# name: full_import.R
# date: 12-28-24
# desc: detect whether json is front api or desktop and run import code

# requires:
library(fst)

# importing frontend and backend import functions
source("/ref/import_tg_backend.R")
source("/ref/import_tg_frontend.R")

# input/output folders
in_dir <- "IN_DIR"
out_path <- "OUT_PATH.fst"
cores <- 6

# get all json files in in_dir
in_files <- list.files(path = in_dir, pattern = "\\.json$", 
                       recursive = TRUE, full.names = TRUE)

# check whether api or app file
check_json_type <- function(file_path, num_bytes = 20) {
  # Open the file in binary read mode
  con <- file(file_path, "rb")
  first_bytes <- readBin(con, what = "raw", n = num_bytes)
  first_chars <- rawToChar(first_bytes)
  
  # Check the starting character(s) to determine the JSON type
  if (startsWith(first_chars, "{")) {
    
    # checking if ending is valid
    file_size <- file.info(file_path)$size
    seek(con, where = file_size - num_bytes)
    last_chars <- readChar(con, nchars = num_bytes, useBytes = TRUE)
    close(con)
    if (!(endsWith(last_chars, "]\n  }\n ]\n}") |
          endsWith(last_chars,"[\n ]\n}"))){
      stop(paste0("Error: corrupted JSON file. ", file_path))
    }
    return("app")
  } else if (startsWith(first_chars, "[")) {
    close(con)
    return("api")
  } else {
    close(con)
    return(NA)
  }
}

# check whether 

# function to import files
import_merge <- function(api_jsons, app_jsons, cores, out_path){
  if (length(api_jsons)==0){
    cat("no backend data\n") } else {
      cat("--importing backend data--\n") }
  api_imported <- save_tg_tibble_api(api_jsons, cores, out_path = FALSE)
  if (length(app_jsons)==0){
    cat("no frontend data\n") } else {
      cat("--importing frontend data--\n") }
  app_imported <- save_tg_tibble_app(app_jsons, cores, out_path = FALSE)
  cat("writing data as fst\n")
  write.fst(rbind(api_imported, app_imported), out_path)
}

# sorting by app and api-derived json files
json_type <- sapply(in_files, check_json_type)
api_jsons <- in_files[json_type=="api"]
app_jsons <- in_files[json_type=="app"]
import_merge(api_jsons, app_jsons, cores, out_path)
