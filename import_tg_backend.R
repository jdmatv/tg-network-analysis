# name: import_tg_backend.R
# date: 12/30/24
# desc: import raw json telegram data collected from api interface

# requires:
library(dplyr)
library(jsonlite)
library(parallel)
library(lubridate)
library(data.table)
library(pbmcapply)

source("/ref/import_tg_frontend.R")

# define if null return NA
# Function to check if a field is present and return its value or NA
elseNA <- function(x) {
  tryCatch({
    if (!is.null(x)) {
      return(x)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)  # Return NA if any error occurs
  })
}

# Function to check two fields and return the first non-null value or NA
elseNA2 <- function(x, y) {
  tryCatch({
    if (!is.null(x)) {
      return(x)
    } else if (!is.null(y)) {
      return(y)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)  # Return NA if any error occurs
  })
}

# detecting whether group or channel
isChan <- function(auth_id) {
  if(length(unique(auth_id))>2){
    return("public_supergroup")
  } else if (length(unique(auth_id))<=2){
    return("public_channel")
  } else {
    return(NA)
  }
}

get_back_urls <- function(text, msg_link_main) {
  if (is.na(text)){
    return(NA)
  } else {
    # Define a more general regular expression pattern to match URLs
    url_pattern <- "\\b(?:https?://|http://|www\\.)?[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}(?:/[a-zA-Z0-9._~:/?#@!$&'()*+,;=%-]*)?\\b"
    
    # Use regmatches and gregexpr to extract URLs from the text
    matches <- regmatches(text, gregexpr(url_pattern, text, ignore.case = TRUE))
    
    # Flatten the list of matches
    urls <- unlist(matches)
    
    # Filter out invalid URLs (e.g., those with consecutive periods or too short)
    valid_urls <- urls[!grepl("\\.\\.|\\b\\d+\\.\\d+\\b", urls) & nchar(urls) > 4 & grepl("[a-zA-Z]", urls)]
    
    
    unique_urls <- unique(valid_urls)
    
    if ((!is.na(msg_link_main)) & 
        (!(tolower(msg_link_main) %in% tolower(unique_urls)))){
      unique_urls <- c(unique_urls, msg_link_main)
    }
    
    if (length(unique_urls)>=1){
      return(paste(unique_urls, collapse = ";"))
    } else {
      return(NA)
    }
  }
}

utc_to_et <- function(utc_time) {
  if (is.na(utc_time)){
    return(NA)
  } else {
    et_time <- with_tz(utc_time, tzone = "America/New_York")
    return(et_time)
  }
}

extract_mentions <- function(text) {
  if (is.na(text) || nchar(text) == 0) {
    return(NA)
  }
  mention_pattern <- "@[A-Za-z0-9_]{4,32}"
  matches <- regmatches(text, gregexpr(mention_pattern, text, perl = TRUE))
  mentions <- unlist(matches)
  
  if (length(mentions) == 0) {
    return(NA)
  } else {
    mentions = unique(sapply(mentions, function(x) substr(x,2,nchar(x))))
    return(paste(unique(mentions), collapse = ";"))
  }
}

create_tg_tibble_api <- function(dat, chan_url, local_path) {
  # Extracting relevant fields and handling missing fields
  msg_rows <- length(dat)
  if (msg_rows==0){ return(NULL) }
  
  return(
    tibble(
      data_src = rep('api', msg_rows),
      src_id = sapply(dat, function(x) x$peer_id$channel_id),
      msg_id = sapply(dat, function(x) elseNA(x$id)),
      uniq_id = paste0(src_id, "_", msg_id),
      src_name = rep(NA, msg_rows),
      src_type = rep(NA, msg_rows), # function to tell if public or private
      src_url = rep(chan_url, msg_rows),
      auth_name = rep(NA, msg_rows),
      auth_name2 = sapply(dat, function(x) elseNA(x$post_author)),
      auth_id = sapply(dat, function(x) elseNA(x$from_id$user_id)),
      auth_id_type = rep(NA, msg_rows),
      msg_url = unlist(mapply(gen_url, src_id, src_url, msg_id)),
      msg_date_time = as.POSIXct(sapply(dat, function(x) utc_to_et(as.POSIXct(elseNA(x$date), tz='Etc/GMT-3')))),
      msg_date = as.Date(msg_date_time),
      msg_text = sapply(dat, function(x) elseNA(x$message)),
      msg_link_main = sapply(dat, function(x) elseNA(x$media$webpage$url)),
      msg_link_name = sapply(dat, function(x) elseNA(x$media$webpage$site_name)),
      msg_link_desc = sapply(dat, function (x) elseNA(x$media$webpage$description)),
      msg_links = unlist(mapply(get_back_urls, msg_text, msg_link_main)),
      msg_tg_links = sapply(msg_links, get_chan_links),
      msg_tg_links_id = sapply(msg_tg_links, get_clean_links),
      msg_tg_links_type = sapply(msg_tg_links, get_link_type),
      msg_mentions = sapply(msg_text, extract_mentions),
      fwd_name = rep(NA, msg_rows),
      fwd_name2 = sapply(dat, function(x) elseNA2(x$fwd_from$from_name, x$fwd_from$post_author)),
      fwd_id = sapply(dat, function(x) elseNA2(x$fwd_from$from_id$channel_id, x$fwd_from$from_id$user_id)),
      fwd_date_time = as.POSIXct(sapply(dat, function(x) utc_to_et(as.POSIXct(elseNA(x$fwd_from$date), tz='Etc/GMT-3')))),
      fwd_msg_id = sapply(dat, function(x) elseNA(x$fwd_from$channel_post)),
      reply_to_peer = sapply(dat, function(x) elseNA2(x$reply_to$reply_to_peer_id$channel_id, x$reply_to$reply_to_peer_id$user_id)),
      reply_to_msg = sapply(dat, function(x) elseNA(x$reply_to$reply_to_msg_id)),
      views = sapply(dat, function(x) elseNA(x$views)),
      forwards = sapply(dat, function(x) elseNA(x$forwards)),
      replies = sapply(dat, function (x) elseNA(x$replies$replies)),
      file_path = local_path
    ) %>% 
      mutate_if(is.character, ~na_if(., "")) %>%
      mutate(src_type = isChan(auth_id))
  )
}

save_tg_tibble_api <- function(json_files, cores, out_path){
  
  if (length(json_files)==0){
    return(NULL)
  }
  
  json_file_names <- unlist(lapply(basename(json_files), 
                                   function(x) substr(x, 1, nchar(x)-5)))
  
  cat("1/3. reading backend files\n")
  
  # Read and convert each JSON file to a data frame (in parallel)
  json_data_list <- pbmclapply(json_files, function(file) {
    jsonlite::fromJSON(file, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  }, mc.cores = cores)
  
  cat("2/3. preprocessing backend data\n")
  
  #Determine the format of each JSON file and process accordingly
  chan_dfs <- pbmcmapply(create_tg_tibble_api, json_data_list, json_file_names, 
                         json_files, SIMPLIFY = F, mc.cores = cores)
  
  cat("3/3. binding frontend data\n\n")
  
  # Bind all tibbles into one
  tg_tibble <- as_tibble(rbindlist(chan_dfs))
  
  if (out_path) {
    # Save as RDS
    saveRDS(tg_tibble, out_path)
  } else {
    return(tg_tibble)
  }
}