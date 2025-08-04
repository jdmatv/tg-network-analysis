# name: import_tg_frontend.R
# date: 5/17/24
# desc: import raw json telegram data collected from front-end interface
# documentation:

# input: folder of json files exported from the Telegram desktop app
# output: formatted tibble containing data from the json files
# note: be sure to name json files as 'INSERT END OF CHANNEL URL'.json
# ex: for channel 'https://t.me/swodki', name the file 'swodki.json'

# requires
library(jsonlite)
library(dplyr)
library(parallel)
library(lubridate)
library(data.table)
library(pbmcapply)

# function to get plain text from message text fields in JSONs
get_plain_text <- function(text_ents){
  gt <- function(df) {
    if (is.character(df$text)){
      return(paste0(df$text, collapse = ""))
    } else {return(NA)}
  }
  return(unlist(lapply(text_ents, gt)))
}

# function to get links from text fields in JSONs
get_links <- function(text_ents){
  if (is.character(text_ents$text)){
    links <- c()
    
    # for hyperlinks, append the web address
    if ("text_link" %in% text_ents$type){
      links <- c(links, na.omit(text_ents$href))
    }
    
    # for links in text, append the web address
    if ("link" %in% text_ents$type){
      link_df <-  text_ents[(text_ents$type=="link"),]
      links <- c(links, link_df$text)
    }
    
    # return the semicolon separated list of links if not NA
    if (length(links)>0){
      return(paste(na.omit(unique(links)), collapse=";"))
    } else {return(NA)}
  } else {return(NA)}
}

# function to get telegram channel links from links obtained from get_links
get_chan_links <- function(links){
  if (is.na(links)){return(NA)}
  
  # separate list back out by semicolons
  links_sp <- unlist(strsplit(links, ";"))
  
  # return semicolon-separated links that contain 't.me' or 'telegram.me'
  if (length(links_sp)>0){
    matches <- grep("t\\.me/|telegram\\.me/", links_sp, ignore.case = TRUE)
    if (length(matches)>0){
      matched_links <- na.omit(unique(links_sp[matches]))
      return(paste(matched_links[nchar(matched_links)>4], collapse=";"))
    } else {return(NA)}
  } else {return(NA)}
}

# function to get tg channel link id (most informative portion) from tg links  
get_clean_links <- function(tg_links, 
                            link_words = c("joinchat", "addlist", "contact",
                                           "share", "addstickers", "addemoji",
                                           "addtheme", "c", "bg", "s", "boost",
                                           "proxy", "socks", "giftcode")){
  if (is.na(tg_links)){return(NA)}
  
  # separate list back out by semicolons
  links_sp <- unlist(strsplit(tg_links, ";"))
  if (length(links_sp)>0){
    
    # clean links (extract part after 'me' and before any '?')
    matched_links <- sub(".*\\.me/", "\\1", links_sp)
    matched_links <- sub("(.*?)\\?|&.*", "\\1", matched_links)
    
    # split each link by forward slashes and identify most informative portion
    ml_sps <- c()
    for (ml in 1:length(matched_links)){
      ml_sp <- unlist(strsplit(matched_links[ml], "/"))
      ml_sp_clean <- c()
      ml_sp_lengths <- c()
      for (mls in 1:length(ml_sp)){
        if ((is.null(ml_sp[mls])) || (is.na(ml_sp[mls])) || 
            (length(ml_sp[mls]) == 0)){
          print(paste("NULL", ml_sp[mls], matched_links[ml]))
        } else if ((((!grepl("^[0-9]+$", ml_sp[mls])) | (nchar(ml_sp[mls])>=8))) 
                   & (!(ml_sp[mls] %in% link_words))) {
          ml_sp_clean <- c(ml_sp_clean, ml_sp[mls])
          ml_sp_lengths <- c(ml_sp_lengths, nchar(ml_sp[mls]))
        }
      }
      # return longest portion w/o joinchat or less than 9 numbers
      if (length(ml_sp_clean)>0) {
        ml_sps <- c(ml_sps, ml_sp_clean[nchar(ml_sp_clean)==max(ml_sp_lengths)][1])
      } else {
        ml_sps <- c(ml_sps, "NA")
      }
    }
    # final cleaning and return semicolon separated list
    ml_sps <- sub(".*@", "", ml_sps)
    ml_sps <- sub("%.*$", "", ml_sps)
    if (length(ml_sps)>0){
      return(paste(na.omit((ml_sps)),collapse=";"))
    } else {return(NA)}
  } else {return(NA)}
}

# function to get tg channel link id type from tg links
get_link_type <- function(tg_links){
  if (is.na(tg_links)){return(NA)}
  
  # separate back out by semicolons
  links_sp <- unlist(strsplit(tg_links, ";"))
  if (length(links_sp)>0){
    types <- c()
    for (link in 1:length(links_sp)){
      if ((is.null(links_sp[link])) || (is.na(links_sp[link])) || 
          (length(links_sp[link]) == 0) || (nchar(links_sp[link]) == 0)){
        types <- c(types, "NA")
      } else if (grepl("/joinchat/", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "joinchat")
      } else if (grepl("/addlist/", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "addlist")
      } else if (grepl("/contact/", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "contact")
      } else if (grepl("/c/", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "private")
      } else if (grepl("/s/", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "pubchangroup_web")
      } else if (grepl("\\+", links_sp[link])){
        types <- c(types, "joinlink")
      } else if (grepl("/addstickers/|/addemoji/|/addtheme/|/bg/", 
                       links_sp[link], ignore.case=TRUE)){
        types <- c(types, "emoji_sticker")
      } else if (grepl("/boost/", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "boost")
      } else if (grepl("/proxy|/socks", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "proxy_socks")
      } else if (grepl("/giftcode/", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "giftcode")
      } else if (grepl("/share/", links_sp[link], ignore.case=TRUE)){
        types <- c(types, "share")
      } else {
        types <- c(types, "pubchangroup")
      }
    }
    return(paste(types, collapse=";"))
  }
}

# function to get mentions (when you @ an account)
get_mentions <- function(text_ents){
  gm <- function(df) {
    if (is.character(df$text)){
      mentions <- c()
      
      if ("mention" %in% df$type){
        mention_df <-  df[(df$type=="mention"),]
        mentions <- c(mentions, mention_df$text)
      }
      if (length(mentions)>0){
        mentions <- sapply(mentions, function(x) substr(x, 2, nchar(x)))
        return(paste(unique(mentions), collapse=";"))
      } else {return(NA)}
    } else {return(NA)}
  }
  return(unlist(lapply(text_ents, gm)))
}

# function to add url
gen_url <- function(src_id, src_url, msg_id){
  if (!is.na(msg_id) & !is.na(src_url) & !endsWith(tolower(src_url), '--chat')){
    return(paste0('https://t.me/',src_url,'/',msg_id))
  } else if (!is.na(src_id) & !is.na(msg_id)) {
    return(paste0('https://t.me/c/',src_id,'/',msg_id))
  } else {
    return(NA)
  }
}

# functions to split auth id into type and ID 
auth_id_id <- function(auth_id){
  if (is.na(auth_id)){ return(NA) }
  matches <- regmatches(auth_id, regexec("([a-zA-Z]+)([0-9]+)", auth_id))
  return(as.numeric(matches[[1]][3]))
}

auth_id_type <- function(auth_id){
  if (is.na(auth_id)){ return(NA) }
  matches <- regmatches(auth_id, regexec("([a-zA-Z]+)([0-9]+)", auth_id))
  return(matches[[1]][2])
}

# function to create tibbles for each json file
create_tg_tibble_front <- function(dat, chan_url, local_path) {
  if (length(dat$messages)==0){ return(NULL) }
  text_ent <- dat$messages$text_entities
  msg_rows <- length(dat$messages$id)
  fwd_froms <- dat$messages$forwarded_from
  reply_to_peers <- dat$message$reply_to_peer_id
  reply_to_msgs <- dat$message$reply_to_message_id
  links <- unlist(lapply(text_ent, get_links))
  tg_links <- unlist(lapply(links, get_chan_links))
  tg_links_id <- unlist(lapply(tg_links, get_clean_links))
  tg_links_type <- unlist(lapply(tg_links, get_link_type))
  
  # if these columns don't exist in the json data, set to NA
  if (length(fwd_froms) == 0) {fwd_froms <- rep(NA, msg_rows)}
  if (length(reply_to_peers) == 0) {reply_to_peers <- rep(NA, msg_rows)}
  if (length(reply_to_msgs) == 0) {reply_to_msgs <- rep(NA, msg_rows)}
  
  # creating tibble (dataframe)
  return(
    tibble(
      data_src = rep('app', msg_rows),
      src_id = rep(dat$id, msg_rows),
      msg_id = dat$messages$id,
      uniq_id = paste0(src_id, "_", msg_id),
      src_name = rep(dat$name, msg_rows),
      src_type = rep(dat$type, msg_rows),
      src_url = rep(chan_url, msg_rows),
      auth_name = dat$messages$from,
      auth_name2 = rep(NA, msg_rows), # Add missing column with NA
      auth_id = sapply(dat$messages$from_id, auth_id_id),
      auth_id_type = sapply(dat$messages$from_id, auth_id_type),
      msg_url = unlist(mapply(gen_url, src_id, src_url, msg_id)),
      msg_type = dat$messages$type,
      msg_date_time = as.POSIXct(dat$messages$date, 
                                 format = "%Y-%m-%dT%H:%M:%S"),
      msg_date = as.Date(dat$messages$date, 
                         format = "%Y-%m-%d"),
      msg_text = get_plain_text(text_ent),
      msg_link_main = rep(NA, msg_rows), # Add missing column with NA
      msg_link_name = rep(NA, msg_rows), # Add missing column with NA
      msg_link_desc = rep(NA, msg_rows), # Add missing column with NA
      msg_links = links,
      msg_tg_links = tg_links,
      msg_tg_links_id = tg_links_id,
      msg_tg_links_type = tg_links_type,
      msg_mentions = get_mentions(text_ent),
      fwd_name = fwd_froms,
      fwd_name2 = rep(NA, msg_rows), # Add missing column with NA
      fwd_id = rep(NA, msg_rows), # Add missing column with NA
      fwd_date_time = as.POSIXct(rep(NA, msg_rows)), # class for consistency w/ api
      fwd_msg_id = rep(NA, msg_rows), # Add missing column with NA
      reply_to_peer = reply_to_peers,
      reply_to_msg = reply_to_msgs,
      views = rep(NA, msg_rows), # Add missing column with NA
      forwards = rep(NA, msg_rows), # Add missing column with NA
      replies = rep(NA, msg_rows), # Add missing column with NA
      file_path = local_path
      
      # Converting empty strings to NA  
    ) %>% mutate_if(is.character, ~na_if(., ""))
  )
}

# function to import files and save them
save_tg_tibble_app <- function(json_files, cores, out_path){
  
  if (length(json_files)==0){
    return(NULL)
  }
  
  json_file_names <- unlist(lapply(basename(json_files), 
                                   function(x) substr(x, 1, nchar(x)-5)))
  
  cat("1/3. reading frontend files\n")
  
  # Read and convert each JSON file to a data frame (in parallel)
  json_data_list <- pbmclapply(json_files, function(file) {
    jsonlite::fromJSON(file, simplifyVector = TRUE)
  }, mc.cores = cores)
  
  cat("2/3. preprocessing frontend data\n")
  
  # create tg tibbles (in parallel)
  chan_dfs <- pbmcmapply(create_tg_tibble_front, json_data_list, json_file_names,
                         json_files, SIMPLIFY = F, mc.cores = cores)
  
  cat("3/3. binding frontend data\n\n")
  
  # bind all tibbles into one and remove non-message content
  tg_tibble <- rbindlist(chan_dfs) %>% 
    as_tibble() %>%
    filter(msg_type=="message") %>% 
    select(-msg_type)
  
  if (out_path) {
    # save as RDS
    saveRDS(tg_tibble, out_path)
  } else {
    return(tg_tibble)
  }
}