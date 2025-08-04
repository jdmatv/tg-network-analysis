library(dplyr)

add_link_id <- function(link, link_type){
  if (is.na(link)){
    return(NA)
  } else {
    link_sp <- strsplit(link, ";")
    link_type_sp <- strsplit(link_type, ";")
    
    link_ids <- c()
    for (i in 1:length(link_sp)){
      if (link_type_sp[i] %in% c("pubchangroup", "pubchangroup_web")){
        link_ids <- c(link_ids, paste0("@:",tolower(link_sp[i])))
      }
    }
    
    if (length(link_ids)>0){
      return(paste(unique(link_ids), collapse=";"))
    } else {
      return(NA)
    }
  }
}

dat <- tg_dat_enhanced
dat$link_ids <- unlist(mapply(add_link_id, dat$msg_tg_links_id,
                              dat$msg_tg_links_type))
