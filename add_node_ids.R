#add_ids
library(dplyr)

add_ids <- function(dat) {
  source_id = c()
  fwd_id = c()
  mention_ids = c()
  for (i in 1:nrow(dat)){
    if (grepl("\\+", dat$src_url[i])){
      source_id = c(source_id, dat$src_name[i])
    } else {
      source_id = c(source_id, paste0("@:",tolower(dat$src_url[i])))
    }
    
    if (is.na(dat$fwd_name[i])){
      fwd_id = c(fwd_id, NA)
    } else if (!is.na(dat$fwd_chan_url[i])){
      fwd_id = c(fwd_id, paste0("@:",tolower(dat$fwd_chan_url[i])))
    } else {
      fwd_id = c(fwd_id, dat$fwd_name[i])
    }
    
    if (is.na(dat$msg_mentions[i])){
      mention_ids = c(mention_ids, NA)
    } else {
      mentions = unlist(strsplit(dat$msg_mentions[i], ";"))
      men_id = paste0("@:", tolower(mentions))
      mention_ids = c(mention_ids, paste(na.omit(unique(men_id)), collapse=";"))
    }
    
  }
  dat$src_id2 = source_id
  dat$fwd_id = fwd_id
  dat$mention_ids = mention_ids
  
  dat = dat %>% group_by(src_id2) %>% mutate(src_ind = cur_group_id())
  
  too_small = function(x, g_id, thresh = 4){
    new_xs <- c()
    for (i in 1:length(x)){
      if (is.na(x[i])){
        new_xs <- c(new_xs, NA)
      } else {
        x_sp = unlist(strsplit(x[i], ";"))
        new_x_sps = c()
        for (j in 1:length(x_sp)){
          if (nchar(x_sp[j])<=4){
            new_x_sps <- c(new_x_sps, paste0("SRC",g_id[i],":",x_sp[j]))
          } else {
            new_x_sps <- c(new_x_sps, x_sp[j])
          }
        }
        new_xs <- c(new_xs, paste(new_x_sps, collapse=";"))
      }
    }
    return(new_xs)
  }
  
  dat$src_id2 <- too_small(dat$src_id2, dat$src_ind)
  dat$fwd_id <- too_small(dat$fwd_id, dat$src_ind)
  dat$mention_ids <- too_small(dat$mention_ids, dat$src_ind)
  dat$link_ids <- too_small(dat$link_ids, dat$src_ind)
  dat$orig_id <- ifelse(is.na(dat$fwd_id), dat$src_id2, fwd_id)
  #dat$ind <- 1:nrow(dat)
  
  return(dat)
  
}