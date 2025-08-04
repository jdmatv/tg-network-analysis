library(dplyr)

most_common = function(x){
  counts <- table(x, useNA = "ifany")
  
  # Find the most common value excluding NAs
  return(names(counts)[which.max(counts)][1])
}

get_chan <- function(pub, src_url){
  pub_nonna <- pub[!is.na(pub)]
  if (length(pub_nonna) >= 1){
    pubs <- src_url[(!is.na(pub)) & (pub)]
    privs <- src_url[(!is.na(pub)) & (!pub)]
    if (length(pubs) >= length(privs)){
      return(most_common(pubs))
    } else {
      return(most_common(privs))
    }
  } else {
    return(NA)
  }
}

get_chan_nm <- function(pub, ch_name){
  pub_nonna <- pub[!is.na(pub)]
  if (length(pub_nonna) >= 1){
    pubs <- ch_name[(!is.na(pub)) & (pub)]
    privs <- ch_name[(!is.na(pub)) & (!pub)]
    if (length(pubs) >= length(privs)){
      return(most_common(pubs))
    } else {
      return(most_common(privs))
    }
  } else {
    return(NA)
  }
}

get_pubs <- function(pub){
  pub_nonna <- pub[!is.na(pub)]
  if (length(pub_nonna) >= 1){
    pubs <- pub[(!is.na(pub)) & (pub)]
    privs <- pub[(!is.na(pub)) & (!pub)]
    if (length(pubs) >= length(privs)){
      return(most_common(pubs))
    } else {
      return(most_common(privs))
    }
  } else {
    return(NA)
  }
}

# split by seed

node_id_by_seed <- split(nodes_id, nodes_id$seed_url)

node_id_dic <- list()
for (i in 1:length(node_id_by_seed)){
  dat <- node_id_by_seed[[i]]
  node_id_dic[[i]] <- dat %>% group_by(node_id) %>% summarise(chan_url = get_chan(public, chan_url),
                                                              chan_name = get_chan_nm(public, chan_name),
                                                              public = as.logical(get_pubs(public)),
                                                              seed_url = seed_url[1])
}

node_id_dic <- do.call(rbind, node_id_dic)


gen_node_id_dic <- nodes_id %>% group_by(node_id) %>% summarise(chan_url = get_chan(public, chan_url),
                                                                chan_name = get_chan_nm(public, chan_name),
                                                                public = as.logical(get_pubs(public)),
                                                                seed_url = "general_seed")

node_dic <- rbind(node_id_dic, gen_node_id_dic)