# Language ID function for a vector of messages
library(cld2)
library(cld3)
library(fastText)
library(qdapRegex)

add_lang <- function(dat){
  
  test_lang <- function(vec, thresh1 = 0.02, thresh2 = 0.01, sample_size = 10000, min_char = 20){
    sample_vec <- vec[sample(length(vec), sample_size)]
    sample_vec_nonna <- sample_vec[!is.na(sample_vec)]
    sample_vec_f <- sample_vec_nonna[nchar(rm_non_words(sample_vec_nonna))>=20]
    sample_vec_ff <- sample_vec_f[!grepl("http", sample_vec_f)]
    sample_vec_f_langs <- unlist(lapply(sample_vec_ff, cld2::detect_language))
    sample_vec_f_langs2 <- unlist(lapply(sample_vec_ff, cld3::detect_language))
    svfl <- c(sample_vec_f_langs, sample_vec_f_langs2)
    sample_vec_table <- sort(table(svfl), decreasing=TRUE)
    tot_table <- sum(sample_vec_table)
    ratio <- sample_vec_table/tot_table
    sample_vec_table_f <- sample_vec_table[ratio >= thresh1]
    sample_vec_table_f2 <- sample_vec_table[ratio < thresh2]
    return(list(names(sample_vec_table_f), names(sample_vec_table_f2)))
  }
  
  bad_lang <- c("xx-Qaai", "bg-Latn", "el-Latn", "zh-Latn",
                "fy", "hi-Latn", "ceb", "mg", "ht", "xh",
                "lb", "jv", "mi", "af", "fil", "ru-Latn",
                "rw", "sn", "su", "zu", "eo", "ha", "haw", "jw",
                "ny", "sm", "xx-Runr", "sw", "ig", "ms", "so", "co",
                "yo", "st", "hmn", "xx-Ital", "ja-Latn", "sh", "gl",
                "ne", "tg", "mn", "zh", "ja", "mt", "la", "vi", "cy",
                "ku", "id", "fa", "ga", "uz", "ky", "az", "be", "ka", 
                "gd", "et", "lg", "is", "kk")
  
  proc_id <- function(x){
    if (is.na(x) | nchar(rm_non_words(x))<5) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  
  remove_tags <- function(x) {
    clean <- gsub("(\\s|^)[@#]\\S+|\\b(?:https?|www)://\\S+", "", x, perl = TRUE)
    return(clean)
  }
  
  lang_id_pt1 <- function(x){
    return(cld2::detect_language(x))
  }
  
  lang_id_pt2 <- function(x){
    return(cld3::detect_language(x))
  }
  
  lang_id_pt3 <- function(x){
    return(cld2::detect_language(rm_non_words(x)))
  }
  
  lang_id_pt4 <- function(x){
    return(cld3::detect_language(remove_tags(x)))
  }
  
  lang_id_pt5 <- function(x){
    return(cld3::detect_language(rm_non_words(x)))
  }
  
  lang_id_pt6 <- function(x){
    return(cld3::detect_language(rm_non_words(remove_tags(x))))
  }
  
  lang_id_s1 <- function(x, gl, bl){
    if (proc_id(x)){
      p1 <- lang_id_pt1(x)
      if (p1 %in% gl){
        return(p1)
      }
      p2 <- lang_id_pt2(x)
      if (p2 %in% gl){
        return(p2)
      }
      p3 <- lang_id_pt3(x)
      if (p3 %in% gl){
        return(p3)
      }
      p4 <- lang_id_pt4(x)
      if (p4 %in% gl){
        return(p4)
      }
      p5 <- lang_id_pt5(x)
      if (p5 %in% gl){
        return(p5)
      }
      p6 <- lang_id_pt6(x)
      if (p6 %in% gl){
        return(p6)
      }
      langs <- c(p1, p2, p3, p4, p5, p6)
      langs_bl <- langs[!(langs %in% bl)]
      tab <- sort(table(langs_bl), decreasing=TRUE)
      if (length(tab)>=1){
        return(names(tab)[1])
      } else {
        return("9999")
      }
    } else {
      return(NA)
    }
  }
  
  repl_small <- function(x, thresh = 0.05, repl_with = "9999"){
    x_tab <- sort(table(x), decreasing=TRUE)
    x_tab_small <- x_tab[x_tab/sum(x_tab) < thresh]
    xts_names <- names(x_tab_small)
    inds <- c()
    for (i in 1:length(x)){
      if (x[i] %in% xts_names){
        inds <- c(inds, i)
      }
    }
    y <- replace(x, inds, rep(repl_with,length(inds)))
    return(y)
  }
  
  file_pretrained = system.file("language_identification/lid.176.ftz", package = "fastText")
  
  # function for identifying the language of a post using fastText package
  fastText_id <- function(x, lid_path=file_pretrained) {
    id_vec <- fastText::language_identification(input_obj = x,
                                                pre_trained_language_model_path = lid_path,
                                                k = 1,
                                                th = 0.0,
                                                threads = 1,
                                                verbose = FALSE)
    return(id_vec[1,1])
  }
  
  lang_id_s2 <- function(x){
    inds <- c()
    for (i in 1:nrow(x)){
      if ((!is.na(x$lang[i])) & (x$lang[i]=="9999")){
        inds <- c(inds, i)
      }
    }
    x_f <- x$text[(!is.na(x$lang)) & (x$lang=="9999")]
    x_f_lang <- unlist(lapply(x_f, fastText_id))
    x$lang <- replace(x$lang, inds, x_f_lang)
    return(x)
  }
  
  langs <- test_lang(dat$text)
  good_lang <- langs[1]
  bad_lang <- unique(unlist(c(bad_lang, langs[2])))
  dat$lang <- unlist(lapply(dat$text, function(x) {return(lang_id_s1(x, gl=good_lang, bl=bad_lang))}))
  dat$lang <- repl_small(dat$lang)
  dat <- lang_id_s2(dat)
  dat$lang <- repl_small(dat$lang, thresh = 0.0005, repl_with = NA)
  
  return(dat)
}