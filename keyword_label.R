# unclassified
# file: keyword_label.R
# date: feb 17
# description: adding labels for each keyword tg posts

# inputs: tibble with grep patterns, and tibble with post and id columns
# outputs: matrix with post id and keyword values

# note: re-write this to exclude URL and have as separate field

# requires
library(dplyr)
library(parallel)
library(Matrix)

posts_to_keys <- function(in_path, out_path, num_cores = 5, platform = "TG"){
  
  # INPUTS
  
  # paths to input data
  post_file_path <- in_path
  query_file_path <- "IN_PATH.rds"
  
  # path to outputs
  log_dir_path <- "LOG_PATH"
  output_file_path <- out_path
  
  # END USER INPUT
  
  # create tictoc functions
  tic <- function(text) {
    assign(".my_tic", proc.time(), envir = .GlobalEnv)
    cat(text)
  }
  
  toc <- function(text) {
    toc_time <- proc.time() - get(".my_tic", envir = .GlobalEnv)
    if (toc_time[3] < 200) {
      cat(text, "- time elapsed: ", format(toc_time[3], digits = 3), " seconds\n")
    } else {
      cat(text, "- time elapsed: ", format(toc_time[3]/60, digits = 3), " minutes\n")
    }
  }
  
  # Create log file
  log_file_path <- sprintf("%s/log_file_%s.txt",
                           log_dir_path,
                           format(Sys.time(), '%y%m%d-%H%M%S'))
  log_file <- file(log_file_path, open="wt")
  sink(log_file, append = TRUE)
  cat("Starting keyword labeling at", date(), "\n\n")
  
  # Read input data
  tic("Reading input data\n")
  posts <- readRDS(post_file_path)
  queries <- readRDS(query_file_path)
  toc("Loaded input data")
  
  # running queries on each post
  tic(sprintf("\nRunning queries on each post: %s total\n", nrow(posts)))
  query_list = queries$pattern
  
  # function for creating post labels
  label_post <- function(post, ind, src_ind, msg_id, orig_ind, patterns = query_list){
    results <- c(ind, src_ind, msg_id, orig_ind)
    for (i in seq_along(patterns)){
      if (grepl(patterns[i], post, perl = TRUE)) {
        results <- c(results, 1)
      } else {
        results <- c(results, 0)
      }
    }
    if (ind %% 10000 == 0) {
      cat(ind,"\n")
    }
    return(results)
  }
  
  # getting names for query labels
  query_names <- c("ind", "src_ind", "msg_id", "orig_ind", queries$name)
  names(query_names) <- NULL
  
  
  if (platform=="TG") {
    # creating label matrix using label_post function
    label_matrix <- mcmapply(label_post,
                             posts$msg_text, seq(nrow(posts)),
                             posts$src_ind, posts$msg_id, posts$orig_ind,
                             USE.NAMES = FALSE, mc.cores=num_cores)
  } else if (platform=="TGF"){
    label_matrix <- mcmapply(label_post,
                             posts$msg_text, seq(nrow(posts)),
                             posts$uniq_id, # FIX
                             USE.NAMES = FALSE, mc.cores=num_cores)
  }
  
  toc("\n\nDone labeling posts")
  
  # converting to sparse matrix
  label_matrix <- t(label_matrix)
  label_matrix <- as(label_matrix, "sparseMatrix")
  
  # saving column names
  saveRDS(query_names, paste0(output_file_path, "_cols.rds"))
  
  # saving label matrix
  saveRDS(label_matrix, paste0(output_file_path, ".rds"))
  
  cat("Completed keyword labeling at", date())
}