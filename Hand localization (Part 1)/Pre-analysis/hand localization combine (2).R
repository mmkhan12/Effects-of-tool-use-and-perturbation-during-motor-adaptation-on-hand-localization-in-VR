setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processed/per_ppt")
getwd()
dir()

rm(list = ls()) # clean environment
library(data.table)
library(tidyverse)

to_load_dir_path <- "data/processed/per_ppt"
to_save_dir_path <- "../omnibus"

make_omnibus_raw_file <- function() {
  ppt_list <- list.dirs(recursive = FALSE)
  # make a list of length length(ppt_list) 
  trial_list <- vector("list", length(ppt_list))
  # loop through all directories in the to_load_dir_path
  for (i in 1:length(ppt_list)) {
    trial_list[[i]] <- make_one_omnibus_file(i, ppt_list)
  }
  
  
  
  # row bind all the trial_dfs
  omnibus_df <- do.call(rbind, trial_list)
  
  # save the omnibus_df
  fwrite(omnibus_df, file = paste(to_save_dir_path, "omnibus_raw.csv", sep = "/"))
}
make_one_omnibus_file <- function(directory_index, ppt_list) {
  
  exp_dir <- ppt_list[directory_index]
  
  # load in all trial_results.csv files
  trial_df <- fread(paste(exp_dir, "trial_results.csv", sep = "/"))
  
  #select out long strings
 
   # return the trial_df
  return(trial_df)
}

##### Do #####

make_omnibus_raw_file()  

print(directory_index)

