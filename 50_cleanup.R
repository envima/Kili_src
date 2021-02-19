# Description: cleanup dataset
# get rid of deprecated more general trophic levels that are not based on species level.
# Author: Alice Ziegler
# Date: 2018-12-06 13:46:07
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
source("000_setup.R")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  file <- tryCatch(
  readRDS(file = paste0(modDir, "data/", "40_master_lst_ldr_",o, ".rds")),    
  error = function(e)file <- NA)
  return(file)
})
names(set_lst) <- set
set_lst <- set_lst[!is.na(set_lst)]

########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################

cnt <- 0
set_lst_cln <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  ##get rid of old sum_generalists... 
  i$resp <- i$resp[names(i$resp)[!(grepl("sum_", names(i$resp)))]]
  
  cnt <<- cnt+1
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  for (k in names(i$resp)){ #k <- "SRmammals
    # print(k)
    i$resp[[k]][["pred_sumSR"]] <- rowSums(cbind(i$resp[[k]]$pred_elevSR, i$resp[[k]]$pred_lidarRES))
  }
  if (file.exists(paste0(modDir, "data/"))==F){
    dir.create(file.path(paste0(modDir, "data/")), recursive = T)
  }
  saveRDS(i, file = paste0(modDir, "data/", "50_master_lst_all_mods_", names(set_lst)[cnt], ".rds"))
})
