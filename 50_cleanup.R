# Description:
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

#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "dez18/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"

#####
###where are the models and derived data
#####
set_dir <- "20181210_frst_nofrst_allplts/"
mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
# set <- c("frst", "nofrst", "allplts")
set <- c("frst")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  readRDS(file = paste0(modDir, "data/", "40_master_lst_ldr_",o, ".rds"))
})
names(set_lst) <- set

########################################################################################
###Settings
########################################################################################


########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################

cnt <- 0
set_lst_cln <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  cnt <<- cnt+1
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  for (k in names(i$resp)){
    # print(k)
    i$resp[[k]][["sum_elev_pred_ldr_pred_resid"]] <- rowSums(cbind(i$resp[[k]]$elev_pred, i$resp[[k]]$ldr_pred_resid))
  }
  if (file.exists(paste0(modDir, "data/"))==F){
    dir.create(file.path(paste0(modDir, "data/")), recursive = T)
  }
  saveRDS(i, file = paste0(modDir, "data/", "50_master_lst_all_mods_", names(set_lst)[cnt], ".rds"))
})
