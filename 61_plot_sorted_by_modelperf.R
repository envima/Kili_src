# Description: include 
# Author: Alice Ziegler
# Date: 2019-03-06 15:31:33
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
sub <- "feb19/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)

#####
###read files
#####
set_dir <- "2019-02-26frst_nofrst_allplts_noelev/"
mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  file <- tryCatch(
    readRDS(file = paste0(modDir, "data/", "50_master_lst_all_mods_",o, ".rds")),    
    error = function(e)file <- NA)
  return(file)
})
names(set_lst) <- set
set_lst <- set_lst[!is.na(set_lst)]

set_dir_elev <- "2019-02-15frst_nofrst_allplts_elev/"
mod_dir_lst_elev <- list.dirs(path = paste0(inpath, set_dir_elev), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

set_lst_elev <- lapply(set, function(o){
  set_moddir <- mod_dir_lst_elev[grepl(paste0("_", o, "_"), mod_dir_lst_elev)]
  modDir <- paste0(inpath, set_dir_elev, set_moddir, "/")
  file <- tryCatch(
    readRDS(file = paste0(modDir, "data/", "50_master_lst_all_mods_",o, ".rds")),    
    error = function(e)file <- NA)
  return(file)
})
names(set_lst_elev) <- set
set_lst_elev <- set_lst_elev[!is.na(set_lst_elev)]
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
##change 