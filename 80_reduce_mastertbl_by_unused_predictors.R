# Description: use table 65 to check which predictors are useful for any model that does not seem to be influenced 
# by the elevation problem. write out vector that can be used to restart the modelling with script 30 with only 
# those predictors.
# 
# Author: Alice Ziegler
# Date: 2019-01-24 14:14:59
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
library(plyr)
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "dez18_qa/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
#####
###where are the models and derived data
#####
set_dir <- "2018-12-13nofrst_frst_allplts/"
mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  file <- tryCatch(
    readRDS(file = paste0(modDir, "data/", "65_master_lst_rdc_by_elevprob_",o, ".rds")),
    error = function(e)file <- NA)
  return(file)
})
names(set_lst) <- set
set_lst <- set_lst[!is.na(set_lst)]

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

# pred_all <- lapply(set_lst, function(i){ #i <- set_lst[[1]]
#   vars <- lapply(i$varsel_ok, function(k){ #k <- i$varsel_ok$SRbats
#     pred <- do.call(rbind,k)$pred
#   })
#   vars_all <- unlist(vars)
# })
# preds_flt <- unlist(pred_all)
# preds_flt <- as.character(unique(preds_flt))
# saveRDS(preds_flt, file = paste0(inpath, set_dir, "80_preds_flt.rds"))

#####
###considering frq
#####
nmbr_mods <- 0
pred_all <- lapply(set_lst, function(i){ #i <- set_lst[[1]]
  vars <- lapply(i$varsel_ok, function(k){ #k <- i$varsel_ok$SRbats
    pred <- do.call(rbind,k)
  })
  nmbr_mods <<- nmbr_mods + length(vars)
  vars_all <- do.call(rbind, vars)
})
preds_flt <- do.call(rbind, pred_all)
preds_flt <- ddply(preds_flt,~pred, summarise,frq_all=sum(freq))

#figure out threshold
nmbr_mods_runs <- nmbr_mods * 5 #for each response that wasn't affected by the elevation problem 5 runs
min_nmbr <- floor(nmbr_mods_runs/10) #predictor has to be in a minimum of 10% of the models

preds_flt <- preds_flt[preds_flt$frq_all > min_nmbr,] # take only vars that were used in more than 10% of the models without elevprob
saveRDS(preds_flt, file = paste0(inpath, set_dir, "80_preds_flt.rds"))