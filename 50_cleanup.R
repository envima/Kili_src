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
outpath <- paste0("../data/", sub)
# set <- c("frst", "nofrst", "allplts")
set <- c("nofrst")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(outpath, "master_lst_ldr_", o, ".rds"))
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
  for (k in names(i$resp)){
    # print(k)
    i$resp[[k]][["sum_elev_pred_ldr_pred_resid"]] <- rowSums(cbind(i$resp[[k]]$elev_pred, i$resp[[k]]$ldr_pred_resid))
  }
  saveRDS(i, file = paste0(outpath, "master_lst_all_mods_", names(set_lst)[cnt], ".rds"))
})
