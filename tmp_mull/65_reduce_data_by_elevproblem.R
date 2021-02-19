# Description:
# Author: Alice Ziegler
# Date: 2018-12-10 11:49:55
# to do: colnames(df) <- sicherer gegen umsortieren machen
# bei selvar plot legende eindeutig welche farbe welche zahl ist. (nicht tick im übergang)
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
sub <- "feb19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
set_dir <- "2019-02-15frst_nofrst_allplts_elev/"
mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  file <- tryCatch(
    readRDS(file = paste0(modDir, "data/", "60_master_lst_varimp_",o, ".rds")), 
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
for (i in set_lst){# i <- set_lst[[1]]
  cnt <<- cnt+1
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  if (length(set_moddir) > 0){
    modDir <- paste0(inpath, set_dir, set_moddir, "/")
    resp_ok <- c()
    # j <- "SRmagnoliids"
    # j <- "SRmammals"
    # j <- "SRbirds"
    for(j in names(i$resp)){
      print(j)
      if(sum(i$val[[j]]$RMSEsd_elev_pred, na.rm = T) !=0 &
         sum(i$val[[j]]$RMSEsd_ldr_pred_SR, na.rm = T) !=0 &
         sum(i$val[[j]]$RMSEsd_sum_elev_pred_ldr_pred_resid, na.rm = T)){
        if (median(i$val[[j]]$RMSEsd_elev_pred, na.rm = T) > 
            median(i$val[[j]]$RMSEsd_ldr_pred_SR, na.rm = T)){
          resp_ok <- c(resp_ok, j)
      }}
      
    }# j respnames
    i$val_ok <- i$val[names(i$val) %in% resp_ok]
    i$varimp_ok <- i$varimp[names(i$varimp) %in% resp_ok]
    i$varsel_ok <- i$varsel[names(i$varsel) %in% resp_ok]
    i$resp <- i$resp[names(i$resp) %in% resp_ok]
    
    if (file.exists(paste0(modDir, "data/"))==F){
      dir.create(file.path(paste0(modDir, "data/")), recursive = T)
    }
    saveRDS(i, file = paste0(modDir, "data/", "65_master_lst_rdc_by_elevprob_", names(set_lst)[cnt], ".rds"))
  }
  }#i 
