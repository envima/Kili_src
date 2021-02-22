# Description: predict species richness with different models and write into list with original data $resp
# Author: Alice Ziegler
# Date: 2018-12-06 10:26:41
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
library(CAST)
library(doParallel)
library(foreach)
library(parallel)
source("000_setup.R")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(inpath, "20_master_lst_resid_", o, ".rds"))
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
set_lst_ldr <- lapply(set_lst, function(i){ # i <- set_lst[[1]]
  cnt <<- cnt+1
  if(grepl("cv_index", cv)){
    runs <- sort(unique(i$meta$cvindex_run))
  }else{
    runs <- seq(sum(grepl("outerrun", colnames(i$meta))))
  }
  
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  for(k in names(i$resp)){ # k <- "SRmammals" k  <- "SRpredator"
    # print(k)
    # for (k in names(i$resp)){
    for (outs in runs){ #outs <- 1
      # print(outs)
      #####
      ###split for outer loop (independet cv)
      #####
      if(grepl("cv_index", cv)){
        ###index-cv
        plt_in <- i$meta$plotID[-which(i$meta$cvindex_run == outs)]
        plt_out <- i$meta$plotID[which(i$meta$cvindex_run == outs)]
      }else{
        ###cv-x
        cv_nm <- colnames(i$meta)[grepl("outerrun", colnames(i$meta))][outs]
        plt_in <- i$meta$plotID[i$meta[cv_nm] == 0]
        plt_out <- i$meta$plotID[i$meta[cv_nm] == 1]
      }
      for (m in resp_set){ #m <- "lidarSR"
        # print(m)
        # if(length(unique(tbl_in[,m])) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
          #####
          ###create newdata dataframes
          #####
          new_dat <- i$meta[i$meta$plotID%in%plt_out,]
          #####
          ###actual model
          #####
          mod <- tryCatch(
            readRDS(file = paste0(modDir, "mod_run_", outs, "_", k, "_", m, ".rds")),
            error = function(e)mod <- NA)
          #####
          ###predict and write into new column
          #####
          if (!is.na(mod)){
            prdct <- predict(object = mod, newdata = new_dat)
            col_nm <- paste0("pred_", m) #column depending on model
            i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- prdct
            ncomp_nm <- paste0("ncomp_", m) #column name depending on model
            i$resp[[k]][[ncomp_nm]] <- mod$bestTune[[1]]
          }else{
            col_nm <- paste0("pred_", m)
            i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- NA
            ###vor?bergehend: 
            colnames(i$resp[[k]])[colnames(i$resp[[k]]) == "elev_pred"] <- "pred_elevSR"
            colnames(i$resp[[k]])[colnames(i$resp[[k]]) == "RES"] <- "calc_elevRES"
            ncomp_nm <- paste0("ncomp_", m) #column name depending on model
            i$resp[[k]][[ncomp_nm]] <- NA
            
          }
        }
      }
    }
  
# if (file.exists(paste0(modDir, "data/"))==F){
  if (file.exists(paste0(outpath, set_dir, set_moddir, "/"))==F){
    dir.create(file.path(paste0(outpath, set_dir, set_moddir, "/")), recursive = T)
  }
  

saveRDS(i, file = paste0(outpath, set_dir, set_moddir, "/", "40_master_lst_ldr_", names(set_lst)[cnt], ".rds"))
# readRDS(file = paste0(outpath, set_dir, set_moddir, "/", "40_master_lst_ldr_", names(set_lst)[cnt], ".rds"))
return(i)

})
names(set_lst_ldr) <- set 