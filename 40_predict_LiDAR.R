# Description:
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
  readRDS(file = paste0(outpath, "master_lst_resid_", o, ".rds"))
})
names(set_lst) <- set

########################################################################################
###Settings
########################################################################################
comm <- ""
method <- "pls"
type <- "ffs"
modDir <- "../data/dez18/2018-12-06_nofrst_ffs_pls_"

########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
cnt <- 0
set_lst_ldr <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  cnt <<- cnt+1
  runs <- sort(unique(i$meta$run))
  for(k in names(i$resp)){
    for (outs in runs){
      #####
      ###split for outer loop (independet cv)
      #####
      plt_in <- i$meta$plotID[-which(i$meta$run == outs)]
      plt_out <- i$meta$plotID[which(i$meta$run == outs)]
      tbl_in <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),]
      # tbl_out <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_out),]
      
      resp_set <- c(k, "resid") # loop model for SR and resid
      for (m in resp_set){
        if(length(unique(tbl_in[,m])) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
          #####
          ###create resp, pred and newdata dataframes
          #####
          resp <- tbl_in[!is.na(tbl_in[,m]),m] # take out NAs from resp so model can run
          preds <- i$meta[i$meta$plotID %in% plt_in & #take only plots from plt_in and take out NAs, take only scl columns
                            !(i$meta$plotID %in% tbl_in[is.na(tbl_in[,m]), "plotID"]),
                          grepl(pattern = "scl_", colnames(i$meta))]
          new_dat <- i$meta[i$meta$plotID %in% plt_out, grepl(pattern = "scl_", colnames(i$meta))]
          #####
          ###read actual model
          #####
          mod <- tryCatch(
          readRDS(file = paste0(modDir, "/mod_run_", outs, "_", k, "_", m, ".rds")),
          error = function(e)mod <- NA)
          #####
          ###predict and write into new column
          #####
          if (!is.na(mod)){
          prdct <- predict(object = mod, newdata = new_dat)
          col_nm <- paste0("ldr_pred_", m) #column depending on sr or resid
          i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- prdct
          }else{
            col_nm <- paste0("ldr_pred_", m)
            i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- NA
          }
        }else{ # if only one value in tbl_in: modeling isn't possible ==> NA in prediction
          col_nm <- paste0("ldr_pred_", m)
          i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- NA
        } 
      }
    }
  }
  saveRDS(i, file = paste0(outpath, "master_lst_ldr_", names(set_lst)[cnt], ".rds"))
  # readRDS(file = paste0(outpath, "master_lst_ldr_", names(set_lst)[cnt], ".rds"))
  return(i)
})
names(set_lst_ldr) <- set