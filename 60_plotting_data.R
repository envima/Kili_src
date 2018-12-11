# Description:
# Author: Alice Ziegler
# Date: 2018-12-07 14:45:46
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
library(CAST)
library(caret)
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
  readRDS(file = paste0(outpath, "50_master_lst_all_mods_", o, ".rds"))
})
names(set_lst) <- set

########################################################################################
###Settings
########################################################################################
modDir <- "../data/dez18/2018-12-06_nofrst_ffs_pls_"
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
########################################################################################
###validation data
########################################################################################
cnt <- 0
set_lst_val <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  cnt <<- cnt+1
  runs <- sort(unique(i$meta$run))
  for (k in names(i$resp)){
    val_df_all_lst <- lapply (runs, function(outs){
      plt_in <- i$meta$plotID[-which(i$meta$run == outs)]
      plt_out <- i$meta$plotID[which(i$meta$run == outs)]
      #####
      ###RMSE
      #####
      RMSE_elev_pred <- caret::RMSE(pred = i$resp[[k]]$elev_pred[i$meta$plotID %in% plt_out], obs = i$resp[[k]][[k]][i$meta$plotID %in% plt_out], na.rm = T)
      RMSE_ldr_pred_SR <- caret::RMSE(pred = i$resp[[k]][[paste0("ldr_pred_", k)]][i$meta$plotID %in% plt_out], obs = i$resp[[k]][[k]][i$meta$plotID %in% plt_out], na.rm = T)
      RMSE_ldr_pred_resid <- caret::RMSE(pred = i$resp[[k]]$ldr_pred_resid[i$meta$plotID %in% plt_out], obs = i$resp[[k]]$resid[i$meta$plotID %in% plt_out], na.rm = T)
      RMSE_sum_elev_pred_ldr_pred_resid <- caret::RMSE(pred = i$resp[[k]]$sum_elev_pred_ldr_pred_resid[i$meta$plotID %in% plt_out], obs = i$resp[[k]][[k]][i$meta$plotID %in% plt_out], na.rm = T)
      #####
      ###RMSE/sd
      #####
      RMSEsd_elev_pred <- RMSE_elev_pred/sd(i$resp[[k]][[k]])
      RMSEsd_ldr_pred_SR<- RMSE_ldr_pred_SR/sd(i$resp[[k]][[k]])
      RMSEsd_ldr_pred_resid <- RMSE_ldr_pred_resid/sd(i$resp[[k]]$resid)
      RMSEsd_sum_elev_pred_ldr_pred_resid <- RMSE_sum_elev_pred_ldr_pred_resid/sd(i$resp[[k]][[k]])
      #####
      ###new list element with validation
      #####
      val_df <- data.frame(run = outs, 
                           RMSE_elev_pred = RMSE_elev_pred, 
                           RMSE_ldr_pred_SR = RMSE_ldr_pred_SR,
                           RMSE_ldr_pred_resid = RMSE_ldr_pred_resid,
                           RMSE_sum_elev_pred_ldr_pred_resid = RMSE_sum_elev_pred_ldr_pred_resid, 
                           RMSEsd_elev_pred = RMSEsd_elev_pred, 
                           RMSEsd_ldr_pred_SR = RMSEsd_ldr_pred_SR,
                           RMSEsd_ldr_pred_resid = RMSEsd_ldr_pred_resid,
                           RMSEsd_sum_elev_pred_ldr_pred_resid = RMSEsd_sum_elev_pred_ldr_pred_resid)
      
    })
    val_df_all <- do.call(rbind, val_df_all_lst)
    i$val[[k]] <- val_df_all ##<- hier muss soll eigentlich ein df oder lsite, oder so reingeschreiben werden.
  }
  saveRDS(i, file = paste0(outpath, "60_master_lst_val_", names(set_lst)[cnt], ".rds"))
  return(i)
})

########################################################################################
###var Imp
########################################################################################
cnt <- 0
set_lst_var_imp <- lapply(set_lst, function(i){# i <- set_lst[[1]]
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
          ###read actual model
          #####
          mod <- tryCatch(
            readRDS(file = paste0(modDir, "/mod_run_", outs, "_", k, "_", m, ".rds")),
            error = function(e)mod <- NA)
          mod <- tryCatch(
          var_imp <- data.frame(sel_vars = mod$selectedvars, 
                               varimp = varImp(mod)$importance), 
          error = function(e)var_imp <- NA)
        }
        i$varimp[[k]][[m]][[outs]] <- var_imp
      }
      
    }
    
  }
  })
