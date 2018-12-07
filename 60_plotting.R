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
  readRDS(file = paste0(outpath, "master_lst_all_mods_", o, ".rds"))
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
set_lst_val <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  cnt <<- cnt+1
  runs <- sort(unique(i$meta$run))
  for (k in names(i$resp)){
    val_df_all_lst <- lapply (runs, function(outs){
      plt_in <- i$meta$plotID[-which(i$meta$run == outs)]
      plt_out <- i$meta$plotID[which(i$meta$run == outs)]
      
      RMSE_elev_pred <- caret::RMSE(pred = i$resp[[k]]$elev_pred[i$meta$plotID %in% plt_out], obs = i$resp[[k]][[k]][i$meta$plotID %in% plt_out], na.rm = T)
      RMSE_ldr_pred_SR <- caret::RMSE(pred = i$resp[[k]][[paste0("ldr_pred_", k)]][i$meta$plotID %in% plt_out], obs = i$resp[[k]][[k]][i$meta$plotID %in% plt_out], na.rm = T)
      RMSE_ldr_pred_resid <- caret::RMSE(pred = i$resp[[k]]$ldr_pred_resid[i$meta$plotID %in% plt_out], obs = i$resp[[k]]$resid[i$meta$plotID %in% plt_out], na.rm = T)
      RMSE_sum_elev_pred_ldr_pred_resid <- caret::RMSE(pred = i$resp[[k]]$sum_elev_pred_ldr_pred_resid[i$meta$plotID %in% plt_out], obs = i$resp[[k]][[k]][i$meta$plotID %in% plt_out], na.rm = T)
      val_df <- data.frame(run = outs, 
                           RMSE_elev_pred = RMSE_elev_pred, 
                           RMSE_ldr_pred_SR = RMSE_ldr_pred_SR,
                           RMSE_ldr_pred_resid = RMSE_ldr_pred_resid,
                           RMSE_sum_elev_pred_ldr_pred_resid = RMSE_sum_elev_pred_ldr_pred_resid)
      
    })
    val_df_all <- do.call(rbind, val_df_all_lst)
    i$val[[k]] <- val_df_all ##<- hier muss soll eigentlich ein df oder lsite, oder so reingeschreiben werden.
  }
  return(i)
})
