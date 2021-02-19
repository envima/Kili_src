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

set_dir_SR <- "2019-03-06frst_nofrst_allplts_noelev/"
mod_dir_lst_SR <- list.dirs(path = paste0(inpath, set_dir_SR), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

set_lst_SR <- lapply(set, function(o){
  set_moddir <- mod_dir_lst_SR[grepl(paste0("_", o, "_"), mod_dir_lst_SR)]
  modDir <- paste0(inpath, set_dir_SR, set_moddir, "/")
  file <- tryCatch(
    readRDS(file = paste0(modDir, "data/", "50_master_lst_all_mods_",o, ".rds")),    
    error = function(e)file <- NA)
  return(file)
})
names(set_lst_SR) <- set
set_lst_SR <- set_lst_SR[!is.na(set_lst_SR)]

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
# cv <- "cv_index"
cv <- "cv_20"
# cv <- "cv_50"

########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
cnt <- 0
set_lst_val <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  cnt <<- cnt + 1
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  if (length(set_moddir) > 0){
    modDir <- paste0(inpath, set_dir, set_moddir, "/")
    if(grepl("cv_index", cv)){
      runs <- sort(unique(i$meta$cvindex_run))
    }else{
      runs <- seq(sum(grepl("outerrun", colnames(i$meta))))
    }
  for (k in names(i$resp)){ # k<- "SRmammals", k <- "SRdungbeetles" k <- "SRbirds" k <- "SRrosids"
    #####
    ###hier werden spalten aus verschiedenen Modellen zusammengefügt
    #####
    i$resp[[k]]$ldr_pred_SR_elev <- set_lst_elev[[names(set_lst)[cnt]]]$resp[[k]]$ldr_pred_SR
    i$resp[[k]]$ldr_pred_SR <- set_lst_SR[[names(set_lst)[cnt]]]$resp[[k]]$ldr_pred_SR
    i$resp[[k]]$ldr_pred_resid <- set_lst_elev[[names(set_lst)[cnt]]]$resp[[k]]$ldr_pred_resid # bei sauberer Version müsste hier die nicht elev Variante verwendet werden. (ist exakt gleich, weil höhe in Modellen nicht verwendet wird)
    i$resp[[k]]$sum_elev_pred_ldr_pred_resid <- set_lst_elev[[names(set_lst)[cnt]]]$resp[[k]]$sum_elev_pred_ldr_pred_resid # bei sauberer Version müsste hier die nicht elev Variante verwendet werden. (ist exakt gleich, weil höhe in Modellen nicht verwendet wird)
    
    #####
    ###end:spalten sind zusammengefügt
    #####
        val_df_all_lst <- lapply (runs, function(outs){ #outs <- 1
          #####
          ###out rows for thisrun (has to be chosen first, as depending on cv20/cv-index, 
          ###the rows are chosen by one column, or by several.)
          #####
          if(grepl("cv_index", cv)){
            outrows <- which(i$meta$cvindex_run == outs)
          }else{
            cv_nm <- colnames(i$meta)[grepl("outerrun", colnames(i$meta))][outs]
            outrows <- which(i$meta[cv_nm] == 1)
          }
          #####
          ###RMSE
          #####
          RMSE_elev_pred <- caret::RMSE(pred = i$resp[[k]]$elev_pred[outrows], 
                                        obs = i$resp[[k]]$SR[outrows], na.rm = T)
          RMSE_ldr_pred_SR <- caret::RMSE(pred = i$resp[[k]]$ldr_pred_SR[outrows], 
                                          obs = i$resp[[k]]$SR[outrows], na.rm = T)
          RMSE_ldr_pred_resid <- caret::RMSE(pred = i$resp[[k]]$ldr_pred_resid[outrows], 
                                             obs = i$resp[[k]]$resid[outrows], na.rm = T)
          RMSE_sum_elev_pred_ldr_pred_resid <- caret::RMSE(pred = i$resp[[k]]$sum_elev_pred_ldr_pred_resid[outrows], 
                                                           obs = i$resp[[k]]$SR[outrows], na.rm = T)
          RMSE_ldr_pred_SR_elev <- caret::RMSE(pred = i$resp[[k]]$ldr_pred_SR_elev[outrows], 
                                               obs = i$resp[[k]]$SR[outrows], na.rm = T)
          #####
          ###RMSE/sd
          #####
          sd <- sd(i$resp[[k]]$SR, na.rm = T)
          RMSEsd_elev_pred <- RMSE_elev_pred/sd
          RMSEsd_ldr_pred_SR <- RMSE_ldr_pred_SR/sd
          RMSEsd_ldr_pred_resid <- RMSE_ldr_pred_resid/sd(i$resp[[k]]$resid, na.rm = T)
          RMSEsd_sum_elev_pred_ldr_pred_resid <- RMSE_sum_elev_pred_ldr_pred_resid/sd
          RMSEsd_ldr_pred_SR_elev <- RMSE_ldr_pred_SR_elev/sd
          
          #####
          ###RMSE/median
          #####
          mdn <- median(i$resp[[k]]$SR, na.rm = T)
          RMSEmdn_elev_pred <- RMSE_elev_pred/mdn
          RMSEmdn_ldr_pred_SR<- RMSE_ldr_pred_SR/mdn
          RMSEmdn_ldr_pred_resid <- RMSE_ldr_pred_resid/median(i$resp[[k]]$resid, na.rm = T)
          RMSEmdn_sum_elev_pred_ldr_pred_resid <- RMSE_sum_elev_pred_ldr_pred_resid/mdn
          RMSEmdn_ldr_pred_SR_elev <- RMSE_ldr_pred_SR_elev/mdn
          
          #####
          ###new list element with validation
          #####
          val_df <- data.frame(run = outs, 
                               RMSE_elev_pred = RMSE_elev_pred, 
                               RMSE_ldr_pred_SR = RMSE_ldr_pred_SR,
                               RMSE_ldr_pred_resid = RMSE_ldr_pred_resid,
                               RMSE_sum_elev_pred_ldr_pred_resid = RMSE_sum_elev_pred_ldr_pred_resid, 
                               RMSE_ldr_pred_SR_elev = RMSE_ldr_pred_SR_elev, 
                               RMSEsd_elev_pred = RMSEsd_elev_pred, 
                               RMSEsd_ldr_pred_SR = RMSEsd_ldr_pred_SR,
                               RMSEsd_ldr_pred_resid = RMSEsd_ldr_pred_resid,
                               RMSEsd_sum_elev_pred_ldr_pred_resid = RMSEsd_sum_elev_pred_ldr_pred_resid, 
                               RMSEsd_ldr_pred_SR_elev = RMSEsd_ldr_pred_SR_elev, 
                               sd = sd, 
                               RMSEmdn_elev_pred = RMSEmdn_elev_pred, 
                               RMSEmdn_ldr_pred_SR = RMSEmdn_ldr_pred_SR, 
                               RMSEmdn_ldr_pred_resid = RMSEmdn_ldr_pred_resid, 
                               RMSEmdn_sum_elev_pred_ldr_pred_resid = RMSEmdn_sum_elev_pred_ldr_pred_resid,
                               RMSEmdn_ldr_pred_SR_elev = RMSEmdn_ldr_pred_SR_elev, 
                               mdn = mdn)
          
        })
        val_df_all <- do.call(rbind, val_df_all_lst)
        #####
        ###add columns with statistical information about the RMSE and RMSEsd errors
        #####
        for (p in colnames(val_df_all)[!colnames(val_df_all) %in% c("run", "sd", "mdn")]){ #p <- "RMSEsd_ldr_pred_SR"
          val_df_all$sd_tmp <- sd(val_df_all[[p]])
          colnames(val_df_all)[colnames(val_df_all) == "sd_tmp"] <- paste0(p, "_sd")
          val_df_all$mdn_tmp <- median(val_df_all[[p]])
          colnames(val_df_all)[colnames(val_df_all) == "mdn_tmp"] <- paste0(p, "_mdn")
          qntls <- quantile(val_df_all[[p]], probs = c(0.25, 0.75), na.rm = T)
          val_df_all$q25_tmp <- qntls[[1]]
          val_df_all$q75_tmp <- qntls[[2]]
          val_df_all$IQR_tmp <- val_df_all$q75_tmp - val_df_all$q25_tmp
          colnames(val_df_all)[colnames(val_df_all) == "q25_tmp"] <- paste0(p, "_q25")
          colnames(val_df_all)[colnames(val_df_all) == "q75_tmp"] <- paste0(p, "_q75")
          colnames(val_df_all)[colnames(val_df_all) == "IQR_tmp"] <- paste0(p, "_IQR")
          }
        val_df_all$RMSE_IQR_min <- min(val_df_all[,grepl(pattern = "q25", colnames(val_df_all))& 
                                                    grepl(pattern = "RMSE_", colnames(val_df_all))])
        val_df_all$RMSE_IQR_max <- max(val_df_all[,grepl(pattern = "q75", colnames(val_df_all)) & 
                                                   grepl(pattern = "RMSE_", colnames(val_df_all))])
        val_df_all$RMSEsd_IQR_min <- min(val_df_all[,grepl(pattern = "q25", colnames(val_df_all))& 
                                                    grepl(pattern = "RMSEsd_", colnames(val_df_all))])
        val_df_all$RMSEsd_IQR_max <- max(val_df_all[,grepl(pattern = "q75", colnames(val_df_all)) & 
                                                    grepl(pattern = "RMSEsd_", colnames(val_df_all))])
        val_df_all$RMSEmdn_IQR_min <- min(val_df_all[,grepl(pattern = "q25", colnames(val_df_all))& 
                                                      grepl(pattern = "RMSEmdn_", colnames(val_df_all))])
        val_df_all$RMSEmdn_IQR_max <- max(val_df_all[,grepl(pattern = "q75", colnames(val_df_all)) & 
                                                      grepl(pattern = "RMSEmdn_", colnames(val_df_all))])
        #####
        ###add szenario information, which model performances resemble the ranking of other respnses between each other
        #####
        ###rank by median - RMSEsd
        # mod_df_mdn_tmp <- val_df_all[,grepl(pattern = "mdn", colnames(val_df_all)) & 
        #              grepl(pattern = "RMSEsd_", colnames(val_df_all))]
        mod_df_mdn_tmp <- val_df_all[,c("RMSEsd_elev_pred_mdn", 
                                        "RMSEsd_ldr_pred_SR_mdn", 
                                        "RMSEsd_sum_elev_pred_ldr_pred_resid_mdn")]
        mod_df_mdn_tmp_t <- t(mod_df_mdn_tmp[!duplicated(mod_df_mdn_tmp),])
        mod_df_mdn_tmp_srt <- mod_df_mdn_tmp_t[order(mod_df_mdn_tmp_t[,1]),]
        val_df_all$RMSEsd_elev_pred_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_elev_pred_mdn")
        val_df_all$RMSEsd_sum_elev_pred_ldr_pred_resid_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_sum_elev_pred_ldr_pred_resid_mdn")
        val_df_all$RMSEsd_ldr_pred_SR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_ldr_pred_SR_mdn")
        # val_df_all$RMSEsd_ldr_pred_SR_elev_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_ldr_pred_SR_elev_mdn")
        # val_df_all$RMSEsd_ldr_pred_resid_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_ldr_pred_resid_mdn")
        
        ###rank by median - RMSEmdn
        mod_df_mdn_tmp <- val_df_all[,grepl(pattern = "_mdn$", colnames(val_df_all)) & 
                                       grepl(pattern = "RMSEmdn_", colnames(val_df_all))]
        mod_df_mdn_tmp_t <- t(mod_df_mdn_tmp[!duplicated(mod_df_mdn_tmp),])
        mod_df_mdn_tmp_srt <- mod_df_mdn_tmp_t[order(mod_df_mdn_tmp_t[,1]),]
        val_df_all$RMSEmdn_elev_pred_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_elev_pred_mdn")
        val_df_all$RMSEmdn_sum_elev_pred_ldr_pred_resid_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_sum_elev_pred_ldr_pred_resid_mdn")
        val_df_all$RMSEmdn_ldr_pred_SR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_ldr_pred_SR_mdn")
        val_df_all$RMSEmdn_ldr_pred_SR_elev_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_ldr_pred_SR_elev_mdn")
        val_df_all$RMSEmdn_ldr_pred_resid_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_ldr_pred_resid_mdn")
        #####
        ###column with different model constellations
        #####
        #best model by mdn RMSEsd
        val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_elev_pred_mdn_rank == 1] <- 1
        val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_sum_elev_pred_ldr_pred_resid_mdn_rank == 1] <- 2
        val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_ldr_pred_SR_mdn_rank == 1] <- 3
        # val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_ldr_pred_SR_elev_mdn_rank == 1] <- 4
        # val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_ldr_pred_resid_mdn_rank == 1] <- 5
        #best model by median RMSemdn
        val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_elev_pred_mdn_rank == 1] <- 1
        val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_sum_elev_pred_ldr_pred_resid_mdn_rank == 1] <- 2
        val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_ldr_pred_SR_mdn_rank == 1] <- 3
        val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_ldr_pred_SR_elev_mdn_rank == 1] <- 4
        val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_ldr_pred_resid_mdn_rank == 1] <- 5
        ###second best, ... could follow
        ###rank by IQR - RMSEsd
        mod_df_IQR_tmp <- val_df_all[,grepl(pattern = "IQR$", colnames(val_df_all)) & 
                                       grepl(pattern = "RMSEsd_", colnames(val_df_all))]
        mod_df_IQR_tmp_t <- t(mod_df_IQR_tmp[!duplicated(mod_df_IQR_tmp),])
        mod_df_IQR_tmp_srt <- mod_df_IQR_tmp_t[order(mod_df_IQR_tmp_t[,1]),]
        val_df_all$RMSEsd_elev_pred_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_elev_pred_IQR")
        val_df_all$RMSEsd_sum_elev_pred_ldr_pred_resid_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_sum_elev_pred_ldr_pred_resid_IQR")
        val_df_all$RMSEsd_ldr_pred_SR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_ldr_pred_SR_IQR")
        val_df_all$RMSEsd_ldr_pred_SR_elev_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_ldr_pred_SR_elev_IQR")
        val_df_all$RMSEsd_ldr_pred_resid_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_ldr_pred_resid_IQR")
          
        ###rank by IQR - RMSEmdn
        mod_df_IQR_tmp <- val_df_all[,grepl(pattern = "IQR$", colnames(val_df_all)) & 
                                       grepl(pattern = "RMSEmdn_", colnames(val_df_all))]
        mod_df_IQR_tmp_t <- t(mod_df_IQR_tmp[!duplicated(mod_df_IQR_tmp),])
        mod_df_IQR_tmp_srt <- mod_df_IQR_tmp_t[order(mod_df_IQR_tmp_t[,1]),]
        val_df_all$RMSEmdn_elev_pred_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_elev_pred_IQR")
        val_df_all$RMSEmdn_sum_elev_pred_ldr_pred_resid_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_sum_elev_pred_ldr_pred_resid_IQR")
        val_df_all$RMSEmdn_ldr_pred_SR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_ldr_pred_SR_IQR")
        val_df_all$RMSEmdn_ldr_pred_SR_elev_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_ldr_pred_SR_elev_IQR")
        val_df_all$RMSEmdn_ldr_pred_resid_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_ldr_pred_resid_IQR")
        
        
        i$val[[k]] <- val_df_all 
      }
      if (file.exists(paste0(modDir, "data/"))==F){
        dir.create(file.path(paste0(modDir, "data/")), recursive = T)
      }
      saveRDS(i, file = paste0(modDir, "data/", "61_master_lst_val_elevANDnoelev_", names(set_lst)[cnt], ".rds"))
      return(i)
  }
    })
    

# ###noch ncith angepasst für das eingefügte modell mit elevation. model file noch nicht eingelesen!
#     ########################################################################################
#     ###var Imp
#     ########################################################################################
#     cnt <- 0
#     set_lst_var_imp <- lapply(set_lst_val, function(i){# i <- set_lst[[1]]
#       cnt <<- cnt+1
#       set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
#       modDir <- paste0(inpath, set_dir, set_moddir, "/")
#       if(grepl("cv_index", cv)){
#         runs <- sort(unique(i$meta$cvindex_run))
#       }else{
#         runs <- seq(sum(grepl("outerrun", colnames(i$meta))))
#       }
#       for(k in names(i$resp)){ # k <- "SRasterids"
#         # print(k)
#         for (outs in runs){
#           # print(outs)
#           #####
#           ###split for outer loop (independet cv)
#           ###and inner index selection for model
#           #####
#           if(grepl("cv_index", cv)){
#             ###index-cv
#             plt_in <- i$meta$plotID[-which(i$meta$cvindex_run == outs)]
#             plt_out <- i$meta$plotID[which(i$meta$cvindex_run == outs)]
#             tbl_in <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),]
# 
#           }else{
#             ###cv-x
#             cv_nm <- colnames(i$meta)[grepl("outerrun", colnames(i$meta))][outs]
#             plt_in <- i$meta$plotID[i$meta[cv_nm] == 0]
#             plt_out <- i$meta$plotID[i$meta[cv_nm] == 1]
#             tbl_in <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),]
#           }
# 
#           resp_set <- c("SR", "resid") # loop model for SR and resid # m <- "SR"
#           for (m in resp_set){
#             # print(m)
#             if(length(unique(tbl_in[,m])) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
#               #####
#               ###read actual model
#               #####
#               mod <- tryCatch(
#                 readRDS(file = paste0(modDir, "mod_run_", outs, "_", k, "_", m, ".rds")),
#                 error = function(e)mod <- NA)
#               var_imp <- tryCatch(
#                 data.frame(sel_vars = mod$selectedvars,
#                            varimp = varImp(mod)$importance),
#                 error = function(e)var_imp <- NA)
#               # print(m)
#               if (!is.na(var_imp)){
#                 i$varimp[[k]][[m]][[outs]] <- var_imp
#               }else{
#                 i$varimp[[k]][[m]][[outs]] <- data.frame(sel_vars = NA, Overall = NA)
#               }
#             }else{
#               i$varimp[[k]][[m]][[outs]] <- data.frame(sel_vars = NA, Overall = NA)
#             }
#           }
#         }
#         #######################
#         ###varsel plots
#         #######################
#         # for (k in i$varimp){
#         # print(k)
#         resp_set <- c("SR", "resid") # loop model for SR and resid
#         for (m in resp_set){
#           selvars_allruns <- do.call(rbind, i$varimp[[k]][[m]])
#           if (!is.na(sum(selvars_allruns$Overall))){
#             frq <- as.data.frame(table(selvars_allruns$sel_vars))
#           }else{
#             frq <- data.frame(resp = NA, frq = NA)
#           }
#           colnames(frq) <- c("pred", "freq")
#           # colnames(frq) <- c(paste0(k,"_", m), "freq")
#           i$varsel[[k]][[m]] <- frq
#         }
# 
# 
#         # }
#       }##k names resp
# 
#       if (file.exists(paste0(modDir, "data/"))==F){
#         dir.create(file.path(paste0(modDir, "data/")), recursive = T)
#       }
#       saveRDS(i, file = paste0(modDir, "data/", "60_master_lst_varimp_", names(set_lst)[cnt], ".rds"))
#       return(i)
#   })
  
