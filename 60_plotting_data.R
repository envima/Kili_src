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
sub <- "mar19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
#####
###where are the models and derived data
#####
set_dir <- "2019-03-19frst_nofrst_allplts_noelev/"
mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

#####
###read files
#####
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

########################################################################################
###Settings
########################################################################################
# cv <- "cv_index"
cv <- "cv_20"
# cv <- "cv_50"
resp_set <- c("lidarSR", "lidarelevSR", "lidarRES") #m <- "lidarSR" #loop model for SR and resid
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
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  if(grepl("cv_index", cv)){
    runs <- sort(unique(i$meta$cvindex_run))
  }else{
    runs <- seq(sum(grepl("outerrun", colnames(i$meta))))
  }
  for (k in names(i$resp)){ # k <- "SRmammals"
    print(k)
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
      RMSE_elevSR <- caret::RMSE(pred = i$resp[[k]]$pred_elevSR[outrows], 
                                    obs = i$resp[[k]]$SR[outrows], na.rm = T)
      RMSE_lidarSR <- caret::RMSE(pred = i$resp[[k]]$pred_lidarSR[outrows], 
                                      obs = i$resp[[k]]$SR[outrows], na.rm = T)
      RMSE_lidarRES <- caret::RMSE(pred = i$resp[[k]]$pred_lidarRES[outrows], 
                                         obs = i$resp[[k]]$calc_elevRES[outrows], na.rm = T)
      RMSE_sumSR <- caret::RMSE(pred = i$resp[[k]]$pred_sumSR[outrows], 
                                                       obs = i$resp[[k]]$SR[outrows], na.rm = T)
      #####
      ###RMSE/sd
      #####
      sd <- sd(i$resp[[k]]$SR, na.rm = T)
      RMSEsd_elevSR <- RMSE_elevSR/sd(i$resp[[k]]$SR, na.rm = T)
      RMSEsd_lidarSR<- RMSE_lidarSR/sd(i$resp[[k]]$SR, na.rm = T)
      RMSEsd_lidarRES <- RMSE_lidarRES/sd(i$resp[[k]]$calc_elevRES, na.rm = T)
      RMSEsd_sumSR <- RMSE_sumSR/sd(i$resp[[k]]$SR, na.rm = T)
      #####
      ###RMSE/median
      #####
      mdn <- median(i$resp[[k]]$SR, na.rm = T)
      RMSEmdn_elevSR <- RMSE_elevSR/mdn
      RMSEmdn_lidarSR<- RMSE_lidarSR/mdn
      RMSEmdn_lidarRES <- RMSE_lidarRES/median(i$resp[[k]]$calc_elevRES, na.rm = T)
      RMSEmdn_sumSR <- RMSE_sumSR/mdn
      
      #####
      ###new list element with validation
      #####
      val_df <- data.frame(run = outs, 
                           RMSE_elevSR = RMSE_elevSR, 
                           RMSE_lidarSR = RMSE_lidarSR,
                           RMSE_lidarRES = RMSE_lidarRES,
                           RMSE_sumSR = RMSE_sumSR, 
                           RMSEsd_elevSR = RMSEsd_elevSR, 
                           RMSEsd_lidarSR = RMSEsd_lidarSR,
                           RMSEsd_lidarRES = RMSEsd_lidarRES,
                           RMSEsd_sumSR = RMSEsd_sumSR, 
                           sd = sd,
                           RMSEmdn_elevSR = RMSEmdn_elevSR, 
                           RMSEmdn_lidarSR = RMSEmdn_lidarSR, 
                           RMSEmdn_lidarRES = RMSEmdn_lidarRES, 
                           RMSEmdn_sumSR = RMSEmdn_sumSR,
                           mdn = mdn)
      
    })
    val_df_all <- do.call(rbind, val_df_all_lst)
    i$val[[k]] <- val_df_all ##<- hier muss soll eigentlich ein df oder lsite, oder so reingeschreiben werden.
  }
  if (file.exists(paste0(modDir, "data/"))==F){
    dir.create(file.path(paste0(modDir, "data/")), recursive = T)
  }
  saveRDS(i, file = paste0(modDir, "data/", "60_master_lst_val_", names(set_lst)[cnt], ".rds"))
  return(i)
})

########################################################################################
###var Imp
########################################################################################
cnt <- 0
set_lst_var_imp <- lapply(set_lst_val, function(i){# i <- set_lst[[1]]
  cnt <<- cnt+1
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  if(grepl("cv_index", cv)){
    runs <- sort(unique(i$meta$cvindex_run))
  }else{
    runs <- seq(sum(grepl("outerrun", colnames(i$meta))))
  }  
  for(k in names(i$resp)){ # k <- "SRmammals"
    # print(k)
    for (outs in runs){ #outs <- 1
      # print(outs)
      #####
      ###split for outer loop (independet cv)
      ###and inner index selection for model
      #####
      if(grepl("cv_index", cv)){
        ###index-cv
        plt_in <- i$meta$plotID[-which(i$meta$cvindex_run == outs)]
        plt_out <- i$meta$plotID[which(i$meta$cvindex_run == outs)]
        tbl_in <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),]
        
      }else{
        ###cv-x
        cv_nm <- colnames(i$meta)[grepl("outerrun", colnames(i$meta))][outs]
        plt_in <- i$meta$plotID[i$meta[cv_nm] == 0]
        plt_out <- i$meta$plotID[i$meta[cv_nm] == 1]
        tbl_in <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),]
      }
      
      for (m in resp_set){ # m <- "lidarSR"
        # print(m)
        m_name <- paste0("pred_", m)
        # if(length(unique(tbl_in[,m])) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
        if(length(unique(tbl_in[,m_name])) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
            
          #####
          ###read actual model
          #####
          mod <- tryCatch(
            readRDS(file = paste0(modDir, "mod_run_", outs, "_", k, "_", m, ".rds")),
            error = function(e)mod <- NA)
          var_imp <- tryCatch(
          data.frame(sel_vars = mod$selectedvars,
                               varimp = varImp(mod)$importance),
          error = function(e)var_imp <- NA)
          # print(m)
          if (!is.na(var_imp)){
            i$varimp[[k]][[m_name]][[outs]] <- var_imp
          }else{
            i$varimp[[k]][[m_name]][[outs]] <- data.frame(sel_vars = NA, Overall = NA)
          }
        }else{
          i$varimp[[k]][[m_name]][[outs]] <- data.frame(sel_vars = NA, Overall = NA)
        }
      }
    }
    #######################
    ###varsel plots
    #######################
    # for (k in i$varimp){
    # print(k)
    # loop model for SR and resid
    for (m in resp_set){
      m_name <- paste0("pred_", m)
      selvars_allruns <- do.call(rbind, i$varimp[[k]][[m_name]])
      if (!is.na(sum(selvars_allruns$Overall))){
      frq <- as.data.frame(table(selvars_allruns$sel_vars))
      }else{
        frq <- data.frame(resp = NA, frq = NA)
      }
      colnames(frq) <- c("pred", "freq")
      # colnames(frq) <- c(paste0(k,"_", m), "freq")
      i$varsel[[k]][[m_name]] <- frq
    }


    # }
  }##k names resp

  if (file.exists(paste0(modDir, "data/"))==F){
    dir.create(file.path(paste0(modDir, "data/")), recursive = T)
  }
  saveRDS(i, file = paste0(modDir, "data/", "60_master_lst_varimp_", names(set_lst)[cnt], ".rds"))
  return(i)
  })
