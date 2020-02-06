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
# sub <- "oct19/"
sub <- "apr19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
# set_dir <- "2019-10-10frst_nofrst_allplts_noelev/" 
#paper: 
set_dir <- "2019-03-26frst_nofrst_allplts_noelev/"
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


#####
###create mixed dataset
#####
###mix responses
mix_lst_resp <- lapply(names(set_lst$allplts$resp), function(i){
  mix_df_resp<- rbind(set_lst$nofrst$resp[[i]], set_lst$frst$resp[[i]])
})
names(mix_lst_resp) <- names(set_lst$allplts$resp)

###mix meta
mix_meta_nofrst <- set_lst$nofrst$meta
mix_meta_frst <- set_lst$frst$meta

mix_meta_frst$cvindex_run <- mix_meta_frst$cvindex_run + 5 ###hard coded

col_nm <- lapply(colnames(mix_meta_frst)[grepl("outerrun", 
                                               colnames(mix_meta_frst))], 
                 function(i){
                   elem1 <- strsplit(i, "_")[[1]][[1]]
                   elem2 <- strsplit(i, "_")[[1]][[2]]
                   num <- as.numeric(strsplit(i, "_")[[1]][[3]]) + 20
                   col_nm <- paste0(elem1, "_", elem2, "_", num)
                   
                 })

colnames(mix_meta_frst)[grepl("outerrun", colnames(mix_meta_frst))] <- col_nm
mix_meta_nofrst[setdiff(colnames(mix_meta_frst), 
                        colnames(mix_meta_nofrst))] <- NA

mix_meta_frst[setdiff(colnames(mix_meta_nofrst), 
                      colnames(mix_meta_frst))] <- NA
mix_df_meta <- rbind(mix_meta_nofrst, mix_meta_frst)

mix_lst <- list(meta = mix_df_meta, resp = mix_lst_resp)


modDir <- paste0(inpath, set_dir, "mix/")


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
if(grepl("cv_index", cv)){
  runs <- sort(unique(mix_lst$meta$cvindex_run)) 
}else{
  runs <- seq(sum(grepl("outerrun", colnames(mix_lst$meta))))
}
for (k in names(mix_lst$resp)){ # k <- "SRmammals" k <- "SRpredator"
  # print(k)
  val_df_all_lst <- lapply (runs, function(outs){ #outs <- 1
    #####
    ###out rows for thisrun (has to be chosen first, as depending on cv20/cv-index, 
    ###the rows are chosen by one column, or by several.)
    #####
    if(grepl("cv_index", cv)){
      outrows <- which(mix_lst$meta$cvindex_run == outs)
    }else{
      cv_nm <- colnames(mix_lst$meta)[grepl("outerrun", colnames(mix_lst$meta))][outs]
      outrows <- which(mix_lst$meta[cv_nm] == 1)
    }
    #####
    ###RMSE
    #####
    RMSE_elevSR <- caret::RMSE(pred = mix_lst$resp[[k]]$pred_elevSR[outrows], 
                               obs = mix_lst$resp[[k]]$SR[outrows], na.rm = T)
    RMSE_lidarSR <- caret::RMSE(pred = mix_lst$resp[[k]]$pred_lidarSR[outrows], 
                                obs = mix_lst$resp[[k]]$SR[outrows], na.rm = T)
    
    RMSE_lidarRES <- caret::RMSE(pred = mix_lst$resp[[k]]$pred_lidarRES[outrows], 
                                 obs = mix_lst$resp[[k]]$calc_elevRES[outrows], na.rm = T)
    RMSE_sumSR <- caret::RMSE(pred = mix_lst$resp[[k]]$pred_sumSR[outrows], 
                              obs = mix_lst$resp[[k]]$SR[outrows], na.rm = T)
    RMSE_lidarelevSR <- caret::RMSE(pred = mix_lst$resp[[k]]$pred_lidarelevSR[outrows], 
                                    obs = mix_lst$resp[[k]]$SR[outrows], na.rm = T)
    #####
    ###RMSE/sd
    #####
    sd <- sd(mix_lst$resp[[k]]$SR, na.rm = T)
    RMSEsd_elevSR <- RMSE_elevSR/sd
    RMSEsd_lidarSR<- RMSE_lidarSR/sd
    # residuen fehler soll auf sd absoluten werten gerechnet werden
    # es ist egal ob residuen + oder - x sind
    RMSEsd_lidarRES <- RMSE_lidarRES/sd(abs(mix_lst$resp[[k]]$calc_elevRES), na.rm = T)
    RMSEsd_sumSR <- RMSE_sumSR/sd
    RMSEsd_lidarelevSR <- RMSE_lidarelevSR/sd
    #####
    ###RMSE/median
    #####
    mdn <- median(mix_lst$resp[[k]]$SR, na.rm = T)
    RMSEmdn_elevSR <- RMSE_elevSR/mdn
    RMSEmdn_lidarSR<- RMSE_lidarSR/mdn
    # residuen fehler soll auf mdn absoluten werten gerechnet werden
    # es ist egal ob residuen + oder - x sind
    RMSEmdn_lidarRES <- RMSE_lidarRES/median(abs(mix_lst$resp[[k]]$calc_elevRES), na.rm = T)
    RMSEmdn_sumSR <- RMSE_sumSR/mdn
    RMSEmdn_lidarelevSR <- RMSE_lidarelevSR/mdn
    
    armean <- mean(mix_lst$resp[[k]]$SR, na.rm = T)
    
    #####
    ###check variation of ncomp
    #####
    # ncomp_lidarSR_frst <- unique(mix_lst$resp[[k]]$ncomp_lidarSR)
    # ncomp_lidarelevSR_frst <- unique(mix_lst$resp[[k]]$ncomp_lidarelevSR)
    # ncomp_lidarRES_frst <- unique(mix_lst$resp[[k]]$ncomp_lidarRES)
    # ncomp_lidarSR_nofrst <- unique(mix_lst$resp[[k]]$ncomp_lidarSR)
    # ncomp_lidarelevSR_nofrst <- unique(mix_lst$resp[[k]]$ncomp_lidarelevSR)
    # ncomp_lidarRES_nofrst <- unique(mix_lst$resp[[k]]$ncomp_lidarRES)
    #####
    ###new list element with validation
    #####
    val_df <- data.frame(run = outs, 
                         RMSE_elevSR = RMSE_elevSR, 
                         RMSE_lidarSR = RMSE_lidarSR,
                         RMSE_lidarRES = RMSE_lidarRES,
                         RMSE_sumSR = RMSE_sumSR, 
                         RMSE_lidarelevSR = RMSE_lidarelevSR, 
                         RMSEsd_elevSR = RMSEsd_elevSR, 
                         RMSEsd_lidarSR = RMSEsd_lidarSR,
                         RMSEsd_lidarRES = RMSEsd_lidarRES,
                         RMSEsd_sumSR = RMSEsd_sumSR, 
                         RMSEsd_lidarelevSR = RMSEsd_lidarelevSR, 
                         sd = sd,
                         RMSEmdn_elevSR = RMSEmdn_elevSR, 
                         RMSEmdn_lidarSR = RMSEmdn_lidarSR, 
                         RMSEmdn_lidarRES = RMSEmdn_lidarRES, 
                         RMSEmdn_sumSR = RMSEmdn_sumSR,
                         RMSEmdn_lidarelevSR = RMSEmdn_lidarelevSR, 
                         mdn = mdn, 
                         armean = armean #, 
                         # ncomp_lidarSR = ncomp_lidarSR, 
                         # ncomp_lidarelevSR = ncomp_lidarelevSR, 
                         # ncomp_lidarRES = ncomp_lidarRES
                         )
    
  })
  val_df_all <- do.call(rbind, val_df_all_lst)
  
  
  #####
  ###add columns with statistical information about the RMSE and RMSEsd errors
  #####
  for (p in colnames(val_df_all)[!colnames(val_df_all) %in% 
                                 c("run", "sd", "mdn", "armean")]){ #p <- "RMSEsd_lidarRES"
    val_df_all$sd_tmp <- sd(val_df_all[[p]], na.rm = T)
    colnames(val_df_all)[colnames(val_df_all) == "sd_tmp"] <- paste0(p, "_sd")
    val_df_all$mdn_tmp <- median(val_df_all[[p]], na.rm = T)
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
  ###add ranking of performances by model
  #####
  ###rank by median - RMSEsd
  mod_df_mdn_tmp <- val_df_all[,c("RMSEsd_elevSR_mdn", 
                                  "RMSEsd_lidarSR_mdn", 
                                  "RMSEsd_sumSR_mdn", 
                                  "RMSEsd_lidarelevSR_mdn", 
                                  "RMSEsd_lidarRES_mdn")]
  mod_df_mdn_tmp_t <- t(mod_df_mdn_tmp[!duplicated(mod_df_mdn_tmp),])
  mod_df_mdn_tmp_srt <- mod_df_mdn_tmp_t[order(mod_df_mdn_tmp_t[,1]),]
  val_df_all$RMSEsd_elevSR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_elevSR_mdn")
  val_df_all$RMSEsd_sumSR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_sumSR_mdn")
  val_df_all$RMSEsd_lidarSR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_lidarSR_mdn")
  val_df_all$RMSEsd_lidarelevSR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_lidarelevSR_mdn")
  val_df_all$RMSEsd_lidarRES_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEsd_lidarRES_mdn")
  
  ###rank by median - RMSEmdn
  mod_df_mdn_tmp <- val_df_all[,grepl(pattern = "_mdn$", colnames(val_df_all)) & 
                                 grepl(pattern = "RMSEmdn_", colnames(val_df_all))]
  mod_df_mdn_tmp_t <- t(mod_df_mdn_tmp[!duplicated(mod_df_mdn_tmp),])
  mod_df_mdn_tmp_srt <- mod_df_mdn_tmp_t[order(mod_df_mdn_tmp_t[,1]),]
  val_df_all$RMSEmdn_elevSR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_elevSR_mdn")
  val_df_all$RMSEmdn_sumSR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_sumSR_mdn")
  val_df_all$RMSEmdn_lidarSR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_lidarSR_mdn")
  val_df_all$RMSEmdn_lidarelevSR_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_lidarelevSR_mdn")
  val_df_all$RMSEmdn_lidarRES_mdn_rank <- which(names(mod_df_mdn_tmp_srt) == "RMSEmdn_lidarRES_mdn")
  #####
  ###column with different model constellations
  #####
  #best model by mdn RMSEsd
  val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_elevSR_mdn_rank == 1] <- 1
  val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_sumSR_mdn_rank == 1] <- 2
  val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_lidarSR_mdn_rank == 1] <- 3
  val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_lidarelevSR_mdn_rank == 1] <- 4
  val_df_all$constll1_RMSEsd_mdn[val_df_all$RMSEsd_lidarRES_mdn_rank == 1] <- 5
  #best model by median RMSemdn
  val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_elevSR_mdn_rank == 1] <- 1
  val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_sumSR_mdn_rank == 1] <- 2
  val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_lidarSR_mdn_rank == 1] <- 3
  val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_lidarelevSR_mdn_rank == 1] <- 4
  val_df_all$constll1_RMSEmdn_mdn[val_df_all$RMSEmdn_lidarRES_mdn_rank == 1] <- 5
  ###second best, ... could follow
  ###rank by IQR - RMSEsd
  mod_df_IQR_tmp <- val_df_all[,grepl(pattern = "IQR$", colnames(val_df_all)) & 
                                 grepl(pattern = "RMSEsd_", colnames(val_df_all))]
  mod_df_IQR_tmp_t <- t(mod_df_IQR_tmp[!duplicated(mod_df_IQR_tmp),])
  mod_df_IQR_tmp_srt <- mod_df_IQR_tmp_t[order(mod_df_IQR_tmp_t[,1]),]
  val_df_all$RMSEsd_elevSR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_elevSR_IQR")
  val_df_all$RMSEsd_sumSR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_sumSR_IQR")
  val_df_all$RMSEsd_lidarSR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_lidarSR_IQR")
  val_df_all$RMSEsd_lidarelevSR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_lidarelevSR_IQR")
  val_df_all$RMSEsd_lidarRES_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEsd_lidarRES_IQR")
  
  ###rank by IQR - RMSEmdn
  mod_df_IQR_tmp <- val_df_all[,grepl(pattern = "IQR$", colnames(val_df_all)) & 
                                 grepl(pattern = "RMSEmdn_", colnames(val_df_all))]
  mod_df_IQR_tmp_t <- t(mod_df_IQR_tmp[!duplicated(mod_df_IQR_tmp),])
  mod_df_IQR_tmp_srt <- mod_df_IQR_tmp_t[order(mod_df_IQR_tmp_t[,1]),]
  val_df_all$RMSEmdn_elevSR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_elevSR_IQR")
  val_df_all$RMSEmdn_sumSR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_sumSR_IQR")
  val_df_all$RMSEmdn_lidarSR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_lidarSR_IQR")
  val_df_all$RMSEmdn_lidarelevSR_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_lidarelevSR_IQR")
  val_df_all$RMSEmdn_lidarRES_IQR_rank <- which(names(mod_df_IQR_tmp_srt) == "RMSEmdn_lidarRES_IQR")
  
  
  mix_lst$val[[k]] <- val_df_all ##<- hier muss soll eigentlich ein df oder lsite, oder so reingeschreiben werden.
}
if (file.exists(paste0(modDir, "data/"))==F){
  dir.create(file.path(paste0(modDir, "data/")), recursive = T)
}
saveRDS(mix_lst, file = paste0(modDir, "data/", "61_master_lst_val.rds"))


########################################################################################
###var Imp
########################################################################################
pot_dirs <- list.dirs(path = paste0(modDir, "../"), recursive = F, full.names = F)          
for(k in names(mix_lst$resp)){ # k <- "SRmammals"
    # print(k)
    for (outs in runs){ #outs <- 19
      if (outs <= max(runs)/2){
        landuse <- "nofrst"
        outs_corr <- outs
      } else{
        landuse <- "frst"
        outs_corr <- outs - 20
      }
        
        dir_landuse <- pot_dirs[grepl(paste0("_", landuse, "_"), pot_dirs)]
      
      # print(outs)
      #####
      ###split for outer loop (independet cv)
      ###and inner index selection for model
      #####
      if(grepl("cv_index", cv)){
        ###index-cv
        plt_in <- mix_lst$meta$plotID[-which(mix_lst$meta$cvindex_run == outs)]
        plt_out <- mix_lst$meta$plotID[which(mix_lst$meta$cvindex_run == outs)]
        tbl_in <- mix_lst[[k]][which(mix_lst[[k]]$plotID %in% plt_in),]
        
      }else{
        ###cv-x
        cv_nm <- colnames(mix_lst$meta)[grepl("outerrun", colnames(mix_lst$meta))][outs]
        plt_in <- mix_lst$meta$plotID[mix_lst$meta[cv_nm] == 0]
        plt_out <- mix_lst$meta$plotID[mix_lst$meta[cv_nm] == 1]
        tbl_in <- mix_lst$resp[[k]][which(mix_lst$resp[[k]]$plotID %in% plt_in),]
      }

      for (m in resp_set){ # m <- "lidarSR"
        # print(m)
        m_name <- paste0("pred_", m)
        # if(length(unique(tbl_in[,m])) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
        if(length(unique(tbl_in[,m_name])) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
          #####
          ###read actual model
          #####
          ####hier einfügen, dass alle frst und nofrst modelle geladen werden
          mod <- tryCatch(
            readRDS(file = paste0(modDir, "../", dir_landuse, "/mod_run_", outs_corr, "_", k, "_", m, ".rds")),
            error = function(e)mod <- NA)
          var_imp <- tryCatch(
          data.frame(sel_vars = mod$selectedvars,
                               varimp = varImp(mod)$importance),
          error = function(e)var_imp <- NA)
          # print(m)
          if (!is.na(var_imp)){
            mix_lst$varimp[[k]][[m_name]][[landuse]][[outs_corr]] <- var_imp
          }else{
            mix_lst$varimp[[k]][[m_name]][[landuse]][[outs_corr]] <- data.frame(sel_vars = NA, Overall = NA)
          }
        }else{
          mix_lst$varimp[[k]][[m_name]][[landuse]][[outs_corr]] <- data.frame(sel_vars = NA, Overall = NA)
        }
      }
    } #end for n 
    
    #######################
    ###varsel plots
    #######################
    # for (k in mix_lst$varimp){
    # print(k)
    # loop model for SR and resid
    for (m in resp_set){ # m <- "lidarSR"
      m_name <- paste0("pred_", m)
      a <- do.call(c, mix_lst$varimp[[k]][[m_name]])
      selvars_allruns <- do.call(rbind, a)#mix_lst$varimp[[k]][[m_name]])
      if (!is.na(sum(selvars_allruns$Overall))){
      frq <- as.data.frame(table(selvars_allruns$sel_vars))
      }else{
        frq <- data.frame(resp = NA, frq = NA)
      }
      colnames(frq) <- c("pred", "freq")
      # colnames(frq) <- c(paste0(k,"_", m), "freq")
      mix_lst$varsel[[k]][[m_name]] <- frq
    }

    # }
  }##k names resp

  if (file.exists(paste0(modDir, "data/"))==F){
    dir.create(file.path(paste0(modDir, "data/")), recursive = T)
  }
  saveRDS(mix_lst, file = paste0(modDir, "data/", "61_master_lst_varimp_", ".rds"))



########################################################################################
###descriptive stuff for general overview of responses (in frst, nofrst, all, sr, median, sd, mean)
########################################################################################
resp_overview <- data.frame(resp = names(mix_lst$resp))


  for (k in resp_overview$resp){
    resp_overview$tmp[which(resp_overview$resp == k)] <- mix_lst$val[[k]]$armean
    resp_overview$mdn[which(resp_overview$resp == k)] <- mix_lst$val[[k]]$mdn
    resp_overview$sd[which(resp_overview$resp == k)] <- mix_lst$val[[k]]$sd
  }
  colnames(resp_overview)[colnames(resp_overview) == "tmp"] <- "mean_mix"
  colnames(resp_overview)[colnames(resp_overview) == "mdn"] <- "median_mix"
  colnames(resp_overview)[colnames(resp_overview) == "sd"] <- "sd_mix"


saveRDS(resp_overview, file = paste0(modDir, "data/61_resp_overview_descriptive.rds"))
write.csv(resp_overview, file = paste0(modDir, "data/61_resp_overview_descriptive.csv"))
#resp_overview <- read.csv(file = paste0(modDir, "data/61_resp_overview_descriptive.csv"))
