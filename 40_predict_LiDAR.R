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
# #####
# ###set paths
# #####
# setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# # setwd("/mnt/sd19006/data/users/aziegler/src")
# sub <- "feb20_allresp/"
# #paper:
# # sub <- "apr19/"
# inpath <- paste0("../data/", sub)
# inpath_general <- "../data/"
# outpath <- paste0("../data/", sub)
# #####
# ###where are the models and derived data
# #####
# set_dir <- "2020-02-12frst_nofrst_allplts_noelev/"
# #paper: 
# # set_dir <- "2019-03-26frst_nofrst_allplts_noelev/"
# mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
# set <- c("nofrst", "frst", "allplts")
# set <- c("nofrst")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(inpath, "20_master_lst_resid_", o, ".rds"))
})
names(set_lst) <- set 
set_lst <- set_lst[!is.na(set_lst)]

# ###vorl?ufig um alten datensatz und neue trophs zusammenzupacken
# set_lst <- lapply(set, function(o){
#   all <- readRDS(file = paste0(inpath, "20_master_lst_resid_", o, ".rds"))
#   new_trophs <- readRDS(file = paste0(inpath, "26_master_lst_resid_", o, ".rds"))
#   new_moreplts <- lapply(new_trophs$resp, function(resp){
#     merge_tbls <- merge(all$meta, resp, by = "plotID", all = T)
#     flt_tbls <- merge_tbls[,c("plotID", "SR", "elev_pred", "RES")]
#     colnames(flt_tbls)[colnames(flt_tbls) == "RES"] <- "calc_elevRES"
#     colnames(flt_tbls)[colnames(flt_tbls) == "elev_pred"] <- "pred_elevSR"
#     return(flt_tbls)
#   })
#   all$resp <- append(all$resp, new_moreplts)
#   all$meta <- merge(all$meta, new_trophs$meta[,c(colnames(new_trophs$meta) %in% c("plotID",
#                                                    setdiff(colnames(new_trophs$meta),
#                                                            colnames(all$meta))))], all = T)
#   return(all)
# })
# ###ende vorl?ufig

########################################################################################
###Settings
########################################################################################
# method <- "pls"
# type <- "ffs"
# # cv <- "cv_index"
# cv <- "cv_20"
# # cv <- "cv_50"
# resp_set <- c("lidarSR", "lidarelevSR", "lidarRES") #m <- "lidarSR" #loop model for SR and resid
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
  # modDir <- paste0(inpath, Sys.Date(), "_", names(set_lst)[cnt], "_", type, "_", method, "_", comm)
  # modDir <- paste0(inpath, set_dir, "_", names(set_lst)[cnt], "_", type, "_", method, "_", comm)
  
  
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
        # }else{ # if only one value in tbl_in: modeling isn't possible ==> NA in prediction
          # col_nm <- paste0("pred_", m)
          # i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- NA
        # } 
        }
      }
    }
  
if (file.exists(paste0(modDir, "data/"))==F){
  dir.create(file.path(paste0(modDir, "data/")), recursive = T)
}
  

saveRDS(i, file = paste0(modDir, "data/", "40_master_lst_ldr_", names(set_lst)[cnt], ".rds"))
# readRDS(file = paste0(modDir, "40_master_lst_ldr_", names(set_lst)[cnt], ".rds"))
return(i)

})
names(set_lst_ldr) <- set 


