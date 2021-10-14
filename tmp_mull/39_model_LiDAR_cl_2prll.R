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
library(caret)
library(doParallel)
library(foreach)
library(parallel)
#####
###set paths
#####
setwd("/home/ziegler5/src") # marc2
# setwd(dirname(rstudioapi::getSourceEditorContext()[[2]])) # lokal
# setwd("/mnt/sd19006/data/users/aziegler/src") # 60er
sub <- "feb19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../data/", sub)
set <- c("frst", "nofrst", "allplts")
# set <- c("frst")
###settings from pre-models
set_dir <- "2019-02-15frst_nofrst_allplts_elev/"
inpath_pre <- paste0(inpath, set_dir)
########################################################################################
###Settings
########################################################################################
cmd_input <- base::commandArgs(trailingOnly =TRUE)
i <- as.numeric(cmd_input[1])
core_num <- 30
# comm <- "elev"
comm <- "noelev"
# comm <- "flt_elev"
# comm <- "flt_noelev"
rdc <- T # use reduced data (plots with elevation problem are eliminated by script 65)
method <- "pls"
type <- "ffs"
# cv <- "cv_index"
cv <- "cv_20"
# cv <- "cv_50"
cv_fold_in <- 4
cv_times_in <- 20

#####
###read files
#####
set_lst <- lapply(set, function(o){ # o <- "frst"
  file <- readRDS(file = paste0(outpath, "20_master_lst_resid_", o, ".rds"))
  if (rdc == T){
    rdc_path <- list.dirs(path = inpath_pre, recursive = F)[grep(paste0("_", o, "_ffs_"), list.dirs(path = inpath_pre, recursive = F))]
    file <- readRDS(paste0(rdc_path, "/data/", "65_master_lst_rdc_by_elevprob_", o, ".rds"))
  }
  return(file)
})
names(set_lst) <- set
set_dir <- paste0(Sys.Date(), paste(set, collapse = "_"), "_", comm, "/")
if (file.exists(paste0(outpath, set_dir))==F){
  dir.create(file.path(paste0(outpath, set_dir)))
}
if(grepl("flt", comm)){
  preds_flt <- readRDS(file = paste0(inpath_pre, "80_preds_flt.rds"))
}

########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
modDir <- paste0(outpath, set_dir, Sys.Date(), "_", names(set_lst)[i], "_", type, "_", method, "_", comm)
if (file.exists(modDir)==F){
  dir.create(file.path(modDir))
}
if(grepl("cv_index", cv)){
  runs <- sort(unique(set_lst[[i]]$meta$cvindex_run))
}else{
  runs <- seq(sum(grepl("outerrun", colnames(set_lst[[i]]$meta))))
}  

cl <- makeCluster(core_num, type = "FORK", outfile = paste0("/home/ziegler5/data/feb19/", set_dir, "out.txt")) 
# cl <- makeCluster(core_num, outfile = paste0("../data/feb19/", set_dir, "out.txt")) 

# cnt <- 0
# set_lst_ldr <- lapply(set_lst, function(i){# i <- set_lst[[1]]
#   cnt <<- cnt+1
registerDoParallel(cl)
foreach(k = names(set_lst[[i]]$resp), 
        .errorhandling = "remove", 
        .packages=c("caret", "CAST", "plyr"))%dopar%{ # testing: k <- "SRmammals"
          # k <- "SRmammals"
          foreach (outs = runs, 
                   .errorhandling = "remove", 
                   .packages=c("caret", "CAST", "plyr"))%dopar%{ # outs <- 1
            #####
            ###split for outer loop (independet cv)
            ###and inner index selection for model
            #####
            if(grepl("cv_index", cv)){
              ###index-cv
              plt_in <- set_lst[[i]]$meta$plotID[-which(set_lst[[i]]$meta$cvindex_run == outs)]
              plt_out <- set_lst[[i]]$meta$plotID[which(set_lst[[i]]$meta$cvindex_run == outs)]
              tbl_in <- list("meta"=set_lst[[i]]$meta[which(set_lst[[i]]$meta$plotID %in% plt_in),],
                             "resp"=set_lst[[i]]$resp[[k]][which(set_lst[[i]]$resp[[k]]$plotID %in% plt_in),])
              #####
              ### create index for inner loop within tbl_in
              #####
              cvIndex <- lapply(runs[-which(runs %in% outs)], function(cvouts){
                plt_cv_in <- set_lst[[i]]$meta$plotID[-which(set_lst[[i]]$meta$cvindex_run == outs | set_lst[[i]]$meta$cvindex_run == cvouts)]
                res <- which(tbl_in$meta$plotID %in% plt_cv_in)
              })
              cvIndex_out <- lapply(runs[-which(runs %in% outs)], function(cvouts){
                plt_cv_out <- set_lst[[i]]$meta$plotID[which(set_lst[[i]]$meta$cvindex_run == cvouts)]
                res <- which(tbl_in$meta$plotID %in% plt_cv_out)
              })
            }else{
              ###cv-x
              cv_nm <- colnames(set_lst[[i]]$meta)[grepl("outerrun", colnames(set_lst[[i]]$meta))][outs]
              plt_in <- set_lst[[i]]$meta$plotID[set_lst[[i]]$meta[cv_nm] == 0]
              plt_out <- set_lst[[i]]$meta$plotID[set_lst[[i]]$meta[cv_nm] == 1]
              tbl_in <- list("meta"=set_lst[[i]]$meta[which(set_lst[[i]]$meta$plotID %in% plt_in),],
                             "resp"=set_lst[[i]]$resp[[k]][which(set_lst[[i]]$resp[[k]]$plotID %in% plt_in),])
              
              #create multifolds for inner lop in traincontrol
              tbl_folds <- data.frame(tbl_in$resp, cat = substr(tbl_in$resp$plotID, 1, 3))
              set.seed(10)
              cvIndex <- createMultiFolds(y = tbl_folds$cat, k = cv_fold_in, times = cv_times_in)
              cvIndex_out <- lapply(seq(cvIndex), function(rsmpl){
                seq(1, nrow(tbl_folds))[!seq(1, nrow(tbl_folds)) %in% cvIndex[[rsmpl]]]
              })
            }
            
            resp_set <- c("SR", "resid") #m <- "SR" #loop model for SR and resid
            for (m in resp_set){
              if(length(unique(tbl_in$resp$SR)) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
                #####
                ###create resp, pred and newdata dataframes
                #####
                notmissing <- !is.na(tbl_in$resp$SR)
                resp <- tbl_in$resp[notmissing,m] # take out NAs from resp so model can run
                predictors <- colnames(tbl_in$meta)[grepl(pattern = "scl_", colnames(set_lst[[i]]$meta))]
                if(grepl("noelev", comm)){
                  predictors <- predictors[!grepl("elev", predictors)]
                }
                if(grepl("flt", comm)){
                  predictors <- predictors[predictors %in% preds_flt$pred]#possible, that not all preds_flt$pred appear 
                  # in predictors, because predictors is mix from all 3 landscapes
                }
                preds <- tbl_in$meta[notmissing,predictors] # take out NAs from resp so model can run
                # resp <- tbl_in$resp[!is.na(tbl_in[,k]),k] # take out NAs from resp so model can run
                # new_dat <- i$meta[i$meta$plotID%in%plt_out,]
                #####
                ###actual model
                #####
                mod <- ffs(preds, resp,
                           method = "pls",
                           metric = "RMSE",
                           withinSE = F, 
                           tuneGrid = expand.grid(ncomp = c(1:7)), #:5, 10, 15, 20, 25, 30, 34)),
                           trControl = trainControl(method = "cv", index = cvIndex, indexOut = cvIndex_out),
                           verbose = T)
                saveRDS(mod, file = paste0(modDir, "/mod_run_", outs, "_", k, "_", m, ".rds"))
              }
            }
          }

}
# names(set_lst_ldr) <- set 
stopCluster(cl)
