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
# setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "dez18_qa/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../data/", sub)
set <- c("frst", "nofrst", "allplts")
# set <- c("frst")
###settings from pre-models
set_dir <- "2018-12-13nofrst_frst_allplts/"
inpath_pre <- paste0(inpath, set_dir)
########################################################################################
###Settings
########################################################################################
ncores <- 1
cl <- makeCluster(ncores)
# comm <- "elev"
# comm <- "noelev"
comm <- "flt_elev"
# comm <- "flt_noelev"
method <- "pls"
type <- "ffs"

#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(outpath, "20_master_lst_resid_", o, ".rds"))
})
names(set_lst) <- set
set_dir <- paste0(Sys.Date(), paste(set, collapse = "_"), "/")
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
cnt <- 0
set_lst_ldr <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  cnt <<- cnt+1
  runs <- sort(unique(i$meta$run))
  modDir <- paste0(outpath, set_dir, Sys.Date(), "_", names(set_lst)[cnt], "_", type, "_", method, "_", comm)
  if (file.exists(modDir)==F){
    dir.create(file.path(modDir))
  }
  # modDir <- "../data/dez18/2018-12-06_nofrst_ffs_pls_"
  registerDoParallel(cl)
  foreach(k = names(i$resp)[1], .errorhandling = "remove", .packages=c("caret", "CAST", "plyr"))%dopar%{
  # for (k in names(i$resp)){
    for (outs in runs[1]){
      #####
      ###split for outer loop (independet cv)
      #####
      plt_in <- i$meta$plotID[-which(i$meta$run == outs)]
      plt_out <- i$meta$plotID[which(i$meta$run == outs)]
      tbl_in <- list("meta"=i$meta[which(i$meta$plotID %in% plt_in),],
                     "resp"=i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),])
      # tbl_out <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_out),]
      #####
      ### create index for inner loop within tbl_in
      #####
      cvIndex <- lapply(runs[-which(runs %in% outs)], function(cvouts){
        plt_cv_in <- i$meta$plotID[-which(i$meta$run == outs | i$meta$run == cvouts)]
        res <- which(tbl_in$meta$plotID %in% plt_cv_in)
      })
      cvIndex_out <- lapply(runs[-which(runs %in% outs)], function(cvouts){
        plt_cv_out <- i$meta$plotID[which(i$meta$run == cvouts)]
        res <- which(tbl_in$meta$plotID %in% plt_cv_out)
      })
      
      resp_set <- c("SR", "resid") #m <- "SR" #loop model for SR and resid
      for (m in resp_set){
      if(length(unique(tbl_in$resp$SR)) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
        #####
        ###create resp, pred and newdata dataframes
        #####
        notmissing <- !is.na(tbl_in$resp$SR)
        resp <- tbl_in$resp[notmissing,m] # take out NAs from resp so model can run
        predictors <- colnames(tbl_in$meta)[grepl(pattern = "scl_", colnames(i$meta))]
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
        # ##### Predict
        # 
        # prdct <- predict(object = mod, newdata = new_dat)
        # col_nm <- paste0("ldr_pred_", m)
        # i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- prdct
  }
})
names(set_lst_ldr) <- set 
stopCluster(cl)