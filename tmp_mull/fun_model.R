model<-function(i, set_lst, cv, comm, sub, inpath, inpath_general, outpath, 
                                 set, set_dir, method, type, cv_fold_in, 
                                 cv_times_in, resp_set, preds_flt = NA){
  
  modDir <- paste0(outpath, set_dir, Sys.Date(), "_", names(set_lst)[i], "_", type, "_", 
                   method, "_", comm)
  if (file.exists(modDir)==F){
    dir.create(file.path(modDir))
  }
  if(grepl("cv_index", cv)){
    runs <- sort(unique(set_lst[[i]]$meta$cvindex_run))
  }else{
    runs <- seq(sum(grepl("outerrun", colnames(set_lst[[i]]$meta))))
  }
  foreach(k = names(set_lst[[i]]$resp), 
          .errorhandling = "remove", 
          .packages=c("caret", "CAST", "plyr"))%:% #{ # testing: k <- "SRmammals"
            foreach (outs = runs, 
                     .errorhandling = "remove", 
                     .packages=c("caret", "CAST", "plyr"))%dopar%{ # outs <- 1
            # for (outs in runs){ # outs <- 1
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
                
                #create multifolds for inner loop in traincontrol
                tbl_folds <- data.frame(tbl_in$resp, cat = substr(tbl_in$resp$plotID, 1, 3))
                set.seed(10)
                cvIndex <- createMultiFolds(y = tbl_folds$cat, k = cv_fold_in, times = cv_times_in)
                cvIndex_out <- lapply(seq(cvIndex), function(rsmpl){
                  seq(1, nrow(tbl_folds))[!seq(1, nrow(tbl_folds)) %in% cvIndex[[rsmpl]]]
                })
              }
              
              # m <- "lidarSR" #loop model for lidarSR, lidarelevSR, lidarRES
              for (m in resp_set){
                if(length(unique(tbl_in$resp$SR)) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
                  #####
                  ###create resp, pred and newdata dataframes
                  #####
                  notmissing <- !is.na(tbl_in$resp$SR)
                  if(grepl("SR", m)){
                    resp <- tbl_in$resp[notmissing,"SR"] # take out NAs from resp so model can run
                  } else if (grepl("RES", m)){
                    resp <- tbl_in$resp[notmissing,"calc_elevRES"] # take out NAs from resp so model can run
                  }
                  
                  predictors <- colnames(tbl_in$meta)[grepl(pattern = "scl_", colnames(set_lst[[i]]$meta))]
                  if(!grepl("elev", m)){
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
            
          #  #foreach1
}