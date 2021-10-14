# Description: calculate residual model
# Author: Alice Ziegler
# Date: 2018-12-03 11:21:51
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
library(caret)
source("000_setup.R")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(inpath, "15_master_lst_", o, ".rds"))
})
names(set_lst) <- set
########################################################################################
###Settings
########################################################################################
# cv <- "cv_index"
cv <- "cv_20"
# cv <- "cv_50"

#model validation inside loop
mod_meth <- "cv"
cv_times_in <- 20
cv_fold_in <- 4

##reduce response variables with  names
subset <- c()
# subset <- c("SRpredator", "SRherbivore", "SRgeneralist", "SRdecomposer")

if (length(subset) > 0){
  set_lst <- lapply(set_lst, function(i){ #i <- set_lst[[2]]
   reduced <- i$resp[names(i$resp) %in% subset]
   rdc_list <- list(meta = i$meta, resp = reduced)
    return(rdc_list)
  })
}
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
cnt <- 0
set_lst_res <- lapply(set_lst, function(i){# i <- set_lst[[2]]
  cnt <<- cnt+1
  if(grepl("cv_index", cv)){
    runs <- sort(unique(i$meta$cvindex_run))
  }else{
    runs <- seq(sum(grepl("outerrun", colnames(i$meta))))
  }
  for (k in names(i$resp)){ # k <- "SRmammals" k  <- "SRpredator"
    print(k)
    for (outs in runs){ # outs <- 1 outs <- 5
      print(outs)
      #####
      ###split for outer loop (independet cv)
      ###and inner index selection for model
      #####
      if(grepl("cv_index", cv)){
      ###index-cv
      plt_in <- i$meta$plotID[-which(i$meta$cvindex_run == outs)]
      plt_out <- i$meta$plotID[which(i$meta$cvindex_run == outs)]
      tbl_in <- list("meta"=i$meta[which(i$meta$plotID %in% plt_in),],
        "resp"=i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),])
      #####
      ### create index for inner loop within tbl_in
      #####
      cvIndex <- lapply(runs[-which(runs %in% outs)], function(cvouts){
        plt_cv_in <- i$meta$plotID[-which(i$meta$cvindex_run == outs | i$meta$cvindex_run == cvouts)]
        res <- which(tbl_in$meta$plotID %in% plt_cv_in)
      })
      cvIndex_out <- lapply(runs[-which(runs %in% outs)], function(cvouts){
        plt_cv_out <- i$meta$plotID[which(i$meta$cvindex_run == cvouts)]
        res <- which(tbl_in$meta$plotID %in% plt_cv_out)
      })
      }else{
      ###cv-x
        cv_nm <- colnames(i$meta)[grepl("outerrun", colnames(i$meta))][outs]
        plt_in <- i$meta$plotID[i$meta[cv_nm] == 0]
        plt_out <- i$meta$plotID[i$meta[cv_nm] == 1]
        tbl_in <- list("meta"=i$meta[which(i$meta$plotID %in% plt_in),],
                       "resp"=i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),])
        
        #create multifolds for inner lop in traincontrol
        tbl_folds <- data.frame(tbl_in$resp, cat = substr(tbl_in$resp$plotID, 1, 3))
        set.seed(10)
        cvIndex <- createMultiFolds(y = tbl_folds$cat, k = cv_fold_in, times = cv_times_in)
        cvIndex_out <- lapply(seq(cvIndex), function(rsmpl){
          seq(1, nrow(tbl_folds))[!seq(1, nrow(tbl_folds)) %in% cvIndex[[rsmpl]]]
        })
        }

      if(length(unique(tbl_in$resp$SR)) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
        
        
        notmissing <- !is.na(tbl_in$resp$SR)
        resp <- tbl_in$resp[notmissing,"SR"] # take out NAs from resp so model can run
        predictors <- c("scl_elevation","scl_elevsq")
        preds <- tbl_in$meta[notmissing,predictors] # take out NAs from resp so model can run

        new_dat <- i$meta[i$meta$plotID %in% plt_out,]
        #####
        ###actual model
        #####

        mod_elev <- train(x = preds, 
                          y = resp, 
                          method = "pls", 
                          metric = "RMSE",
                          tuneGrid = expand.grid(ncomp = c(1,2)),
                          trControl = trainControl(method = mod_meth, index = cvIndex, indexOut = cvIndex_out))
        #####
        ###predict and write into new column
        #####
        prdct <- predict(object = mod_elev, newdata = new_dat)
        i$resp[[k]]$pred_elevSR[i$resp[[k]]$plotID %in% plt_out] <- prdct
      }else{ # if only one value in tbl_in: modeling isn't possible ==> NA in prediction
        i$resp[[k]]$pred_elevSR[i$resp[[k]]$plotID %in% plt_out] <- NA
      }
    }
    #####
    ###calculate residuals
    #####
    i$resp[[k]]$calc_elevRES <- i$resp[[k]]$SR - i$resp[[k]]$pred_elevSR
    # i$resp[[k]]$resid <- i$resp[[k]]$SR - i$resp[[k]]$pred_elevSR
    
  }
  saveRDS(i, file = paste0(inpath, "20_master_lst_resid_", names(set_lst)[cnt], ".rds"))
  return(i)
})
names(set_lst_res) <- set
