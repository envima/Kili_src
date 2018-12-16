# Description:
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
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "dez18_qa/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../data/", sub)
set <- c("nofrst", "frst", "allplts")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(outpath, "15_master_lst_", o, ".rds"))
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
set_lst_res <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  cnt <<- cnt+1
  runs <- sort(unique(i$meta$run))
  for (k in names(i$resp)){
    print(k)
    for (outs in runs){
      print(outs)
      #####
      ###split for outer loop (independet cv)
      #####
      plt_in <- i$meta$plotID[-which(i$meta$run == outs)]
      plt_out <- i$meta$plotID[which(i$meta$run == outs)]
      tbl_in <- list("meta"=i$meta[which(i$meta$plotID %in% plt_in),],
        "resp"=i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),])
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

      if(length(unique(tbl_in$resp$SR)) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
        
        
        notmissing <- !is.na(tbl_in$resp$SR)
        resp <- tbl_in$resp[notmissing,"SR"] # take out NAs from resp so model can run
        predictors <- c("scl_elevation","scl_elevsq")
        preds <- tbl_in$meta[notmissing,predictors] # take out NAs from resp so model can run

        new_dat <- i$meta[i$meta$plotID%in%plt_out,]
        #####
        ###actual model
        #####
        mod_elev <- train(x = preds, 
                          y = resp, 
                          method = "pls", 
                          metric = "RMSE",
                          tuneGrid = expand.grid(ncomp = c(1,2)),
                          trControl = trainControl(method = "cv", index = cvIndex, indexOut = cvIndex_out))
        #####
        ###predict and write into new column
        #####
        prdct <- predict(object = mod_elev, newdata = new_dat)
        i$resp[[k]]$elev_pred[i$resp[[k]]$plotID %in% plt_out] <- prdct
      }else{ # if only one value in tbl_in: modeling isn't possible ==> NA in prediction
        i$resp[[k]]$elev_pred[i$resp[[k]]$plotID %in% plt_out] <- NA
      }
    }
    #####
    ###calculate residuals
    #####
    i$resp[[k]]$resid <- i$resp[[k]]$SR - i$resp[[k]]$elev_pred
    # i$resp[[k]]$resid <- i$resp[[k]]$SR - i$resp[[k]]$elev_pred
    
  }
  saveRDS(i, file = paste0(outpath, "20_master_lst_resid_", names(set_lst)[cnt], ".rds"))
  return(i)
})
names(set_lst_res) <- set
