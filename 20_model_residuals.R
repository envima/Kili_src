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
sub <- "dez18/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../data/", sub)
set <- c("frst", "nofrst", "allplts")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(outpath, "master_lst_", o, ".rds"))
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
      tbl_in <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),]
      # tbl_out <- i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_out),]
      #####
      ### create index for inner loop within tbl_in
      #####
      cvIndex <- lapply(runs[-which(runs %in% outs)], function(cvouts){
        plt_cv_in <- i$meta$plotID[-which(i$meta$run == outs | i$meta$run == cvouts)]
        res <- which(tbl_in$plotID %in% plt_cv_in)
      })
      cvIndex_out <- lapply(runs[-which(runs %in% outs)], function(cvouts){
        plt_cv_out <- i$meta$plotID[which(i$meta$run == cvouts)]
        res <- which(tbl_in$plotID %in% plt_cv_out)
      })
      
      if(length(unique(tbl_in[,k])) > 1){ #check if tbl_in has only 0 zB: SRlycopodiopsida/nofrst/outs = 1
        
        #####
        ###create resp, pred and newdata dataframes
        #####
        resp <- tbl_in[!is.na(tbl_in[,k]),k] # take out NAs from resp so model can run
        preds <- data.frame(scl_elevation = i$meta$scl_elevation[i$meta$plotID %in% plt_in & #take only plots from plt_in and take out NAs
                                                                   !(i$meta$plotID %in% tbl_in[is.na(tbl_in[,k]), "plotID"])],
                            scl_elevsq = i$meta$scl_elevsq[i$meta$plotID %in% plt_in &
                                                             !(i$meta$plotID %in% tbl_in[is.na(tbl_in[,k]), "plotID"])])
        new_dat <- data.frame(scl_elevation = i$meta$scl_elevation[i$meta$plotID %in% plt_out], 
                              scl_elevsq = i$meta$scl_elevsq[i$meta$plotID %in% plt_out])
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
      }else{ # if only one value in tbl_in: modeling istnt possible ==> NA in prediction
        i$resp[[k]]$elev_pred[i$resp[[k]]$plotID %in% plt_out] <- NA
      }
    }
    #####
    ###calculate residuals
    #####
    i$resp[[k]]$resid <- i$resp[[k]][[k]] - i$resp[[k]]$elev_pred
  }
  saveRDS(i, file = paste0(outpath, "master_lst_resid_", names(set_lst)[cnt], ".rds"))
  return(i)
})
names(set_lst_res) <- set
