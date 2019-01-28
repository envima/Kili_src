
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
library(doParallel)
library(foreach)
library(parallel)
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "dez18_qa/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../data/", sub)
####################################################################################change modDir in i loop!!!!!!!!!!!!!!!!!!!!!!!!!
#####
###where are the models and derived data
#####
set_dir <- "2019-01-08frst_nofrst_allplts/"
mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")
# set <- c("nofrst")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(outpath, "20_master_lst_resid_", o, ".rds"))
})
names(set_lst) <- set 
set_lst <- set_lst[!is.na(set_lst)]
########################################################################################
###Settings
########################################################################################
method <- "pls"
type <- "ffs"

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
  # modDir <- paste0(outpath, Sys.Date(), "_", names(set_lst)[cnt], "_", type, "_", method, "_", comm)
  # modDir <- paste0(outpath, set_dir, "_", names(set_lst)[cnt], "_", type, "_", method, "_", comm)
  
  
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  for(k in names(i$resp)){
    # print(k)
    # for (k in names(i$resp)){
    for (outs in runs){
      #####
      ###split for outer loop (independet cv)
      #####
      plt_in <- i$meta$plotID[-which(i$meta$run == outs)]
      plt_out <- i$meta$plotID[which(i$meta$run == outs)]
      # tbl_in <- list("meta"=i$meta[which(i$meta$plotID %in% plt_in),],
      #                "resp"=i$resp[[k]][which(i$resp[[k]]$plotID %in% plt_in),])

      resp_set <- c("SR", "resid") # loop model for SR and resid
      for (m in resp_set){
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
            col_nm <- paste0("ldr_pred_", m) #column depending on sr or resid
            i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- prdct
          }else{
            col_nm <- paste0("ldr_pred_", m)
            i$resp[[k]][[col_nm]][i$resp[[k]]$plotID %in% plt_out] <- NA
          }
        # }else{ # if only one value in tbl_in: modeling isn't possible ==> NA in prediction
          # col_nm <- paste0("ldr_pred_", m)
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


















