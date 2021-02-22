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


if(machine == "local"){
  setwd(dirname(rstudioapi::getSourceEditorContext()[[2]])) # lokal
}else if(machine == "server"){
  setwd("/mnt/sd19006/data/users/aziegler/src") # 60er
}else if(machine == "cluster"){
  setwd("/home/ziegler5/src") # marc2
}

# sub <- "oct19/"
# sub <- "apr19/"#paper
sub <- "feb20_allresp/"
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
# comm <- "elev"
comm <- "noelev"
# comm <- "flt_elev"
# comm <- "flt_noelev"
method <- "pls"
type <- "ffs"
# cv <- "cv_index"
cv <- "cv_20"
# cv <- "cv_50"
cv_fold_in <- 4
cv_times_in <- 20
# resp_set <- c("SR", "resid") #m <- "SR" #loop model for SR and resid
resp_set <- c("lidarSR", "lidarelevSR", "lidarRES") #m <- "SR" #loop model for SR and resid

# resp_set <- c("SR")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(outpath, "20_master_lst_resid_", o, ".rds"))
})
names(set_lst) <- set
set_dir <- paste0(Sys.Date(), paste(set, collapse = "_"), "_", comm, "/")
if (file.exists(paste0(outpath, set_dir))==F){
  dir.create(file.path(paste0(outpath, set_dir)))
}
if(grepl("flt", comm)){
  preds_flt <- readRDS(file = paste0(inpath_pre, "80_preds_flt.rds"))
}

source("fun_model.R")
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
if (machine == "local"){
  set_lst_ldr <- lapply(seq(length(set_lst)), function(i){# i <- 1 , set_lst[[i]]
    
    model(i = i, set_lst = set_lst, cv = cv, comm = comm, sub = sub, 
                           inpath = inpath, inpath_general = inpath_general, 
                           outpath = outpath, set = set, set_dir = set_dir, 
                           method = method, type = type, cv_fold_in = cv_fold_in, 
                           cv_times_in = cv_times_in, resp_set = resp_set)
  }
  )
}
if (machine == "server"){ # not tested yet
  registerDoParallel(core_num)
  set_lst_ldr <- lapply(seq(length(set_lst)), function(i){# i <- 1 , set_lst[[i]]
    model(i = i, set_lst = set_lst, cv = cv, comm = comm, sub = sub, 
                           inpath = inpath, inpath_general = inpath_general, 
                           outpath = outpath, set = set, set_dir = set_dir, 
                           method = method, type = type, cv_fold_in = cv_fold_in, 
                           cv_times_in = cv_times_in, resp_set = resp_set)}
  )
}
if (machine == "cluster"){
  cmd_input <- base::commandArgs(trailingOnly =TRUE)
  i <- as.numeric(cmd_input[1])
  cl <- makeCluster(core_num, type = "FORK", outfile = paste0("/home/ziegler5/data/", sub, set_dir, "out.txt")) 
  registerDoParallel(cl)
  model(i = i, set_lst = set_lst, cv = cv, comm = comm, sub = sub, 
                         inpath = inpath, inpath_general = inpath_general, 
                         outpath = outpath, set = set, set_dir = set_dir, 
                         method = method, type = type, cv_fold_in = cv_fold_in, 
                         cv_times_in = cv_times_in, resp_set = resp_set)
}
stopCluster(cl)
