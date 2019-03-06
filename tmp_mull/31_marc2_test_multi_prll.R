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
setwd("/home/ziegler5/src")
sub <- "dez18_qa/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)
set <- c("frst", "nofrst", "allplts")
# set <- c("frst")
###settings from pre-models
# set_dir <- "2018-12-13nofrst_frst_allplts/"
# inpath_pre <- paste0(inpath, set_dir)
########################################################################################
###Settings
########################################################################################
cmd_input <- base::commandArgs(trailingOnly =TRUE)
i <- as.numeric(cmd_input[1])
core_num <- 30
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
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
# cl <- makeCluster(core_num, outfile = paste0(getwd(), "/", inpath, set_dir)) #../ in cluster für eine ebene hoch nicht möglich?
# cl <- makeCluster(core_num, outfile = paste0("/home/ziegler5/data/dez18_qa/", set_dir, "out.txt")) #../ in cluster für eine ebene hoch nicht möglich
cl <- makeCluster(core_num, type = "FORK", outfile = paste0("/home/ziegler5/data/dez18_qa/", set_dir, "out.txt")) 
#../ in cluster für eine ebene hoch nicht möglich
#type = "FORK" funktioniert nur auf Linux

#testing: i <- 1 # from SGE_TASK_ID argument
registerDoParallel(cl)
mod_lst <- foreach(k = names(set_lst[[i]]$resp), 
                   .errorhandling = "remove", 
                   # .final = function(x) setNames(x, paste0(i, "_", k, "_", outs, "_", m, "_")), 
                   .packages=c("caret", "CAST", "plyr"))%dopar%{ # testing: k <- "SRmammals"
                     runs <- sort(unique(set_lst[[i]]$meta$run))
                     # for (outs in runs){
                     out_loop <- lapply(runs, function(outs){
                       resp_set <- c("SR", "resid") #testing: m <- "SR" #loop model for SR and resid
                       respons <- lapply(resp_set, function(m){
                         # for (m in resp_set){
                         mod <- paste0(i, "_", k, "_", outs, "_", m, "_") #test ohne modell
                         # saveRDS(mod, file = paste0(modDir, "/mod_run_", outs, "_", k, "_", m, ".rds"))
                       })
                     })
                   }
modDir <- paste0(outpath, set_dir, Sys.Date(), "_", names(set_lst)[i])
if (file.exists(modDir)==F){
  dir.create(file.path(modDir))
}
saveRDS(mod_lst, file = paste0(modDir, "/mod_lst.rds"))
stopCluster(cl)