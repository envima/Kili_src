# Description:
# Author: Alice Ziegler
# Date: 2018-12-07 14:45:46
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####

#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "apr19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
#####
###where are the models and derived data
#####
set_dir <- "2019-03-26frst_nofrst_allplts_noelev/"
mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  file <- tryCatch(
    readRDS(file = paste0(modDir, "data/", "50_master_lst_all_mods_",o, ".rds")),    
    error = function(e)file <- NA)
  return(file)
})
names(set_lst) <- set
set_lst <- set_lst[!is.na(set_lst)]

# cv <- "cv_index"
cv <- "cv_20"
# cv <- "cv_50"
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
########################################################################################
###validation data
########################################################################################
cnt <- 0
set_lst_ldr_ncomp <- lapply(set_lst, function(i){# i <- set_lst[[1]]
  cnt <<- cnt+1
if(grepl("cv_index", cv)){
  runs <- sort(unique(i$meta$cvindex_run)) 
}else{
  runs <- seq(sum(grepl("outerrun", colnames(i$meta))))
}

ncomp_k <- lapply(names(i$resp), function(k){ # k <- "SRmammals" k <- "SRpredator"
  # print(k)
    
    ncomp_lidarSR <- unique(i$resp[[k]]$ncomp_lidarSR)
    ncomp_lidarelevSR <- unique(i$resp[[k]]$ncomp_lidarelevSR)
    ncomp_lidarRES <- unique(i$resp[[k]]$ncomp_lidarRES)
    
    ncomp_df <- data.frame(resp = k, 
                           ncomp_lidarSR = ncomp_lidarSR, 
                           ncomp_lidarelevSR = ncomp_lidarelevSR, 
                           ncomp_lidarRES = ncomp_lidarRES)

})
ncomp_k_df <- do.call(rbind, ncomp_k)
  })

saveRDS(set_lst_ldr_ncomp, file = paste0("../out/", sub, set_dir, "62_ncomp_overview.rds"))
