# Description: setup script for script in Kili project
# Author: Alice Ziegler
# Date: 2021-02-19 11:42:51
# to do:
rm(list=ls())

########################################################################################
### Presettings
########################################################################################

#####
### load packages
#####

#####
### set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))

sub <- "feb_21_article/"

inpath_general <- "../data/"

inpath <- paste0("../data/", sub)
if (file.exists(inpath)==F){
  dir.create(file.path(inpath), recursive = T)
}

LiDAR_path <- paste0(inpath, "LiDAR/")
if (file.exists(LiDAR_path)==F){
  dir.create(file.path(LiDAR_path))
}

outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

figpath <- paste0("../fig/", sub)
if (file.exists(figpath)==F){
  dir.create(file.path(figpath), recursive = T)
}
#alter outpath war mal gleich mit inpath
###where are the models within the "sub"
set_dir <- "2020-02-12frst_nofrst_allplts_noelev/"

mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

#####
### read files
#####

########################################################################################
### Settings
########################################################################################
cv <- "cv_20"
resp_set <- c("lidarSR", "lidarelevSR", "lidarRES") #m <- "lidarSR" #loop model for SR and resid

##relevant for 30_model_LiDAR.R
# machine <- "local"
# machine <- "server"
machine <- "cluster"
core_num <- 63

