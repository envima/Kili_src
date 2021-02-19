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

sub <- "feb_21_deleted_everything_from_40-end/"

inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
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

########################################################################################
########################################################################################
########################################################################################
### Do it (Don't change anything past this point except you know what you are doing!) ##
########################################################################################
########################################################################################
########################################################################################
