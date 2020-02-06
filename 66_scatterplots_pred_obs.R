# Description:
# Author: Alice Ziegler
# Date: 2018-12-10 11:49:55
# to do: colnames(df) <- sicherer gegen umsortieren machen
# bei selvar plot legende eindeutig welche farbe welche zahl ist. (nicht tick im übergang)
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
# library(stringr)
# library(tidyr)
library(ggplot2)
# library(raster)
# library(rasterVis)
# library(compositions)
# library(RColorBrewer)
library(dplyr)
# library(grid)
# library(gridExtra)
# library(pBrackets)  
# library(gtable)

#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
# sub <- "oct19/" 
#paper: 
sub <- "apr19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
# set_dir <- "2019-10-10frst_nofrst_allplts_noelev/" 
#paper: 
set_dir <- "2019-03-26frst_nofrst_allplts_noelev/"

mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
modDir <- paste0(inpath, set_dir, "mix/")
# comm <- ""
# grp <- c("specs", "trophs")
# trophs <- c("generalist", "herbivore", "decomposer", "predator")
#####
###read files
#####

mix_lst <- readRDS(file = paste0(modDir, "data/", "61_master_lst_varimp_.rds")) 

########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################

resp_lst <- bind_rows(mix_lst$resp, .id = "resp")

ggplot(resp_lst, aes(x=SR, y=pred_elevSR, color = resp)) + 
  geom_point(shape = 1)+
  coord_fixed(ratio = 1)

ggplot(resp_lst, aes(x=SR, y=pred_lidarSR, color = resp)) + 
  geom_point(shape = 1)+
  coord_fixed(ratio = 1)

ggplot(resp_lst, aes(x=SR, y=pred_lidarRES, color = resp)) + 
  geom_point(shape = 1)+
  coord_fixed(ratio = 1)

ggplot(resp_lst, aes(x=SR, y=pred_sumSR, color = resp)) + 
  geom_point(shape = 1)+
  coord_fixed(ratio = 1)
