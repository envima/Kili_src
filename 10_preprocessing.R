# Description: preparing data for modelling
# get LiDAR parameters
# calc trophic levels
# clean dataset
# scale predictors
# initiate tbl_prepro
                    
# Author: Alice Ziegler
# Date: 2018-12-01 14:50:05
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
library(LiDARtools)

#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "dez18/"
inpath <- paste0("../data/", sub)
if (file.exists(inpath)==F){
  dir.create(file.path(inpath))
}
inpath_general <- "../data/"
LiDAR_path <- paste0(inpath, "LiDAR/")
# LiDAR_path <- "C:/Users/Alice/Uni/Projekte/Kili/data/dez18/LiDAR/"
if (file.exists(LiDAR_path)==F){
  dir.create(file.path(LiDAR_path))
}
outpath <- paste0("../data/", sub)
#####
###read files
#####
# optional file in home directory content: user:password
login_file <- paste0(inpath_general, ".remote_sensing_userpwd.txt") 
tec_crdnt <- read.csv(paste0(inpath_general,"tec_crdnt.csv"), header=T, sep=",")

########################################################################################
###Settings
########################################################################################
#######################
###LiDAR Settings
#######################
r_pnts <- 25
d_rst <- 50
db_layers <- c("kili_campaign1_lidar_classified_2015", "kili_campaign2_lidar_classified_2016")
db_login <- readChar(login_file, file.info(login_file)$size) # optional read account from file
db <- "http://137.248.191.215:8081"
location <- unique(tec_crdnt[, c("plotID", "x_pnt", "y_pnt")])
rst_type <- c("chm")
group_name <- "kili_poi_plots"
gap_hght <- 10
gap_sze <- 9
chm_path <- paste0(LiDAR_path, "raster_db_", d_rst, "m/", rst_type[1], "/")



########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
#######################
###get LiDAR 
#######################
points_query(db_layers = db_layers,
             dat_path = LiDAR_path,
             location = location,
             r_pnts = r_pnts,
             db = db,
             db_login = db_login)
point_structure(dat_path = LiDAR_path)

db_structure(dat_path = LiDAR_path, r_pnts = r_pnts, db_layers= db_layers,
             db = db, db_login = db_login, group = "kili_poi_plots")

raster_query(dat_path = LiDAR_path, d_rst = d_rst, db_layers = db_layers, group_name = group_name, db = db,
             db_login = db_login, location = location, rst_type = rst_type)

gap_fraction(dat_path = LiDAR_path, chm_path = chm_path, gap_hght = gap_hght, gap_sze = gap_sze)

#merge lidarstuff ####mit reduce ersetzen
# ldr_mrg <- var_merge(dat_path = LiDAR_path, lst_vars_path = lst_vars_path_ldr, descr = "ldr", mrg_col = "plotUnq")
ldr_nms <- list.files(path = LiDAR_path, pattern = "structure")
ldr_lst <- lapply(ldr_nms, function(i){
  ldr_file <- readRDS(paste0(LiDAR_path, i))
})
ldr_mrg <- Reduce(function(x, y) merge(x, y, all=TRUE), ldr_lst)

#######################
###prepare general dataset
#######################
#####
###combine LiDAR with fielddat
#####
#####
###get rid of doubled LiDAR plots (foc0)
#####
#####
###decide which columns go to meta, resp and potential pred
#####
#######################
###write out outs_lst and cvouts_lst for cross validation
#######################

