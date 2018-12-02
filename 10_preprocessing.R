# Description: preparing data for modelling
# get LiDAR parameters
# calc trophic levels
# clean dataset
# scale predictors
# initiate tbl_prepro
                    
# Author: Alice Ziegler
# Date: 2018-12-01 14:50:05
# to do: 
##check flm6 und fer2 for availability
##wirklich nur 5 durchläufe trotz flm6? wird das bei fehlendem flm5 zufällig gelost?? =>mechanismus einbauen
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
field_dat <- as.data.frame(read.table(file = paste0(inpath_general, "Biodiversity_Data_Marcel.csv"), 
                                    sep = ";", header = T, na.strings = "NA", dec = ","))
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
# saveRDS(ldr_mrg, file = paste0(outpath, "zw_ldr_mrg.rds"))
# ldr_mrg <- readRDS(file = paste0(outpath, "zw_ldr_mrg.rds"))

#######################
###prepare general dataset
#######################
#clean LiDAR data
#get rid of doubled LiDAR plots (foc0)
ldr_mrg <- ldr_mrg[-which(ldr_mrg$plotUnq == "foc1_kili_campaign1_lidar_classified_2015"),]
#clean field_dat
#remove plots/rows which lack data for more than 50% of taxa (apply sum NAs in taxa columns - if more than half => 
#                                                             delete row)
field_dat <- field_dat[!(apply(field_dat[,c(which(colnames(field_dat) == "SRmammals") : 
                                              which(colnames(field_dat) == "SRmagnoliids"))], 
                             1, function(x) {sum(is.na(x)) > 
                                 (ncol(field_dat[,c(which(colnames(field_dat) == "SRmammals") : 
                                                      which(colnames(field_dat) == "SRmagnoliids"))])/2)})),]
#combine LiDAR with field_dat
mrg_tbl <- merge(field_dat, ldr_mrg, by = "plotID")
#create column selID
mrg_tbl$selID <- as.numeric(substr(mrg_tbl$plotID, 4, 4))
#####
###decide which columns go to meta, resp and potential pred
#####
meta <- c("plotID", "cat", "selID")
resp_SR <- c(colnames(mrg_tbl)[c(which(colnames(mrg_tbl) == "SRmammals") : 
                                     which(colnames(mrg_tbl) == "SRmagnoliids"))])
pred <- c(colnames(mrg_tbl)[c(which(colnames(mrg_tbl) %in% "AGB"),
                              which(colnames(mrg_tbl) %in% "BE_FHD") : 
                                which(colnames(mrg_tbl) %in% "LAI"),
                              which(colnames(mrg_tbl) %in% "chm_surface_ratio"),
                              which(colnames(mrg_tbl) %in% "pulse_returns_max") : 
                                which(colnames(mrg_tbl) %in% "pulse_returns_mean"),
                              which(colnames(mrg_tbl) %in% "pulse_returns_sd"),
                              which(colnames(mrg_tbl) %in% "vegetation_coverage_01m") : 
                                which(colnames(mrg_tbl) %in% "vegetation_coverage_10m"),
                              which(colnames(mrg_tbl) %in% "gap_frac"),
                              which(colnames(mrg_tbl) %in% "mdn_rtrn"),
                              which(colnames(mrg_tbl) %in% "sd_rtrn_1"),
                              which(colnames(mrg_tbl) %in% "qntl_rng"),
                              which(colnames(mrg_tbl) %in% "elevation"))])

#######################
###write out outs_lst and cvouts_lst for cross validation - 
###an Tabelle wird drangeschrieben, wann diese vorhergesagt werden, 
###rausgelassen werden können Sie auch öfter. 
#######################
#create outs list (outer loop)
ind_nums <- sort(unique(mrg_tbl$selID))
runs <- ind_nums[ind_nums > 0 & ind_nums < 6]
cats <- unique(mrg_tbl$cat)

rndm_draw <- c()
outs_lst_rw <- lapply(runs, function(k){
  out_sel <- mrg_tbl[which(mrg_tbl$selID == k),]
  miss <- cats[!(cats %in% out_sel$cat)]
  df_miss <- mrg_tbl[mrg_tbl$cat %in% as.vector(miss),]
  if (length(rndm_draw) > 0){
    df_miss <- df_miss[-which(df_miss$plotID %in% rndm_draw),]
  }
  set.seed(k)
  out_miss <- ddply(df_miss, .(cat), function(x){
    #check for flm6. If flm6 still wasn't picked, take that
    if (max(x$selID) > max(runs)){ 
      x[which(x$selID == max(x$selID)),]
    }else if (min(x$selID) < min(runs)){ 
      x[which(x$selID == min(x$selID)),]
    }else{
      x[sample(nrow(x), 1), ]
    }
  })
  rndm_draw <<- c(rndm_draw, as.character(out_miss$plotID)) #<<- leider nötig, weil sonst rndm_draw außerhalb nciht verändert wird
  out <- rbind(out_sel, out_miss)
  # outs_lst_rw <- append(outs_lst_rw, out) 
})
outs_lst <- outs_lst_rw
for (j in runs){
  outs_lst[[j]] <- outs_lst_rw[[j]]$plotID
}
saveRDS(outs_lst, file = paste0(outpath, "outs_lst.rds"))


###cv index gleiches system wie outer loop (innerloop)
rndm_draw_cv <- c()
cvouts_lst <- lapply(seq(outs_lst), function(i){
  cvind_num <- runs[-which(runs == i)]
  cvouts_lst_rw <- lapply(cvind_num, function(k){
    out_sel <- mrg_tbl[which(mrg_tbl$selID == k),]
    miss <- cats[!(cats %in% out_sel$cat)]
    df_miss <- mrg_tbl[mrg_tbl$cat %in% as.vector(miss),]
    if (length(rndm_draw_cv) > 0){
      df_miss <- df_miss[-which(df_miss$plotID %in% rndm_draw_cv),]
    }
    set.seed(k)
    out_miss <- ddply(df_miss, .(cat), function(x){
      #check for flm6. If flm6 still wasn't picked, take that
      if (max(x$selID) > max(runs)){ 
        x[which(x$selID == max(x$selID)),]
      }else if (min(x$selID) < min(runs)){ 
        x[which(x$selID == min(x$selID)),]
      }else{
        x[sample(nrow(x), 1), ]
      }
    })
  #   out_sel <- mrg_tbl[which(mrg_tbl$selID == k),]
  #   miss <- cats[!(cats %in% out_sel$cat)]
  #   df_miss <- mrg_tbl[mrg_tbl$cat %in% as.vector(miss),]
  #   set.seed(k)
  #   out_miss <- ddply(df_miss, .(cat), function(x){
  #     x[sample(nrow(x), 1), ]
    # })
    out <- rbind(out_sel, out_miss)
  })
  
  cvouts_lst_runs <- cvouts_lst_rw
  for (j in seq(cvouts_lst_rw)){
    cvouts_lst_runs[[j]] <- cvouts_lst_rw[[j]]$plotID
  }
  return(cvouts_lst_runs)
})

saveRDS(cvouts_lst, file = paste0(outpath, "cvouts_lst.rds"))

