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
library(plyr)
library(stringr)
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
# tec_crdnt <- read.csv(paste0(inpath_general,"tec_crdnt.csv"), header=T, sep=",")
field_dat <- as.data.frame(read.table(file = paste0(inpath_general, "Biodiversity_Data_Marcel.csv"), 
                                      sep = ";", header = T, na.strings = "NA", dec = ","))
trophic_tbl <- as.data.frame(read.csv(paste0(inpath_general, "trophic_tbl.csv"), sep = ";"))
########################################################################################
###Settings
########################################################################################
set <- c("frst", "nofrst", "allplts")
#######################
###LiDAR Settings
#######################
r_pnts <- 25
d_rst <- 50
db_layers <- c("kili_campaign1_lidar_classified_2015", "kili_campaign2_lidar_classified_2016")
db_login <- readChar(login_file, file.info(login_file)$size) # optional read account from file
db <- "http://137.248.191.215:8081"
# location <- unique(tec_crdnt[, c("plotID", "x_pnt", "y_pnt")])
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
             # location = location,
             r_pnts = r_pnts,
             db = db,
             db_login = db_login, 
             group = "kili_poi_plots")
# points_query <- readRDS(file = paste0(LiDAR_path, "points_25m.rds"))
point_structure(dat_path = LiDAR_path)
# point_structure <- readRDS(file = paste0(LiDAR_path, "point_structure.rds"))
db_structure(dat_path = LiDAR_path, r_pnts = r_pnts, db_layers= db_layers,
             db = db, db_login = db_login, group = "kili_poi_plots")
# db_structure <- readRDS(file = paste0(LiDAR_path, "db_structure.rds"))
raster_query(dat_path = LiDAR_path, d_rst = d_rst, db_layers = db_layers, group_name = group_name, db = db,
             db_login = db_login, rst_type = rst_type)
gap_fraction(dat_path = LiDAR_path, chm_path = chm_path, gap_hght = gap_hght, gap_sze = gap_sze)
# gap_structure <- readRDS(file = paste0(LiDAR_path, "gap_structure.rds"))
#merge lidarstuff ####mit reduce ersetzen
# ldr_mrg <- var_merge(dat_path = LiDAR_path, lst_vars_path = lst_vars_path_ldr, descr = "ldr", mrg_col = "plotUnq")
ldr_nms <- list.files(path = LiDAR_path, pattern = "structure")
ldr_lst <- lapply(ldr_nms, function(i){
  ldr_file <- readRDS(paste0(LiDAR_path, i))
})
ldr_mrg <- Reduce(function(x, y) merge(x, y, all=TRUE), ldr_lst)

saveRDS(ldr_mrg, file = paste0(outpath, "zw_ldr_mrg.rds"))
# ldr_mrg <- readRDS(file = paste0(outpath, "zw_ldr_mrg.rds"))

#######################
###prepare general dataset
#######################
#clean LiDAR data
#get rid of doubled LiDAR plots (foc1)
dups_lst <- ldr_mrg[which(duplicated(ldr_mrg$plotID)),"plotID"]
if(length(dups_lst) > 0){
  for (m in dups_lst){
    toss <- ldr_mrg[which(ldr_mrg$plotID == m),]
    toss$yr <- as.numeric(str_sub(toss$plotUnq, -4, -1))
    toss <- toss[which(toss$yr == min(toss$yr)),"plotUnq"]
    ldr_mrg <- ldr_mrg[-which(ldr_mrg$plotUnq == toss),]
  }
}

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
#create column elevation squared
mrg_tbl$elevsq <- mrg_tbl$elevation^2
#####
###decide which columns go to meta, resp and potential pred
#####
nm_meta_base <- c("plotID", "cat", "selID")
nm_resp_SR <- c(colnames(mrg_tbl)[c(which(colnames(mrg_tbl) == "SRmammals") : 
                                      which(colnames(mrg_tbl) == "SRmagnoliids"))])
nm_pred_pot <- c(colnames(mrg_tbl)[c(which(colnames(mrg_tbl) %in% "AGB"),
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
                                     which(colnames(mrg_tbl) %in% "elevation"), 
                                     which(colnames(mrg_tbl) %in% "elevsq"))])
mrg_tbl <- mrg_tbl[which(colnames(mrg_tbl) %in% c(nm_meta_base, nm_resp_SR, nm_pred_pot))]
#######################
###create column "run" with index for crossvalidation
#######################
#indeces that are not comun will be merged into different index for cv
frq <- as.data.frame(table(mrg_tbl$selID))
noruns <- frq[which(frq$Freq < (0.5 * max(frq$Freq))), "Var1"]
runs <- frq[which(frq$Freq >= (0.5 * max(frq$Freq))), "Var1"]

mrg_tbl$run <- mrg_tbl$selID
if(length(noruns) > 0){
  for (i in noruns){
    dist <- mrg_tbl[which(mrg_tbl$selID %in% i),]
    df_tmp <- mrg_tbl[which(mrg_tbl$cat == mrg_tbl$cat[which(mrg_tbl$selID == i)] & 
                        mrg_tbl$selID != i),]
    run_miss <- runs[-which(runs %in% df_tmp$selID)]
    mrg_tbl[which(mrg_tbl$plotID == dist$plotID),"run"] <- as.numeric(as.character(run_miss))
  }
}
nm_meta <- c(nm_meta_base, "run")

#######################
###calculate trophic levels
#######################
lvl <- c("predator", "generalist", "decomposer", "herbivore", "plant", "birds", "bats")
trophic_tbl$Taxon <- as.character(trophic_tbl$Taxon)
trophic_tbl$diet <- factor(trophic_tbl$diet, levels = lvl)

troph_resp <- lapply(colnames(mrg_tbl)[which(colnames(mrg_tbl) %in% nm_resp_SR)], function(x){
  trop <- NA
  for (i in trophic_tbl$Taxon){
    match <- grep(i, x, value=TRUE)
    if (length(match) != 0){
      trop <- i
    }
  }
  return(c(resp = x, Taxon = trop))
})
troph_mrg <- merge(trophic_tbl, as.data.frame(do.call(rbind, troph_resp)), by = "Taxon")
troph_sum <- data.frame(plotID = mrg_tbl$plotID)
for (i in levels(trophic_tbl$diet)){
  match <- colnames(mrg_tbl)[c(which(colnames(mrg_tbl) %in% 
                                       as.character(troph_mrg$resp[which(troph_mrg$diet == i)])))]
  # drop = F, muss sein, weil es sonst für level mit nur einer Spalte (bats/birds) 
  # fehlermeldung gibt, so können sie trotzdemtrotzdem weiter in die nächste Tabelle 
  # geschrieben werden
  summed <- rowSums(mrg_tbl[,match, drop = F], na.rm = T)
  troph_sum$summed <- summed
  colnames(troph_sum)[which(colnames(troph_sum) == "summed")] <- paste0("sum_", i, "_N", length(match))
}
mrg_tbl_troph <- merge(mrg_tbl, troph_sum, by = "plotID")
#####
###append nm_resp mit nm_resp_troph
#####
nm_resp_troph <- colnames(troph_sum)[-which(colnames(troph_sum) == "plotID")]
#######################
###stuff done for each set (frst/nofrst/allplts)
#######################
for (o in set){
  if (o != "allplts"){
    if (o == "frst"){
      cat <- c("fer", "flm", "foc", "fod", "fpd", "fpo", "hom")
    }else if (o == "nofrst"){
      cat <- c("cof", "gra", "hel", "mai", "sav")
    }}else{
      cat <- unique(mrg_tbl_troph$cat)
    }
  #####
  ###subset table
  #####
  tbl_set <- mrg_tbl_troph[which(mrg_tbl_troph$cat %in% cat),]
  #####
  ###filter predictors  (min 50% different values)
  #####
  nm_pred <- nm_pred_pot
  for (i in nm_pred_pot){
    frq <- table(tbl_set[i])
    if (max(frq) > floor(nrow(tbl_set) * 0.5)){
      nm_pred <- nm_pred[!(nm_pred == i)]
    }
  }
  tbl_set <- tbl_set[,which(colnames(tbl_set) %in% c(nm_meta, nm_resp_SR, nm_resp_troph, nm_pred))]
  #####
  ###scale predictors
  #####
  scl_lst <- lapply(colnames(tbl_set), function(m){
    if(m %in% nm_pred){
      if (class(tbl_set[,m]) == "numeric"){
        scale(tbl_set[,m], center = T, scale = T)
      }else if (class(tbl_set[,m]) == "integer"){
        scale(as.numeric(tbl_set[,m]), center = T, scale = T)
      }else{
        tbl_set[,m] <- tbl_set[,m] #non numeric or integer columns stay as they are
      }
    }else{
      tbl_set[,m] <- tbl_set[,m] #non numeric or integer columns stay as they are}
    }})#lapply m
  tbl_scl <- do.call(data.frame, scl_lst)
  colnames(tbl_scl) <- colnames(tbl_set)
  tbl_scl <- tbl_scl[,c(which(colnames(tbl_scl) == "plotID"), 
                        which(colnames(tbl_scl) %in% nm_pred))]
  colnames(tbl_scl)[which(colnames(tbl_scl) %in% nm_pred)] <- 
    paste0("scl_", colnames(tbl_scl)[which(colnames(tbl_scl) %in% nm_pred)])
  #append nm_pred with nm_pred_scl
  nm_pred_scl <- colnames(tbl_scl)[-which(colnames(tbl_scl) == "plotID")]
  #merge tbl_set and tbl_scl
  tbl_mrg_set <- merge(tbl_set, tbl_scl, by = "plotID")
  ########################################################################################
  ###initiate an write list
  ########################################################################################
  master_lst <- list(meta = tbl_mrg_set[,which(colnames(tbl_mrg_set) %in% c(nm_meta, nm_pred, nm_pred_scl))], 
                     resp = lapply(c(nm_resp_SR, nm_resp_troph), function(i){
                        # print(i)
                       tbl <- tbl_mrg_set[,c(which(colnames(tbl_mrg_set) == "plotID"), 
                                             which(grepl(i, colnames(tbl_mrg_set))))]
                     }))
  names(master_lst$resp) <- c(nm_resp_SR, nm_resp_troph)
  saveRDS(master_lst, file = paste0(outpath, "master_lst_", o, ".rds"))
  # master_lst <- readRDS(file = paste0(outpath, "master_lst_", o, ".rds"))
}#for o in set

# 
# #old cv init
# #######################
# ###write out outs_lst and cvouts_lst for cross validation - 
# ###an Tabelle wird drangeschrieben, wann diese vorhergesagt werden, 
# ###rausgelassen werden können Sie auch öfter. 
# #######################
# #create outs list (outer loop)
# ind_nums <- sort(unique(mrg_tbl$selID))
# runs <- ind_nums[ind_nums > 0 & ind_nums < 6]
# cats <- unique(mrg_tbl$cat)
# rndm_draw <- c()
# outs_lst_rw <- lapply(runs, function(k){
#   out_sel <- mrg_tbl[which(mrg_tbl$selID == k),]
#   miss <- cats[!(cats %in% out_sel$cat)]
#   df_miss <- mrg_tbl[mrg_tbl$cat %in% as.vector(miss),]
#   if (length(rndm_draw) > 0){
#     df_miss <- df_miss[-which(df_miss$plotID %in% rndm_draw),]
#   }
#   set.seed(k)
#   out_miss <- ddply(df_miss, .(cat), function(x){
#     #check for flm6. If flm6 still wasn't picked, take that
#     if (max(x$selID) > max(runs)){ 
#       x[which(x$selID == max(x$selID)),]
#     }else if (min(x$selID) < min(runs)){ 
#       x[which(x$selID == min(x$selID)),]
#     }else{
#       x[sample(nrow(x), 1), ]
#     }
#   })
#   rndm_draw <<- c(rndm_draw, as.character(out_miss$plotID)) #<<- leider nötig, weil sonst rndm_draw außerhalb nciht verändert wird
#   out <- rbind(out_sel, out_miss)
#   # outs_lst_rw <- append(outs_lst_rw, out) 
# })
# outs_lst <- outs_lst_rw
# for (j in runs){
#   outs_lst[[j]] <- outs_lst_rw[[j]]$plotID
# }
# saveRDS(outs_lst, file = paste0(outpath, "outs_lst.rds"))
# 
# ###cv index gleiches system wie outer loop (innerloop)
# rndm_draw_cv <- c()
# cvouts_lst <- lapply(seq(outs_lst), function(i){
#   cvind_num <- runs[-which(runs == i)]
#   cvouts_lst_rw <- lapply(cvind_num, function(k){
#     out_sel <- mrg_tbl[which(mrg_tbl$selID == k),]
#     miss <- cats[!(cats %in% out_sel$cat)]
#     df_miss <- mrg_tbl[mrg_tbl$cat %in% as.vector(miss),]
#     if (length(rndm_draw_cv) > 0){
#       df_miss <- df_miss[-which(df_miss$plotID %in% rndm_draw_cv),]
#     }
#     set.seed(k)
#     out_miss <- ddply(df_miss, .(cat), function(x){
#       #check for flm6. If flm6 still wasn't picked, take that
#       if (max(x$selID) > max(runs)){ 
#         x[which(x$selID == max(x$selID)),]
#       }else if (min(x$selID) < min(runs)){ 
#         x[which(x$selID == min(x$selID)),]
#       }else{
#         x[sample(nrow(x), 1), ]
#       }
#     })
#     #   out_sel <- mrg_tbl[which(mrg_tbl$selID == k),]
#     #   miss <- cats[!(cats %in% out_sel$cat)]
#     #   df_miss <- mrg_tbl[mrg_tbl$cat %in% as.vector(miss),]
#     #   set.seed(k)
#     #   out_miss <- ddply(df_miss, .(cat), function(x){
#     #     x[sample(nrow(x), 1), ]
#     # })
#     out <- rbind(out_sel, out_miss)
#   })
#   
#   cvouts_lst_runs <- cvouts_lst_rw
#   for (j in seq(cvouts_lst_rw)){
#     cvouts_lst_runs[[j]] <- cvouts_lst_rw[[j]]$plotID
#   }
#   return(cvouts_lst_runs)
# })
# saveRDS(cvouts_lst, file = paste0(outpath, "cvouts_lst.rds"))
