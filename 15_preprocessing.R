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
library(plyr)
library(stringr)
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "dez18_qa/"
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
ldr_mrg <- readRDS(file = paste0(inpath, "10_ldr_mrg.rds"))
field_dat <- as.data.frame(read.table(file = paste0(inpath_general, "Biodiversity_Data_Marcel.csv"), 
                                      sep = ";", header = T, na.strings = "NA", dec = ","))
trophic_tbl <- as.data.frame(read.csv(paste0(inpath_general, "trophic_tbl.csv"), sep = ";"))
########################################################################################
###Settings
########################################################################################
set <- c("nofrst", "frst", "allplts")
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
                                      which(colnames(mrg_tbl) == "SRsnails"), 
                                    which(colnames(mrg_tbl) == "SRrosids") : 
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
#indices that are not comun will be merged into different index for cv
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
for (i in unique(troph_mrg$diet[!troph_mrg$diet %in% c("birds", "bats")])){
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
###append troph_mrg with sum trophics
#####
for (i in seq(colnames(troph_sum)[(grepl("sum", colnames(troph_sum)))])){
  print(i)
  tmp <- data.frame(Taxon = colnames(troph_sum)[[i+1]], 
                    diet = str_split(colnames(troph_sum)[(grepl("sum", colnames(troph_sum)))], pattern = "_")[[i]][2], 
                    resp =  colnames(troph_sum)[[i+1]])
  troph_mrg <- rbind(troph_mrg, tmp)
}

saveRDS(troph_mrg, file = paste0(outpath, "15_troph_mrg.rds"))
#####
###append nm_resp mit nm_resp_troph
#####
nm_resp_troph <- colnames(troph_sum)[-which(colnames(troph_sum) == "plotID")]
#######################
###stuff done for each set (frst/nofrst/allplts)
#######################
for (o in set){ # o <- set[[1]]
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
  ###initiate a write list
  ########################################################################################
  master_lst <- list(meta = tbl_mrg_set[,which(colnames(tbl_mrg_set) %in% c(nm_meta, nm_pred, nm_pred_scl))], 
                     resp = lapply(c(nm_resp_SR, nm_resp_troph), function(i){
                       # print(i)
                       tbl <- tbl_mrg_set[,c(which(colnames(tbl_mrg_set) == "plotID"), 
                                             which(grepl(i, colnames(tbl_mrg_set))))]
                       ###rename resp in SR##################################################################################
                       colnames(tbl) <- c("plotID", "SR")
                       return(tbl)
                     }))
  names(master_lst$resp) <- c(nm_resp_SR, nm_resp_troph)
  saveRDS(master_lst, file = paste0(outpath, "15_master_lst_", o, ".rds"))
  # master_lst <- readRDS(file = paste0(outpath, "master_lst_", o, ".rds"))
}#for o in set
