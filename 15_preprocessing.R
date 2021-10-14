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
library(plyr)
library(stringr)
library(caret)
library(gpm)
source("000_setup.R")

#####
###set paths
#####
#####
###read files
#####
ldr_mrg <- readRDS(file = paste0(inpath, "10_ldr_mrg.rds"))
field_dat <- as.data.frame(read.table(file = paste0(inpath_general, "Biodiversity_Data_Marcel.csv"), 
                                      sep = ";", header = T, na.strings = "NA", dec = ","))
trophic_tbl <- as.data.frame(read.csv(paste0(inpath_general, "trophic_tbl.csv"), sep = ";"))
gpm_tn <- readRDS(paste0(inpath_general, "ki_hyperspec_biodiv_non_scaled.rds"))

########################################################################################
###Settings
########################################################################################
set <- c("nofrst", "frst", "allplts")
cv_times <- 20
cv_fold <- 5
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
                                      which(colnames(mrg_tbl) == "SRsnails")
                                    # , 
                                    # which(colnames(mrg_tbl) == "SRrosids") : 
                                    #   which(colnames(mrg_tbl) == "SRmagnoliids")
)])
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
###create column "cvindex_run" with index for crossvalidation by index (transect)
#######################
#indices that are not column will be merged into different index for cv
frq <- as.data.frame(table(mrg_tbl$selID))
noruns <- frq[which(frq$Freq < (0.5 * max(frq$Freq))), "Var1"]
runs <- frq[which(frq$Freq >= (0.5 * max(frq$Freq))), "Var1"]

mrg_tbl$cvindex_run <- mrg_tbl$selID
if(length(noruns) > 0){
  for (i in noruns){
    dist <- mrg_tbl[which(mrg_tbl$selID %in% i),]
    df_tmp <- mrg_tbl[which(mrg_tbl$cat == mrg_tbl$cat[which(mrg_tbl$selID == i)] & 
                              mrg_tbl$selID != i),]
    run_miss <- runs[-which(runs %in% df_tmp$selID)]
    mrg_tbl[which(mrg_tbl$plotID == dist$plotID),"cvindex_run"] <- as.numeric(as.character(run_miss))
  }
}
nm_meta <- c(nm_meta_base, "cvindex_run")

#######################
###create dataframe with 20 colums for cv20 cross validation by landuseclass 20 times randomly drawn plot of each landuse
#######################
set.seed(10)
folds <- createMultiFolds(y = mrg_tbl$cat, k = cv_fold, times = cv_times)
folds_outer <- folds[grepl(pattern = "Fold1", names(folds))]
plots_outer <- lapply(folds_outer, function(reps){
  mrg_tbl$plotID[-reps]
})
for (run in seq(1:length(plots_outer))){
  mrg_tbl$tmp_run <- ifelse((mrg_tbl$plotID %in% plots_outer[run][[1]]), 1, 0)
  colnames(mrg_tbl)[colnames(mrg_tbl) == "tmp_run"] <- paste0("cv", length(plots_outer), "_outerrun_", run)
}
paste0("cv", length(plots_outer), "_outerrun_", seq(1:length(plots_outer)))
nm_meta <- c(nm_meta, paste0("cv", length(plots_outer), "_outerrun_", seq(1:length(plots_outer))))
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
  print(i)
  match <- colnames(mrg_tbl)[c(which(colnames(mrg_tbl) %in% 
                                       as.character(troph_mrg$resp[which(troph_mrg$diet == i)])))]
  # drop = F, muss sein, weil es sonst f?r level mit nur einer Spalte (bats/birds) 
  # fehlermeldung gibt, so k?nnen sie trotzdemtrotzdem weiter in die n?chste Tabelle 
  # geschrieben werden
  summed <- rowSums(mrg_tbl[,match, drop = F], na.rm = T)
  troph_sum$summed <- summed
  colnames(troph_sum)[which(colnames(troph_sum) == "summed")] <- paste0("sum_", i, "_N", length(match))
}
mrg_tbl_troph_first <- merge(mrg_tbl, troph_sum, by = "plotID")

#####
###Thomas Daten reduziert auf trophische level 
###(trophische level, die auf Artenniveau getrennt sind))
#####
tbl_tn <- as.data.frame(gpm_tn@data$input)

##correction dataset: spiders are 100% predators, collembola are 100% decomposers
troph_tn <- data.frame(plotID = tbl_tn$plotID, 
                          SRpredator = rowSums(cbind(tbl_tn$SRpredator, tbl_tn$SRspiders), na.rm = T), 
                          SRherbivore = tbl_tn$SRherbivore, 
                          SRgeneralist = tbl_tn$SRgeneralist, 
                          SRdecomposer = rowSums(cbind(tbl_tn$SRdecomposer, tbl_tn$SRcollembola), na.rm = T))


mrg_tbl_troph <- merge(mrg_tbl_troph_first, troph_tn, by = "plotID", all = T)

#####
###append troph_mrg with sum trophics
#####
troph_mrg_frst <- troph_mrg
for (i in seq(colnames(troph_sum)[(grepl("sum", colnames(troph_sum)))])){
  print(i)
  tmp <- data.frame(Taxon = colnames(troph_sum)[[i+1]], 
                    diet = str_split(colnames(troph_sum)[(grepl("sum", colnames(troph_sum)))], pattern = "_")[[i]][2], 
                    resp =  colnames(troph_sum)[[i+1]])
  print(tmp)
  troph_mrg_frst <- rbind(troph_mrg_frst, tmp)
}

troph_mrg_tn <- data.frame(Taxon = c("predator", "herbivore", "generalist", "decomposer"), 
                           diet = c("predator", "herbivore", "generalist", "decomposer"), 
                           resp = c(colnames(troph_tn[2:ncol(troph_tn)])))
troph_mrg <- rbind(troph_mrg_frst, troph_mrg_tn)

troph_mrg$Tax_label <- troph_mrg$Taxon
troph_mrg$Tax_label[troph_mrg$Tax_label == "bats"] <- "insectivorous bats"
troph_mrg$Tax_label[troph_mrg$Tax_label == "dungbeetles"] <- "dung beetles"
troph_mrg$Tax_label[troph_mrg$Tax_label == "mammals"] <- "large mammals"
troph_mrg$Tax_label[troph_mrg$Tax_label == "otheraculeata"] <- "other aculeate wasps"
troph_mrg$Tax_label[troph_mrg$Tax_label == "othercoleoptera"] <- "other beetles"
troph_mrg$Tax_label[troph_mrg$Tax_label == "parasitoids"] <- "parasitoid wasps"
troph_mrg$Tax_label[troph_mrg$Tax_label == "syrphid"] <- "syrphid flies"
troph_mrg$Tax_label[troph_mrg$Tax_label == "orthoptera"] <- "grasshoppers"
troph_mrg$Tax_label[troph_mrg$Tax_label == "collembola"] <- "springtails"
troph_mrg$Tax_label[troph_mrg$Tax_label == "heteroptera"] <- "bugs"


###fly - binary
troph_mrg$fly <- 0
troph_mrg$fly[troph_mrg$Tax_label == "insectivorous bats"] <- 1
troph_mrg$fly[troph_mrg$Tax_label == "bees"] <- 1
troph_mrg$fly[troph_mrg$Tax_label == "birds"] <- 1
troph_mrg$fly[troph_mrg$Tax_label == "moths"] <- 1
troph_mrg$fly[troph_mrg$Tax_label == "grasshoppers"] <- 1
troph_mrg$fly[troph_mrg$Tax_label == "other aculeate wasps"] <- 1
troph_mrg$fly[troph_mrg$Tax_label == "parasitoid wasps"] <- 1
troph_mrg$fly[troph_mrg$Tax_label == "syrphid flies"] <- 1



saveRDS(troph_mrg, file = paste0(inpath, "15_troph_mrg.rds"))
#####
###append nm_resp mit nm_resp_troph
#####
nm_resp_troph <- c(colnames(troph_sum)[-which(colnames(troph_sum) == "plotID")], 
                   colnames(troph_tn[2:ncol(troph_tn)]))


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
  
  saveRDS(master_lst, file = paste0(inpath, "15_master_lst_", o, ".rds"))
  # master_lst <- readRDS(file = paste0(inpath, "master_lst_", o, ".rds"))
}#for o in set
