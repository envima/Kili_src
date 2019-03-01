# Description: Woher kommt die Höhenproblematik?? feb19 cv20 test für Motten
# Author: Alice Ziegler
# Date: 2019-02-19 16:15:42
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
library(ggplot2)
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "feb19/"
inpath <- "../data/"
outpath <- paste0("../out/", sub)
modDir <- "2019-02-15frst_nofrst_allplts_elev/2019-02-15_frst_ffs_pls_elev/"
#####
###read files
#####
tbl <- readRDS(file = paste0(inpath, sub, modDir, "data/", "60_master_lst_varimp_frst.rds"))
########################################################################################
###Settings
########################################################################################
mod_eudicots <- readRDS(file = paste0(inpath, sub, modDir, "mod_run_2_SReudicots_SR.rds"))
source("multiplot.R")
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
#ausschreiben der folds
folds <- lapply(seq(1:20), function(outs){
  cv_nm <- colnames(tbl$meta)[grepl("outerrun", colnames(tbl$meta))][outs]
  plt_in <- tbl$meta$plotID[tbl$meta[cv_nm] == 0]
  plt_out <- tbl$meta$plotID[tbl$meta[cv_nm] == 1]
  
  return(plt_out)
  # #create multifolds for inner lop in traincontrol
  # tbl_folds <- data.frame(tbl_in$resp, cat = substr(tbl_in$resp$plotID, 1, 3))
  # set.seed(10)
  # cvIndex <- createMultiFolds(y = tbl_folds$cat, k = cv_fold_in, times = cv_times_in)
  # cvIndex_out <- lapply(seq(cvIndex), function(rsmpl){
  #   seq(1, nrow(tbl_folds))[!seq(1, nrow(tbl_folds)) %in% cvIndex[[rsmpl]]]})
})



tbl_eudicots <- merge(tbl$meta, tbl$resp$SReudicots, by = "plotID")
val_eudicots <- tbl$val$SReudicots
varsel_eudicots <- tbl$varsel$SReudicots
srt_varsel_eudicots <- varsel_eudicots$SR[order(varsel_eudicots$SR$freq),]
tbl_dungbeetles <- merge(tbl$meta, tbl$resp$SRdungbeetles, by = "plotID")
val_dungbeetles <- tbl$val$SRdungbeetles
varsel_dungbeetles <- tbl$varsel$SRdungbeetles
srt_varsel_dungbeetles <- varsel_dungbeetles$SR[order(varsel_dungbeetles$SR$freq),]

###plotten der am häufigsten selektierten Variablen gegen die SR der Motten
plot_qntl_rng <- ggplot(tbl_eudicots, aes(x=SR , y= scl_qntl_rng, colour = factor(cat), label=plotID))+
  geom_point() +
  # coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_pr16 <- ggplot(tbl_eudicots, aes(x=SR , y= BE_PR_16, colour = factor(cat), label=plotID))+
  geom_point() +
  # coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_pr03 <- ggplot(tbl_eudicots, aes(x=SR , y= BE_PR_03, colour = factor(cat), label=plotID))+
  geom_point() +
  # coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_rd17 <- ggplot(tbl_eudicots, aes(x=SR , y= BE_RD_17, colour = factor(cat), label=plotID))+
  geom_point() +
  # coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_elev <- ggplot(tbl_eudicots, aes(x=SR , y= elevation, colour = factor(cat), label=plotID))+
  geom_point() +
  # coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

pdf(file = paste0(outpath, modDir, "eudicots_gg_varsel_multiplot.pdf"), paper = "a4")
print(multiplot(plot_qntl_rng, plot_pr16, plot_pr03, plot_rd17, plot_elev, cols=2))
dev.off()
png(paste0(outpath, modDir, "eudicots_gg_varsel_multiplot.png"), 
    width = 210, height = 297, units = "mm", res = 720)
print(multiplot(plot_qntl_rng, plot_pr16, plot_pr03, plot_rd17, plot_elev, cols=2))
dev.off()


###plotten der am häufigsten selektierten Variablen gegen die SR der dungbeetles


plot_pr01 <- ggplot(tbl_dungbeetles, aes(x=SR , y= BE_PR_01, colour = factor(cat), label=plotID))+
  geom_point() +
  # coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_elevsq <- ggplot(tbl_dungbeetles, aes(x=SR , y= elevsq, colour = factor(cat), label=plotID))+
  geom_point() +
  # coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_elev <- ggplot(tbl_dungbeetles, aes(x=SR , y= elevation, colour = factor(cat), label=plotID))+
  geom_point() +
  # coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

pdf(file = paste0(outpath, modDir, "dungbeetles_gg_varsel_multiplot.pdf"), paper = "a4")
multiplot(plot_pr01, plot_elevsq, plot_elev, cols=2)
dev.off()
png(paste0(outpath, modDir, "dungbeetles_gg_varsel_multiplot.png"), 
    width = 210, height = 297, units = "mm", res = 720)
multiplot(plot_pr01, plot_elevsq, plot_elev, cols=2)
dev.off()
