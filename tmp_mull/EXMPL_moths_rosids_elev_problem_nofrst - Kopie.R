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
modDir <- "2019-02-15frst_nofrst_allplts_elev/2019-02-15_nofrst_ffs_pls_elev/"
#####
###read files
#####
tbl <- readRDS(file = paste0(inpath, sub, modDir, "data/", "60_master_lst_varimp_nofrst.rds"))
########################################################################################
###Settings
########################################################################################
mod_moth <- readRDS(file = paste0(inpath, sub, modDir, "mod_run_2_SRmoths_SR.rds"))
source("multiplot.R")
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
tbl_moths <- merge(tbl$meta, tbl$resp$SRmoths, by = "plotID")
val_moths <- tbl$val$SRmoths
tbl_rosids <- merge(tbl$meta, tbl$resp$SRrosids, by = "plotID")
val_rosids <- tbl$val$SRrosids

###plotten der am häufigsten selektierten Variablen gegen die SR der Motten
plot_rd03 <- ggplot(tbl_moths, aes(x=SR , y= BE_RD_03, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_vegcov01 <- ggplot(tbl_moths, aes(x=SR , y= vegetation_coverage_01m, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_vegcov05 <- ggplot(tbl_moths, aes(x=SR , y= vegetation_coverage_05m, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_rd05 <- ggplot(tbl_moths, aes(x=SR , y= BE_RD_05, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_pr05 <- ggplot(tbl_moths, aes(x=SR , y= BE_PR_05, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_agb <- ggplot(tbl_moths, aes(x=SR , y= AGB, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_elev <- ggplot(tbl_moths, aes(x=SR , y= elevation, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 


pdf(file = paste0(outpath, modDir, "moths_gg_varsel_multiplot.pdf"), paper = "a4")
multiplot(plot_rd03, plot_vegcov01, plot_vegcov05, plot_rd05, plot_agb, plot_elev, cols=2)
dev.off()
png(paste0(outpath, modDir, "moths_gg_varsel_multiplot.png"), 
    width = 210, height = 297, units = "mm", res = 720)
multiplot(plot_rd03, plot_vegcov01, plot_vegcov05, plot_rd05, plot_agb, plot_elev, cols=2)
dev.off()


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


###plotten der am häufigsten selektierten Variablen gegen die SR der Rosids
varsel_rosids <- tbl$varsel$SRrosids
srt_varsel_rosids <- varsel_rosids$SR[order(varsel_rosids$SR$freq),]

plot_rd03 <- ggplot(tbl_rosids, aes(x=SR , y= BE_RD_03, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_vegcov01 <- ggplot(tbl_rosids, aes(x=SR , y= vegetation_coverage_01m, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_rd02 <- ggplot(tbl_rosids, aes(x=SR , y= BE_RD_02, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_rd01 <- ggplot(tbl_rosids, aes(x=SR , y= BE_RD_01, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_hp70 <- ggplot(tbl_rosids, aes(x=SR , y= BE_H_P70, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_rd08 <- ggplot(tbl_rosids, aes(x=SR , y= BE_RD_08, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 

plot_elev <- ggplot(tbl_rosids, aes(x=SR , y= elevation, colour = factor(cat), label=plotID))+
  geom_point() +
  coord_cartesian(xlim = c(0,80))+
  geom_text(aes(label=plotID),hjust=0, vjust=0, size = 3) 


pdf(file = paste0(outpath, modDir, "rosids_gg_varsel_multiplot.pdf"), paper = "a4")
multiplot(plot_rd03, plot_vegcov01, plot_vegcov05, plot_rd02, 
          plot_rd03, plot_rd08, plot_hp70, plot_elev, cols=2)
dev.off()
png(paste0(outpath, modDir, "rosids_gg_varsel_multiplot.png"), 
    width = 210, height = 297, units = "mm", res = 720)
multiplot(plot_rd03, plot_vegcov01, plot_vegcov05, plot_rd02, 
          plot_rd03, plot_rd08, plot_hp70, plot_elev, cols=2)
dev.off()