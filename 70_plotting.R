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
library(stringr)
library(tidyr)
library(ggplot2)
library(raster)
library(rasterVis)
library(compositions)
library(RColorBrewer)
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "feb19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
set_dir <- "2019-02-26frst_nofrst_allplts_noelev/"

mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  file <- tryCatch(
    readRDS(file = paste0(modDir, "data/", "60_master_lst_varimp_",o, ".rds")), 
    error = function(e)file <- NA)
  return(file)
})
names(set_lst) <- set
set_lst <- set_lst[!is.na(set_lst)]

troph_mrg <- readRDS(paste0(inpath, "15_troph_mrg.rds"))
troph_mrg <- troph_mrg[!duplicated(troph_mrg),]
#####
###read functions
#####
source("lvlplt.R")
########################################################################################
###Settings
########################################################################################
plts <- c("RMSEsd_", "RMSE_")
comm <- ""
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
cnt <- 0
for (i in set_lst){# i <- set_lst[[1]]
  cnt <<- cnt+1
  set_moddir <- mod_dir_lst[grepl(paste0("_", names(set_lst)[cnt], "_"), mod_dir_lst)]
  if (length(set_moddir) > 0){
  modDir <- paste0(outpath, set_dir, set_moddir, "/")
  #######################
  ###validation Plots
  #######################
  val_all <- do.call(rbind, i$val)
  resp_nm <- lapply(rownames(val_all), function(nm){
    splt <- strsplit(nm, split = "\\.")[[1]][1]
    })
  val_all$resp <- do.call(c,resp_nm)
  val_troph <- merge(val_all, troph_mrg, by = "resp") 
  val_troph$troph_sep[grepl("sum", val_troph$resp)] <- paste0(val_troph$diet[grepl("sum", val_troph$resp)], "_sum")
  val_troph$troph_sep[is.na(val_troph$troph_sep)] <- as.character(val_troph$diet[is.na(val_troph$troph_sep)])
  levels(val_troph$troph_sep) <- c("predator", "predator_sum", "generalist", "generalist_sum", "decomposer", 
                                   "decomposer_sum", "herbivore", "herbivore_sum", "plant", "plant_sum", 
                                   "birds", "bats")
  
  val_type <- gather(val_troph, type, value, -resp, -run, -diet, -troph_sep, -Taxon)
  #####
  ###sort resp by troph levels for further operations
  #####
  resp_srt <- unique(val_type[with(val_type, order(troph_sep, resp)),"resp"])
  #####
  ###plot settings
  #####
  myColors <- c("mediumslateblue", "blue2", "aquamarine3", "chocolate1", 
                "firebrick1", "darkmagenta")
  fillscl <- scale_fill_manual(name = "col",values = myColors)
  #####
  ###actual plotting
  #####
  for (n in plts){
    val_plt <- subset(val_type, grepl(n, val_type$type))
    levels(val_plt$troph_sep) <- levels(val_type$troph_sep)

    if (file.exists(paste0(outpath, set_dir, set_moddir))==F){
      dir.create(file.path(paste0(outpath, set_dir, set_moddir)), recursive = T)
    }
    
    p <- ggplot(data = val_plt, aes(x=resp, y=value)) + 
          #geom_rect(fill=grey_pal()(length(levels(stats_all$troph_sep))+5)[as.numeric(stats_all$troph_sep)+5], 
          #xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
          geom_boxplot(aes(fill=type), width = 1) + 
          facet_grid(~troph_sep, scales = "free_x", space="free_x", switch = "x") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
          fillscl + 
          ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", n))
    pdf(file = paste0(modDir, "/val_plot_", names(set_lst)[cnt], "_", n, ".pdf"), height= 10, width = 20)#, paper = "a4r")
    print(p)
    dev.off()
    png(paste0(modDir, "/val_plot_", names(set_lst)[cnt], "_", n, ".png"), 
        width = 297, height = 210, units = "mm", res = 720)
    print(p)
    dev.off()
  }
  #######################
  ###validation Plots
  #######################
  #####
  ###create df for heatmap, separate for SR and resid
  #####
  resp_set <- c("SR", "resid") # loop model for SR and resid
  SR_resid_lst <- lapply(resp_set, function(m){
    resp_lst <- lapply(names(i$resp), function(k){
      print(k)
      varsel_lst <- i$varsel[[k]][[m]]
    })
    names(resp_lst) <- names(i$resp)
    df <- Reduce(function(x,y) merge(x,y, by = "pred", all = T), resp_lst)
    df <- df[complete.cases(df$pred),]
    colnames(df) <- c("pred", names(i$resp)) #unschön und unsicher...Spalten könnten irgendwie durcheinander kommen??? gecheckt: 17.12.
    df[is.na(df)] <- 0 #NAs are not working in levelplot
    #####
    ###sort dataframe for heatmap
    ###cols by trophic levels
    ###rows by number of occurances
    #####
    df <- df[c("pred", resp_srt)]
    df <- df[order(rowSums(df[,2:ncol(df)], na.rm = T), decreasing = T),]
    row.names(df) <- as.character(df$pred)
    mat <- as.matrix(df[,!colnames(df) == "pred"])
    
    #######################
    ###actual plotting
    #######################
    l <- lvlplt(mat = mat, 
           lbl_x = colnames(mat), 
           lbl_y = rownames(mat), 
           rnge = seq(min(mat)+0.5, max(mat)+0.5, 1), 
           main = paste0(names(set_lst)[cnt], "_", sub, "_", m))
    pdf(file = paste0(modDir, "heat_selvars_", names(set_lst)[cnt], "_", m, "_", comm, ".pdf"), width = 7, height = 10)#paper = "a4")
    print(l)
    dev.off()
    png(paste0(modDir, "heat_selvars_", names(set_lst)[cnt], "_", m, "_", comm, ".png"), 
        width = 210, height = 297, units = "mm", res = 720)
    print(l)
    dev.off()
    return(mat)
  })
  names(SR_resid_lst) <- resp_set
  }
}
