# Description:
# Author: Alice Ziegler
# Date: 2018-12-10 11:49:55
# to do:
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
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "dez18/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
set_dir <- "20181210_frst_nofrst_allplts/"
mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
# set <- c("frst", "nofrst", "allplts")
set <- c("frst")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  readRDS(file = paste0(modDir, "data/", "60_master_lst_varimp_",o, ".rds"))
})
names(set_lst) <- set
troph_mrg <- readRDS(paste0(inpath, "troph_mrg.rds"))
########################################################################################
###Settings
########################################################################################
plts <- c("RMSEsd_", "RMSE_")

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
  modDir <- paste0(outpath, set_dir, set_moddir, "/")
  #######################
  ###validation Plots
  #######################
  val_all <- do.call(rbind, i$val)
  val_all$resp <- substr(rownames(val_all),1, nchar(rownames(val_all))-2)
  val_troph <- merge(val_all, troph_mrg, by = "resp") ###woher kommen die zusätzlchen einträge
  val_troph$troph_sep[grepl("sum", val_troph$resp)] <- paste0(val_troph$diet[grepl("sum", val_troph$resp)], "_sum")
  val_troph$troph_sep[is.na(val_troph$troph_sep)] <- as.character(val_troph$diet[is.na(val_troph$troph_sep)])
  levels(val_troph$troph_sep) <- c("predator", "predator_sum", "generalist", "generalist_sum", "decomposer", 
                                   "decomposer_sum", "herbivore", "herbivore_sum", "plant", "plant_sum", 
                                   "birds", "bats")
  val_type <- gather(val_troph, type, value, -resp, -run, -diet, -troph_sep, -Taxon)
  myColors <- c("mediumslateblue", "blue2", "aquamarine3", "chocolate1", 
                "firebrick1", "darkmagenta")
  fillscl <- scale_fill_manual(name = "col",values = myColors)
  for (n in plts){
    val_plt <- subset(val_type, grepl(n, val_type$type))
    levels(val_plt$troph_sep) <- levels(val_type$troph_sep)

    if (file.exists(paste0(outpath, set_dir, set_moddir))==F){
      dir.create(file.path(paste0(outpath, set_dir, set_moddir)), recursive = T)
    }
    pdf(file = paste0(modDir, "/val_plot_", set, "_", n, ".pdf"), height= 10, width = 20)#, paper = "a4r")
    print(ggplot(data = val_plt, aes(x=resp, y=value)) + 
          #geom_rect(fill=grey_pal()(length(levels(stats_all$troph_sep))+5)[as.numeric(stats_all$troph_sep)+5], 
          #xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
          geom_boxplot(aes(fill=type), width = 1) + 
          facet_grid(~troph_sep, scales = "free_x", space="free_x", switch = "x") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
          fillscl + 
          ggtitle(paste0(set, "_", sub, " ", n)))
    dev.off()
  }
  
  #######################
  ###varsel plots
  #######################
  i$varimp$SRmammals

  
  
  }

