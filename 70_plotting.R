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
outpath <- paste0("../data/", sub)
# set <- c("frst", "nofrst", "allplts")
set <- c("nofrst")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(outpath, "60_master_lst_val_",o, ".rds"))
})
names(set_lst) <- set
troph_mrg <- readRDS(paste0(inpath, "troph_mrg.rds"))
########################################################################################
###Settings
########################################################################################


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
  val_all <- do.call(rbind, i$val)
  val_all$resp <- substr(rownames(val_all),1, nchar(rownames(val_all))-2)
  for (k in val_all$resp){
    val_all$troph[val_all$resp == k] <- as.character(troph_mrg$diet[troph_mrg$resp == k])
  }
  val_all$troph[grepl("sum", val_all$resp)] <- paste0(val_all$troph[grepl("sum", val_all$resp)], "_sum")
  val_type <- gather(val_all, type, value, -resp, -run, -troph)
  myColors <- c("mediumslateblue", "blue2", "aquamarine3", "chocolate1", 
                "firebrick1", "darkmagenta")
  fillscl <- scale_fill_manual(name = "col",values = myColors)
  print(ggplot(data = val_type, aes(x=resp, y=value)) + 
          #geom_rect(fill=grey_pal()(length(levels(stats_all$troph_sep))+5)[as.numeric(stats_all$troph_sep)+5], 
          #xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
          geom_boxplot(aes(fill=type), width = 1) + 
          facet_grid(~troph, scales = "free_x", space="free_x", switch = "x") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
          fillscl + 
          ggtitle(paste0(set, "_", sub)))
  
  }

