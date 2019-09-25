# Description:
# Author: Alice Ziegler
# Date: 2018-12-03 11:21:51
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
library(tidyr)
library(ggplot2)
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "apr19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
set <- c("nofrst", "frst", "allplts")
#####
###read files
#####
set_lst <- lapply(set, function(o){
  readRDS(file = paste0(inpath, "15_master_lst_", o, ".rds"))
})
names(set_lst) <- set

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
df_nofrst <- data.frame(plotID = set_lst$nofrst$meta$plotID, LAI = set_lst$nofrst$meta$LAI, 
                        AGB = set_lst$nofrst$meta$AGB, veg = "non-forest")
df_frst <- data.frame(plotID = set_lst$frst$meta$plotID, LAI = set_lst$frst$meta$LAI, 
                                   AGB = set_lst$frst$meta$AGB, veg = "forest")

df <- rbind(df_nofrst, df_frst)

df_vars <- gather(df, key = "vars", value = value, -c(plotID, veg))

  plt <-
  ggplot(data = df_vars, aes(x = veg, y = value))+
    geom_boxplot(notch = T)+
    xlab("")+
    ylab("")+
    facet_wrap(. ~ vars, scales = "free_y") 
    
  
  pdf(file = paste0(outpath, "frst_nofrst_diff.pdf"))
  print(plt)
  dev.off()


