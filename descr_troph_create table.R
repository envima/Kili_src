# Description: Describing properties and relationships between taxa and trophic levels
# Author: Alice Ziegler
# Date: 2019-07-29 12:08:23
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####

#####
###set paths
#####

setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "feb20_allresp/"
# sub <- "apr19/" #paper
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###read files
#####
tbl <- read.csv(file = paste0(inpath_general, "animals_plotIDcomplete_Syn1.csv"), sep = ";")

# troph_mrg <- readRDS(paste0(inpath, "15_troph_mrg.rds"))
# troph_mrg <- troph_mrg[!duplicated(troph_mrg),]
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



x_tbl <- table(tbl$taxon, tbl$diet)

x_tbl_rel <- prop.table(x_tbl, margin = 1) #get relative values for each row= taxa
x_tbl_rel <- round(x_tbl_rel, 2)
write.csv(x_tbl_rel, file = paste0(outpath, "descr_troph_composition_taxa_animals_plotIDcomplete_Syn1.csv"))

x_tbl_rel_col <- prop.table(x_tbl, margin = 2) #get relative values for each col= troph
x_tbl_rel_col <- round(x_tbl_rel_col, 2)
write.csv(x_tbl_rel_col, file = paste0(outpath, "descr_troph_composition_troph_animals_plotIDcomplete_Syn1.csv"))
###tabelle 
