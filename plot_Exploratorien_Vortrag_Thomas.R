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
sub <- "dez18_qa/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)

#####
###read files
#####
i <- readRDS(file = paste0(inpath, "2018-12-16nofrst_frst_allplts_noelev/2018-12-18_allplts_ffs_pls_noelev/data/60_master_lst_varimp_allplts.rds"))
troph_mrg <- readRDS(paste0(inpath, "15_troph_mrg.rds"))
#####
###read functions
#####
source("lvlplt.R")
########################################################################################
###Settings
########################################################################################
comm <- ""
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
    modDir <- paste0(outpath, "2018-12-16nofrst_frst_allplts_noelev/2018-12-18_allplts_ffs_pls_noelev", "/")
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
    #####
    ###sort resp by troph levels for further operations
    #####
    resp_srt <- unique(val_type[with(val_type, order(troph_sep, resp, decreasing = T)),"resp"])

    #####
    ###plot settings
    #####
    # myColors <- c("mediumslateblue", "blue2", "aquamarine3", "chocolate1", 
    #               "firebrick1", "darkmagenta")
    myColors <- c("aquamarine3", "chocolate1")
    #####
    ###actual plotting
    #####
    # val_plt <- subset(val_type, grepl("RMSEsd_", val_type$type))
    val_plt <- subset(val_type, grepl("RMSEsd_sum_elev_pred_ldr_pred_resid|RMSEsd_ldr_pred_SR", val_type$type))
    val_plt$troph_sep <- factor(val_plt$troph_sep, levels = levels(val_type$troph_sep))
    val_plt$resp <- factor(val_plt$resp, levels = resp_srt)
    
    lbl <- levels(val_plt$troph_sep)[!grepl("_sum", levels(val_plt$troph_sep))]
    lbl <- c("predator", "", "generalist", "", "decomposer", "", "herbivore", "", "plant", "", "birds", "bats")

    facet_names = list("predator" = "predator", "predator_sum" = "", 
                       "generalist" = "generalist", "generalist_sum" = "", 
                       "decomposer" = "decomposer", "decomposer_sum" = "", 
                       "herbivore" = "herbivore", "herbivore_sum" = "", 
                       "plant" = "plant", "plant_sum" = "", "birds" = "birds", "bats" = "bats")
    facet_labeller <- function(variable,value){
      return(facet_names[value])
    }

      pdf(file = paste0(modDir, "/abb_thomas_val_plot_", "RMSEsd_", ".pdf"), height= 10, width = 20)#, paper = "a4r")
      print(ggplot(data = val_plt, aes(x=resp, y=value)) + 
              #geom_rect(fill=grey_pal()(length(levels(stats_all$troph_sep))+5)[as.numeric(stats_all$troph_sep)+5], 
              #xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
              geom_boxplot(aes(fill=type), width = 1) + 
              facet_grid(~troph_sep, scales = "free_x", space="free_x", switch = "x", labeller = facet_labeller) +
              # theme(strip.background = element_blank(), strip.text.x = element_blank()) + ###get rid of rectangles: + 
              # annotate("text", label = lbl, size = 4, x = 1, y = 1.4, angle = 0, size = 12) + 
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) + 
              scale_fill_manual(name = "",values = myColors, labels = c("species richness prediction", "elevation model + residual prediction")) + 
              ylab("RMSE (normalised by sd)") + 
              xlab("Response") + 
              # ggtitle(paste0("allplts", "_", sub, "_", "RMSEsd_"))+ 
              scale_x_discrete(labels=c("SRants" = "ants", "SRasterids" ="asterids", "SRbats" ="bats", 
                                        "SRbees" ="bees", "SRbirds" ="birds", "SRcollembola" ="collembola", 
                                        "SRconifers" ="conifers", "SRdungbeetles" ="dungbeetles", 
                                        "SReudicots" ="eudicots", "SRferns" = "ferns", 
                                        "SRheteroptera" = "heteroptera", "SRlycopodiopsida" = "lycopodiopsida",  
                                        "SRmagnoliids" ="magnoliids", "SRmammals" ="mammals", 
                                        "SRmillipedes" ="millipedes", "SRmonocots" ="monocots", "SRmoths" ="moths", 
                                        "SRorthoptera" ="orthoptera", "SRotheraculeata" ="other aculeata", 
                                        "SRothercoleoptera" ="other coleoptera", "SRparasitoids" ="parasitoids", 
                                        "SRrosids" ="rosids", "SRsnails" ="snails", "SRspiders" ="spiders", 
                                        "SRsyrphids" ="syrphids", "sum_decomposer_N3" ="sum decomposer", 
                                        "sum_generalist_N4" ="sum generalist", "sum_herbivore_N3" ="sum herbivore", 
                                        "sum_plant_N8" ="sum plant", "sum_predator_N5" = "sum predator")))
      dev.off()

