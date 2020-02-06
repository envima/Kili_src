# Description:
# Author: Alice Ziegler
# Date: 2018-12-10 11:49:55
# t-test: check if RMSE/sd resultes for elevation as best model difer between flying and non-flying animals
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####

library(pastecs)
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
# sub <- "oct19/" 
#paper: 
sub <- "apr19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
# set_dir <- "2019-10-10frst_nofrst_allplts_noelev/" 
#paper: 
set_dir <- "2019-03-26frst_nofrst_allplts_noelev/"

mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
modDir <- paste0(inpath, set_dir, "mix/")
comm <- ""
grp <- c("specs", "trophs")
trophs <- c("generalist", "herbivore", "decomposer", "predator")
#####
###read files
#####

mix_lst <- readRDS(file = paste0(modDir, "data/", "61_master_lst_varimp_.rds")) 


troph_mrg <- readRDS(paste0(inpath, "15_troph_mrg.rds"))
troph_mrg <- troph_mrg[!duplicated(troph_mrg),]

val_overview <- readRDS(file = paste0(modDir, "val_mix_overview_mix_", comm, ".rds"))

val_troph <- readRDS(file = paste0(modDir, "val_troph_mix_", comm, ".rds"))
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################

#####
###Tabellenübersicht der RMSE_sd Median Werte
#####
results_summary <- val_troph[!(val_troph$resp %in% c("sum_decomposer_N3", "sum_generalist_N4", 
                                                     "sum_herbivore_N3", "sum_predator_N5")), 
                             c("resp", "run", "Tax_label", "fly", "troph_sep", 
                        "RMSEsd_elevSR_mdn", "RMSEsd_lidarSR_mdn", "RMSEsd_sumSR_mdn", 
                        "RMSEsd_lidarRES_mdn", 
                        "RMSEsd_elevSR", "RMSEsd_lidarSR", "RMSEsd_sumSR", 
                        "RMSEsd_lidarRES")]

names(results_summary) <- c("response", "run", "Tax_label", "fly", "troph_sep", 
                    "fig3_elev_median", "fig3_structure_median", "fig3_combination_median", 
                    "fig_4_median", "fig3_elev", "fig3_structure", "fig3_combination", 
                    "fig_4")

results_mdn_all <- results_summary[,c(1:9)]
results_mdn <- results_mdn_all[!duplicated(results_mdn_all[c("fig3_elev_median")]),]

write.csv(x = results_summary, file = paste0(outpath, "results_summary.csv"))
write.csv(x = results_mdn, file = paste0(outpath, "results_mdn.csv"))



# val_troph, where elevation is best model
val_troph_elev <- val_troph[(val_troph$RMSEsd_elevSR_mdn_rank == 1 | 
                              #hier | statement, weil auch das lidar UND elevation auf 1 sein kann. 
                              #da das inzwischen nciht mehr verwendet wird, wäre das elev aber unter 
                              #umständen trotzdem noch das beste
                              (val_troph$RMSEsd_lidarelevSR_mdn_rank == 1 & 
                                 val_troph$RMSEsd_elevSR_mdn_rank == 2)),
                            c("resp", "run", "RMSEsd_elevSR", "RMSEsd_elevSR_mdn", "Tax_label", "fly", "troph_sep")]
# only flying
fly <- val_troph_elev[val_troph_elev$fly == 1,]
# only noflying
nofly <- val_troph_elev[val_troph_elev$fly == 0,]

#######################
###test all RMSEsd_elevSR valueas against each other
#######################

#####
###explore data
#####
shapiro.test(fly$RMSEsd_elevSR) 
hist(fly$RMSEsd_elevSR)

shapiro.test(nofly$RMSEsd_elevSR) 
hist(nofly$RMSEsd_elevSR)

by(val_troph_elev$RMSEsd_elevSR, val_troph_elev$fly, stat.desc)
#####
###test 
#####
t.test(fly$RMSEsd_elevSR, nofly$RMSEsd_elevSR)

#######################
###test all RMSEsd_elevSR_mdn valueas against each other(only median RMSEs)
#######################
shapiro.test(fly$RMSEsd_elevSR_mdn) 
hist(fly$RMSEsd_elevSR_mdn)

shapiro.test(nofly$RMSEsd_elevSR_mdn) 
hist(nofly$RMSEsd_elevSR_mdn)

by(val_troph_elev$RMSEsd_elevSR_mdn, val_troph_elev$fly, stat.desc)
#####
###test 
#####
t.test(fly$RMSEsd_elevSR_mdn, nofly$RMSEsd_elevSR_mdn)
