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
# sub <- "apr19/"
sub <- "feb20_allresp/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
# set_dir <- "2019-10-10frst_nofrst_allplts_noelev/" 
#paper: 
# set_dir <- "2019-03-26frst_nofrst_allplts_noelev/"
set_dir <- "2020-02-12frst_nofrst_allplts_noelev/"


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

#size ranking
size <- data.frame(response = c("SRants", "SRbats", "SRbees", "SRbirds", "SRcollembola", "SRdungbeetles", 
          "SRheteroptera", "SRmammals", "SRmillipedes", "SRmoths", "SRorthoptera", "SRotheraculeata", 
          "SRothercoleoptera", "SRparasitoids", "SRsnails", "SRspiders", "SRsyrphids"), 
          size = c(7, 3, 6, 2, 8, 6, 7, 1, 7, 6, 5, 7, 7, 9, 4, 7, 7))
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
                    "fig4_median", "fig3_elev", "fig3_structure", "fig3_combination", 
                    "fig4")

results_mdn_all <- results_summary[,c(1:9)]
results_mdn <- results_mdn_all[!duplicated(results_mdn_all[c("fig3_elev_median")]),]
results_mdn <- merge(results_mdn, size, by = "response")

write.csv(x = results_summary, file = paste0(outpath, "results_summary.csv"))
write.csv(x = results_mdn, file = paste0(outpath, "results_mdn.csv"))


#####
### Spearman correlation test: cor.test with size and median performance
#####
ctest_lst <- lapply(results_mdn[,c(6:9)], function(i){
  ctest <- cor.test(results_mdn$size, i)
  return(data.frame(estimate_size = ctest$estimate, p_val_size = ctest$p.value))
})
ctest <- do.call(rbind, ctest_lst)

#####
### U-Test: wilcox.test with fly and median performance
#####
wtest_lst <- lapply(results_mdn[,c(6:9)], function(i){
  wtest <- wilcox.test(i~results_mdn$fly)
  return(data.frame(p_val_fly = wtest$p.value))
})
wtest <- do.call(rbind, wtest_lst)

fly_size_tests <- merge(ctest, wtest, by = 0)
fly_size_tests <- fly_size_tests[c(2,3,1,4),]


write.csv(fly_size_tests, file = paste0(outpath, "fly_size_test.csv"))

# 
# 
# ###############################
# # data file = body_size_fly.csv
# # data<- read.csv2("body_size_fly.csv")
# # attach(data)
# fly<-as.factor(fly)
# 
# ############################
# # Spearman correlation tests
# cor.test(results_mdn$size, results_mdn$fig3_elev_median)
# cor.test(rank_size,Fig3a_struc)
# cor.test(rank_size,Fig3a_combi)
# cor.test(rank_size,Fig4)
# 
# ############################
# # U-tests tests
# wilcox.test(results_mdn$fig3_elev_median~results_mdn$fly)
# wilcox.test(Fig3a_struc~fly)
# wilcox.test(Fig3a_combi~fly)
# wilcox.test(Fig4~fly)
# 
# 
# 
# 
# 
# 
# # val_troph, where elevation is best model
# val_troph_elev <- val_troph[(val_troph$RMSEsd_elevSR_mdn_rank == 1 | 
#                               #hier | statement, weil auch das lidar UND elevation auf 1 sein kann. 
#                               #da das inzwischen nciht mehr verwendet wird, wäre das elev aber unter 
#                               #umständen trotzdem noch das beste
#                               (val_troph$RMSEsd_lidarelevSR_mdn_rank == 1 & 
#                                  val_troph$RMSEsd_elevSR_mdn_rank == 2)),
#                             c("resp", "run", "RMSEsd_elevSR", "RMSEsd_elevSR_mdn", "Tax_label", "fly", "troph_sep")]
# # only flying
# fly <- val_troph_elev[val_troph_elev$fly == 1,]
# # only noflying
# nofly <- val_troph_elev[val_troph_elev$fly == 0,]
# 
# #######################
# ###test all RMSEsd_elevSR valueas against each other
# #######################
# 
# #####
# ###explore data
# #####
# shapiro.test(fly$RMSEsd_elevSR) 
# hist(fly$RMSEsd_elevSR)
# 
# shapiro.test(nofly$RMSEsd_elevSR) 
# hist(nofly$RMSEsd_elevSR)
# 
# by(val_troph_elev$RMSEsd_elevSR, val_troph_elev$fly, stat.desc)
# #####
# ###test 
# #####
# t.test(fly$RMSEsd_elevSR, nofly$RMSEsd_elevSR)
# 
# #######################
# ###test all RMSEsd_elevSR_mdn valueas against each other(only median RMSEs)
# #######################
# shapiro.test(fly$RMSEsd_elevSR_mdn) 
# hist(fly$RMSEsd_elevSR_mdn)
# 
# shapiro.test(nofly$RMSEsd_elevSR_mdn) 
# hist(nofly$RMSEsd_elevSR_mdn)
# 
# by(val_troph_elev$RMSEsd_elevSR_mdn, val_troph_elev$fly, stat.desc)
# #####
# ###test 
# #####
# t.test(fly$RMSEsd_elevSR_mdn, nofly$RMSEsd_elevSR_mdn)
