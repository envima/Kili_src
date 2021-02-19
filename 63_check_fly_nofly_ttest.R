# Description:
# Author: Alice Ziegler
# Date: 2018-12-10 11:49:55
#
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
library(pastecs)
source("000_setup.R")

#####
### Settings
#####
comm <- ""
grp <- c("specs", "trophs")
trophs <- c("generalist", "herbivore", "decomposer", "predator")
#####
###read files
#####
modDir <- paste0(inpath, set_dir, "mix/")
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
###table overview of RMSE_sd Median values
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

write.csv(x = results_summary, file = paste0(figpath, "results_summary.csv"))
write.csv(x = results_mdn, file = paste0(figpath, "results_mdn.csv"))


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


write.csv(fly_size_tests, file = paste0(figpath, "fly_size_test.csv"))
