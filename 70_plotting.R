# Description:
# Author: Alice Ziegler
# Date: 2018-12-10 11:49:55
# to do: colnames(df) <- sicherer gegen umsortieren machen
# bei selvar plot legende eindeutig welche farbe welche zahl ist. (nicht tick im ?bergang)
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
library(dplyr)
library(grid)
library(pBrackets)  
source("000_setup.R")


# #####
# ###set paths
# #####
# setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# # setwd("/mnt/sd19006/data/users/aziegler/src")
# sub <- "feb20_allresp/"
# # sub <- "apr19/" #paper
# inpath <- paste0("../data/", sub)
# inpath_general <- "../data/"
# outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
# #####
# set_dir <- "2020-02-12frst_nofrst_allplts_noelev/"
# #paper: 
# # set_dir <- "2019-03-26frst_nofrst_allplts_noelev/"
# 
# mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
# set <- c("nofrst", "frst", "allplts")

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
source("fun_lvlplt.R")
########################################################################################
###Settings
########################################################################################
plts <- c("RMSEsd_")#, "RMSE_")
comm <- ""
maxcat <- 20 #depending on the number of levels run
# resp_set <- c("lidarSR", "lidarelevSR", "lidarRES") #m <- "lidarSR" #loop model for SR and resid

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
    # val_troph$troph_sep <- factor(val_troph$troph_sep,
    #                               levels = c("predator", "predator_sum", "generalist",
    #                                          "generalist_sum", "decomposer",
    #                                          "decomposer_sum", "herbivore", "herbivore_sum",
    #                                          "plant", "plant_sum", "birds", "bats"))
    # val_troph$resp <- factor(val_troph$resp,
    # levels = unique(val_troph[with(val_troph, order(troph_sep, resp)),"resp"]))
    val_troph$diet <- factor(val_troph$diet,
                             levels = levels(val_troph$diet))

    val_troph_flt <- val_troph[is.finite(val_troph$RMSEsd_lidarSR),]


    val_type <- gather(val_troph_flt, key = type, value = value, -c(resp, run, sd, mdn:troph_sep))

    val_type_per_resp <- val_type[,!names(val_type) %in% c("run", "value")]
    val_overview <- val_type_per_resp[!duplicated(val_type_per_resp),]
    if (file.exists(modDir)==F){
      dir.create(file.path(modDir), recursive = T)
    }
    saveRDS(val_overview, file = paste0(modDir, "val_overview", names(set_lst)[cnt], "_", comm, ".rds"))


  #   
  #   
  #   #######################
  #   ###val plots sorted by trophic levels
  #   #######################
  #   val_type$color[val_type$diet == "birds"] <- "cadetblue3"
  #   val_type$color[val_type$diet == "bats"] <- "grey25"
  #   val_type$color[val_type$diet == "predator"] <- "orange"
  #   val_type$color[val_type$diet == "generalist"] <- "dodgerblue4"
  #   val_type$color[val_type$diet == "decomposer"] <- "indianred3"
  #   val_type$color[val_type$diet == "herbivore"] <- "limegreen"
  #   val_type$color[val_type$diet == "plant"] <- "springgreen4"
  #   val_type$color <- factor(val_type$color, levels = c("orange", "dodgerblue4", "indianred3", "limegreen", 
  #                                                       "springgreen4", "cadetblue3", "grey25"))
  #   
  #   val_type$type_col[grepl("elevSR", val_type$type)] <- "firebrick1"
  #   val_type$type_col[grepl("sumSR", val_type$type)] <- "darkorange2"
  #   val_type$type_col[grepl("lidarSR", val_type$type)] <- "dodgerblue4"
  #   val_type$type_col[grepl("lidarelevSR", val_type$type)] <- "darkorchid3"
  #   val_type$type_col[grepl("lidarRES", val_type$type)] <- "goldenrod1"
  #   
  #   
  #   
  #   #####
  #   ###sort resp by troph levels for further operations
  #   #####
  #   sort_by_diet_alphabet <- (val_type %>% distinct(diet, .keep_all = TRUE))[match(sort(levels(droplevels(val_type$diet))), (val_type %>% distinct(diet, .keep_all = TRUE))[,which(colnames(val_type %>% distinct(diet, .keep_all = TRUE)) == "diet")]),]
  #   myColors <- c(as.character(sort_by_diet_alphabet$color), "black", "grey70", "grey30", "grey90", "white")
  #   for (n in plts){ #n <- "RMSEsd_"
  #     val_plt <- subset(val_type, grepl(n, val_type$type))
  #     levels(val_plt$troph_sep) <- levels(val_type$troph_sep)
  #     levels(val_plt$color) <- levels(val_type$color)
  #     levels(val_plt$resp) <- levels(val_type$resp)
  #     levels(val_plt$diet) <- levels(val_type$diet)
  #     
  #     # val_plt$troph_sep <- droplevels(val_plt$troph_sep)
  #     val_plt$color <- droplevels(val_plt$color)
  #     # val_plt$resp <- droplevels(val_plt$resp)
  #     val_plt$diet <- droplevels(val_plt$diet)
  #     
  #     table <- aggregate(val_plt$value, by = list(val_plt$Taxon, val_plt$type), FUN = median)
  #     colnames(table) <- c("Taxon", "type", "mdn_value")
  #     unq_sd <- data.frame(val_troph_flt[,c("Taxon", "sd")])[!duplicated(data.frame(val_troph_flt[,c("Taxon", "sd")])),]
  #     
  #     table_sd <- merge(table, unq_sd, by = "Taxon", all = F)
  #     write.csv(table_sd, file = paste0(inpath, set_dir, set_moddir, "/data/validation_table_", comm, n, ".csv"))
  #     
  #     if (file.exists(paste0(outpath, set_dir, set_moddir))==F){
  #       dir.create(file.path(paste0(outpath, set_dir, set_moddir)), recursive = T)
  #     }
  #     
  #     p <- ggplot() +
  #       geom_rect(data = val_plt,aes(fill = val_plt$diet),xmin = -Inf,xmax = Inf,
  #                 ymin = -Inf,ymax = Inf, alpha = 0.007) +
  #       geom_boxplot(data = val_plt, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
  #       facet_grid(~val_plt$troph_sep, scales = "free_x", space="free_x", switch = "x") +
  #       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
  #             strip.text.x = element_blank()) +
  #       theme(axis.text.x = element_text(size = 16))+
  #       scale_fill_manual(name = "col",values = myColors) +
  #       ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", n))
  #     pdf(file = paste0(modDir, "/val_plot_srt_trophicslvl_", comm, n, names(set_lst)[cnt], ".pdf"), height= 10, 
  #         width = 20)
  #     # par(mar=c(50, 50, 50, 50) + 1)#, paper = "a4r")
  #     print(p)
  #     dev.off()
  #     png(paste0(modDir, "/val_plot_srt_trophicslvl_", comm, n, names(set_lst)[cnt], ".png"), 
  #         width = 297, height = 210, units = "mm", res = 720)
  #     # par(mai=c(50, 50, 50, 50) + 1)
  #     print(p)
  #     dev.off()
  #   }
  #   
  #   
  #   #######################
  #   ### val plots sorted by best model performance
  #   #######################
  #   # nm <- substr(n , 1, nchar(n)-1)
  #   val_plt <- subset(val_type, grepl("RMSEsd_", val_type$type))
  #   
  #   rank_df <- val_plt[,grepl("_mdn_rank", colnames(val_plt)) & 
  #                        grepl("RMSEsd_", colnames(val_plt))]
  #   rank_df <- rank_df[,grepl("_elevSR", colnames(rank_df)) | 
  #                        grepl("_sumSR", colnames(rank_df)) |
  #                        grepl("_lidarSR", colnames(rank_df))]
  #   # val_plt$szenario <- colnames(rank_df)[]
  #   
  #   for(row_nr in seq(nrow(val_plt))){
  #     min_colnm <- colnames(rank_df)[rank_df[row_nr,] == min(rank_df[row_nr,])]
  #     val_plt$best_mod[row_nr] <- strsplit(min_colnm, "_")[[1]][2]
  #   }
  #   
  #   # val_plt$szenario[val_plt$constll1_RMSEsd_mdn == 4 & 
  #   #                    val_plt$RMSEsd_elevSR_mdn_rank < val_plt$RMSEsd_lidarSR_mdn_rank] <- 4.1###verallgemeinern! mit RMSEmdn hard gecoded!
  #   # val_plt$szenario[val_plt$constll1_RMSEsd_mdn == 4 & 
  #   #                    val_plt$RMSEsd_lidarRES_mdn_rank < val_plt$RMSEsd_elevSR_mdn_rank] <- 4.2
  #   val_plt_flt <- val_plt[val_plt$type %in% c("RMSEsd_elevSR", 
  #                                              "RMSEsd_sumSR", 
  #                                              "RMSEsd_lidarSR"),]
  #   
  #   plt <- ggplot() +
  #     geom_boxplot(data = val_plt_flt, aes(x=Taxon, y=value, fill=type), notch = T) +   #in aes(position=position_dodge(5))
  #     facet_grid(~val_plt_flt$best_mod, scales = "free_x", space="free_x", switch = "x") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16, 
  #                                      # colour = val_plt_flt$troph_sep, 
  #                                      margin = margin(0,0,0,0))) + #,
  #     #       strip.text.x = element_blank()) +
  #     # # scale_fill_manual(name = "col",values = myColors) +
  #     labs(x = "", y = "RMSE/sd")+
  #     scale_fill_manual(name = "col",values = c(unique(val_plt_flt$type_col))) +
  #     ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", "RMSEsd_"))
  #   pdf(file = paste0(modDir, "val_plot_srt_bestmodel_", comm, "RMSEsd_", names(set_lst)[cnt], ".pdf"), 
  #       height= 21, width = 29)
  #   print(plt)
  #   dev.off()
  #   
  #   # grid.locator(unit="native") 
  #   # grid.brackets(220, 400, 40, 400, lwd=2, col="black")
  #   # grid.text("bla", x = unit(3.5, "cm"), y = unit(0.7, "cm"))
  #   ##plan: vertikale Linie an den enden der Klammern hochzeihen bis in Plot
  #   
  #   
  #   #####
  #   ###check if some taxa are put into mixed category, if lidarelev is also part of the ranking
  #   #####
  #   val_plt <- subset(val_type, grepl("RMSEsd_", val_type$type))
  #   
  #   rank_df <- val_plt[,grepl("_mdn_rank", colnames(val_plt)) & 
  #                        grepl("RMSEsd_", colnames(val_plt))]
  #   rank_df <- rank_df[,grepl("_elevSR", colnames(rank_df)) | 
  #                        grepl("_sumSR", colnames(rank_df)) |
  #                        grepl("_lidarSR", colnames(rank_df))|
  #                        grepl("_lidarelevSR", colnames(rank_df))]
  #   # val_plt$szenario <- colnames(rank_df)[]
  #   
  #   for(row_nr in seq(nrow(val_plt))){
  #     min_colnm <- colnames(rank_df)[rank_df[row_nr,] == min(rank_df[row_nr,])]
  #     val_plt$best_mod[row_nr] <- strsplit(min_colnm, "_")[[1]][2]
  #   }
  #   
  #   # val_plt$szenario[val_plt$constll1_RMSEsd_mdn == 4 & 
  #   #                    val_plt$RMSEsd_elevSR_mdn_rank < val_plt$RMSEsd_lidarSR_mdn_rank] <- 4.1###verallgemeinern! mit RMSEmdn hard gecoded!
  #   # val_plt$szenario[val_plt$constll1_RMSEsd_mdn == 4 & 
  #   #                    val_plt$RMSEsd_lidarRES_mdn_rank < val_plt$RMSEsd_elevSR_mdn_rank] <- 4.2
  #   val_plt_flt <- val_plt[val_plt$type %in% c("RMSEsd_elevSR", 
  #                                              "RMSEsd_sumSR", 
  #                                              "RMSEsd_lidarSR", 
  #                                              "RMSEsd_lidarelevSR"),]
  #   
  #   plt <- ggplot() +
  #     geom_boxplot(data = val_plt_flt, aes(x=Taxon, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
  #     facet_grid(~val_plt_flt$best_mod, scales = "free_x", space="free_x", switch = "x") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16, 
  #                                      # colour = val_plt_flt$troph_sep, 
  #                                      margin = margin(0,0,0,0))) + #,
  #     #       strip.text.x = element_blank()) +
  #     # # scale_fill_manual(name = "col",values = myColors) +
  #     labs(x = "", y = "RMSE/sd")+
  #     scale_fill_manual(name = "col",values = c(unique(val_plt_flt$type_col))) +
  #     ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", "RMSEsd_"))
  #   pdf(file = paste0(modDir, "val_plot_srt_bestmodel_with_lidarelevSR", comm, "RMSEsd_", names(set_lst)[cnt], ".pdf"), 
  #       height= 21, width = 29)
  #   print(plt)
  #   dev.off()
  #   
  #   
  #   # #####
  #   # ###test f?r bboxplot mit punkten, eingef?rbt nach run (eigentlich sollte hier 
  #   # ###helichrysum gezeigt werden, aber das geht a gar nciht nach landuse.)
  #   # #####
  #   # ggplot() +
  #   #   geom_boxplot(data = val_plt_flt, aes(x=resp, y=value, fill=type), 
  #   #                width = 1) +   #in aes(position=position_dodge(5))
  #   #   facet_grid(~val_plt_flt$best_mod, scales = "free_x", space="free_x", switch = "x") +
  #   #   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16,
  #   #                                    # colour = val_plt_flt$troph_sep,
  #   #                                    margin = margin(0,0,0,0))) + #,
  #   #   geom_point(data = val_plt_flt, 
  #   #              aes(x=resp, y=value, fill = val_plt_flt$type), 
  #   #              size = 2, 
  #   #              shape = 21,
  #   #              position = position_jitterdodge(), 
  #   #              color = factor(val_plt_flt$run)) +
  #   #   ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", n))
  #   # #####
  #   # ###ende test f?r bboxplot
  #   # #####
  #   
  #   #####
  #   ###boxplot f?r RMSE/mdn to see the elevation influence within the lidarSR 
  #   ###(lidarSR, lidarRES, elevSR)
  #   #####
  #   resp_lidarSR <- unique(val_plt$resp[val_plt$best_mod == "lidarSR"])
  #   flt_grp_lidarSR <- val_type[c((val_type$resp %in% resp_lidarSR) & 
  #                                   val_type$type %in% c("RMSEmdn_lidarSR", 
  #                                                        "RMSEmdn_lidarRES", 
  #                                                        "RMSEmdn_elevSR")),]
  #   
  #   plt_lidarSR <- ggplot() +
  #     geom_boxplot(data = flt_grp_lidarSR, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
  #     #facet_grid(~flt_grp_sumSR$best_mod, scales = "free_x", space="free_x", switch = "x") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16, 
  #                                      # colour = val_plt_flt$troph_sep, 
  #                                      margin = margin(0,0,0,0))) + #,
  #     labs(x = "", y = "RMSE/median")+
  #     scale_fill_manual(name = "col",values = c(unique(flt_grp_lidarSR$type_col))) +
  #     #       strip.text.x = element_blank()) +
  #     # # scale_fill_manual(name = "col",values = myColors) +
  #     ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", "RMSEmdn_lidarSR"))
  #   pdf(file = paste0(modDir, "val_plot_lidarSR_", comm, "RMSEmdn_", names(set_lst)[cnt], ".pdf"), 
  #       height= 21, width = 29)
  #   print(plt_lidarSR)
  #   dev.off()
  #   
  #   
  #   
  #   #####
  #   ###boxplot f?r RMSE to see the elevation influence within the lidarSR 
  #   ###(lidarSR, lidarRES, elevSR)
  #   #####
  #   resp_lidarSR <- unique(val_plt$resp[val_plt$best_mod == "lidarSR"])
  #   flt_grp_lidarSR <- val_type[c((val_type$resp %in% resp_lidarSR) & 
  #                                   val_type$type %in% c("RMSE_lidarSR", 
  #                                                        "RMSE_lidarRES", 
  #                                                        "RMSE_elevSR")),]
  #   
  #   plt_lidarSR <- ggplot() +
  #     geom_boxplot(data = flt_grp_lidarSR, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
  #     #facet_grid(~flt_grp_sumSR$best_mod, scales = "free_x", space="free_x", switch = "x") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16, 
  #                                      # colour = val_plt_flt$troph_sep, 
  #                                      margin = margin(0,0,0,0))) + #,
  #     labs(x = "", y = "RMSE")+
  #     scale_fill_manual(name = "col",values = c(unique(flt_grp_lidarSR$type_col))) +
  #     #       strip.text.x = element_blank()) +
  #     # # scale_fill_manual(name = "col",values = myColors) +
  #     ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", "RMSE_lidarSR"))
  #   pdf(file = paste0(modDir, "val_plot_lidarSR_", comm, "RMSE_", names(set_lst)[cnt], ".pdf"), 
  #       height= 21, width = 29)
  #   print(plt_lidarSR)
  #   dev.off()
  #   #####
  #   ###test f?r Taxa die in sumSR Gruppe landen RMSE/sd plotten. 
  #   ###sumSR und lidarelevSR plotten ==> man kann sehen, ob Taxa 
  #   ###von zweistufigem Verfahren profitiert
  #   #####
  #   
  #   resp_sumSR <- unique(val_plt$resp[val_plt$best_mod == "sumSR"])
  #   
  #   flt_grp_sumSR_2stp <- val_type[c((val_type$resp %in% resp_sumSR) & 
  #                                      val_type$type %in% c("RMSEsd_sumSR", 
  #                                                           "RMSEsd_lidarelevSR")),]
  #   resp_2stp <- unique(flt_grp_sumSR_2stp[flt_grp_sumSR_2stp$RMSEsd_sumSR_mdn < 
  #                                            flt_grp_sumSR_2stp$RMSEsd_lidarelevSR_mdn,"resp"])
  #   resp_1stp <- setdiff(resp_sumSR, resp_2stp)
  #   
  #   plt_2stp <- ggplot() +
  #     geom_boxplot(data = flt_grp_sumSR_2stp, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
  #     #facet_grid(~flt_grp_sumSR$best_mod, scales = "free_x", space="free_x", switch = "x") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16, 
  #                                      # colour = val_plt_flt$troph_sep, 
  #                                      margin = margin(0,0,0,0))) + #,
  #     #       strip.text.x = element_blank()) +
  #     # # scale_fill_manual(name = "col",values = myColors) +
  #     labs(x = "", y = "RMSE/sd")+
  #     scale_fill_manual(name = "col",values = c(unique(flt_grp_sumSR_2stp$type_col))) +
  #     ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", "RMSEsd_2stp"))
  #   pdf(file = paste0(modDir, "val_plot_sumSR_mix_", comm, "RMSEsd_", names(set_lst)[cnt], ".pdf"), 
  #       height= 21, width = 29)
  #   print(plt_2stp)
  #   dev.off()
  #   #####
  #   ###test f?r Taxa die in sumSR Gruppe landen RMSE/median plotten. 
  #   ###sumSR, lidarRES und elevSR plotten ==> man kann sehen, ob elevSR oder lidarRES 
  #   ###mehr Einfluss auf sum SR hat
  #   #####
  #   flt_grp_sumSR_infl <- val_type[c((val_type$resp %in% resp_2stp) & 
  #                                      val_type$type %in% c("RMSEmdn_sumSR", 
  #                                                           "RMSEmdn_lidarRES", 
  #                                                           "RMSEmdn_elevSR")),]
  #   plt_infl <- ggplot() +
  #     geom_boxplot(data = flt_grp_sumSR_infl, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
  #     #facet_grid(~flt_grp_sumSR$best_mod, scales = "free_x", space="free_x", switch = "x") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16, 
  #                                      # colour = val_plt_flt$troph_sep, 
  #                                      margin = margin(0,0,0,0))) + #,
  #     #       strip.text.x = element_blank()) +
  #     # # scale_fill_manual(name = "col",values = myColors) +
  #     labs(x = "", y = "RMSE/median")+
  #     scale_fill_manual(name = "col",values = c(unique(flt_grp_sumSR_infl$type_col))) +
  #     ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", "RMSEmdn_"))
  #   pdf(file = paste0(modDir, "val_plot_sumSR_2stp_", comm, "RMSEmdn_", names(set_lst)[cnt], ".pdf"), 
  #       height= 21, width = 29)
  #   print(plt_infl)
  #   dev.off()
  #   
  #   
  #   #####
  #   ###die taxa, die von 2stufigem Verfahren profitieren: 
  #   ###elevSR, lidarSR und lidarelevSR vergleichen. 
  #   #####
  #   flt_grp_sumSR_infl_ldr <- val_type[c((val_type$resp %in% resp_1stp) & 
  #                                          val_type$type %in% c("RMSEsd_lidarelevSR", 
  #                                                               "RMSEsd_lidarSR", 
  #                                                               "RMSEsd_elevSR")),]
  #   plt_infl_ldr <- ggplot() +
  #     geom_boxplot(data = flt_grp_sumSR_infl_ldr, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
  #     #facet_grid(~flt_grp_sumSR$best_mod, scales = "free_x", space="free_x", switch = "x") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16, 
  #                                      # colour = val_plt_flt$troph_sep, 
  #                                      margin = margin(0,0,0,0))) + #,
  #     #       strip.text.x = element_blank()) +
  #     # # scale_fill_manual(name = "col",values = myColors) +
  #     labs(x = "", y = "RMSE/sd")+
  #     scale_fill_manual(name = "col",values = c(unique(flt_grp_sumSR_infl_ldr$type_col))) +
  #     ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", "RMSEmdn_"))
  #   pdf(file = paste0(modDir, "val_plot_sumSR_1stp_", comm, "RMSEsd_", names(set_lst)[cnt], ".pdf"), 
  #       height= 21, width = 29)
  #   print(plt_infl_ldr)
  #   dev.off()
  #   #######################
  #   ###varimp Plots
  #   #######################
  #   #####
  #   ###create df for heatmap, separate for SR and resid
  #   #####
    
    SR_resid_lst <- lapply(resp_set, function(m){ #m <- "lidarSR"
      m_name <- paste0("pred_", m)
      resp_lst <- lapply(names(i$resp), function(k){ # k <- "SRmammals"
        print(k)
        varsel_lst <- i$varsel[[k]][[m_name]]
      })
      names(resp_lst) <- names(i$resp)
      df <- Reduce(function(x,y) merge(x,y, by = "pred", all = T), resp_lst)
      df <- df[complete.cases(df$pred),]
      colnames(df) <- c("pred", names(i$resp)) #unsch?n und unsicher...Spalten k?nnten irgendwie durcheinander kommen??? gecheckt: 17.12.
      df[is.na(df)] <- 0 #NAs are not working in levelplot
      #####
      ###sort dataframe for heatmap
      ###cols by trophic levels
      ###rows by number of occurances
      #####
      df <- df[c("pred", unique(val_troph$resp))]
      df <- df[order(rowSums(df[,2:ncol(df)], na.rm = T), decreasing = T),]
      row.names(df) <- as.character(df$pred)
      
      df_flt <- df[,which(colSums(df[,c(2:ncol(df))]) > 0) +1]
      row.names(df_flt) <- substr(row.names(df_flt), 5, nchar(row.names(df_flt)))
      
      mat <- as.matrix(df_flt[,!colnames(df_flt) == "pred"])
      
      ##taxa umbenennen und nach alphabet sortieren
      colnames(mat) <- troph_mrg$Tax_label[match(colnames(mat), troph_mrg$resp)]
      mat <- mat[, order(colnames(mat))]
     
      #######################
      ###heatmap plotting
      #######################
      l <- lvlplt(mat = mat, 
                  maxcat = maxcat, 
                  lbl_x = colnames(mat), 
                  lbl_y = rownames(mat), 
                  rnge = seq(min(mat)+0.5, max(mat)+0.5, 1) #, 
                  #main = paste0(names(set_lst)[cnt], "_", sub, m)
      )
      pdf(file = paste0(modDir, "heats_", m, "_", names(set_lst)[cnt], "_", comm, ".pdf"), 
          width = 7, height = 10); par(mar=c(6, 4, 4, 2) + 0.1)#paper = "a4")
      print(l)
      dev.off()
      png(paste0(modDir, "heats_", m, "_", names(set_lst)[cnt], "_", comm, ".png"), 
          width = 210, height = 297, units = "mm", res = 720); par(mar=c(6, 4, 4, 2) + 0.1)
      print(l)
      dev.off()
      return(mat)
    })
    names(SR_resid_lst) <- resp_set
  }
}
