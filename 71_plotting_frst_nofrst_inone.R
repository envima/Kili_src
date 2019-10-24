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
library(dplyr)
library(grid)
library(gridExtra)
library(pBrackets)  
library(gtable)

#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "apr19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
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

#####
###create list with dataframes including non-forested and forested validation measures
#####

val_all <- do.call(rbind, mix_lst$val)
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

val_type_per_resp <- val_type[,!names(val_type) %in% c("run", "value", "type")]
val_overview <- val_type_per_resp[!duplicated(val_type_per_resp),]
if (file.exists(modDir)==F){
  dir.create(file.path(modDir), recursive = T)
}
saveRDS(val_overview, file = paste0(modDir, "val_mix_overview_mix_", comm, ".rds"))

val_results <- val_overview[, colnames(val_overview) %in% c("resp", "Taxon", 
                                                            "sd", "mdn", "armean", 
                                                            "RMSEsd_elevSR_mdn", 
                                                            "RMSEsd_lidarSR_mdn", 
                                                            "RMSEsd_lidarRES_mdn",
                                                            "RMSEsd_sumSR_mdn", 
                                                            "RMSE_elevSR_mdn", 
                                                            "RMSE_lidarSR_mdn",
                                                            "RMSE_lidarRES_mdn", 
                                                            "RMSE_sumSR_mdn")]
write.csv(val_results, file = paste0(outpath, set_dir, "mix/val_results_mix_", comm, ".csv"))
#####
###plotting trophic levels
#####
#######################
###val plots sorted by trophic levels
#######################
val_type$color[val_type$diet == "birds"] <- "cadetblue3"
val_type$color[val_type$diet == "bats"] <- "grey25"
val_type$color[val_type$diet == "predator"] <- "orange"
val_type$color[val_type$diet == "generalist"] <- "dodgerblue4"
val_type$color[val_type$diet == "decomposer"] <- "indianred3"
val_type$color[val_type$diet == "herbivore"] <- "limegreen"
val_type$color[val_type$diet == "plant"] <- "springgreen4"
val_type$color <- factor(val_type$color, levels = c("orange", "dodgerblue4", "indianred3", "limegreen", 
                                                    "springgreen4", "cadetblue3", "grey25"))

val_type$type_col[grepl("elevSR", val_type$type)] <- "firebrick1"
val_type$type_col[grepl("sumSR", val_type$type)] <- "darkorange2"
val_type$type_col[grepl("lidarSR", val_type$type)] <- "dodgerblue4"
val_type$type_col[grepl("lidarelevSR", val_type$type)] <- "darkorchid3"
val_type$type_col[grepl("lidarRES", val_type$type)] <- "goldenrod1"



#####
###sort resp by troph levels for further operations
#####


sort_by_diet_alphabet <- (val_type %>% distinct(diet, .keep_all = TRUE))[match(sort(levels(droplevels(val_type$diet))), (val_type %>% distinct(diet, .keep_all = TRUE))[,which(colnames(val_type %>% distinct(diet, .keep_all = TRUE)) == "diet")]),]
myColors <- c(as.character(sort_by_diet_alphabet$color), "black", "grey70", "grey30", "grey90", "white")
n <- "RMSEsd_"
  val_plt <- subset(val_type, grepl(n, val_type$type))
  val_plt <- subset(val_type, grepl("RMSEsd_", val_type$type))
  
  rank_df <- val_plt[,grepl("_mdn_rank", colnames(val_plt)) & 
                       grepl("RMSEsd_", colnames(val_plt))]
  rank_df <- rank_df[,grepl("_elevSR", colnames(rank_df)) | 
                       grepl("_sumSR", colnames(rank_df)) |
                       grepl("_lidarSR", colnames(rank_df))]
  # val_plt$szenario <- colnames(rank_df)[]
  
  for(row_nr in seq(nrow(val_plt))){
    min_colnm <- colnames(rank_df)[rank_df[row_nr,] == min(rank_df[row_nr,])]
    val_plt$best_mod[row_nr] <- strsplit(min_colnm, "_")[[1]][2]
    if (val_plt$best_mod[row_nr] == "elevSR"){
      val_plt$best_mod[row_nr] <- "elevation"
    }else if (val_plt$best_mod[row_nr] == "lidarSR"){
      val_plt$best_mod[row_nr] <- "structure"
    }else if (val_plt$best_mod[row_nr] == "sumSR"){
      val_plt$best_mod[row_nr] <- "combination"
    }
  }
  val_plt$best_mod <- factor(val_plt$best_mod, levels = c("elevation", "structure", "combination"))
  
  val_plt_flt <- val_plt[val_plt$type %in% c("RMSEsd_elevSR", 
                                             "RMSEsd_sumSR", 
                                             "RMSEsd_lidarSR"),]
  levels(val_plt_flt$troph_sep) <- levels(val_type$troph_sep)
  levels(val_plt_flt$color) <- levels(val_type$color)
  levels(val_plt_flt$resp) <- levels(val_type$resp)
  levels(val_plt_flt$diet) <- levels(val_type$diet)
  
  # val_plt_flt$troph_sep <- droplevels(val_plt_flt$troph_sep)
  val_plt_flt$color <- droplevels(val_plt_flt$color)
  # val_plt_flt$resp <- droplevels(val_plt_flt$resp)
  val_plt_flt$diet <- droplevels(val_plt_flt$diet)
  
  table <- aggregate(val_plt_flt$value, by = list(val_plt_flt$Taxon, val_plt_flt$type), FUN = median)
  colnames(table) <- c("Taxon", "type", "mdn_value")
  unq_sd <- data.frame(val_troph_flt[,c("Taxon", "sd")])[!duplicated(data.frame(val_troph_flt[,c("Taxon", "sd")])),]
  
  table_sd <- merge(table, unq_sd, by = "Taxon", all = F)
  if (file.exists(paste0(inpath, set_dir, "mix/data/"))==F){
    dir.create(file.path(paste0(inpath, set_dir, "mix/data/")), recursive = T)
  }
  write.csv(table_sd, file = paste0(inpath, set_dir, "mix/data/validation_table_", comm, n, ".csv"))
  
  
  p <-
    ggplot() +
    geom_rect(data = val_plt_flt,aes(fill = val_plt_flt$diet),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf, alpha = 0.007) +
    geom_boxplot(data = val_plt_flt, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
    facet_grid(~val_plt_flt$troph_sep, scales = "free_x", space="free_x", switch = "x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 10),
          strip.text.x = element_blank()) +
    theme(axis.text.x = element_text(size = 16))+
    scale_fill_manual(name = "col",values = myColors) +
    ggtitle(paste0(sub, "_", n))
  pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_trophicslvl_", comm, n, ".pdf"), height= 10, 
      width = 20)
  # par(mar=c(50, 50, 50, 50) + 1)#, paper = "a4r")
  print(p)
  dev.off()
  # png(paste0(modDir, "/val_plot_srt_trophicslvl_", comm, n, names(set_lst)[cnt], ".png"), 
  #     width = 297, height = 210, units = "mm", res = 720)
  # # par(mai=c(50, 50, 50, 50) + 1)
  # print(p)
  # dev.off()

#####
###best model sorting
#####

  for (i in grp){
    if(i == "specs"){
      val_plt_grp <- val_plt_flt[!val_plt_flt$Taxon %in% trophs,]
      width = 20
    } else if(i == "trophs"){
      width = 6.5
      val_plt_grp <- val_plt_flt[val_plt_flt$Taxon %in% trophs,]
    }

    
    plt <- 
      ggplot() +
      geom_boxplot(data = val_plt_grp, aes(x=Tax_label, y=value, fill=type), notch = T) +   #in aes(position=position_dodge(5))
      facet_grid(~best_mod, scales = "free_x", space="free_x") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16))+
      # colour = val_plt_grp$troph_sep,
      labs(x = "", y = "RMSE/sd")+
      ylim(0,4.5)+
      scale_fill_manual(labels = c("elevation", "structure", "combination"), 
                        values = c("lightblue2", "mediumseagreen", "orange2"))+
      theme(plot.margin=unit(c(1,0,0,0),"cm"))
    
    # grid.locator(unit="npc")
    plt_crds <- par( "plt" )
    v <- ggplotGrob(plt)
    v <- gtable_add_rows(v, unit(0.75, 'cm'), 2)
    v <- gtable_add_grob(v,
                         list(rectGrob(gp = gpar(col = NA, fill = gray(0.8))),
                              textGrob("best model performance:", gp = gpar(col = "black"), x = unit(plt_crds[3], "npc"), 
                                       y = unit(plt_crds[4], "npc"), vjust = 1.3, hjust = 1.55
                                       # hjust = c(2,0)
                              )),
                         3, 5, 3, 9, name = paste(runif(2)))
    
    grid.newpage()
    grid.draw(v)
  pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, i, ".pdf"), height= 10, 
      width = width)
  # par(mar=c(50, 50, 50, 50) + 1)#, paper = "a4r")
  # print(plt)
  print(grid.draw(v))
  dev.off()
  
  }
  
#####
###comparing RMSE of only residuals
#####
  
  val_plt_res <- val_plt[val_plt$type %in% c("RMSEsd_lidarRES"),]
  levels(val_plt_res$troph_sep) <- levels(val_type$troph_sep)
  levels(val_plt_res$color) <- levels(val_type$color)
  levels(val_plt_res$resp) <- levels(val_type$resp)
  levels(val_plt_res$diet) <- levels(val_type$diet)
  
  # val_plt_res$troph_sep <- droplevels(val_plt_res$troph_sep)
  val_plt_res$color <- droplevels(val_plt_res$color)
  # val_plt_res$resp <- droplevels(val_plt_res$resp)
  val_plt_res$diet <- droplevels(val_plt_res$diet)
  
  table_res <- aggregate(val_plt_res$value, by = list(val_plt_res$Taxon, val_plt_res$type), FUN = median)
  colnames(table_res) <- c("Taxon", "type", "mdn_value")
  unq_sd <- data.frame(val_troph_res[,c("Taxon", "sd")])[!duplicated(data.frame(val_troph_res[,c("Taxon", "sd")])),]
  
  # table_res_sd <- merge(table_res, unq_sd, by = "Taxon", all = F)
  # if (file.exists(paste0(inpath, set_dir, "mix/data/"))==F){
  #   dir.create(file.path(paste0(inpath, set_dir, "mix/data/")), recursive = T)
  # }
  # write.csv(table_res_sd, file = paste0(inpath, set_dir, "mix/data/validation_table_res_", comm, n, ".csv"))
  # 
  
  
  
  for (i in grp){
    #i <- "specs"
    if(i == "specs"){
      val_plt_grp <- val_plt_res[!val_plt_res$Taxon %in% trophs,]
      width = 7
    } else if(i == "trophs"){
      width = 3
      val_plt_grp <- val_plt_res[val_plt_res$Taxon %in% trophs,]
    }
    
    unq_Taxon <- unique(data.frame(Tax_label = val_plt_grp$Tax_label, RMSEsd_lidarRES_mdn = val_plt_grp$RMSEsd_lidarRES_mdn))
    srt_Taxon <- unq_Taxon[order(unq_Taxon$RMSEsd_lidarRES_mdn),]
    val_plt_grp$Tax_label <- factor(val_plt_grp$Tax_label, levels = srt_Taxon$Tax_label)
    
    plt <- 
      ggplot(data = val_plt_grp, aes(x=Tax_label, y=value, fill = type)) +
      geom_boxplot(notch = T) +   #in aes(position=position_dodge(5))
      # facet_grid(~best_mod, scales = "free_x", space="free_x") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16))+
      # colour = val_plt_grp$troph_sep,
      labs(x = "", y = "RMSE/sd")+
      ylim(0,9)+
        # scale_y_continuous(limits = c(0,4.5))+
      scale_fill_manual(labels = c("residuals"), values = "tomato1")+
      theme(plot.margin=unit(c(1,0,0,0),"cm"))
    
    # grid.locator(unit="npc")
    # plt_crds <- par( "plt" )
    # v <- ggplotGrob(plt)
    # v <- gtable_add_rows(v, unit(0.75, 'cm'), 2)
    # v <- gtable_add_grob(v,
    #                      list(rectGrob(gp = gpar(col = NA, fill = gray(0.8))),
    #                           textGrob("best model performance:", gp = gpar(col = "black"), x = unit(plt_crds[3], "npc"), 
    #                                    y = unit(plt_crds[4], "npc"), vjust = 1.3, hjust = 1.55
    #                                    # hjust = c(2,0)
    #                           )),
    #                      3, 5, 3, 9, name = paste(runif(2)))
    # 
    # grid.newpage()
    # grid.draw(v)
    pdf(file = paste0(outpath, set_dir, "mix/val_plot_residuals_", comm, n, i, ".pdf"), height= 10, 
        width = width)
    # par(mar=c(50, 50, 50, 50) + 1)#, paper = "a4r")
    # print(plt)
    print(plt)
    dev.off()
    
  }

