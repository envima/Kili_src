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
library(cowplot)


#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "feb20/"
# sub <- "apr19/" #paper
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####

set_dir <- "2020-02-06frst_nofrst_allplts_noelev/"
#paper: 
# set_dir <- "2019-03-26frst_nofrst_allplts_noelev/"

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

saveRDS(val_troph, file = paste0(modDir, "val_troph_mix_", comm, ".rds"))

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
if (file.exists(paste0(outpath, set_dir, "mix/"))==F){
  dir.create(file.path(paste0(outpath, set_dir, "mix/")), recursive = T)
}
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
  order_fun <- function(dat){
    unq_mdn <- unique(data.frame(Tax_label = dat$Tax_label, 
                                 RMSEsd_elevSR_mdn = dat$RMSEsd_elevSR_mdn, 
                                 RMSEsd_lidarSR_mdn = dat$RMSEsd_lidarSR_mdn, 
                                 RMSEsd_sumSR_mdn = dat$RMSEsd_sumSR_mdn, 
                                 best_mod = dat$best_mod))
    unq_mdn$Tax_label <- as.character(unq_mdn$Tax_label)
    
    srt_mdn <- c()
    lookup <- data.frame(best_mod = c("elevation", "structure", "combination"), 
                         title = c("RMSEsd_elevSR_mdn", "RMSEsd_lidarSR_mdn", "RMSEsd_sumSR_mdn"))
    lookup[] <- lapply(lookup, as.character)    
    for(o in levels(dat$best_mod)){
      #o <- "elevation"
      sub_tmp <- unq_mdn[unq_mdn$best_mod == o,]
      title_tmp <- lookup$title[lookup$best_mod == o]
      srt_tmp <- sub_tmp[order(sub_tmp[as.character(title_tmp)]),]
      srt_mdn <- c(srt_mdn, rev(srt_tmp$Tax_label))
    }
    dat$Tax_label <- factor(dat$Tax_label, levels = srt_mdn)  
    return(dat)
  }
  
  dat_text <- data.frame(label = c("elevation", "structure", "combination"), 
                         best_mod = c("elevation", "structure", "combination"))
  
  plt_fun <- function(dat){
    plt <- 
      ggplot() +
      geom_boxplot(data = dat, aes(x=Tax_label, y=value, fill=type), notch = T, width=0.8) +   #in aes(position=position_dodge(5))
      facet_grid(~best_mod, scales = "free_x", space="free_x") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20), 
            axis.text.y = element_text(size = 20), 
            legend.title = element_blank(),
            legend.text = element_text(size = 20),
            legend.key.size = unit(3,"line"),
            plot.margin=unit(c(1,1,0,1),"cm"), 
            strip.text.x =  element_blank(),#element_text(size = 20), 
            axis.title=element_text(size=20), 
            axis.ticks.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
      # theme_bw()+
      # colour = dat$troph_sep,
      geom_text(data = dat_text, mapping = aes(x = c(0.5,0.5,0.5), y = c(4.5,4.5,4.5), label = label, angle = 90), 
                hjust   = 1, vjust   = 1, size = (20*0.352777778))+ #divide by pts per inch
      labs(x = "", y = "RMSE/sd")+
      ylim(0,4.5)+
      scale_fill_manual(labels = c("elevation", "structure", "combination"), 
                        values = c("lightblue2", "mediumseagreen", "orange2"))
    return(plt)
  }
  

  # specs
  specs_ord <- order_fun(dat = val_plt_flt[!val_plt_flt$Taxon %in% trophs,])
  specs_plt <- plt_fun(dat = specs_ord)
  leg <- get_legend(specs_plt) #get legend
  specs_plt <- specs_plt + theme(legend.position = "none")
  
  
  #trophics
  trophs_ord <- order_fun(dat = val_plt_flt[val_plt_flt$Taxon %in% trophs,])
  trophs_plt <- plt_fun(dat = trophs_ord) + theme(legend.position = "none")
  
  # a <- plot_grid(specs_plt, trophs_plt, leg, labels = c('A', 'B'), label_size = 12)
  # # a <- arrangeGrob(specs_plt, trophs_plt, leg, ncol=3, widths=c(2.3, 2.3, 0.8))
  # 
  ##printing
  # specs
  pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "specs.pdf"), height= 11, width = 22)
  print(specs_plt)
  dev.off()
  # trophs
  pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "trophs.pdf"), height= 9.8, width = 7)
  print(trophs_plt)
  dev.off()
  # legend #to be cropped outside R
  # pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "legend.pdf"))
  # plot(leg)
  # dev.off()


  # both
  # pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "both.pdf"))
  # print(a)
  # dev.off()
  
  
  
  # a <- plot_grid(specs_plt, trophs_plt, leg, labels = c('A', 'B'), label_size = 12)
  # # a <- arrangeGrob(specs_plt, trophs_plt, leg, ncol=3, widths=c(2.3, 2.3, 0.8))
  # # both
  # pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "both.pdf"))
  # print(a)
  # dev.off()
  
  
  # 
  # ###
  # #reorder by median performance of each facet (elevation facet ordered by elevation perf., structure facet ordered by structure,...)
  # ### 
  # unq_mdn <- unique(data.frame(Tax_label = val_plt_grp$Tax_label, 
  #                              RMSEsd_elevSR_mdn = val_plt_grp$RMSEsd_elevSR_mdn, 
  #                              RMSEsd_lidarSR_mdn = val_plt_grp$RMSEsd_lidarSR_mdn, 
  #                              RMSEsd_sumSR_mdn = val_plt_grp$RMSEsd_sumSR_mdn, 
  #                              best_mod = val_plt_grp$best_mod))
  # unq_mdn$Tax_label <- as.character(unq_mdn$Tax_label)
  # 
  # srt_mdn <- c()
  # lookup <- data.frame(best_mod = c("elevation", "structure", "combination"), 
  #                      title = c("RMSEsd_elevSR_mdn", "RMSEsd_lidarSR_mdn", "RMSEsd_sumSR_mdn"))
  # lookup[] <- lapply(lookup, as.character)    
  # for(o in levels(val_plt_grp$best_mod)){
  #   #o <- "elevation"
  #   sub_tmp <- unq_mdn[unq_mdn$best_mod == o,]
  #   title_tmp <- lookup$title[lookup$best_mod == o]
  #   srt_tmp <- sub_tmp[order(sub_tmp[as.character(title_tmp)]),]
  #   srt_mdn <- c(srt_mdn, rev(srt_tmp$Tax_label))
  # }
  # val_plt_grp$Tax_label <- factor(val_plt_grp$Tax_label, levels = srt_mdn)  
  # 
  # 
  # 
  # for (i in grp){ #i <- "specs", i <- "trophs"
  #   if(i == "specs"){
  #     val_plt_grp <- val_plt_flt[!val_plt_flt$Taxon %in% trophs,]
  #     width = 22
  #   } else if(i == "trophs"){
  #     width = 7.5
  #     val_plt_grp <- val_plt_flt[val_plt_flt$Taxon %in% trophs,]
  #   }
  # 
  #   ###
  #   #reorder by median performance of each facet (elevation facet ordered by elevation perf., structure facet ordered by structure,...)
  #   ### 
  #   unq_mdn <- unique(data.frame(Tax_label = val_plt_grp$Tax_label, 
  #                                  RMSEsd_elevSR_mdn = val_plt_grp$RMSEsd_elevSR_mdn, 
  #                                  RMSEsd_lidarSR_mdn = val_plt_grp$RMSEsd_lidarSR_mdn, 
  #                                  RMSEsd_sumSR_mdn = val_plt_grp$RMSEsd_sumSR_mdn, 
  #                                  best_mod = val_plt_grp$best_mod))
  #   unq_mdn$Tax_label <- as.character(unq_mdn$Tax_label)
  #   
  #   srt_mdn <- c()
  #   lookup <- data.frame(best_mod = c("elevation", "structure", "combination"), 
  #                        title = c("RMSEsd_elevSR_mdn", "RMSEsd_lidarSR_mdn", "RMSEsd_sumSR_mdn"))
  #   lookup[] <- lapply(lookup, as.character)    
  #   for(o in levels(val_plt_grp$best_mod)){
  #     #o <- "elevation"
  #     sub_tmp <- unq_mdn[unq_mdn$best_mod == o,]
  #     title_tmp <- lookup$title[lookup$best_mod == o]
  #     srt_tmp <- sub_tmp[order(sub_tmp[as.character(title_tmp)]),]
  #     srt_mdn <- c(srt_mdn, rev(srt_tmp$Tax_label))
  #   }
  #   val_plt_grp$Tax_label <- factor(val_plt_grp$Tax_label, levels = srt_mdn)    
  #   
  #   
  #   plt <-
  #     ggplot() +
  #     geom_boxplot(data = val_plt_grp, aes(x=Tax_label, y=value, fill=type), notch = T) +   #in aes(position=position_dodge(5))
  #     facet_grid(~best_mod, scales = "free_x", space="free_x") +
  #       # theme_minimal_grid()+
  #       theme_bw()
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20), 
  #           axis.text.y = element_text(size = 20), 
  #           # legend.position = "none",
  #           legend.title = element_blank(),
  #           legend.text = element_text(size = 20),
  #           legend.key.size = unit(3,"line"),
  #           plot.margin=unit(c(1,1,0,1),"cm"), 
  #           strip.text.x = element_text(size = 12), 
  #           axis.title=element_text(size=22))+
  #       # theme_bw()+
  #     # colour = val_plt_grp$troph_sep,
  #     labs(x = "", y = "RMSE/sd")+
  #     ylim(0,4.5)+
  #     scale_fill_manual(labels = c("elevation", "structure", "combination"), 
  #                       values = c("lightblue2", "mediumseagreen", "orange2"))
  # 
  #     
  #     
  #     
  #     
  #   # # grid.locator(unit="npc")
  #   # plt_crds <- par( "plt" )
  #   # v <- ggplotGrob(plt)
  #   # v <- gtable_add_rows(v, unit(1, 'cm'), 2)
  #   # v <- gtable_add_grob(v,
  #   #                      list(rectGrob(gp = gpar(col = NA, fill = gray(0.8))),
  #   #                           textGrob("best model performance:", gp = gpar(col = "black", fontsize = 22), 
  #   #                                    x = unit(plt_crds[3], "npc"), 
  #   #                                    y = unit(plt_crds[4], "npc"), vjust = 1, hjust = 0.9
  #   #                                    # hjust = c(2,0)
  #   #                           )),
  #   #                      3, 5, 3, 9, name = paste(runif(2)))
  #   # 
  #   # grid.newpage()
  #   # grid.draw(v)
  # pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, i, ".pdf"), height= 10, 
  #     width = width)
  # # par(mar=c(50, 50, 50, 50) + 1)#, paper = "a4r")
  # print(plt)
  # # print(grid.draw(v))
  # dev.off()
  
  # }
  
  
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
  unq_sd <- data.frame(val_plt_res[,c("Taxon", "sd")])[!duplicated(data.frame(val_plt_res[,c("Taxon", "sd")])),]
  
  # table_res_sd <- merge(table_res, unq_sd, by = "Taxon", all = F)
  # if (file.exists(paste0(inpath, set_dir, "mix/data/"))==F){
  #   dir.create(file.path(paste0(inpath, set_dir, "mix/data/")), recursive = T)
  # }
  # write.csv(table_res_sd, file = paste0(inpath, set_dir, "mix/data/validation_table_res_", comm, n, ".csv"))
  # 
  
  
  
  order_fun_res <- function(dat){
    unq_Taxon <- unique(data.frame(Tax_label = dat$Tax_label, RMSEsd_lidarRES_mdn = dat$RMSEsd_lidarRES_mdn))
    srt_Taxon <- unq_Taxon[order(unq_Taxon$RMSEsd_lidarRES_mdn),]
    dat$Tax_label <- factor(dat$Tax_label, levels = rev(srt_Taxon$Tax_label))
    return(dat)
  }
  
  
  plt_fun_res <- function(dat){
    plt <- 
      ggplot() +
      geom_boxplot(data = dat, aes(x=Tax_label, y=value, fill = type), notch = T, width = 0.8) +   #in aes(position=position_dodge(5))
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20), 
            axis.text.y = element_text(size = 20), 
            plot.margin=unit(c(1,1,0,1),"cm"), 
            legend.position = "none", 
            strip.text.x =  element_blank(),#element_text(size = 20), 
            axis.title=element_text(size=20), 
            axis.ticks.y = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
      # colour = val_plt_grp$troph_sep,
      labs(x = "", y = "RMSE/sd")+
      ylim(0,9)+
      # scale_y_continuous(limits = c(0,4.5))+
      scale_fill_manual(labels = c("residuals"), values = "gray50")
    return(plt)
  }
  
    # for (i in grp){
    #   #i <- "specs"
    #   if(i == "specs"){
    #     val_plt_grp <- val_plt_res[!val_plt_res$Taxon %in% trophs,]
    #     width = 7
    #   } else if(i == "trophs"){
    #     width = 3
    #     val_plt_grp <- val_plt_res[val_plt_res$Taxon %in% trophs,]
    #   }
    #   
    #   unq_Taxon <- unique(data.frame(Tax_label = val_plt_grp$Tax_label, RMSEsd_lidarRES_mdn = val_plt_grp$RMSEsd_lidarRES_mdn))
    #   srt_Taxon <- unq_Taxon[order(unq_Taxon$RMSEsd_lidarRES_mdn),]
    #   val_plt_grp$Tax_label <- factor(val_plt_grp$Tax_label, levels = rev(srt_Taxon$Tax_label))
    # 
      # specs
      specs_ord_res <- order_fun_res(dat = val_plt_res[!val_plt_res$Taxon %in% trophs,])
      specs_plt_res <- plt_fun_res(dat = specs_ord_res)
      
      #trophics
      trophs_ord_res <- order_fun_res(dat = val_plt_res[val_plt_res$Taxon %in% trophs,])
      trophs_plt_res <- plt_fun_res(dat = trophs_ord_res)
      
      ##printing
      # specs
      pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_residuals_", comm, n, "specs.pdf"), height= 11, width = 7.5)
      print(specs_plt_res)
      dev.off()
      # trophs
      pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_residuals_", comm, n, "trophs.pdf"), height= 9.8, width = 3.5)
      print(trophs_plt_res)
      dev.off()
    
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
  #   # grid.draw(v)
  #   pdf(file = paste0(outpath, set_dir, "mix/val_plot_residuals_", comm, n, i, ".pdf"), height= 10, 
  #       width = width)
  #   # par(mar=c(50, 50, 50, 50) + 1)#, paper = "a4r")
  #   # print(plt)
  #   print(plt)
  #   dev.off()
  #   
  # }

