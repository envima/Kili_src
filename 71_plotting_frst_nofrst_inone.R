# Description:
# Author: Alice Ziegler
# Date: 2018-12-10 11:49:55
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
source("000_setup.R")

#####
###settings
#####

comm <- ""
grp <- c("specs", "trophs")
trophs <- c("generalist", "herbivore", "decomposer", "predator")
#####
###read files
#####

mix_lst <- readRDS(file = paste0(outpath, set_dir, "mix/", "61_master_lst_varimp_.rds")) 


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

val_troph$diet <- factor(val_troph$diet, 
                         levels = levels(val_troph$diet))

val_troph_flt <- val_troph[is.finite(val_troph$RMSEsd_lidarSR),]

saveRDS(val_troph, file = paste0(outpath, set_dir, "mix/", "val_troph_mix_", comm, ".rds"))

val_type <- gather(val_troph_flt, key = type, value = value, -c(resp, run, sd, mdn:troph_sep))

val_type_per_resp <- val_type[,!names(val_type) %in% c("run", "value", "type")]
val_overview <- val_type_per_resp[!duplicated(val_type_per_resp),]

saveRDS(val_overview, file = paste0(outpath, set_dir, "mix/", "val_mix_overview_mix_", comm, ".rds"))

relv_cols <- c("resp", "Tax_label", 
  "sd", "mdn", "armean", 
  "RMSEsd_elevSR_mdn", 
  "RMSEsd_elevSR_sd", 
  "RMSEsd_lidarSR_mdn", 
  "RMSEsd_lidarSR_sd", 
  "RMSEsd_lidarRES_mdn",
  "RMSEsd_lidarRES_sd", 
  "RMSEsd_sumSR_mdn", 
  "RMSEsd_sumSR_sd", 
  "RMSE_elevSR_mdn", 
  "RMSE_elevSR_sd", 
  "RMSE_lidarSR_mdn",
  "RMSE_lidarSR_sd",
  "RMSE_lidarRES_mdn", 
  "RMSE_lidarRES_sd",
  "RMSE_sumSR_mdn", 
  "RMSE_sumSR_sd")
val_results <- val_overview[, colnames(val_overview) %in% relv_cols]
val_results <- val_results[relv_cols]
val_results <- data.frame(lapply(val_results, function(y) if(is.numeric(y)) signif(y, digits = 2) else y)) 

##bugfix (literally!) should be done in script 15
val_results$Tax_label[val_results$Tax_label == "bugs"] <- "true bugs"

row_ord <- c("ants", "bees", "birds", "dung beetles", "grasshoppers", "insectivorous bats", "large mammals", 
             "millipedes", "moths", "other aculeate wasps", "other beetles", "parasitoid wasps", "snails", "spiders", "springtails", 
             "syrphid flies", "true bugs", "decomposer", "generalist", "herbivore", "predator")
new_order <- sapply(row_ord, function(x,df){which(val_results$Tax_label == x)}, df=val_results)
val_results <- val_results[new_order,]

if (file.exists(paste0(figpath, set_dir, "mix/"))==F){
  dir.create(file.path(paste0(figpath, set_dir, "mix/")), recursive = T)
}
write.csv(val_results, file = paste0(figpath, set_dir, "mix/val_results_mix_", comm, ".csv"))
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
  
  val_plt_flt$color <- droplevels(val_plt_flt$color)
  val_plt_flt$diet <- droplevels(val_plt_flt$diet)
  
  table <- aggregate(val_plt_flt$value, by = list(val_plt_flt$Taxon, val_plt_flt$type), FUN = median)
  colnames(table) <- c("Taxon", "type", "mdn_value")
  unq_sd <- data.frame(val_troph_flt[,c("Taxon", "sd")])[!duplicated(data.frame(val_troph_flt[,c("Taxon", "sd")])),]
  
  table_sd <- merge(table, unq_sd, by = "Taxon", all = F)
  if (file.exists(paste0(inpath, set_dir, "mix/data/"))==F){
    dir.create(file.path(paste0(inpath, set_dir, "mix/data/")), recursive = T)
  }
  write.csv(table_sd, file = paste0(outpath, set_dir, "mix/validation_table_", comm, n, ".csv"))
  
  
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
  pdf(file = paste0(figpath, set_dir, "mix/val_plot_srt_trophicslvl_", comm, n, ".pdf"), height= 10, 
      width = 20)
  print(p)
  dev.off()

#####
###best model sorting
#####
  order_fun <- function(dat){
    unq_mdn <- unique(data.frame(signif_Tax_label = dat$signif_Tax_label, 
                                 RMSEsd_elevSR_mdn = dat$RMSEsd_elevSR_mdn, 
                                 RMSEsd_lidarSR_mdn = dat$RMSEsd_lidarSR_mdn, 
                                 RMSEsd_sumSR_mdn = dat$RMSEsd_sumSR_mdn, 
                                 best_mod = dat$best_mod))
    unq_mdn$signif_Tax_label <- as.character(unq_mdn$signif_Tax_label)
    
    srt_mdn <- c()
    lookup <- data.frame(best_mod = c("elevation", "structure", "combination"), 
                         title = c("RMSEsd_elevSR_mdn", "RMSEsd_lidarSR_mdn", "RMSEsd_sumSR_mdn"))
    lookup[] <- lapply(lookup, as.character)    
    for(o in levels(dat$best_mod)){
      #o <- "elevation"
      sub_tmp <- unq_mdn[unq_mdn$best_mod == o,]
      title_tmp <- lookup$title[lookup$best_mod == o]
      srt_tmp <- sub_tmp[order(sub_tmp[as.character(title_tmp)]),]
      srt_mdn <- c(srt_mdn, rev(srt_tmp$signif_Tax_label))
    }
    dat$signif_Tax_label <- factor(dat$signif_Tax_label, levels = srt_mdn)  
    return(dat)
  }
  
  dat_text <- data.frame(label = factor(c("elevation", "structure", "combination"), levels = c("elevation", "structure", "combination")), 
                         best_mod = factor(c("elevation", "structure", "combination"), levels = c("elevation", "structure", "combination")))
  
  plt_fun <- function(dat, dat_text){
    plt <-
      ggplot() +
      geom_boxplot(data = dat, aes(x=signif_Tax_label, y=value, fill=type), notch = T, width=0.8) +   #in aes(position=position_dodge(5))
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
      geom_text(data = dat_text, mapping = aes(x = rep(0.5, nrow(dat_text)), y = rep(4.5, nrow(dat_text)), label = label, angle = 90),
                hjust   = 1, vjust   = 1, size = (20*0.352777778))+ #divide by pts per inch
      labs(x = "", y = "RMSE/sd")+
      ylim(0,4.5)+
      scale_fill_manual(labels = c("elevation", "structure", "combination"),
                        values = c("lightblue2", "mediumseagreen", "orange2"))
    return(plt)
  }

  
  ###test significance between model performances
  signif_lst <- lapply(unique(val_plt_flt$Tax_label), function(i){
    # i <- "collembola"
    # print(i)
    subset <- val_plt_flt[val_plt_flt$Tax_label == i,]
    best_mod <- unique(subset$best_mod)
    anova <- aov(value ~ as.factor(type), data = subset)
    tuk_rw <- TukeyHSD(anova)
    tuk_rw <- as.data.frame(tuk_rw$`as.factor(type)`)
    tuk <- data.frame(comparison = row.names(tuk_rw), diff = tuk_rw$diff, p = tuk_rw$`p adj`)
    df_long <- pivot_longer(tuk, cols=2:3, names_to = "metric", values_to = "values")
    df_long$comp_met <- paste0(df_long$comparison, "_", df_long$metric)
    df <- data.frame(comp_met = df_long$comp_met, value = df_long$values)
    #renaming
    df$comp_met <- gsub('RMSEsd_lidarSR', 'structure', df$comp_met)
    df$comp_met <- gsub('RMSEsd_sumSR', 'combination', df$comp_met)
    df$comp_met <- gsub('RMSEsd_elevSR', 'elevation', df$comp_met)
    df_t <- setNames(data.frame(t(df[,-1])), df[,1])
    
    #checking columns that regard p-value of comparison with best model:
    match_cols <- colnames(df_t)[grepl(pattern = as.character(best_mod), colnames(df_t)) & 
                                   grepl(pattern = "p", colnames(df_t))]
    
    
    if(all(df_t[,match_cols]<0.05)){
      df_t$signif_best_mod <- 1
    }else{
      df_t$signif_best_mod <- 0
    }
    return(df_t)
  })
  
  names(signif_lst) <- unique(val_plt_flt$Tax_label)
  signif_df <- do.call(rbind, signif_lst)
  signif_df$Tax_label <- row.names(signif_df)
  # if difference of best odel to others is significant, mark Taxon with star
  signif_df <- transform(signif_df, signif_Tax_label = ifelse(signif_best_mod ==0, paste0(signif_df$Tax_label, "  "), paste0(signif_df$Tax_label, " *")))
 
  #merge to big data_frame
  val_plt_flt <- merge(val_plt_flt, signif_df, by = "Tax_label")
  
  
  #bugfix (literally!) "bugs" to "True bugs"
  val_plt_flt$Tax_label[val_plt_flt$Tax_label == "bugs"] <- "true bugs"
  val_plt_flt$signif_Tax_label[val_plt_flt$signif_Tax_label == "bugs  "] <- "true bugs  "
  ####!!!!
  
  # specs
  specs_ord <- order_fun(dat = val_plt_flt[!val_plt_flt$Taxon %in% trophs,])
  dat_text_specs <- dat_text[dat_text$label %in% unique(specs_ord$best_mod),]
  specs_plt <- plt_fun(dat = specs_ord, dat_text = dat_text_specs)
  leg <- get_legend(specs_plt) #get legend
  specs_plt <- specs_plt + theme(legend.position = "none")

    #trophics
  trophs_ord <- order_fun(dat = val_plt_flt[val_plt_flt$Taxon %in% trophs,])
  dat_text_trophs <- dat_text[dat_text$label %in% unique(trophs_ord$best_mod),]
  trophs_plt <- plt_fun(dat = trophs_ord, dat_text = dat_text_trophs) + theme(legend.position = "none")
  
  write.csv(specs_ord, paste0(figpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "specs.csv"))
  write.csv(trophs_ord, paste0(figpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "trophs.csv"))
  

  ##printing
  # specs
  pdf(file = paste0(figpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "specs.pdf"), height= 11, width = 22)
  print(specs_plt)
  dev.off()
  # trophs
  pdf(file = paste0(figpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "trophs.pdf"), height= 9.8, width = 7)
  print(trophs_plt)
  dev.off()
  legend #to be cropped outside R
  pdf(file = paste0(figpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "legend.pdf"))
  plot(leg)
  dev.off()

#####
###comparing RMSE of only residuals
#####
  
  val_plt_res <- val_plt[val_plt$type %in% c("RMSEsd_lidarRES"),]
  levels(val_plt_res$troph_sep) <- levels(val_type$troph_sep)
  levels(val_plt_res$color) <- levels(val_type$color)
  levels(val_plt_res$resp) <- levels(val_type$resp)
  levels(val_plt_res$diet) <- levels(val_type$diet)
  
  val_plt_res$color <- droplevels(val_plt_res$color)
  val_plt_res$diet <- droplevels(val_plt_res$diet)
  
  table_res <- aggregate(val_plt_res$value, by = list(val_plt_res$Taxon, val_plt_res$type), FUN = median)
  colnames(table_res) <- c("Taxon", "type", "mdn_value")
  unq_sd <- data.frame(val_plt_res[,c("Taxon", "sd")])[!duplicated(data.frame(val_plt_res[,c("Taxon", "sd")])),]
  
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
      labs(x = "", y = "RMSE/sd")+
      ylim(0,9)+
      scale_fill_manual(labels = c("residuals"), values = "gray50")
    return(plt)
  }
 
  ###bugfix (literally!) "bugs" changed to "true bugs" should be done in script 15
  val_plt_res$Tax_label[val_plt_res$Tax_label == "bugs"] <- "true bugs"
  
      # specs
      specs_ord_res <- order_fun_res(dat = val_plt_res[!val_plt_res$Taxon %in% trophs,])
      specs_plt_res <- plt_fun_res(dat = specs_ord_res)
      
      #trophics
      trophs_ord_res <- order_fun_res(dat = val_plt_res[val_plt_res$Taxon %in% trophs,])
      trophs_plt_res <- plt_fun_res(dat = trophs_ord_res)
      
      ##printing
      # specs
      pdf(file = paste0(figpath, set_dir, "mix/val_plot_srt_residuals_", comm, n, "specs.pdf"), height= 11, width = 7.5)
      print(specs_plt_res)
      dev.off()
      # trophs
      pdf(file = paste0(figpath, set_dir, "mix/val_plot_srt_residuals_", comm, n, "trophs.pdf"), height= 9.8, width = 3.5)
      print(trophs_plt_res)
      dev.off()
    