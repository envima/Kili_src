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
library(vegan)
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "feb19/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
#####
###where are the models and derived data
#####
set_dir <- "2019-02-26frst_nofrst_allplts_noelev/"

mod_dir_lst <- list.dirs(path = paste0(inpath, set_dir), recursive = F, full.names = F)
set <- c("nofrst", "frst", "allplts")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  modDir <- paste0(inpath, set_dir, set_moddir, "/")
  file <- tryCatch(
    readRDS(file = paste0(modDir, "data/", "61_master_lst_val_elevANDnoelev_",o, ".rds")), 
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
# source("lvlplt.R")
########################################################################################
###Settings
########################################################################################
plts <- c("RMSEsd_", "RMSE_")
comm <- "elevANDnoelev"
maxcat <- 20 #depending on the number ofruns
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
  val_troph$troph_sep <- factor(val_troph$troph_sep, 
                                levels = c("predator", "predator_sum", "generalist", 
                                           "generalist_sum", "decomposer", 
                                           "decomposer_sum", "herbivore", "herbivore_sum", 
                                           "plant", "plant_sum", "birds", "bats"))
  val_troph$resp <- factor(val_troph$resp, 
                           levels = unique(val_troph[with(val_troph, order(troph_sep, resp)),"resp"]))
  val_troph$diet <- factor(val_troph$diet, 
                           levels = levels(val_troph$diet))
  
  val_troph_flt <- val_troph[is.finite(val_troph$RMSEsd_ldr_pred_SR),]
  
  
  # val_type <- gather(val_troph_flt, type, value, -resp, -run, -diet, -troph_sep, -Taxon, -sd)
  val_type <- gather(val_troph_flt, key = type, value = value, -c(resp, run, sd, mdn:troph_sep))
  

  #######################
  ###begin: sort and troph by median relations
  #######################
  # n <- "RMSEmdn_"
  # # nm <- substr(n , 1, nchar(n)-1)
  # val_plt <- subset(val_type, grepl(n, val_type$type))
  # val_plt$szenario <- val_plt$constll1_mdn
  # val_plt$szenario[val_plt$constll1_RMSEmdn_mdn == 4 & 
  #                    val_plt$RMSEmdn_elev_pred_mdn_rank < val_plt$RMSEmdn_ldr_pred_resid_mdn_rank] <- 4.1###verallgemeinern! mit RMSEmdn hard gecoded!
  # val_plt$szenario[val_plt$constll1_mdn == 4 & 
  #                    val_plt$RMSEmdn_ldr_pred_resid_mdn_rank < val_plt$RMSEmdn_elev_pred_mdn_rank] <- 4.2
  # 
  
  n <- "RMSEsd_"
  # nm <- substr(n , 1, nchar(n)-1)
  val_plt <- subset(val_type, grepl(n, val_type$type))
  val_plt$szenario <- val_plt$constll1_RMSEsd_mdn
  val_plt$szenario[val_plt$constll1_RMSEsd_mdn == 4 & 
                     val_plt$RMSEsd_elev_pred_mdn_rank < val_plt$RMSEsd_ldr_pred_resid_mdn_rank] <- 4.1###verallgemeinern! mit RMSEmdn hard gecoded!
  val_plt$szenario[val_plt$constll1_RMSEsd_mdn == 4 & 
                     val_plt$RMSEsd_ldr_pred_resid_mdn_rank < val_plt$RMSEsd_elev_pred_mdn_rank] <- 4.2
  val_plt <- val_plt[val_plt$diet != "plant",]
  plt <- ggplot() +
                  geom_boxplot(data = val_plt, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
                  facet_grid(~val_plt$szenario, scales = "free_x", space="free_x", switch = "x") +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + #,
                  #       strip.text.x = element_blank()) +
                  theme(axis.text.x = element_text(size = 16))+
                  # # scale_fill_manual(name = "col",values = myColors) +
                  ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", n))
  pdf(file = paste0(modDir, "val_plot_sortconstll1_", names(set_lst)[cnt], "_", comm, n, ".pdf"), 
      height= 21, width = 29)
  print(plt)
  dev.off()
  #######################
  ###end: sort and troph by median relations
  #######################
  
  #######################
  ###begin: ordination plotting
  #######################
  ord_tbl <- val_plt[,colnames(val_plt) %in% c("resp", 
                                               "RMSEsd_elev_pred_sd", 
                                               "RMSEsd_sum_elev_pred_ldr_pred_resid_sd", 
                                               "RMSEsd_ldr_pred_SR_sd", 
                                               "RMSEsd_ldr_pred_SR_elev_sd")]
                                               
                                               # , 
                                               # "RMSEsd_ldr_pred_resid_sd")]
  ord_tbl <- ord_tbl[!duplicated(ord_tbl),]
  rownames(ord_tbl) <- ord_tbl$resp
  ord_tbl <- ord_tbl[,!colnames(ord_tbl) == "resp"]
  
  ord_tbl_rank <- val_plt[,colnames(val_plt) %in% c("resp", 
                                               "RMSEsd_elev_pred_mdn_rank", 
                                               "RMSEsd_sum_elev_pred_ldr_pred_resid_mdn_rank", 
                                               "RMSEsd_ldr_pred_SR_mdn_rank", 
                                               "RMSEsd_ldr_pred_SR_elev_mdn_rank")]
                                               
                                               # , 
                                               # "RMSEsd_ldr_pred_resid_mdn_rank")]
  ord_tbl_rank <- ord_tbl_rank[!duplicated(ord_tbl_rank),]
  rownames(ord_tbl_rank) <- ord_tbl_rank$resp
  ord_tbl_rank <- ord_tbl_rank[,!colnames(ord_tbl_rank) == "resp"]

  ord <- decorana(ord_tbl_rank)
  plot(ord)
  
  rda <- rda(ord_tbl)
  biplot(rda)
  
  rda_data <- rda(ord_tbl_rank)
  # uscores <- data.frame(rda_data$CA$u)
  # uscores1 <- inner_join(rownames_to_column(ord_tbl_rank), rownames_to_column(data.frame(uscores)), type = "right", by = "rowname")
  # vscores <- data.frame(rda_data$CA$v)
  pdf(file = paste0(modDir, "/ord_plot_", names(set_lst)[cnt], "_", comm, n, ".pdf"), height= 10, 
      width = 20)
  biplot(rda_data)
  dev.off()
  #ord_nmds <- metaMDS(dune)
  #######################
  ###end: ordination plotting
  #######################
  
  
 #######################
 ###begin sort andn plot by trophic levels
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

  #####
  ###sort resp by troph levels for further operations
  #####
  # resp_srt <- unique(val_type[with(val_type, order(troph_sep, resp)),"resp"])
  #####
  ###plot settings
  #####
  # myColors <- c(rev(levels(val_type$color)), "mediumslateblue", "blue2", "aquamarine3", "chocolate1")
  # myColors <- c(rev(levels(val_type$color)), "black", "grey70", "grey30", "white") ##needs to be changed, so reduced number of respnses is dispayed correctly
  # myColors <- c(rev(levels(droplevels(val_type$color))), "black", "grey70", "grey30", "white")
  # myColors <- c(levels(droplevels(val_type$color)), "black", "grey70", "grey30", "white")

  sort_by_diet_alphabet <- (val_type %>% distinct(diet, .keep_all = TRUE))[match(sort(levels(droplevels(val_type$diet))), (val_type %>% distinct(diet, .keep_all = TRUE))[,which(colnames(val_type %>% distinct(diet, .keep_all = TRUE)) == "diet")]),]
  myColors <- c(as.character(sort_by_diet_alphabet$color), "black", "grey80", "grey60", "grey40", "white")
  # unique(val_type$color)[order(match(unique(val_type$color),levels(val_type$color)))]
  # myColors <- c("mediumslateblue", "blue2", "aquamarine3", "chocolate1", 
  #               "firebrick1", "darkmagenta", "aquamarine", "aquamarine1", 
  #               "aquamarine2", "aquamarine3", "aquamarine4", "azure", 
  #               "azure1", "azure2", "azure3", "azure4")

  #####
  ###actual plotting
  #####
  for (n in plts){ #n <- "RMSEsd_"
    val_plt <- subset(val_type, grepl(n, val_type$type))
    levels(val_plt$troph_sep) <- levels(val_type$troph_sep)
    levels(val_plt$color) <- levels(val_type$color)
    levels(val_plt$resp) <- levels(val_type$resp)
    levels(val_plt$diet) <- levels(val_type$diet)
    
    val_plt$troph_sep <- droplevels(val_plt$troph_sep)
    val_plt$color <- droplevels(val_plt$color)
    val_plt$resp <- droplevels(val_plt$resp)
    val_plt$diet <- droplevels(val_plt$diet)
    
    table <- aggregate(val_plt$value, by = list(val_plt$Taxon, val_plt$type), FUN = median)
    colnames(table) <- c("Taxon", "type", "mdn_value")
    unq_sd <- data.frame(val_troph_flt[,c("Taxon", "sd")])[!duplicated(data.frame(val_troph_flt[,c("Taxon", "sd")])),]
   
    table_sd <- merge(table, unq_sd, by = "Taxon", all = F)
    write.csv(table_sd, file = paste0(inpath, set_dir, set_moddir, "/data/validation_table_", comm, n, ".csv"))
  
    if (file.exists(paste0(outpath, set_dir, set_moddir))==F){
      dir.create(file.path(paste0(outpath, set_dir, set_moddir)), recursive = T)
    }
    
    
    p <- ggplot() +
      geom_rect(data = val_plt,aes(fill = val_plt$diet),xmin = -Inf,xmax = Inf,
                ymin = -Inf,ymax = Inf, alpha = 0.007) +
      geom_boxplot(data = val_plt, aes(x=resp, y=value, fill=type), width = 1) +   #in aes(position=position_dodge(5))
        facet_grid(~val_plt$troph_sep, scales = "free_x", space="free_x", switch = "x") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              strip.text.x = element_blank()) +
      theme(axis.text.x = element_text(size = 16))+
        scale_fill_manual(name = "col",values = myColors) +
        ggtitle(paste0(names(set_lst)[cnt], "_", sub, "_", n))
    pdf(file = paste0(modDir, "/val_plot_", names(set_lst)[cnt], "_", comm, n, ".pdf"), height= 10, 
        width = 20)
    # par(mar=c(50, 50, 50, 50) + 1)#, paper = "a4r")
    print(p)
    dev.off()
    png(paste0(modDir, "/val_plot_", names(set_lst)[cnt], "_", comm, n, ".png"), 
        width = 297, height = 210, units = "mm", res = 720)
    # par(mai=c(50, 50, 50, 50) + 1)
    print(p)
    dev.off()
  }
  }
}
#######################
###end: sort and plot by trophic levels
#######################



  ##noch nicht angepasst für eingefügtes elev model. müsste erst in script 61 mit eingelesen werden
#   #######################
#   ###varimp Plots
#   #######################
#   #####
#   ###create df for heatmap, separate for SR and resid
#   #####
#   resp_set <- c("SR", "resid") # loop model for SR and resid
#   SR_resid_lst <- lapply(resp_set, function(m){ #m <- "SR"
#     resp_lst <- lapply(names(i$resp), function(k){
#       print(k)
#       varsel_lst <- i$varsel[[k]][[m]]
#     })
#     names(resp_lst) <- names(i$resp)
#     df <- Reduce(function(x,y) merge(x,y, by = "pred", all = T), resp_lst)
#     df <- df[complete.cases(df$pred),]
#     colnames(df) <- c("pred", names(i$resp)) #unschön und unsicher...Spalten könnten irgendwie durcheinander kommen??? gecheckt: 17.12.
#     df[is.na(df)] <- 0 #NAs are not working in levelplot
#     #####
#     ###sort dataframe for heatmap
#     ###cols by trophic levels
#     ###rows by number of occurances
#     #####
#     df <- df[c("pred", levels(val_troph$resp))]
#     df <- df[order(rowSums(df[,2:ncol(df)], na.rm = T), decreasing = T),]
#     row.names(df) <- as.character(df$pred)
#     
#     df_flt <- df[,which(colSums(df[,c(2:ncol(df))]) > 0) +1]
#     
#     mat <- as.matrix(df_flt[,!colnames(df_flt) == "pred"])
#     
#     #######################
#     ###actual plotting
#     #######################
#     l <- lvlplt(mat = mat, 
#                 maxcat = maxcat, 
#            lbl_x = colnames(mat), 
#            lbl_y = rownames(mat), 
#            rnge = seq(min(mat)+0.5, max(mat)+0.5, 1), 
#            main = paste0(names(set_lst)[cnt], "_", sub, "_", m))
#     pdf(file = paste0(modDir, "heat_selvars_", names(set_lst)[cnt], "_", m, "_", comm, ".pdf"), 
#         width = 7, height = 10); par(mar=c(6, 4, 4, 2) + 0.1)#paper = "a4")
#     print(l)
#     dev.off()
#     png(paste0(modDir, "heat_selvars_", names(set_lst)[cnt], "_", m, "_", comm, ".png"), 
#         width = 210, height = 297, units = "mm", res = 720); par(mar=c(6, 4, 4, 2) + 0.1)
#     print(l)
#     dev.off()
#     return(mat)
#   })
#   names(SR_resid_lst) <- resp_set

