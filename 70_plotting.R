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
source("fun_lvlplt.R")

#####
###read files
#####
set_lst <- lapply(set, function(o){
  set_moddir <- mod_dir_lst[grepl(paste0("_", o, "_"), mod_dir_lst)]
  file <- tryCatch(
    readRDS(file = paste0(outpath, set_dir, set_moddir, "/", "60_master_lst_varimp_",o, ".rds")), 
    error = function(e)file <- NA)
  return(file)
})
names(set_lst) <- set
set_lst <- set_lst[!is.na(set_lst)]

troph_mrg <- readRDS(paste0(inpath, "15_troph_mrg.rds"))
troph_mrg <- troph_mrg[!duplicated(troph_mrg),]

########################################################################################
###Settings
########################################################################################
plts <- c("RMSEsd_")#, "RMSE_")
comm <- ""
maxcat <- 20 #depending on the number of levels run

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
    
    val_troph$diet <- factor(val_troph$diet,
                             levels = levels(val_troph$diet))

    val_troph_flt <- val_troph[is.finite(val_troph$RMSEsd_lidarSR),]


    val_type <- gather(val_troph_flt, key = type, value = value, -c(resp, run, sd, mdn:troph_sep))

    val_type_per_resp <- val_type[,!names(val_type) %in% c("run", "value")]
    val_overview <- val_type_per_resp[!duplicated(val_type_per_resp),]
    if (file.exists(paste0(figpath, set_dir, set_moddir, "/"))==F){
      dir.create(file.path(paste0(figpath, set_dir, set_moddir, "/")), recursive = T)
    }
    saveRDS(val_overview, file = paste0(figpath, set_dir, set_moddir, "/", "val_overview", names(set_lst)[cnt], "_", comm, ".rds"))


    #######################
    ###varimp Plots
    #######################
    #####
    ###create df for heatmap, separate for SR and resid
    #####
    
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
      pdf(file = paste0(figpath, set_dir, set_moddir, "/", "heats_", m, "_", names(set_lst)[cnt], "_", comm, ".pdf"), 
          width = 7, height = 10); par(mar=c(6, 4, 4, 2) + 0.1)#paper = "a4")
      print(l)
      dev.off()
      png(paste0(figpath, set_dir, set_moddir, "/", "heats_", m, "_", names(set_lst)[cnt], "_", comm, ".png"), 
          width = 210, height = 297, units = "mm", res = 720); par(mar=c(6, 4, 4, 2) + 0.1)
      print(l)
      dev.off()
      return(mat)
    })
    names(SR_resid_lst) <- resp_set
  }
}
