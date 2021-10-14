library(ggplot2)

data <- readRDS(file = "C:/Users/Alice/Desktop/temp/plot_Kili/data.rds")
outpath <- "C:/Users/Alice/Desktop/temp/plot_Kili/"

trophs <- c("generalist", "herbivore", "decomposer", "predator")

#####
### model sorting function
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


#####
###plotting function
#####

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
specs_ord <- order_fun(dat = data[!data$Taxon %in% trophs,])
specs_plt <- plt_fun(dat = specs_ord) 
leg <- get_legend(specs_plt) #get legend
specs_plt <- specs_plt + theme(legend.position = "none")

#trophics
trophs_ord <- order_fun(dat = data[data$Taxon %in% trophs,])
trophs_plt <- plt_fun(dat = trophs_ord) + theme(legend.position = "none") + coord_fixed()


##printing
# specs
pdf(file = paste0(outpath, "plot_specs.pdf"), height= 10, width = 22)
print(specs_plt)
dev.off()
# trophs
pdf(file = paste0(outpath, "plot_trophs.pdf"), height= 10, width = 7,5)
print(trophs_plt)
dev.off()

# Was ich erreichen will, ist, dass beide plots die geliche Größe haben. 
# Das habe ich so "ungefähr" mit der width, height Einstellung im printing erreicht, aber nicht 
# gut genug für die Veröffentlichung. Wie kann ich das vereinheitlichen?
# freescales gg. coord_fixed()

#######################
###not run
#######################





a <- plot_grid(specs_plt, trophs_plt, leg, labels = c('A', 'B'), label_size = 12)
# a <- arrangeGrob(specs_plt, trophs_plt, leg, ncol=3, widths=c(2.3, 2.3, 0.8))
# both
pdf(file = paste0(outpath, set_dir, "mix/val_plot_srt_bestmodel_", comm, n, "both.pdf"))
print(a)
dev.off()