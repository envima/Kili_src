library(plotwidgets)
library(ggplot2)
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
set <- c("nofrst", "frst", "allplts")

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





a <- set_lst$allplts$meta

set.seed(1)
# a$rndm <- abs(rnorm(nrow(a), mean=1, sd=0.2))
a$rndm <- 1
a$cat <- droplevels(a$cat)

a$cat <- factor(a$cat, levels = c("mai", "sav", "cof", "hom", "gra", "flm", "foc", "fod", "fpo", "fpd", "fer", "hel"))
# levels(a$cat) <- c("mai", "sav", "cof", "hom", "gra", "flm", "foc", "fod", "fpo", "fpd", "fer", "hel")

# cols_cat <- c(hsl2rgb(matrix(c(40,222, 140), nrow = 3)), 
#               hsl2rgb(matrix(c(25, 182, 175), nrow = 3)), 
#               hsl2rgb(matrix(c(23, 136, 94), nrow = 3)), 
#               hsl2rgb(matrix(c(254, 161, 176), nrow = 3)), 
#               hsl2rgb(matrix(c(76, 183, 141), nrow = 3)), 
#               hsl2rgb(matrix(c(96, 160, 72), nrow = 3)), 
#               hsl2rgb(matrix(c(209, 211, 150), nrow = 3)), 
#               hsl2rgb(matrix(c(212, 151, 76), nrow = 3)), 
#               hsl2rgb(matrix(c(125, 114, 163), nrow = 3)), 
#               hsl2rgb(matrix(c(131, 120, 94), nrow = 3)), 
#               hsl2rgb(matrix(c(174, 215, 139), nrow = 3)), 
#               hsl2rgb(matrix(c(156, 85, 218), nrow = 3)))
cols_cat <- c("gold1", "burlywood1", "tan4", "salmon", "green", "palegreen4", "violetred1", 
              "purple", "powderblue", "darkslategray4", "darkblue", "azure3")

# plot(a$rndm, a$elevation)

g <-
  ggplot(a, aes(x=rndm, y=elevation, color=cat)) + 
  geom_point(shape=0, stroke = 1)+ 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        panel.background = element_blank())+ 
  scale_color_manual(values=cols_cat)#+ 

pdf(file = "C:/Users/Alice/Uni/Projekte/Kili/out/apr19/2019-03-26frst_nofrst_allplts_noelev/mix/elev_dist.pdf", height = 10, width = 2)
print(g)
dev.off()
