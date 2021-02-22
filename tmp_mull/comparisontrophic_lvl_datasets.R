rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####
#####
###set paths
#####
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
inpath <- paste0("../data/")

#####
###read files
#####
# trophic_tbl <- as.data.frame(read.csv(paste0(inpath, "trophic_tbl.csv"), sep = ";"))
# field_dat <- as.data.frame(read.table(file = paste0(inpath, "Biodiversity_Data_Marcel.csv"), 
#                                       sep = ";", header = T, na.strings = "NA", dec = ","))
tbl <- readRDS(paste0(inpath, "mar19/15_master_lst_allplts.rds"))
gpm_tn <- readRDS(paste0(inpath, "ki_hyperspec_biodiv_non_scaled.rds"))

#Thomas Daten reduziert auf trophische level
gpm_tn@meta$input$RESPONSE
tbl_tn <- as.data.frame(gpm_tn@data$input)
troph_tn <- tbl_tn[names(tbl_tn) %in% c("plotID", "SRpredator", "SRherbivore", "SRgeneralist", "SRdecomposer")]

#meine Daten reduziert auf trophische level
troph_az_lst <- tbl$resp[grepl("predator", names(tbl$resp))|
                       grepl("herbivore", names(tbl$resp))|
                       grepl("generalist", names(tbl$resp))|
                       grepl("decomposer", names(tbl$resp))]
troph_az <- do.call(cbind, troph_az_lst)
troph_az <- cbind(troph_az[,1], troph_az[,!grepl("plotID", colnames(troph_az))])
nms_lst <- strsplit(colnames(troph_az)[2:length(colnames(troph_az))], "\\.")
nms <- lapply(nms_lst, function(z){
  nm <- z[1]
})
nms <- unlist(nms)
names_lst <- strsplit(nms, "_")
names <- lapply(names_lst, function(z){
  nm <- z[2]
})
colnms <- paste0("SR", unlist(names), "_az")
colnames(troph_az) <- c("plotID", colnms)

tbl_tnaz <- merge(troph_tn, troph_az, by = "plotID")

pdf(file = paste0(inpath, "../out/comparisontrophic_lvl.pdf"))
par(mfrow=c(2,2)) 
plot(tbl_tnaz$SRpredator ~ tbl_tnaz$SRpredator_az)
plot(tbl_tnaz$SRherbivore ~ tbl_tnaz$SRherbivore_az)
plot(tbl_tnaz$SRdecomposer ~ tbl_tnaz$SRdecomposer_az)
plot(tbl_tnaz$SRgeneralist ~ tbl_tnaz$SRgeneralist_az)
dev.off()
