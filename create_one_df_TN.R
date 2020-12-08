# Description: Create one df to send to Thomas for gpm
# Author: Alice Ziegler
# Date: 2020-06-16 16:53:28
# to do:
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
sub <- "/"
inpath <- "../data/"
outpath <- paste0("../data/", sub)

#####
###read files
#####
lst <- readRDS(file = "C:/Users/Alice/Uni/Projekte/Kili/data/feb20_allresp/20_master_lst_resid_allplts.rds")
########################################################################################
###Settings
########################################################################################


########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
#create one df

df <- do.call(cbind,lst$resp)
df <- df[,c(1, grep("[.]SR", colnames(df)))] ###get rid of uneccesary columns
df <- df[,grep("sum", colnames(df), invert = T)] #get rid of sum trophic levels (they origin from old version)
names(df)[1] <- "plotID"

meta <- lst$meta

df_res <- merge(df, lst$meta, by = "plotID")
df_res <- df_res[,c((1:which(names(df_res) == "selID")-1), 
               (which(names(df_res) == "cv20_outerrun_20")+1):ncol(df_res))] ##get rid of additional columns
df_res <- df_res[,grep("cat", colnames(df_res), invert = T)]


saveRDS(df_res, file = "C:/Users/Alice/Uni/Projekte/Kili/data/df_pred_resp_all.rds")


