# Description: ###workon: function to create vector with index for crossvalidation
# number within plotID is used for indexing - except if number doesn't appear in more than 50% 
# of landuse classes. In this case: if there are runs missing in this landuse class it fills this gap. 
# Arguments to provide: plots = vector with plotIDs 
# (letters = landuse, numbers = repetition within landuse (at KiLi number roughly equals transect))
# Author: Alice Ziegler
# Date: 2019-01-31 14:21:36
# to do:

########################################################################################
###for testing
########################################################################################
# plots <- c("cof1", "cof2", "cof3", "cof4", "cof5", "fer0", "fer1", "fer2", "fer3", "fer4", 
#            "flm1", "flm2", "flm3", "flm4", "flm6", "foc1", "foc2", "foc3", "foc4", "foc5", 
#            "fod1", "fod2", "fod3", "fod4", "fod5", "fpd1", "fpd2", "fpd3", "fpd4", "fpd5", 
#            "fpo1", "fpo2", "fpo3", "fpo4", "fpo5", "gra1", "gra2", "gra3", "gra4", "gra5", 
#            "hel1", "hel2", "hel3", "hel4", "hom1", "hom2", "hom3", "hom4", "hom5", "mai1", 
#            "mai2", "mai3", "mai4", "mai5", "sav1", "sav2", "sav3", "sav4", "sav5")
########################################################################################
########################################################################################
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!) ###
########################################################################################
########################################################################################
########################################################################################
########################################################################################
###Presettings
########################################################################################
#####
###load packages
#####

cv_index <- function(plots){
tbl <- data.frame(plotID = plots, selID = as.numeric(gsub("\\D+","",plots)), cat = gsub("[[:digit:]]+","",plots))
###check which indices are rare and do not create their own run
frq <- as.data.frame(table(tbl$selID))
noruns <- frq[which(frq$Freq < (0.5 * max(frq$Freq))), "Var1"]
runs <- frq[which(frq$Freq >= (0.5 * max(frq$Freq))), "Var1"]

###create column "run" with index from selID
tbl$run <- tbl$selID
###change "run" for exceptions
if(length(noruns) > 0){
  for (i in noruns){
    exept <- tbl[which(tbl$selID %in% i),]
    df_tmp <- tbl[which(tbl$cat == tbl$cat[which(tbl$selID == i)] & 
                              tbl$selID != i),]
    ###check which runs of this landuse are missing
    run_miss <- runs[-which(runs %in% df_tmp$selID)]
    ###fill gap with the exception run
    tbl[which(tbl$plotID == exept$plotID),"run"] <- as.numeric(as.character(run_miss))
  }
}
return(tbl$run)
}

