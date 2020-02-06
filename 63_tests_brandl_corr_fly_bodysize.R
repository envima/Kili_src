###############################
# data file = body_size_fly.csv
data<- read.csv2("body_size_fly.csv")
attach(data)
fly<-as.factor(fly)

############################
# Spearman correlation tests
cor.test(rank_size,Fig3a_elev)
cor.test(rank_size,Fig3a_struc)
cor.test(rank_size,Fig3a_combi)
cor.test(rank_size,Fig4)

############################
# U-tests tests
wilcox.test(Fig3a_elev~fly)
wilcox.test(Fig3a_struc~fly)
wilcox.test(Fig3a_combi~fly)
wilcox.test(Fig4~fly)