tbl <- read.csv(file = "C:/Users/Alice/Uni/Projekte/Kili/data/animals_plotIDcomplete_Syn1.csv", sep = ";")

unique(tbl$diet)
tbl[tbl$diet == "Decomposer",]
