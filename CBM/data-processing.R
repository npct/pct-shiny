tdata <- read.csv("data/reduced_tdata.csv")
idata <- read.csv("data/idata.csv")
sdata <- read.csv("data/aggr_summary.csv")
sdata[is.na(sdata)] <- 0