data_dir <- "pct-data/Liverpool-City-Region/"

z <- readRDS(file.path(data_dir, "z.Rds"))
l <- readRDS(file.path(data_dir, "l.Rds"))
rf <- readRDS(file.path(data_dir, "rf.Rds"))
rq <- readRDS(file.path(data_dir, "rq.Rds"))
rnet <- readRDS(file.path(data_dir, "rnet.Rds"))

plot(z)
plot(l, col = "blue", add = T)
plot(rf, col = "red", add = T)
plot(rq, col = "green", add = T)
plot(rnet, col = "grey", add = T)

plot(rnet)
