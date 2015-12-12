data_dir <- "pct-data/Liverpool-City-Region/"

z <- readRDS(file.path(data_dir, "z.Rds"))

sapply(z@data, class)
df <- z@data[4:ncol(z)]
z@data[4:ncol(z)] <- 1

saveRDS(z, file.path(data_dir, "z.Rds"))

apply(df, 1, is.nan)
is.nan(df$cirquity)

df <- data.frame(
  v1 = c(1, 2, 3),
  v2 = c(NaN, 4, 5),
  v3 = c(6, NA, 7)
)

df[is.na(df)]
is.nan(df)

df[apply(df, 2, is.nan)]


df <- sapply(z@data[4:ncol(z)], function(x){
  x[is.nan(x)] <- 1
  x
  }
  )

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
