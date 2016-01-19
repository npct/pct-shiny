

dataDirRoot <- "../pct-data/"
startingCity <- "wiltshire"

# For all plotting data
toPlot <- NULL
# For any other persistent values
helper <- NULL

helper$dataDir <- file.path(dataDirRoot, startingCity)
helper$scenarioWas <- NULL

dataDir <- file.path(dataDirRoot, startingCity)

# check files in pct-shiny
list.files(startingCity)

# To set initialize toPlot
loadData <- function(dataDir){
  toPlot
  toPlot$l <- readRDS(file.path(dataDir, "l.Rds"))

  toPlot$rFast <- readRDS(file.path(dataDir, "rf.Rds" ))
  toPlot$rFast@data <- cbind(toPlot$rFast@data, toPlot$l@data)
  toPlot$rQuiet <- readRDS(file.path(dataDir, "rq.Rds"))
  toPlot$rQuiet@data <- cbind(toPlot$rQuiet@data, toPlot$l@data)

  toPlot$zones <-  readRDS(file.path(dataDir, "z.Rds"))
  toPlot$cents <-   readRDS(file.path(dataDir, "c.Rds"))

  toPlot$rnet <- readRDS(file.path(dataDir, "rnet.Rds"))
  toPlot$rnet$id <- 1:nrow(toPlot$rnet)

  toPlot
}

# render it

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(., data = cents, color = "black") %>%
  mapOptions(zoomToLimits = "first")

# initial data
cents <- readRDS(file.path(dataDir, "c.Rds"))
zones <- readRDS(file.path(dataDir, "z.Rds"))




sapply(z@data, class)
df <- z@data[4:ncol(z)]

head(df)
z@data[4:ncol(z)] <- 1

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



l <- readRDS(file.path(dataDir, "l.Rds"))
rf <- readRDS(file.path(dataDir, "rf.Rds"))
rq <- readRDS(file.path(dataDir, "rq.Rds"))
rnet <- readRDS(file.path(dataDir, "rnet.Rds"))

# sel <- nrow(l)
sel <- 75
plot(l[sel,])
plot(rf[sel,], add = T)
plot(rq[sel,], add = T)

plot(zones)
bbox(z)
plot(l, col = "blue", add = T)
plot(rf, col = "red", add = T)
plot(rq, col = "green", add = T)
plot(rnet, col = "grey", add = T)

leaflet() %>% addTiles() %>% addPolylines(data = l)
leaflet() %>% addTiles() %>% addCircles(data = c)


plot(rnet)

# check CRS
proj4string(c)
proj4string(z)

ll <- list(l, c, rf, z, rq)

lapply(, proj4string)

# debug lines
l <- readRDS("pct-data/liverpool-city-region/l.Rds")
l2 <- readRDS("pct-data/leeds/l.Rds")
names(l2)[!names(l2) %in% names(l)] # found missing vars
