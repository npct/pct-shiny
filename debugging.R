

dataDirRoot <- "~/repos/pct-shiny/pct-data/"
startingCity <- "Liverpool-City-Region/"

# For all plotting data
toPlot <- NULL
# For any other persistent values
helper <- NULL

helper$eLatLng <- ""
helper$dataDir <- file.path(dataDirRoot, startingCity)
helper$scenarioWas <- NULL

dataDir <- helper$dataDir

# check files in pct-shiny
list.files(startingCity)
list.files("Sheffield")
file.copy(paste0("pct-data/", startingCity, "/model-output.html"), startingCity)
dir.create(paste0(startingCity, "www"))
f <- list.files("leeds/www/")
file.copy(paste0("leeds/www/", f), paste0(startingCity, "www/", f))

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
  addCircleMarkers(., data = toPlot$cents, color = "black") %>%
  mapOptions(zoomToLimits = "first")

# initial data
c <- readRDS(file.path(data_dir, "c.Rds"))
z <- readRDS(file.path(data_dir, "z.Rds"))




dfc <- c@data
dfc2 <-

sapply(z@data, class)
df <- z@data[4:ncol(z)]

head(df)
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
