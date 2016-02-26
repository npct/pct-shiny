# mapgen generates the basemap html script
# # V1: builds all zones for a single geography
# pkgs <- c("leaflet", "htmlwidgets", "geojsonio")
# lapply(pkgs, library, character.only = T)
# downloader::download("https://github.com/npct/pct-bigdata/raw/master/national/regions.geojson",
#                      destfile = "regions.geojson")
# regions <- geojson_read("regions.geojson", what = "sp")
# file.remove("regions.geojson")
# names(regions)
# m <- leaflet() %>% addTiles() %>% addPolygons(data = regions, popup = regions$url_text)

# V2: builds all operational zones
library(rgeos)
library(maptools)
library(shiny)
library(leaflet)

pct_data <- file.path("..", "pct-data")
regions_www <- "regions_www"
exclude <- c("gm", "Bedford", "master", "norfolk",
             "liverpool-city-region2", "tiverton", "nottingham", "birmingham") # regions to exclude from map
folders <- list.dirs(path= regions_www, recursive = FALSE, full.names = F)
folders <- folders[-which(folders %in% exclude)]
# extract zone info and simplify
for(i in folders){
  if(file.exists(file.path(regions_www, i, "server.R"))){
    print(i)
    r <- readRDS(file.path(pct_data, i, "z.Rds"))
    if(!any(is.nan(r$av_distance))) { r <- rmapshaper::ms_simplify(r) }
    r <- gBuffer(r, width = 0)
    spChFIDs(r) <- i
    df <- data.frame(Name = i)
    r <- SpatialPolygonsDataFrame(r, df, match.ID = F)
    row.names(r) <- i
    plot(r)
    l <- readRDS(file.path(pct_data, i, "l.Rds"))
    r$pcycle <- round(100 * sum(l$Bicycle) / sum(l$All))

    r$url_text <- as.character(a(paste("See map of", i), href = paste0("./", i)))
    r$url_text <- gsub('">', '" target ="_top">', r$url_text)

    if(exists("region")){
      proj4string(r) <- proj4string(region)
      region <- spRbind(region, r)
    }else{
      region <- r
    }
  }
}
normalise <- function(values, min = 0, max = 1){
  min + max * (values - min(values))/diff(range(values))
}

getColourRamp <- function(values) {
  v <- normalise(values)
  x <- colorRamp(c("red", "green"))(v)
  x[is.na(x)] <- 1
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

popup <- paste0(region$url_text, ", ", region$pcycle, "% cycling ")
m <- leaflet() %>%
  addTiles(urlTemplate= "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png") %>%
  addPolygons(data = region, popup = popup, fillColor = getColourRamp(region$pcycle), weight = 1, color = "black" )
m
htmlwidgets::saveWidget(m, file = "map.html")
file.rename("map.html", file.path(regions_www, "map.html"))