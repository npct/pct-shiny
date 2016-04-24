# mapgen generates the basemap html script
source("../pct-load/set-up.R")

# V2: builds all operational zones
library(rgeos)
library(maptools)
library(shiny)

# load national results at zone level
if(!exists("ukmsoas")) # MSOA zones
  ukmsoas <- readRDS("../pct-bigdata/ukmsoas-scenarios.Rds")
if(!exists("centsa")) # Population-weighted centroids
  centsa <- readOGR("../pct-bigdata/cents-scenarios.geojson", layer = "OGRGeoJSON")
centsa$geo_code <- as.character(centsa$geo_code)
regions = geojson_read("../pct-bigdata/regions.geojson", what = "sp")

for(i in 1:length(regions)){
  print(i)
  region_shape = regions[i,]
  cents <- centsa[region_shape,]
  zones <- ukmsoas[ukmsoas@data$geo_code %in% cents$geo_code, ]
  r$pcycle <- round(100 * sum(zones$Bicycle) / sum(zones$All))

  r$url <- paste0(".", i)
  r$url_text <- as.character(a(i, href = r$url))
  r$url_text <- gsub('">', '" target ="_top">', r$url_text)
}
popup <- paste0(region$url_text, ", ", region$pcycle, "% cycling ")
m <- leaflet() %>% addTiles() %>% addPolygons(data = region, popup = popup)
m
saveWidget(m, file = file.path(regions_www, "map.html"))


# # V1: builds all zones for a single geography
# pkgs <- c("leaflet", "htmlwidgets", "geojsonio")
# lapply(pkgs, library, character.only = T)
# downloader::download("https://github.com/npct/pct-bigdata/raw/master/national/regions.geojson",
#                      destfile = "regions.geojson")
# regions <- geojson_read("regions.geojson", what = "sp")
# file.remove("regions.geojson")
# names(regions)
# m <- leaflet() %>% addTiles() %>% addPolygons(data = regions, popup = regions$url_text)
