# mapgen generates the basemap html script
source("../pct-load/set-up.R")

# V2: builds all operational zones
library(rgeos)
library(maptools)
library(shiny)
library(htmlwidgets)

# load national results at zone level
if(!exists("ukmsoas")) # MSOA zones
  ukmsoas <- readRDS("../pct-bigdata/ukmsoas-scenarios.Rds")
if(!exists("centsa")) # Population-weighted centroids
  centsa <- readOGR("../pct-bigdata/cents-scenarios.geojson", layer = "OGRGeoJSON")
centsa$geo_code <- as.character(centsa$geo_code)
regions = geojson_read("../pct-bigdata/regions.geojson", what = "sp", stringsAsFactors = F)

i = 1
regions$pcycle = NA
regions$Region_cap = R.utils::capitalize(regions$Region)

proj4string(regions)=CRS("+init=epsg:4326 +proj=longlat")
proj4string(centsa)=CRS("+init=epsg:4326 +proj=longlat")
for(i in 1:length(regions)){
  print(i)
  region_shape = regions[i,]
  cents <- centsa[region_shape,]
  zones <- ukmsoas[ukmsoas@data$geo_code %in% cents$geo_code, ]
  regions$pcycle[i] <- round(100 * sum(zones$Bicycle) / sum(zones$All), 1)

  regions$url[i] <- paste0("./", regions$Region[i])
  regions$url_text[i] <- as.character(a(regions$Region_cap[i], href = regions$url[i]))
  regions$url_text[i] <- gsub('">', '" target ="_top">', regions$url_text[i])
}
popup <- paste0(regions$url_text, ", ", regions$pcycle, "% cycle to work")

library(leaflet)
qpal <- colorBin("RdYlGn", regions$pcycle, bins = c(0, 3, 5, 10), pretty = TRUE)

m <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = regions, popup = popup, weight = 1,
              fillColor = ~qpal(regions$pcycle), fillOpacity = 0.5, color = "black") %>%
  addLegend(pal = qpal, values = regions$pcycle, title = "% Cycling\nto work", opacity = 0.5)


m
old = setwd("regions_www/")
saveWidget(m, file = "map.html")
setwd(old)
# # V1: builds all zones for a single geography
# pkgs <- c("leaflet", "htmlwidgets", "geojsonio")
# lapply(pkgs, library, character.only = T)
# downloader::download("https://github.com/npct/pct-bigdata/raw/master/national/regions.geojson",
#                      destfile = "regions.geojson")
# regions <- geojson_read("regions.geojson", what = "sp")
# file.remove("regions.geojson")
# names(regions)
# m <- leaflet() %>% addTiles() %>% addPolygons(data = regions, popup = regions$url_text)
