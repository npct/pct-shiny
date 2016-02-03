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
    r <- gBuffer(r, width = 0)
    spChFIDs(r) <- i
    df <- data.frame(Name = i)
    r <- SpatialPolygonsDataFrame(r, df, match.ID = F)
    row.names(r) <- i
    plot(r)
    l <- readRDS(file.path(pct_data, i, "l.Rds"))
    r$pcycle <- round(100 * sum(l$Bicycle) / sum(l$All))

    r$url <- paste0(".", i)
    r$url_text <- as.character(a(i, href = r$url))
    r$url_text <- gsub('">', '" target ="_top">', r$url_text)

    if(exists("region")){
      proj4string(r) <- proj4string(region)
      region <- spRbind(region, r)
    }else{
      region <- r
    }
  }
}
popup <- paste0(region$url_text, ", ", region$pcycle, "% cycling ")
m <- leaflet() %>% addTiles() %>% addPolygons(data = region, popup = popup)
m
saveWidget(m, file = file.path(regions_www, "map.html"))
