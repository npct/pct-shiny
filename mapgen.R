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
exclude <- c("gm", "Bedford", "master", "norfolk") # regions to exclude from map
folders <- list.dirs(recursive = FALSE, full.names = F)
folders <- folders[-which(folders %in% exclude)]
# extract zone info and simplify
i <- folders[1]
region <- readRDS(paste0("pct-data/", i, "/z.Rds"))
region <- gBuffer(region, width = 0)
spChFIDs(region) <- i
df <- data.frame(Name = i)
region <- SpatialPolygonsDataFrame(region, data.frame(Name = i), match.ID = F)
row.names(region) <- i
plot(region)
l <- readRDS(paste0("pct-data/", i, "/l.Rds"))
region$pcycle <- round(100 * sum(l$Bicycle) / sum(l$All))

region$url <- paste0("http://geo8.webarch.net/", i)
region$url_text <- as.character(a(i, href = region$url))
region$url_text <- gsub('">', '" target ="_top">', region$url_text)

for(i in folders[-1]){
  if(file.exists(paste0(i, "/server.R"))){
    r <- readRDS(paste0("pct-data/", i, "/z.Rds"))
    r <- gBuffer(r, width = 0)
    spChFIDs(r) <- i
    df <- data.frame(Name = i)
    r <- SpatialPolygonsDataFrame(r, df, match.ID = F)
    row.names(r) <- i
    plot(r)
    l <- readRDS(paste0("pct-data/", i, "/l.Rds"))
    r$pcycle <- round(100 * sum(l$Bicycle) / sum(l$All))

    r$url <- paste0("http://geo8.webarch.net/", i)
    r$url_text <- as.character(a(i, href = r$url))
    r$url_text <- gsub('">', '" target ="_top">', r$url_text)

    proj4string(r) <- proj4string(region)
    region <- spRbind(region, r)
  }
}
popup <- paste0(region$url_text, ", ", region$pcycle, "% cycling ", )
m <- leaflet() %>% addTiles() %>% addPolygons(data = region, popup = popup)
m
# saveWidget(m, file = "map.html")

# generate new folders
ddirs <- list.dirs("pct-data/", recursive = F, full.names = F)
ddirs <- ddirs[!ddirs %in% folders]
i <- "sheffield"
for(i in ddirs){
  system(paste0("cp -r leeds ", i))
  sr <- readLines(paste0(i, "/server.R"))
  sr <- gsub(pattern = "leeds", i, sr)
  writeLines(sr, paste0(i, "/server.R"))
}
