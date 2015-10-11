# mapgen generates the basemap html script
pkgs <- c("leaflet", "htmlwidgets", "geojsonio")
lapply(pkgs, library, character.only = T)
downloader::download("https://github.com/npct/pct-bigdata/raw/master/national/regions.geojson",
                     destfile = "regions.geojson")
regions <- geojson_read("regions.geojson", what = "sp")
file.remove("regions.geojson")

names(regions)

m <- leaflet() %>% addTiles() %>% addPolygons(data = regions, popup = regions$url_text)

saveWidget(m, file = "map.html")
