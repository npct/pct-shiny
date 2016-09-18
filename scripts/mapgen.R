# V2: builds all operational zones
library(geojsonio)

# load national results at zone level
regions = geojson_read("../pct-bigdata/regions.geojson", what = "sp", stringsAsFactors = F)

regions$pcycle = NA

proj4string(regions)=CRS("+init=epsg:4326 +proj=longlat")
i = 1
for(i in 1:length(regions)){
  print(i)
  region_shape = regions[i,]
  region_name = region_shape$Region

  zones = readRDS(paste0("../pct-data/", region_name, "/z.Rds"))
  regions$pcycle[i] <- round(100 * sum(zones$bicycle) / sum(zones$all), 1)
  regions$govtarget_slc[i] <- round(100 * sum(zones$govtarget_slc) / sum(zones$all), 1)
  regions$gendereq_slc[i] <- round(100 * sum(zones$gendereq_slc) / sum(zones$all), 1)
  regions$dutch_slc[i] <- round(100 * sum(zones$dutch_slc) / sum(zones$all), 1)
  regions$ebike_slc[i] <- round(100 * sum(zones$ebike_slc) / sum(zones$all), 1)
}
geojson_write(regions, file = "../pct-shiny/regions_www/regions.geojson")
