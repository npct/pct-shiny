# This updates regions.geojson
# Run to update the cycling levels on the front page map

lapply(c("geojsonio", "sp"), library, character.only = T)

regions = geojson_read("regions.geojson", what = "sp", stringsAsFactors = F)

proj4string(regions)=CRS("+init=epsg:4326 +proj=longlat")

for(i in 1:length(regions)){
  region_shape = regions[i,]
  region_name = region_shape$Region

  zones = readRDS(paste0("../pct-data/", region_name, "/z.Rds"))
  regions$pcycle[i] <- round(100 * sum(zones$bicycle) / sum(zones$all), 1)
  regions$govtarget_slc[i] <- round(100 * sum(zones$govtarget_slc) / sum(zones$all), 1)
  regions$gendereq_slc[i] <- round(100 * sum(zones$gendereq_slc) / sum(zones$all), 1)
  regions$dutch_slc[i] <- round(100 * sum(zones$dutch_slc) / sum(zones$all), 1)
  regions$ebike_slc[i] <- round(100 * sum(zones$ebike_slc) / sum(zones$all), 1)
  regions$url <- paste0("./", region_name)
}
geojson_write(regions, file = "regions_www/regions.geojson")
