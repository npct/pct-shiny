pkgs <- c("shiny", "leaflet", "ggmap", "sp", "RColorBrewer", "httr", "rgeos", "rgdal")
lapply(pkgs, library, character.only = TRUE)

sapply('./cyclestreet.R', FUN=source)

bb_poly <- function(bounds){
  lat <- c(bounds$west , bounds$east, bounds$east, bounds$west )
  lng <- c(bounds$north, bounds$north, bounds$south, bounds$south)
  c1 <- cbind(lat, lng)
  r1 <- rbind(c1, c1[1, ])
  SpatialPolygons(list(Polygons(list(Polygon(r1)), 'bb')), proj4string=CRS("+init=epsg:4326 +proj=longlat"))
}

# Load data
rfast <- readRDS("../data/manchester/rf.Rds")
rquiet <- readRDS("../data/manchester/rq.Rds")
l <- readRDS("../data/manchester/l.Rds")
zones <- readRDS("../data/manchester/z.Rds")
flow <- l@data

journeyLabel <- function(distance, percentage, route){
  sprintf("<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd><dt>Type of Route</dt><dd>%s</dd></dl>", distance, percentage, route)
}

sort_lines <- function(lines, scenario, nos, bounds){
  poly <- bb_poly(bounds)
  proj4string(poly)=CRS("+init=epsg:4326 +proj=longlat")
  poly <- spTransform(poly, CRS(proj4string(lines)))
  keep <- gContains( poly, lines,byid=TRUE ) | gOverlaps( poly, lines,byid=TRUE )
  l_in_bb <- lines[drop(keep), ]
  l_in_bb[ tail(order(l_in_bb[[scenario]]), nos), ]
}

shinyServer(function(input, output){

  cents <- coordinates(zones)
  cents <- SpatialPointsDataFrame(cents, data = zones@data, match.ID = F)

  map <- leaflet() %>%
    addTiles(urlTemplate = "http://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png")

  output$map = renderLeaflet(map %>%
                               {
                                 ## Add polygones (of MSOA boundaries)
                                if (input$zone_type == 'msoa')
                                   addPolygons(. , data = zones
                                               , fillOpacity = 0.2
                                               , opacity = 0.3
                                               , fillColor = zones$clc
                                               , color = "grey"
                                   )
                                 else .
                               } %>%
                               {
                                 if (input$line_type == 'straight' && input$nos_lines != 0){
                                   sorted_l <- sort_lines(l, input$line_attr, as.numeric(input$nos_lines), input$map_bounds)
                                   addPolylines(., data = sorted_l, color = 'blue'
                                                # Sequence in descending order
                                                , weight = seq(from = 6, to = 3, length = as.numeric(input$nos_lines))
                                               , popup = sprintf('<p><strong>Line statistics</p></strong><table>
 <thead>
  <tr>
                                                 <th style="text-align:left;"> Variable </th>
                                                 <th style="text-align:left;"> Value </th>
                                                 </tr>
                                                 </thead>
                                                 <tbody>
                                                 <tr>
                                                 <td style="text-align:left;"> Total n. commutes </td>
                                                 <td style="text-align:left;"> %s </td>
                                                 </tr>
                                                 <tr>
                                                 <td style="text-align:left;"> N. Cycle </td>
                                                 <td style="text-align:left;"> %s </td>
                                                 </tr>
                                                 <tr>
                                                 <td style="text-align:left;"> CLC (%% who cycle) </td>
                                                 <td style="text-align:left;"> %s </td>
                                                 </tr>
                                                 <tr>
                                                 <td style="text-align:left;"> PLC (%%) </td>
                                                 <td style="text-align:left;"> %s </td>
                                                 </tr>
                                                 <tr>
                                                 <td style="text-align:left;"> ECP (N.) </td>
                                                 <td style="text-align:left;"> %s </td>
                                                 </tr>
                                                 <tr>
                                                 <td style="text-align:left;"> Euclidean Distance (km) &nbsp; </td>
                                                 <td style="text-align:left;"> %s </td>
                                                 </tr>
                                                 </tbody>
                                                 </table>', sorted_l$All, sorted_l$Bicycle, round(sorted_l$clc * 100, 1), round(sorted_l$plc * 100, 1), round(sorted_l$ecp, 1), round(sorted_l$dist, 1) )
                                     )
                                 }else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'route' && input$nos_lines != 0){
                                   sorted_fast  <- sort_lines(rfast, input$line_attr, as.numeric(input$nos_lines), input$map_bounds)
                                   sorted_quiet <- sort_lines(rquiet, input$line_attr, as.numeric(input$nos_lines), input$map_bounds)
                                   addPolylines(., data = sorted_fast, color = "red"
                                                , weight = seq(4, 3, length = as.numeric(input$nos_lines))
                                     ,opacity = 0.9
                                                , popup = journeyLabel(round(sorted_fast$length / 1000, 1), round(sorted_fast$clc * 10, 2), "Fast")
                                   ) %>%
                                   addPolylines(data = sort_lines(sorted_quiet, input$line_attr, input$nos_lines, input$map_bounds), color = "green",
                                                , weight = seq(4, 3, length = as.numeric(input$nos_lines))
                                     ,opacity = 0.9
                                                , popup = journeyLabel(round(sorted_quiet$length / 1000, 1), round(sorted_quiet$clc * 10, 2), "Quiet")
                                   )
                                 }else
                                   .
                               }%>%
                               addCircleMarkers(data = cents
                                                , radius = 2
                                                , color = "black"
                                                , popup = sprintf("<b>Journeys by bike: </b>%s%%", round(zones$pCycle*100,2))) %>%
                               {
                                 if (input$feature != "none")
                                   addGeoJSON(., from_cycle_streets(input$map_bounds, input$feature))
                                 else .
                               } %>%
                               mapOptions(zoomToLimits = "first")
  )
})
