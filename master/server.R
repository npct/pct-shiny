pkgs <- c("shiny", "leaflet", "ggmap", "sp", "RColorBrewer", "httr", "rgeos", "rgdal")
lapply(pkgs, library, character.only = TRUE)

sapply('./cyclestreet.R', FUN=source)

routePopup <- function(data){
  sprintf('<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd><dt>Type of Route</dt><dd>%s</dd></dl>',
          round(data$length / 1000, 1), round(data$clc * 10, 2), data$plan[1])
}

straightPopup <- function(data){
  sprintf('
  <table>
    <tbody>
      <tr>
        <td> Total n. commutes </td>
        <td> %s </td>
      </tr>
      <tr>
        <td> N. Cycle </td>
        <td> %s </td>
      </tr>
      <tr>
        <td> CLC (%% who cycle) </td>
        <td> %s </td>
      </tr>
      <tr>
        <td> PLC (%%) </td>
        <td> %s </td>
      </tr>
      <tr>
        <td> ECP (N.) </td>
        <td> %s </td>
      </tr>
      <tr>
        <td> Euclidean Distance (km) &nbsp; </td>
        <td> %s </td>
      </tr>
    </tbody>
  </table>', data$All, data$Bicycle, round(data$clc * 100, 1), round(data$plc * 100, 1), round(data$ecp, 1), round(data$dist, 1)
  )
}

# Load data
rfast <- readRDS("../data/manchester/rf.Rds")
rquiet <- readRDS("../data/manchester/rq.Rds")
l <- readRDS("../data/manchester/l.Rds")
zones <- readRDS("../data/manchester/z.Rds")
flow <- l@data
cents <- coordinates(zones)
cents <- SpatialPointsDataFrame(cents, data = zones@data, match.ID = F)

shinyServer(function(input, output){
  sortLines <- function(lines, scenario, nos){
    poly <- bbPoly()
    poly <- spTransform(poly, CRS(proj4string(lines)))
    keep <- gIntersects( poly, lines,byid=TRUE ) | gOverlaps( poly, lines,byid=TRUE )
    if(all(!keep)){
      return(NULL)
    }
    linesInBb <- lines[drop(keep), ]
    linesInBb[ tail(order(linesInBb[[scenario]]), nos), ]
  }

  bbPoly <- reactive({
    if(!input$freeze || !exists("Global.bbPoly")){
      lat <- c(input$map_bounds$west , input$map_bounds$east, input$map_bounds$east, input$map_bounds$west )
      lng <- c(input$map_bounds$north, input$map_bounds$north, input$map_bounds$south, input$map_bounds$south)

      c1 <- cbind(lat, lng)
      r1 <- rbind(c1, c1[1, ])
      assign("Global.bbPoly",
             SpatialPolygons(list(Polygons(list(Polygon(r1)), 'bb')), proj4string=CRS("+init=epsg:4326 +proj=longlat")),
             envir = .GlobalEnv)
      proj4string(Global.bbPoly)=CRS("+init=epsg:4326 +proj=longlat")
    }
    return(Global.bbPoly)
  })

  sortAndPlot <- function(m, lines, attr, nos, color, popupFn){
    sorted_l <- sortLines(lines, attr, nos)
    if(is.null(sorted_l))
      .
    else
      addPolylines(m, data = sorted_l, color = color
                   # Sequence in descending order
                   , weight = seq(from = 1.2, to = 5, length = nos)
                   , popup = popupFn(sorted_l) )
  }

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
                                 if (input$line_type == 'straight'){
                                   sortAndPlot(., l, input$line_attr, input$nos_lines,
                                               "blue", straightPopup)

                                 }else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'route'){
                                   sortAndPlot(., rfast, input$line_attr, input$nos_lines,
                                               "red", routePopup) %>%
                                   sortAndPlot(., rquiet, input$line_attr, input$nos_lines,
                                                 "green", routePopup)

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
