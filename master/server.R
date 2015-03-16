pkgs <- c("shiny", "leaflet", "ggmap", "sp", "RColorBrewer", "httr", "rgeos", "raster")
lapply(pkgs, library, character.only = TRUE)

# cckey <- readLines("~/Dropbox/dotfiles/cyclestreets-api-key-rl")
# after Sys.setenv(CYCLESTREET = cckey) # see http://www.cyclestreets.net/api/

cckey <- Sys.getenv('CYCLESTREET')

empty_geojson <- '{"type": "Point","coordinates": [-1.5492,53.7997]}'

cycle_street_bbox <- function(bounds){
  paste(bounds$west, bounds$south, bounds$east, bounds$north, sep=",")
}

create_bb <- function(bounds){
  lat <- c(bounds$west , bounds$east)
  lng <- c(bounds$north, bounds$south)
  P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
  c1 <- SpatialPoints(cbind(lat, lng), proj4string = P4S.latlon)
  bbox(c1)
}

collisions <- function(bounds){
  if(!is.null(bounds)){
    resp <- GET('https://api.cyclestreets.net/v2/collisions.locations',
                query=list(bbox=cycle_street_bbox(bounds)
                           , casualtiesinclude='cyclist'
                           , key=cckey
                           , limit=20
                )
    )
    if(status_code(resp)==200){ return(content(resp, 'parsed')) }
  }
  empty_geojson
}

pois <- function(bounds, type){
  if(!is.null(bounds)){
    resp <- GET('https://api.cyclestreets.net/v2/pois.locations',
                query=list(bbox=cycle_street_bbox(bounds)
                           , type=type
                           , key=cckey
                           , limit=20
                           , fields='id,latitude,longitude,name,osmTags'
                )
    )
    if(status_code(resp)==200){ return(content(resp, 'parsed')) }
  }
  empty_geojson
}

from_cycle_streets <- function(bounds, type){
  switch(type
         ,"collisions"=collisions(bounds)
         , pois(bounds, type)
  )
}

# Load data
rfast <- readRDS("../manchester-shiny/rf.Rds")
rquiet <- readRDS("../manchester-shiny/rq.Rds")
l <- readRDS("../manchester-shiny/l.Rds")
zones <- readRDS("../manchester-shiny/z.Rds")
flow <- l@data

journeyLabel <- function(distance, percentage, route){
  sprintf("<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd><dt>Type of Route</dt><dd>%s</dd></dl>", distance, percentage, route)
}

sort_lines <- function(lines, scenario, nos, bounds){
  bb <- create_bb(bounds)
  pl <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  l_in_bb <- lines[pl, ]
  if(nos > 0)
    lines[ head(order(l_in_bb[[scenario]]), nos), ]
  else
    lines[ tail(order(l_in_bb[[scenario]]), -nos), ]
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
                                               , fillOpacity = 0.4
                                               , opacity = 0.1
                                               , fillColor = zones$color_pcycle
                                   )
                                 else .
                               } %>%
                               {
                                 if (input$line_type == 'straight' && input$nos_lines != 0)
                                   addPolylines(., data = sort_lines(l, input$line_attr, input$nos_lines, input$map_bounds), color = 'blue'
                                                # Sequence in descending order
                                                , opacity = seq(0.8, 0.0, length = abs(input$nos_lines))
                                                , popup = sprintf("<dl><dt>Distance </dt><dd>%s km</dd></dl>", round(l$dist ,1)))
                                 else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'route' && input$nos_lines != 0)
                                   addPolylines(., data = sort_lines(rfast, input$line_attr, input$nos_lines, input$map_bounds), color = "red"
                                                , opacity = seq(0.8, 0.1, length = abs(input$nos_lines))
                                                , popup = journeyLabel(round(rfast$d / 1000, 1), round(rfast$clc * 10, 2), "Fast")
                                   ) %>%
                                   addPolylines(data = sort_lines(rquiet, input$line_attr, input$nos_lines, input$map_bounds), color = "green",
                                                , opacity = seq(0.8, 0.1, length = abs(input$nos_lines))
                                                , popup = journeyLabel(round(rquiet$d / 1000, 1), round(rquiet$clc * 10, 2), "Quiet")
                                   )
                                 else
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
