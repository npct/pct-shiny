pkgs <- c("shiny", "leaflet", "ggmap", "sp", "RColorBrewer", "httr")
lapply(pkgs, library, character.only = TRUE)

# after Sys.setenv(CYCLESTREET = "my_token") # see http://www.cyclestreets.net/api/
cyclestreet_token <- Sys.getenv('CYCLESTREET')

empty_geojson <- '{"type": "Point","coordinates": [-1.5492,53.7997]}'

cycle_street_bbox <- function(bounds){
  paste(bounds$west, bounds$south, bounds$east, bounds$north, sep=",")
}

ascending_descending <- function(nos, sort_by){
  if(nos>0) paste(sort_by, 'D') else paste(sort_by, 'A')
}

get_lines <- function(bounds, nos, sort_by_attr){
  if(is.null(bounds)){ return(empty_geojson) }
  query_list <- list(service='WFS'
                     ,version='1.0.0'
                     ,request='GetFeature'
                     ,typeName='topp:lines-manchester'
                     ,bbox=cycle_street_bbox(bounds)
                     ,maxFeatures=nos
                     ,outputFormat='application/json'
                     ,sortBy=ascending_descending(nos, sort_by_attr)
  )
  resp <- GET('http://geo8.webarch.net:8080/geoserver/topp/ows', query = query_list )
  if(status_code(resp)==200){ return(content(resp, 'parsed')) }
  empty_geojson
}

collisions <- function(bounds){
  if(!is.null(bounds)){
    resp <- GET('https://api.cyclestreets.net/v2/collisions.locations',
                query=list(bbox=cycle_street_bbox(bounds)
                           , casualtiesinclude='cyclist'
                           , key=cyclestreet_token
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
                           , key=cyclestreet_token
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
rfast <- readRDS("../data/rfast.Rds")
rquiet <- readRDS("../data/rquiet.Rds")
flows <- read.csv("../data/al-flow.csv")
leeds <- readRDS("../data/leeds-msoas-simple.Rds")

journeyLabel <- function(distance, percentage, route){
  sprintf("<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd><dt>Type of Route</dt><dd>%s</dd>", distance, percentage, route)
}

sort_lines <- function(lines, scenario, nos){
  if(nos > 0)
    lines[ head(order(lines[[scenario]]), nos), ]
  else
    lines[ tail(order(lines[[scenario]]), -nos), ]
}

shinyServer(function(input, output){

  cents <- coordinates(leeds)
  cents <- SpatialPointsDataFrame(cents, data = leeds@data, match.ID = F)

  map <- leaflet() %>%
    addTiles(urlTemplate = "http://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png")

  output$map = renderLeaflet(map %>%
                               {
                                 ## Add polygones (of MSOA boundaries)
                                if (input$zone_type == 'msoa')
                                   addPolygons(. , data = leeds
                                               , fillOpacity = 0.4
                                               , opacity = 0.1
                                               , fillColor = leeds$color_pcycle
                                   )
                                 else .
                               } %>%
                               {
                                 if (input$line_type == 'straight' && input$nos_lines != 0)
                                   addGeoJSON(., get_lines(input$map_bounds, input$nos_lines, input$line_attr))
                                 else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'route' && input$nos_lines != 0)
                                   addPolylines(., data = sort_lines(rfast, input$line_attr, input$nos_lines), color = "red"
                                                , opacity = seq(0.8, 0.2, length = abs(input$nos_lines))
                                                , popup = journeyLabel(round(rfast$d / 1000, 1), round(rfast$clc * 10, 2), "Fast")
                                   ) %>%
                                   addPolylines(data = sort_lines(rquiet, input$line_attr, input$nos_lines), color = "green",
                                                , opacity = seq(0.8, 0.2, length = abs(input$nos_lines))
                                                , popup = journeyLabel(round(rquiet$d / 1000, 1), round(rquiet$clc * 10, 2), "Quiet")
                                   )
                                 else
                                   .
                               }%>%
                               addCircleMarkers(data = cents
                                                , radius = 2
                                                , color = "black"
                                                , popup = sprintf("<b>Journeys by bike: </b>%s%%", round(leeds$pCycle*100,2))) %>%
                               {
                                 if (input$feature != "none")
                                   addGeoJSON(., from_cycle_streets(input$map_bounds, input$feature))
                                 else .
                               } %>%
                               mapOptions(zoomToLimits = "first")
  )
})
