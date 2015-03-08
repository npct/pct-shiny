pkgs <- c("shiny", "leaflet", "ggmap", "RColorBrewer", "httr")
lapply(pkgs, library, character.only = TRUE)

cyclestreet_token <- Sys.getenv('CYCLESTREET')

empty_geojson <- '{"type": "Point","coordinates": [-1.5492,53.7997]}'

cycle_street_bbox <- function(bounds){
  paste(bounds$west, bounds$south, bounds$east, bounds$north, sep=",")
}

ascending_descending <- function(sort_by){
  if(sort_by$nos>0) paste(sort_by$attr, '+A') else paste(sort_by$attr, '+D')
}

get_lines <- function(bounds, sort_by){
  if(is.null(bounds)){ return(empty_geojson) }
  query_list <- list(service='WFS'
                     ,version='1.0.0'
                     ,request='GetFeature'
                     ,typeName='shiny:leeds-msoas'
                     ,bbox=cycle_street_bbox(bounds)
                     ,maxFeatures='20'
                     ,outputFormat='application/json'
  )
  if(!is.null(sort_by)){
    query_list <- list(query_list
                       ,list(sortBy=ascending_descending(sort_by))
    )
  }
  resp <- GET('http://geo8.webarch.net:8080/geoserver/shiny/ows', query = query_list )
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
l <- readRDS("../data/l.Rds")
routes <- readRDS("../data/al.Rds")
rfast <- routes[ routes$route == "fast", ]
rquiet <- routes[ routes$route == "quiet", ]
flows <- read.csv("../data/al-flow.csv")
leeds <- readRDS("../data/leeds-msoas-simple.Rds")

journeyLabel <- function(distance, percentage){
  sprintf("<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd>", distance, percentage)
}

sort_lines <- function(lines, scenario, nos){
  if(nos > 0)
    lines[ head(order(lines[[scenario]]), nos), ]
  else
    lines[ tail(order(lines[[scenario]]), (nos*-1)), ]
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
                                               , opacity = 0.4
                                               , fillColor = leeds$color_pcycle
                                   )
                                 else .
                               } %>%
                               {
                                 if (input$line_type == 'straight' && input$nos_lines != 0)
                                   addPolylines(., data = sort_lines(l, input$line_attr, input$nos_lines)
                                                , opacity = sort(runif(abs(input$nos_lines), min = 0.4, max = 0.8), decreasing = T),
                                                , popup = journeyLabel(round(flows$fastest_distance_in_m / 1000, 1), round(flows$p_cycle * 10, 2)))
                                 else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'route' && input$nos_lines != 0)
                                   addPolylines(., data = sort_lines(rfast, input$line_attr, input$nos_lines), color = "red"
                                                , opacity = sort(runif(abs(input$nos_lines), min = 0.2, max = 0.8), decreasing = T)
                                                , popup = journeyLabel(round(rfast$d / 1000, 1), round(rfast$clc * 10, 2))
                                   ) %>%
                                   addPolylines(data = sort_lines(rquiet, input$line_attr, input$nos_lines), color = "green",
                                                , opacity = sort(runif(abs(input$nos_lines), min = 0.2, max = 0.8), decreasing = T)
                                                , popup = journeyLabel(round(rquiet$d / 1000, 1), round(rquiet$clc * 10, 2))
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
