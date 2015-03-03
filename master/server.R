pkgs <- c("shiny", "leaflet", "ggmap", "RColorBrewer", "httr")
lapply(pkgs, library, character.only = TRUE)

cyclestreet_token <- Sys.getenv('CYCLESTREET')

empty_geojson <- '{"type": "Point","coordinates": [-1.5492,53.7997]}'

cycle_street_bbox <- function(bounds){
  paste(bounds$west, bounds$south, bounds$east, bounds$north, sep=",")
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
l <- readRDS("../data/al.Rds")
lfast <- l[ l$color == "green", ]
lquiet <- l[ l$color == "red", ]

flows <- read.csv("../data/al-flow.csv")
leeds <- readRDS("../data/leeds-msoas-simple.Rds")

journeyLabel <- function(distance, percentage){
  sprintf("<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd>", distance, percentage)
}

shinyServer(function(input, output){

  cents <- coordinates(leeds)
  cents <- SpatialPointsDataFrame(cents, data = leeds@data, match.ID = F)

  map <- leaflet() %>%
    addTiles(urlTemplate = "http://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png")

  output$map = renderLeaflet(map%>%
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
                               addPolylines(data = lfast, color = "red"
                                            , opacity = input$transp_fast
                                            , popup = journeyLabel(round(flows$fastest_distance_in_m / 1000, 1), round(flows$p_cycle * 100, 2))
                               ) %>%
                               addPolylines(data = lquiet, color = "green",
                                            , opacity = input$line_transp
                                            , popup = journeyLabel(round(flows$quietest_distance_in_m / 1000, 1), round(flows$p_cycle * 100, 2))
                               ) %>%
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
