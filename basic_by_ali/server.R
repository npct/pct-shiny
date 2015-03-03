pkgs <- c("shiny", "leaflet", "ggmap", "RColorBrewer")
lapply(pkgs, library, character.only = TRUE)

# Load data
l <- readRDS("../data/al.Rds")
lfast <- l[ l$color == "green", ]
lquiet <- l[ l$color == "red", ]

flows <- read.csv("../data/al-flow.csv")
leeds <- readRDS("../data/leeds-msoas-simple.Rds")

shinyServer(function(input, output, session){

  cents <- coordinates(leeds)
  cents <- SpatialPointsDataFrame(cents, data = leeds@data, match.ID = F)

  zoom <- reactive({
    ifelse(is.null(input$map_zoom),11,input$map_zoom)
  })

  observe({
    map <- leaflet() %>%
      addTiles(urlTemplate = "http://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png") %>%
      addPolylines(data = lfast, color = "red"
                   , opacity = input$transp_fast
                   , popup = sprintf("<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd>", round(flows$fastest_distance_in_m / 1000, 1), round(flows$p_cycle * 100, 2))
      ) %>%
      addPolylines(data = lquiet, color = "green",
                   , opacity = input$transp_fast
                   , popup = sprintf("<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd>", round(flows$quietest_distance_in_m / 1000, 1), round(flows$p_cycle * 100, 2))
      ) %>%
      addCircleMarkers(data = cents
                       , radius = 2
                       , color = "black"
                       , popup = sprintf("<b>Journeys by bike: </b>%s%%", round(leeds$pCycle * 100, 2))) %>%
      addGeoJSON(RJSONIO::fromJSON(sprintf("../data/%s.geojson", input$feature))) %>%
      mapOptions(zoomToLimits = "first")
    ## Add polygones (of MSOA boundaries) ONLY when the checkbox transp_zones is CHECKED
    if (input$transp_zones == TRUE){
      map <- map %>%
        addPolygons(data = leeds
                    , fillOpacity = 0.4
                    , opacity = (input$transp_zones)*.4
                    , fillColor = leeds$color_pcycle
        )
    }
    output$map = renderLeaflet(map)
  })



})