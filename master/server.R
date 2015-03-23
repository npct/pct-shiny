url <- "https://github.com/npct/pct-data/archive/master.zip" # data store
if(sum(grepl("pct-data", list.files("../"))) == 0){
  download.file(url, destfile = "../d.zip", method = "wget")
  unzip("../d.zip", exdir = "../")
}

data_dir <- "../pct-data-master/coventry/" # data directory

pkgs <- c("shiny", "leaflet", "ggmap", "RColorBrewer", "httr", "rgeos", "rgdal", "dplyr")
lapply(pkgs, library, character.only = TRUE)

source("cyclestreet.R")

# # # # # # #
# Functions #
# # # # # # #

source("pct-shiny-funs.R")

# # # # # # #
# Load data #
# # # # # # #

rfast <- readRDS(paste0(data_dir, "rf.Rds" ))
rquiet <- readRDS(paste0(data_dir, "rq.Rds"))

l <- readRDS(paste0(data_dir, "l.Rds"))
rfast@data <- cbind(l@data, rfast@data)
rquiet@data <- cbind(l@data, rquiet@data)
zones <- readRDS(paste0(data_dir, "z.Rds"))

cents <- readRDS(paste0(data_dir, "c.Rds"))
flow <- l@data
rfast@data <- cbind(rfast@data, l@data)
rquiet@data <- cbind(rquiet@data, l@data)

shinyServer(function(input, output, session){

  sortLines <- function(lines, sortBy, nos){
    if(!(sortBy %in% names(lines))) return(NULL)
    poly <- bbPoly()
    poly <- spTransform(poly, CRS(proj4string(lines)))
    keep <- gContains( poly, lines,byid=TRUE )
    if(all(!keep)) return(NULL)
    linesInBb <- lines[drop(keep), ]
    linesInBb[ tail(order(linesInBb[[sortBy]]), nos), ]
  }

  attrs <- c("Current Level Cycling (CLC)" =       "clc"
             ,"Potential Level of Cycling (PLC)" = "plc"
             ,"Extra Cycling Potential (ECP)" =    "ecp")

  observe({
    if(input$scenario != "base"){
      updateSelectInput(session, "zone_attr", choices = attrs[2:3])
      updateSelectInput(session, "line_attr", choices = attrs[2:3])
    }else{
      updateSelectInput(session, "zone_attr", choices = attrs)
      updateSelectInput(session, "line_attr", choices = attrs)
    }
  })

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

  sortAndPlot <- function(m, lines, attr, nos, popupFn, color){
    sorted_l <- sortLines(lines, attr, nos)
    if(is.null(sorted_l))
      .
    else
      addPolylines(m, data = sorted_l, color = color
                   # Plot widths proportional to attribute value
                   , weight = normalise(sorted_l@data[[attrWithScenario(input$line_attr, input$scenario)]], min = 3, max = 6)
                   , opacity = 0.7
                   , popup = popupFn(sorted_l))
  }

  map <- leaflet() %>%
    addTiles(urlTemplate = "http://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png")

  output$map = renderLeaflet(map %>%
                               {
                                 ## Add polygons (of MSOA boundaries)
                                 if (input$zone_type == 'msoa' && (attrWithScenario(input$zone_attr, input$scenario) %in% names(zones@data)))
                                   addPolygons(. , data = zones
                                               , weight = 2
                                               , fillOpacity = 0.2
                                               , opacity = 0.3
                                               # From red to blue gradient of colours based on the clc variable of zones dataset
                                               , fillColor = getColourRamp(c("red", "blue"), zones@data[[attrWithScenario(input$zone_attr, input$scenario)]])
                                               , color = "black"
                                    , popup = sprintf("Zone: %s <br> CLC: %s <br> Hilliness %s (degress) ", zones$geo_code, round(zones$clc * 100, ), round(zones$avslope, 2))
                                   )
                                 else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'straight'){
                                   sortAndPlot(., l, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                                               straightPopup, color = "blue")
                                 }else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'route'){
                                   sortAndPlot(., rfast, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                                               routePopup, "red") %>%
                                   sortAndPlot(., rquiet, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                                               routePopup, "green")

                                 }else
                                   .
                               }%>%
                               addCircleMarkers(data = cents
                                                , radius = 2
                                                , color = "black"
                                                , popup = sprintf("<b>%% journeys by bike: </b>%s%%", round(zones$clc * 100,2))) %>%
                               {
                                 if (input$feature != "none")
                                   addGeoJSON(., from_cycle_streets(input$map_bounds, input$feature))
                                 else .
                               } %>%
                               mapOptions(zoomToLimits = "first")
  )
})
