# # # # #
# Setup #
# # # # #

# use install.packages() install these
cranPkgs <- c("shiny", "RColorBrewer", "httr", "rgdal", "rgeos")
# use devtools::install_github("rstudio/PkgName")
devPkgs <- c("leaflet", "DT")

lapply(c(cranPkgs, devPkgs), library, character.only = TRUE)

# Colours
zcols <- c("darkslategrey", "yellow")
#zcols <- c("#F0FFFF", "#838B8B")

dataDirRoot <- '../pct-data'
source("load-shiny-data.R", local = T) # to load data

# Functions
source("pct-shiny-funs.R")
LAs <- readOGR(dsn = "las-pcycle.geojson", layer = "OGRGeoJSON")
LAs <- spTransform(LAs, CRS("+init=epsg:4326 +proj=longlat"))

# # # # # # # #
# shinyServer #
# # # # # # # #

shinyServer(function(input, output, session){
  # For all plotting data
  toPlot <- NULL
  # For any other persitant values
  helper <- NULL

  helper$eLatLng <- ""
  helper$dataDir <- file.path(dataDirRoot, startingCity)
  helper$scenarioWas <- NULL

  # To set initialize toPlot
  loadData <- function(dataDir){
    toPlot
    toPlot$l <- readRDS(file.path(dataDir, "l.Rds"))

    toPlot$rFast <- readRDS(file.path(dataDir, "rf.Rds" ))
    toPlot$rFast@data <- cbind(toPlot$rFast@data, toPlot$l@data)
    toPlot$rQuiet <- readRDS(file.path(dataDir, "rq.Rds"))
    toPlot$rQuiet@data <- cbind(toPlot$rQuiet@data, toPlot$l@data)

    toPlot$zones <-  readRDS(file.path(dataDir, "z.Rds"))
    toPlot$cents <-   readRDS(file.path(dataDir, "c.Rds"))

    toPlot$rnet <- readRDS(file.path(dataDir, "rnet.Rds"))
    toPlot$rnet$id <- 1:nrow(toPlot$rnet)

    toPlot
  }

  toPlot <- loadData(helper$dataDir)

  # Selects and sorts lines within a bounding box - given by flowsBB()
  sortLines <- function(lines, sortBy, nos){
    poly <- flowsBB()
    poly <- spTransform(poly, CRS(proj4string(lines)))
    keep <- gContains( poly, lines,byid=TRUE )
    if(all(!keep)) return(NULL)
    linesInBb <- lines[drop(keep), ]
    linesInBb[ tail(order(linesInBb[[sortBy]]), nos), ]
  }

  # Finds the Local Authority shown inside the map bounds
  findLA <- function(){
    BB <- mapBB()
    if(is.null(BB)) return(NULL)
    mapCenter = gCentroid(BB, byid=T)
    keep <- gContains(LAs, mapCenter, byid=T)
    if(all(drop(!keep))) return(NULL) # return NULL if center is outside the LAs shapefile
    tolower(LAs[drop(keep), ]@data$NAME[1])
  }

  attrsZone <- c("Scenario Level of Cycling (SLC)" =    "slc",
                 "Scenario Increase in Cycling (SIC)" = "sic")

  setModelOutput <- function(LA){
    output$moutput <- renderUI({
      modelFile <- file.path(dataDirRoot, LA, "model-output.html")
      if (file.exists(modelFile))
        includeHTML(modelFile)
      else
        HTML("<strong>Sorry but no model output files are avaiable for this local authority</strong>")
    })
  }
  setModelOutput(startingCity)

  observe({ # For highlighting the clicked line
    event <- input$map_shape_click
    if (is.null(event) || event$id == "highlighted")
      return()
    eLatLng <- paste0(event$lat,event$lng)

    # Fix bug when a line has been click then the click event is
    # re-emmited when the map is moved
    if( eLatLng == helper$eLatLng)
      return()
    helper$eLatLng <<- eLatLng

    isolate({
      idGroupName <- unlist(strsplit(event$id, "-"))
      id <- idGroupName[1]
      groupName <- idGroupName[2]
      line <- switch(groupName,
               'straight_line' = toPlot$l[toPlot$l$id == id,],
               'faster_route' = toPlot$rFast[toPlot$rFast$id == id,],
               'quieter_route' = toPlot$rQuiet[toPlot$rQuiet$id == id,],
               'route_network' = toPlot$rnet[toPlot$rnet$id == id,]
               )
      if (!is.null(line))
        leafletProxy("map") %>% addPolylines(data = line, color = "white",
                                             opacity = 0.4, layerId = "highlighted")
    })
  })

  # Updates the Local Authority if the map is moved
  # over another LA with data
  observe({
    if(file.exists(file.path(helper$dataDir, 'isolated'))) return()
    LA <- findLA()
    dataDir <-  file.path(dataDirRoot, LA)

    # Part of the hack to force re-rending when LA changes
    if(!is.null(helper$scenarioWas)){
      updateSelectInput(session, "scenario", selected = helper$scenarioWas)
      helper$scenarioWas <<- NULL
    }

    if(helper$dataDir != dataDir && !is.null(LA) && file.exists(dataDir) ){
      setModelOutput(LA)
      helper$dataDir <<- dataDir
      helper$scenarioWas <<- input$scenario
      toPlot <<- loadData(dataDir)
      if(input$freeze) # If we change the map data then lines should not be frozen to the old map data
        updateCheckboxInput(session, "freeze", value = F)
      if(input$scenario != "olc") # Hack to force the map to re-render
        updateSelectInput(session, "scenario", selected ="olc")
      else
        updateSelectInput(session, "scenario", selected ="cdp")
    }
  })

  # Plot if lines change
  observe({
    leafletProxy("map")  %>% clearGroup(., "straight_line") %>%
      clearGroup(., "quieter_route") %>% clearGroup(., "faster_route") %>% clearGroup(., "route_network") %>%
      removeShape(., "highlighted")

    leafletProxy("map") %>% {
      switch(input$line_type,
             'straight' = plotLines(., toPlot$l, input$nos_lines, straightPopup, "straight_line", getLineColour("straight_line")),
             'route'= plotLines(., toPlot$rQuiet, input$nos_lines, routePopup, "quieter_route", getLineColour("quieter_route")),
             'd_route'=, 'route'= plotLines(., toPlot$rFast, input$nos_lines, routePopup,"faster_route",  getLineColour("faster_route")),
             'rnet' = plotLines(., toPlot$rnet, input$nos_lines, networkRoutePopup, "route_network", getLineColour("route_network"))
      )
    }
    if(input$line_type == 'rnet')
      updateSelectInput(session, inputId = "nos_lines", label = "Percent (%) of Network")
    else
      updateSelectInput(session, inputId = "nos_lines", label = "Number of Lines")

    # needed to force lines to be redrawn when scenario, zone or base map changes
    paste(input$scenario, input$map_base)
  })

  # This function updates the zones and the lines
  observe({
    leafletProxy("map")  %>%  clearGroup(., "zones") %>% clearGroup(., "centers") %>%
      addPolygons(.,  data = toPlot$zones
                  , weight = 2
                  , fillOpacity = transpRate()
                  , opacity = 0.2
                  , fillColor = getColourRamp(zcols, toPlot$zones[[zoneData()]])
                  , color = "black"
                  , group = "zones"
                  , options = pathOptions(clickable=F)) %>%
      addCircleMarkers(., data = toPlot$cents, radius = circleRadius(), color = "black", group = "centers",
                       popup = zonePopup(toPlot$cents, scenario(), zoneAttr()))

    # Change the lines in isolation from the zones - should replicate previous observe
    isolate({
      leafletProxy("map") %>% {
        switch(input$line_type,
               'straight' = plotLines(., toPlot$l, input$nos_lines, straightPopup, "straight_line", getLineColour("straight_line")),
               'route'= plotLines(., toPlot$rQuiet, input$nos_lines, routePopup, "quieter_route", getLineColour("quieter_route")),
               'd_route'=, 'route'= plotLines(., toPlot$rFast, input$nos_lines, routePopup,"faster_route",  getLineColour("faster_route")),
               'rnet' = plotLines(., toPlot$rnet, input$nos_lines, networkRoutePopup, "route_network", getLineColour("route_network"))
        )
      }
    })
  })

  transpRate <- reactive({
    if (input$map_base == 'roadmap') 0.7 else 0.0
  })

  circleRadius <- reactive({
    if (input$map_base == 'roadmap') 2 else 4
  })

  lineAttr <- reactive({
    if(input$scenario == 'olc') 'olc' else 'slc'
  })

  zoneAttr <- reactive({
    if(input$scenario == 'olc') 'olc' else 'slc'
  })

  scenario <- reactive({
    if(input$scenario == 'olc') 'base' else input$scenario
  })

  lineData <- reactive({
    dataFilter(scenario(), lineAttr())
  })

  zoneData <- reactive({
    dataFilter(scenario(), zoneAttr())
  })

  # Reactive function for the lines data
  # 1) Called when other than 'none' is selected for the Cycling Flows
  # 2) Also called when freeze lines is unchecked and the user navigates the map
  # 3) Or when the user changes the Top Lines slider
  plotLinesData <- reactive({
    (input$line_type != 'none' && ((!input$freeze && !is.null(input$map_bounds)) || input$nos_lines > 0)) && (lineData() %in% names(toPlot$l@data))
  })

  # Returns the map bounding box
  mapBB <- reactive({
    if (is.null(input$map_bounds)){ return (NULL)}
    lat <- c(input$map_bounds$west , input$map_bounds$east, input$map_bounds$east, input$map_bounds$west )
    lng <- c(input$map_bounds$north, input$map_bounds$north, input$map_bounds$south, input$map_bounds$south)
    c1 <- cbind(lat, lng)
    r1 <- rbind(c1, c1[1, ])
    bounds <- SpatialPolygons(list(Polygons(list(Polygon(r1)), 'bb')), proj4string=CRS("+init=epsg:4326 +proj=longlat"))
    proj4string(bounds)=CRS("+init=epsg:4326 +proj=longlat")
    bounds
  })

  # Updates the bounding box (bb) to the current map bb unless the map is frozen
  # Returns a bb
  flowsBB <- reactive({
    if(!input$freeze || is.null(helper$bb)){
      helper$bb <<- mapBB()
    }
    helper$bb
  })

  plotLines <- function(m, lines, nos, popupFn, groupName, color){
    if(groupName=="route_network"){
      nos <- nos / 100 * nrow(lines)
      min <- 1
      max <- 20
    } else {
      min <- 3
      max <- 6
    }
    sorted_l <- sortLines(lines, lineData(), nos)
    toPlot$ldata <<- sorted_l
    if(is.null(sorted_l))
      m
    else
      addPolylines(m, data = sorted_l, color = color
                   # Plot widths proportional to attribute value
                   , weight = normalise(sorted_l[[lineData()]], min = min, max = max)
                   , opacity = 0.8
                   , group = groupName,
                   , popup = popupFn(sorted_l, input$scenario)
                   , layerId = paste0(sorted_l[['id']], '-', groupName))
  }

  mapTileUrl <- reactive({
    switch(input$map_base,
           'roadmap' = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",
           'satellite' = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           'IMD"' =  "http://tiles.oobrien.com/imd2015_eng/{z}/{x}/{y}.png"
    )
  })

  output$map = renderLeaflet(
    leaflet() %>%
      addTiles(., urlTemplate = mapTileUrl(),
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
               Routing <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a> |
               Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
               options=tileOptions(opacity = 1, reuseTiles = T)) %>%
      addCircleMarkers(., data = toPlot$cents, radius = circleRadius(), color = "black") %>%
      mapOptions(zoomToLimits = "first")
  )

  output$legendCyclingPotential <- renderPlot({
    # Create quantiles out of the zone data
    m <- quantile(toPlot$zones@data[[zoneData()]], probs=seq.int(0,1, length.out=4))

    # Create a zone colour based on the value of data
    zone_col <- getColourRamp(zcols, m)

    # Set a full form of the scenario as a label
    ylabel <- switch(zoneAttr(),
                     "slc" = "Scenario Level of Cycling (SLC): N. Commuters",
                     "sic" = "Scenario Increase in Cycling (SIC): N. Commuters",
                     "Census 2011 Cycling: N. Commuters"
    )

    # Set the labelling of Y-axis to bold
    par(font.lab = 2)

    # Barplot the data in vertical manner
    barplot(height = rep(1, 4), names.arg = round(matrix(m, nrow=4,ncol=1)),
            col = zone_col, horiz=TRUE, xlab = "", ylab = ylabel, space = 0, axes = FALSE)
  })

  output$linesDatatable <- DT::renderDataTable({
    # Only render lines data when any of the Cycling Flows is selected by the user
    if(!plotLinesData()){
      # Set the warning message that no lines have been selected by the user
      output$warningMessage <- renderUI(HTML("<strong>No lines selected: </strong> Lines must be displayed on map"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    # Empty the warning message - as some lines have been selected by the user
    output$warningMessage <- renderUI("")
    # Reuse the lines data stored in the ldata session variable
    DT::datatable(toPlot$ldata@data, options = list(pageLength = 10)) %>%
      formatRound(columns = colnames(toPlot$ldata@data[sapply(toPlot$ldata@data,is.numeric)]), digits=2)
  })

  output$zonesDataTable <- DT::renderDataTable({
    if(is.null(toPlot$zones@data)){
      return()
    }
    DT::datatable(toPlot$zones@data, options = list(pageLength = 10)) %>%
      formatRound(columns = colnames(toPlot$zones@data[sapply(toPlot$zones@data,is.numeric)]), digits=2)
  })
})
