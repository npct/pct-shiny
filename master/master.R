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
  helper <- NULL
  helper$eLatLng <- ""
  loadData <- function(helper){
    helper$l <- readRDS(file.path(helper$dataDir, "l.Rds"))

    helper$rFast <- readRDS(file.path(helper$dataDir, "rf.Rds" ))
    helper$rFast@data <- cbind(helper$rFast@data, helper$l@data)
    helper$rQuiet <- readRDS(file.path(helper$dataDir, "rq.Rds"))
    helper$rQuiet@data <- cbind(helper$rQuiet@data, helper$l@data)

    helper$zones <-  readRDS(file.path(helper$dataDir, "z.Rds"))
    helper$cents <-   readRDS(file.path(helper$dataDir, "c.Rds"))
    helper
  }

  helper$dataDir <- file.path(dataDirRoot, startingCity)
  helper$scenarioWas <- NULL
  helper <- loadData(helper)

  sortLines <- function(lines, sortBy, nos){
    if(!(sortBy %in% names(lines))) return(NULL)
    poly <- flowsBB()
    if(is.null(poly)) return(NULL)
    poly <- spTransform(poly, CRS(proj4string(lines)))
    keep <- gContains( poly, lines,byid=TRUE )
    if(all(!keep)) return(NULL)
    linesInBb <- lines[drop(keep), ]
    linesInBb[ tail(order(linesInBb[[sortBy]]), nos), ]
  }

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
      idColor <- unlist(strsplit(event$id, "-"))
      id <- idColor[1]
      color <- idColor[2]
      if (input$line_type == 'straight')
        line <- helper$l[helper$l$id == id,]
      else if (color == "purple")
        line <- helper$rFast[helper$rFast$id == id,]
      else
        line <- helper$rQuiet[helper$rQuiet$id == id,]
      leafletProxy("map") %>% addPolylines(data = line, color = "white", opacity = 0.4,
                                           layerId = "highlighted")
    })
  })
  observe({
    LA <- findLA()
    dataDir <-  file.path(dataDirRoot, LA)

    if(!is.null(helper$scenarioWas)){
      updateSelectInput(session, "scenario", selected = helper$scenarioWas)
      helper$scenarioWas <<- NULL
    }

    if(helper$dataDir != dataDir && !is.null(LA) && file.exists(dataDir) ){
      setModelOutput(LA)
      helper$dataDir <<- dataDir
      helper$scenarioWas <<- input$scenario
      helper <<- loadData(helper)
      if(input$freeze) # If we change the map data then lines should not be frozen to the old map data
        updateCheckboxInput(session, "freeze", value = F)
      if(input$scenario != "olc") # Hack to force the map to re-render when a new zone is selected
        updateSelectInput(session, "scenario", selected ="olc")
      else
        updateSelectInput(session, "scenario", selected ="base")
    }
  })
  observe({
    leafletProxy("map")  %>% clearGroup(., "maroon") %>%
      clearGroup(., "turquoise") %>% clearGroup(., "purple")
    if (input$line_type == 'straight')
      leafletProxy("map") %>% plotLines(., helper$l, input$nos_lines, straightPopup, "maroon")
    if (input$line_type == 'route')
      leafletProxy("map") %>% plotLines(., helper$rQuiet, input$nos_lines, routePopup, "turquoise")
    if (input$line_type %in% c('d_route', 'route'))
      leafletProxy("map") %>% plotLines(., helper$rFast, input$nos_lines, routePopup, "purple")
    transpRate() # needed to force lines to be redrawn when scenario
  })
  observe({
    leafletProxy("map")  %>% clearShapes() %>%
      addPolygons(.,  data = helper$zones
                  , weight = 2
                  , fillOpacity = transpRate()
                  , opacity = 0.2
                  , fillColor = getColourRamp(zcols, helper$zones[[zoneData()]])
                  , color = "black"
                  , options = pathOptions(clickable=F)) %>%
      addCircleMarkers(., data = helper$cents, radius = 2, color = "black",
                       popup = zonePopup(helper$cents, scenario(), zoneAttr()))
  })
  transpRate <- reactive({
    if (input$map_base == 'acetate')  # Have the satalite map more transparent
      0.7
    else
      0.4
  })

  lineAttr <- reactive({
    if(input$scenario == 'olc')
      'olc'
    else
      input$zone_attr
  })

  zoneAttr <- reactive({
    if(input$scenario == 'olc') 'olc' else input$zone_attr
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
    (input$line_type != 'none' && ((!input$freeze && !is.null(input$map_bounds)) || input$nos_lines > 0)) && (lineData() %in% names(helper$l@data))
  })

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

  flowsBB <- reactive({
    if(!input$freeze || is.null(helper$bb)){
      helper$bb <<- mapBB()
    }
    helper$bb
  })

  plotLines <- function(m, lines, nos, popupFn, color){
    sorted_l <- sortLines(lines, lineData(), nos)
    helper$ldata <<- sorted_l
    if(is.null(sorted_l))
      m
    else
      addPolylines(m, data = sorted_l, color = color
                   # Plot widths proportional to attribute value
                   , weight = normalise(sorted_l[[lineData()]], min = 3, max = 6)
                   , opacity = 0.7, group = color,
                   , popup = popupFn(sorted_l, input$scenario)
                   , layerId = paste0(sorted_l[['id']], '-', color))
  }

  mapTileUrl <- reactive({
    if (input$map_base == 'acetate')
      "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png"
    else
      "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
  })

  output$map = renderLeaflet(
    leaflet() %>%
      addTiles(., urlTemplate = mapTileUrl(),
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> | Route data from <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a>',
               options=tileOptions(opacity = 1, reuseTiles = T)) %>%
      addCircleMarkers(., data = helper$cents, radius = 0.1, color = "black") %>%
      mapOptions(zoomToLimits = "first")
  )

  output$legendCyclingPotential <- renderPlot({
    # Read the zone data
    data_ <- helper$zones@data[[zoneData()]]
    # Create quantiles out of the data
    m <- quantile(data_, probs=seq.int(0,1, length.out=4))

    # Create a zone colour based on the value of data
    zone_col <- getColourRamp(zcols, m)

    # Set a full form of the scenario as a label
    ylabel <- "Census 2011 Cycling: N. Commuters"
    if (zoneAttr() == "slc")
      ylabel <- "Scenario Level of Cycling (SLC): N. Commuters"
    else if (zoneAttr() == "sic")
      ylabel <- "Scenario Increase in Cycling (SIC): N. Commuters"

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
    DT::datatable(helper$ldata@data, options = list(pageLength = 10))  %>% formatRound(columns = colnames(helper$ldata@data[sapply(helper$ldata@data,is.numeric)]), digits=2)
  })

  output$zonesDataTable <- DT::renderDataTable({
    if(is.null(helper$zones@data)){
      return()
    }
    DT::datatable(helper$zones@data, options = list(pageLength = 10))   %>% formatRound(columns = colnames(helper$zones@data[sapply(helper$zones@data,is.numeric)]), digits=2)
  })


})
