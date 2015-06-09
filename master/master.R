# # # # #
# Setup #
# # # # #

# use install.packages() or devtools::install_github() to install these
pkgs <- c("shiny", "leaflet", "RColorBrewer", "httr", "rgdal", "rgeos", "DT")

# Install packages if they are not already installed
if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkgs, rownames(installed.packages())))
}

# Install all packages
lapply(pkgs, library, character.only = TRUE)

# Colours
zcols <- c("darkslategrey", "yellow")

dataDirRoot <- file.path('..', 'pct-data')
# clone the data repo if it do not exist
if(!dir.exists(dataDirRoot)) system2('git', args=c('clone', '--depth=1', 'https://github.com/npct/pct-data.git', dataDirRoot))

# Download files
setwd(dataDirRoot)
system2('git', args=c("pull"), wait = FALSE)
setwd(file.path('..', 'master'))

# Functions
source("pct-shiny-funs.R")
LAs <- readOGR(dsn = "las-pcycle.geojson", layer = "OGRGeoJSON")
LAs <- spTransform(LAs, CRS("+init=epsg:4326 +proj=longlat"))

# # # # # # # #
# shinyServer #
# # # # # # # #

shinyServer(function(input, output, session){
  helper <- NULL
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

  attrsLine <- c("Scenario Level of Cycling (SLC)" =    "slc",
                 "Scenario Increase in Cycling (SIC)" = "sic")

  attrsZone <- c(attrsLine, c("None" = "none"))

  observe({
    LA <- findLA()
    dataDir <-  file.path(dataDirRoot, LA)

    if(input$advanced)
      updateSelectInput(session, 'zone_attr', label = 'Zone Attribute', choices = attrsZone)
    else
      updateSelectInput(session, 'zone_attr', label = 'Attribute to display', choices = attrsLine)

    if(!is.null(helper$scenarioWas)){
      updateSelectInput(session, "scenario", selected = helper$scenarioWas)
      helper$scenarioWas <<- NULL
    }

    if(helper$dataDir != dataDir && !is.null(LA) && file.exists(dataDir) ){
      helper$dataDir <<- dataDir
      helper$scenarioWas <<- input$scenario
      helper <<- loadData(helper)
      if(input$freeze)
        updateCheckboxInput(session, "freeze", value = F)
      if(input$scenario != "olc")
        updateSelectInput(session, "scenario", selected ="olc")
      else
        updateSelectInput(session, "scenario", selected ="base")
    }
  })

  lineAttr <- reactive({
    if(input$scenario == 'olc')
      'olc'
    else if(input$advanced)
      input$line_attr
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

  plotZones <- reactive({ # Some attributes are only avaliable for baseline
    (input$zone_attr != 'none') && (zoneData() %in% names(helper$zones@data))
  })

  # Reactive function for the lines data
  # 1) Called when other than 'none' is selected for the Cycling Flows
  # 2) Also called when freeze lines is unchecked and the user navigates the map
  # 3) Or when the user changes the Top Lines slider
  plotLinesData <- reactive({ # Only called when line_type is 'none' or
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
                   , opacity = 0.7
                   , popup = popupFn(sorted_l, input$scenario))
  }

  mapTileUrl <- reactive({
    if (input$map_base == 'bw')
      "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png"
    else
      "http://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png"
  })

  map <- leaflet()

  output$map = renderLeaflet(
    map %>%
      addTiles(urlTemplate = mapTileUrl(),
               attribution = 'Route data from <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a>',
               options=tileOptions(opacity = 0.8, reuseTiles = T))
    %>%{
      ## Add polygons (of MSOA boundaries)
      if(plotZones())
        addPolygons(. , data = helper$zones
                    , weight = 2
                    , fillOpacity = 0.6
                    , opacity = 0.4
                    , fillColor = getColourRamp(zcols, helper$zones[[zoneData()]])
                    , color = "black"
                    , options = pathOptions(clickable=F)
        )
      else
        .
    }%>%{
      if (input$line_type == 'straight'){
        plotLines(., helper$l, input$nos_lines, straightPopup, color = "maroon")
      }else
        .
    }%>%{
      if (input$line_type == 'route')
        plotLines(., helper$rQuiet, input$nos_lines, routePopup, "turquoise")
      else
        .
    }%>%{
      if (input$line_type %in% c('d_route', 'route'))
        plotLines(., helper$rFast, input$nos_lines, routePopup, "purple")
      else
        .
    }%>%{
      if (plotZones())
        addCircleMarkers(., data = helper$cents, radius = 2, color = "black",
                         popup = zonePopup(helper$cents, scenario(), zoneAttr()))
      else
        .
    }%>%
      mapOptions(zoomToLimits = "first")
  )

  output$legendCyclingPotential <- renderPlot({
    if(!plotZones()){
      return()
    }
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
      return()
    }
    # Empty the warning message - as some lines have been selected by the user
    output$warningMessage <- renderUI("")
    # Reuse the lines data stored in the ldata session variable
    DT::datatable(helper$ldata@data, options = list(pageLength = 10))
  })

  output$zonesDataTable <- DT::renderDataTable({
    if(is.null(helper$zones@data)){
      return()
    }
    DT::datatable(helper$zones@data, options = list(pageLength = 10))
  })


})
