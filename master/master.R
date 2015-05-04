# # # # #
# Setup #
# # # # #

# use install.packages() or devtools::install_github() to install these
pkgs <- c("shiny", "shinyBS", "leaflet", "RColorBrewer", "httr", "rgdal", "downloader", "rgeos", "curl", "jsonlite")
lapply(pkgs, library, character.only = TRUE)

# Colours
zcols <- c("darkslategrey", "yellow")

# Download data files
# This will timeout on the server (so a cron job is used instead)
# but will work locally
system2('./update-data.sh', wait = FALSE)

# Functions
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

# # # # # # # #
# shinyServer #
# # # # # # # #

shinyServer(function(input, output, session){


  sortLines <- function(lines, sortBy, nos){
    if(!(sortBy %in% names(lines))) return(NULL)
    poly <- bbPoly()
    if(is.null(poly)) return(NULL)
    poly <- spTransform(poly, CRS(proj4string(lines)))
    keep <- gContains( poly, lines,byid=TRUE )
    if(all(!keep)) return(NULL)
    linesInBb <- lines[drop(keep), ]
    linesInBb[ tail(order(linesInBb[[sortBy]]), nos), ]
  }

  attrsLine <- c("Observed Level Cycling (OLC)" = "olc"
                 ,"Scenario-based Level of Cycling (SLC)" =    "slc"
                 ,"Scenario-based Increase in Cycling (SIC)" = "sic")

  attrsZone <- c(attrsLine, c("None" = "none"))

  observe({
    if(input$scenario != "base"){
      if(input$advanced)
        updateSelectInput(session, "line_attr", choices = attrsLine[2:3])
      updateSelectInput(session, "zone_attr", choices = attrsZone[2:4])
    }else{
      if(input$advanced)
        updateSelectInput(session, "line_attr", choices = attrsLine)
      updateSelectInput(session, "zone_attr", choices = attrsZone)
    }
  })

  lineAttr <- reactive({
    if(input$advanced)
      input$line_attr
    else
      input$zone_attr
  })

  plotZones <- reactive({ # Some attributes are only avaliable for baseline
    (input$zone_attr != 'none') && (dataFilter(input$scenario, input$zone_attr) %in% names(zones@data))
  })

  bbPoly <- reactive({
    if(!input$freeze || is.null(session$bb)){
      if (is.null(input$map_bounds)){ return (NULL)}
      lat <- c(input$map_bounds$west , input$map_bounds$east, input$map_bounds$east, input$map_bounds$west )
      lng <- c(input$map_bounds$north, input$map_bounds$north, input$map_bounds$south, input$map_bounds$south)

      c1 <- cbind(lat, lng)
      r1 <- rbind(c1, c1[1, ])
      session$bb <- SpatialPolygons(list(Polygons(list(Polygon(r1)), 'bb')), proj4string=CRS("+init=epsg:4326 +proj=longlat"))
      proj4string(session$bb)=CRS("+init=epsg:4326 +proj=longlat")
    }
    return(session$bb)
  })

  sortAndPlot <- function(m, lines, attr, nos, popupFn, color){
    sorted_l <- sortLines(lines, attr, nos)
    if(is.null(sorted_l))
      m
    else
      addPolylines(m, data = sorted_l, color = color
                   # Plot widths proportional to attribute value
                   , weight = normalise(sorted_l@data[[dataFilter(input$scenario, lineAttr())]], min = 3, max = 6)
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
      addTiles(urlTemplate = mapTileUrl(), options=tileOptions(opacity = 0.8, reuseTiles = T))
    %>%{
      ## Add polygons (of MSOA boundaries)
      if(plotZones())
        addPolygons(. , data = zones
                    , weight = 2
                    , fillOpacity = 0.6
                    , opacity = 0.4
                    , fillColor = getColourRamp(zcols, zones@data[[dataFilter(input$scenario, input$zone_attr)]])
                    , color = "black"
                    , options = pathOptions(clickable=F)

        )
      else
        .
    }%>%{
      if (input$line_type == 'straight'){
        sortAndPlot(., l, dataFilter(input$scenario, lineAttr()), input$nos_lines,
                    straightPopup, color = "maroon")
      }else
        .
    }%>%{
      if (input$line_type %in% c('route', 'd_route'))
        sortAndPlot(., rfast, dataFilter(input$scenario, lineAttr()), input$nos_lines,
                    routePopup, "red")
      else
        .
    }%>%{
      if (input$line_type == 'route')
        sortAndPlot(., rquiet, dataFilter(input$scenario, lineAttr()), input$nos_lines,
                    routePopup, "darkblue")
      else
        .
    }%>%{
      if (plotZones())
        addCircleMarkers(., data = cents, radius = 2, color = "black", popup = zonePopup(cents, input$scenario, input$zone_attr))
      else
        .
    }%>%
      mapOptions(zoomToLimits = "first")
  )

  output$legendCyclingPotential <- renderPlot({
    if(!(plotZones()) || is.null(input$zone_attr) || is.null(input$scenario)){
      return()
    }
    # Read the zone data
    data_ <- zones@data[[dataFilter(input$scenario, input$zone_attr)]]
    # Create quantiles out of the data
    m <- quantile(data_, probs=seq.int(0,1, length.out=4))

    # Create a zone colour based on the value of data
    zone_col <- getColourRamp(zcols, m)

    # Set a full form of the scenario as a label
    ylabel <- "Observed Level Cycling (OLC): cycle commuters"
    if (input$zone_attr == "slc")
      ylabel <- "Scenario-based Level of Cycling (SLC)"
    else if (input$zone_attr == "sic")
      ylabel <- "Scenario-based Increase in Cycling (SIC)"

    # Set the labelling of Y-axis to bold
    par(font.lab = 2)
    # Barplot the data in vertical manner
    barplot(height = rep(1, 4), names.arg = round(matrix(m, nrow=4,ncol=1)),
            col = zone_col, horiz=TRUE, xlab = "", ylab = ylabel, space = 0, axes = FALSE)

  })
})
