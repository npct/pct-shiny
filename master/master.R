# # # # #
# Setup #
# # # # #

# use install.packages() or devtools::install_github() to install these
pkgs <- c("shiny", "shinyBS", "leaflet", "RColorBrewer", "httr", "rgdal", "downloader", "rgeos", "curl", "jsonlite")
lapply(pkgs, library, character.only = TRUE)

# Colours
zcols <- c("darkslategrey", "yellow")


if (Sys.info()["sysname"] != "Windows") {
  # Download data files
  # This will timeout on the server (so a cron job is used instead)
  # but will work locally
  system2('./update-data.sh', wait = FALSE)
}else {
  dataDir <- file.path('..', 'pct-data')

  # clone the data repo if it do not exist
  ifelse(!dir.exists(dataDir), system2('git', args=c('clone', '--depth=1', 'https://github.com/npct/pct-data.git', dataDir)), FALSE)

  # Download files
  setwd(dataDir)
  system2('git', args=c("pull"), wait = FALSE)
  setwd(file.path('..', 'master'))

}

# Functions
source("pct-shiny-funs.R")

# # # # # # #
# Load data #
# # # # # # #

rFast <- readRDS(paste0(data_dir, "rf.Rds" ))
rQuiet <- readRDS(paste0(data_dir, "rq.Rds"))

l <- readRDS(paste0(data_dir, "l.Rds"))
rFast@data <- cbind(l@data, rFast@data)
rQuiet@data <- cbind(l@data, rQuiet@data)
zones <- readRDS(paste0(data_dir, "z.Rds"))

cents <- readRDS(paste0(data_dir, "c.Rds"))
flow <- l@data
rFast@data <- cbind(rFast@data, l@data)
rQuiet@data <- cbind(rQuiet@data, l@data)

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
    (input$zone_attr != 'none') && (zoneData() %in% names(zones@data))
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
    session$bb
  })

  plotLines <- function(m, lines, nos, popupFn, color){
    sorted_l <- sortLines(lines, lineData(), nos)
    if(is.null(sorted_l))
      m
    else
      addPolylines(m, data = sorted_l, color = color
                   # Plot widths proportional to attribute value
                   , weight = normalise(sorted_l@data[[lineData()]], min = 3, max = 6)
                   , opacity = 0.7
                   , popup = popupFn(sorted_l, scenario()))
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
                    , fillColor = getColourRamp(zcols, zones@data[[zoneData()]])
                    , color = "black"
                    , options = pathOptions(clickable=F)
        )
      else
        .
    }%>%{
      if (input$line_type == 'straight'){
        plotLines(., l, input$nos_lines, straightPopup, color = "maroon")
      }else
        .
    }%>%{
      if (input$line_type %in% c('route', 'd_route'))
        plotLines(., rFast, input$nos_lines, routePopup, "red")
      else
        .
    }%>%{
      if (input$line_type == 'route')
        plotLines(., rQuiet, input$nos_lines, routePopup, "darkblue")
      else
        .
    }%>%{
      if (plotZones())
        addCircleMarkers(., data = cents, radius = 2, color = "black", popup = zonePopup(cents, scenario(), zoneAttr()))
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
    data_ <- zones@data[[zoneData()]]
    # Create quantiles out of the data
    m <- quantile(data_, probs=seq.int(0,1, length.out=4))

    # Create a zone colour based on the value of data
    zone_col <- getColourRamp(zcols, m)

    # Set a full form of the scenario as a label
    ylabel <- "Observed Level Cycling (OLC): cycle commuters"
    if (zoneAttr() == "slc")
      ylabel <- "Scenario-based Level of Cycling (SLC)"
    else if (zoneAttr() == "sic")
      ylabel <- "Scenario-based Increase in Cycling (SIC)"

    # Set the labelling of Y-axis to bold
    par(font.lab = 2)
    # Barplot the data in vertical manner
    barplot(height = rep(1, 4), names.arg = round(matrix(m, nrow=4,ncol=1)),
            col = zone_col, horiz=TRUE, xlab = "", ylab = ylabel, space = 0, axes = FALSE)

  })
})
