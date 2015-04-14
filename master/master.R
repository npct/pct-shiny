# # # # #
# Setup #
# # # # #

# use install.packages() or devtools::install_github() to install these
pkgs <- c("shiny", "leaflet", "RColorBrewer", "httr", "rgdal", "downloader", "rgeos")
lapply(pkgs, library, character.only = TRUE)

# Colours
zcols <- c("MidnightBlue", "Yellow")

# Download files - remove first if needed
if(sum(grepl("rf", list.files(data_dir))) == 0){
  url <- "https://github.com/npct/pct-data/archive/master.zip" # data store
  download(url = url, destfile = "../d.zip")
  unzip("../d.zip", exdir = "..")
}

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
      updateSelectInput(session, "zone_attr", choices = attrsZone[2:4])
      updateSelectInput(session, "line_attr", choices = attrsLine[2:3])
    }else{
      updateSelectInput(session, "zone_attr", choices = attrsZone)
      updateSelectInput(session, "line_attr", choices = attrsLine)
    }
  })

  plotZones <- reactive({ # Some attributes are only avaliable for baseline
    (input$zone_attr != 'none') && (attrWithScenario(input$zone_attr, input$scenario) %in% names(zones@data))
  })

  bbPoly <- reactive({
    if(!input$freeze || is.null(session$bb)){
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
                   , weight = normalise(sorted_l@data[[attrWithScenario(input$line_attr, input$scenario)]], min = 3, max = 6)
                   , opacity = 0.7
                   , popup = popupFn(sorted_l))
  }

  map <- leaflet() %>%
    addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png")

  output$map = renderLeaflet(
    map %>% {
      ## Add polygons (of MSOA boundaries)
      if(plotZones())
        addPolygons(. , data = zones
                    , weight = 2
                    , fillOpacity = 0.5
                    , opacity = 0.3
                    , fillColor = getColourRamp(zcols, zones@data[[attrWithScenario(input$zone_attr, input$scenario)]])
                    , color = "black"
        )
      else
        .
    }%>%{
      if (input$line_type == 'straight'){
        sortAndPlot(., l, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                    straightPopup, color = "maroon")
      }else
        .
    }%>%{
      if (input$line_type %in% c('route', 'd_route'))
        sortAndPlot(., rfast, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                    routePopup, "red")
      else
        .
    }%>%{
      if (input$line_type == 'route')
        sortAndPlot(., rquiet, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                    routePopup, "darkblue")
      else
        .
    }%>%{
      if (input$zone_attr != 'none')
        addCircleMarkers(., data = cents, radius = 2, color = "black", popup = zonePopup(cents, input$scenario, input$zone_attr))
      else
        .
    }%>%
      mapOptions(zoomToLimits = "first")
  )

  output$legendCyclingPotential <- renderPlot({
    if (is.null(input$zone_attr) || is.null(input$scenario)){
      return()
    }
    if (input$zone_attr != 'none'){
      # Read the zone data
      data_ <- zones@data[[attrWithScenario(input$zone_attr, input$scenario)]]
      # Create quantiles out of the data
      m <- quantile(data_, probs=seq.int(0,1, length.out=4))
      # Sort the data
      m <- sort(m)
      # Create a zone colour based on the absolute value of data (as data can be negative as well)
      zone_col <- getColourRamp(zcols, m)
      # Barplot the data in vertical manner
      barplot(height = rep(1, 4), names.arg = round(matrix(m, nrow=4,ncol=1)), col = zone_col, horiz=TRUE, xlab = "", ylab = input$zone_attr, space = 0)

    }
  })
})
