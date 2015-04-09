# # # # #
# Setup #
# # # # #

# use install.packages() or devtools::install_github() to install these
pkgs <- c("shiny", "leaflet", "RColorBrewer", "httr", "rgdal", "downloader", "rgeos")
lapply(pkgs, library, character.only = TRUE)

# Download files
if(sum(grepl("data", list.files("../"))) == 0){
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

  output$map = renderLeaflet(map %>%
                               {
                                 ## Add polygons (of MSOA boundaries)
                                 if (attrWithScenario(input$zone_attr, input$scenario) %in% names(zones@data)) # CLC is only avaliable for baseline
                                   addPolygons(. , data = zones
                                               , weight = 2
                                               , fillOpacity = 0.6
                                               , opacity = 0.3
                                               # From light green to dark green gradient of colours based on the zone variable
                                               , fillColor = getColourRamp(c("lightgreen", "darkgreen"), zones@data[[attrWithScenario(input$zone_attr, input$scenario)]])
                                               , color = "black"
                                   )
                                 else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'straight'){
                                   sortAndPlot(., l, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                                               straightPopup, color = "maroon")
                                 }else
                                   .
                               }%>%
                               {
                                 if (input$line_type %in% c('route', 'd_route'))
                                   sortAndPlot(., rfast, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                                               routePopup, "red")
                                 else
                                   .
                               }%>%
                               {
                                 if (input$line_type == 'route')
                                   sortAndPlot(., rquiet, attrWithScenario(input$line_attr, input$scenario), input$nos_lines,
                                               routePopup, "darkblue")
                                 else
                                   .
                               }%>%
                               addCircleMarkers(data = cents
                                                , radius = 2
                                                , color = "black"
                                                , popup = zonePopup(cents, input$zone_attr)
                                                  ) %>%
                               mapOptions(zoomToLimits = "first")
  )

  output$legendCyclingPotential <- renderPlot({
      # Read the zone data
      data_ <- zones@data[[attrWithScenario(input$zone_attr, input$scenario)]]
      # Sort it and save it as a matrix
      data_ <- as.matrix(sort(data_))
      # Divide the data_ matrix into four quartiles
      quart <- round(length(data_) / 4)
      # Create a new matrix for each quartile having the mean
      m <- matrix(nrow=4,ncol=1)
      m[1, 1] <- mean(data_[1:quart])
      m[2, 1] <- mean(data_[quart:(quart * 2)])
      m[3, 1] <- mean(data_[(quart * 2):(quart * 3)])
      m[4, 1] <- mean(data_[(quart * 3):length(data_)])

      # Create a zone colour based on the absolute value of data (as data can be negative as well)
      zone_col <- getColourRamp(c("lightgreen", "darkgreen"), abs(m))
      # Barplot the data in vertical manner
      barplot(m, names.arg = NA, col = zone_col, horiz=FALSE, xlab = "", ylab = input$zone_attr)

  })
})
