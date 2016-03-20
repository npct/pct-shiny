#     This is server base that the every client will connect to.
#
#     Copyright (C) 2016 Nikolai Berkoff, Ali Abbas and Robin Lovelace
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Affero General Public License as
#     published by the Free Software Foundation, either version 3 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Affero General Public License for more details.
#
#     You should have received a copy of the GNU Affero General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

# # # # #
# Setup #
# # # # #

# Colours
zcols <- c("darkslategrey", "yellow")

# expect pct-data as a sibling of pct-shiny
dataDirRoot <- file.path(shinyRoot, '..', 'pct-data')
# packages required
cranPkgs <- c("shiny", "RColorBrewer", "httr", "rgdal", "rgeos", "leaflet", "DT")

onProduction <- grepl('^/var/shiny/pct-shiny', getwd())

# Run the following lines to check out the current version of the data (see sha)
if(!onProduction){
  source(file.path(shinyRoot, "scripts", "init.R"), local = T)
}
repo_sha <- as.character(readLines(file.path(shinyRoot, "repo_sha")))
lapply(c(cranPkgs), library, character.only = TRUE)

# Functions
source(file.path(shinyRoot, "pct-shiny-funs.R"))
regions <- readOGR(dsn = file.path(shinyRoot, "regions.geojson"), layer = "OGRGeoJSON")
regions <- spTransform(regions, CRS("+init=epsg:4326 +proj=longlat"))

# # # # # # # #
# shinyServer #
# # # # # # # #
shinyServer(function(input, output, session){
  region <- reactiveValues(current = startingCity)
  # For all plotting data
  toPlot <- NULL
  # For any other persistent values
  helper <- NULL

  helper$eLatLng <- ""
  helper$dataDir <- file.path(dataDirRoot, startingCity)

  # To set initialize toPlot
  loadData <- function(dataDir){
    toPlot
    toPlot$l <- readRDS(file.path(dataDir, "l.Rds"))

    toPlot$rFast <- readRDS(file.path(dataDir, "rf.Rds" ))
    toPlot$rFast@data <- cbind(toPlot$rFast@data, toPlot$l@data)
    toPlot$rQuiet <- readRDS(file.path(dataDir, "rq.Rds"))
    toPlot$rQuiet@data <- cbind(toPlot$rQuiet@data,rqincr=toPlot$rQuiet@data$length/toPlot$rFast@data$length, toPlot$l@data)

    toPlot$zones <-  readRDS(file.path(dataDir, "z.Rds"))
    toPlot$cents <-   readRDS(file.path(dataDir, "c.Rds"))

    toPlot$rnet <- readRDS(file.path(dataDir, "rnet.Rds"))
    toPlot$rnet$id <- 1:nrow(toPlot$rnet)

    toPlot
  }

  toPlot <- loadData(helper$dataDir)

  # Select and sort lines within a bounding box - given by flowsBB()
  sortLines <- function(lines, sortBy, nos){
    poly <- flowsBB()
    poly <- spTransform(poly, CRS(proj4string(lines)))
    keep <- gContains(poly, lines,byid=TRUE )
    if(all(!keep)) return(NULL)
    linesInBb <- lines[drop(keep), ]
    linesInBb[ tail(order(linesInBb[[sortBy]]), nos), ]
  }

  # Finds the Local Authority shown inside the map bounds
  findRegion <- function(){
    BB <- mapBB()
    if(is.null(BB)) return(NULL)
    mapCenter = gCentroid(BB, byid=T)
    keep <- gContains(regions, mapCenter, byid=T)
    if(all(drop(!keep))) return(NULL) # return NULL if center is outside the shapefile
    tolower(regions[drop(keep), ]$Region[1])
  }

  attrsZone <- c("Scenario Level of Cycling (SLC)" =    "slc",
                 "Scenario Increase in Cycling (SIC)" = "sic")

  observe({
    output$moutput <- renderUI({
      modelFile <- file.path(dataDirRoot, region$current, "model-output.html")
      if (file.exists(modelFile))
        includeHTML(modelFile)
      else
        HTML("<strong>No model output files are available for this region</strong>")
    })
  })

  observe({ # For highlighting the clicked line
    event <- input$map_shape_click
    if (is.null(event) || event$id == "highlighted")
      return()
    eLatLng <- paste0(event$lat,event$lng)

    # Fix bug when a line has been clicked then the click event is
    # re-emmited when the map is moved
    if( eLatLng == helper$eLatLng)
      return()
    helper$eLatLng <<- eLatLng

    isolate({
      idGroupName <- unlist(strsplit(event$id, "-"))
      id <- idGroupName[1]
      groupName <- idGroupName[2]
      if (groupName != "zones"){
        line <- switch(groupName,
                       'straight_line' = toPlot$l[toPlot$l$id == id,],
                       'faster_route' = toPlot$rFast[toPlot$rFast$id == id,],
                       'quieter_route' = toPlot$rQuiet[toPlot$rQuiet$id == id,],
                       'route_network' = toPlot$rnet[toPlot$rnet$id == id,]
        )
        if (!is.null(line))
          leafletProxy("map") %>% addPolylines(data = line, color = "white",
                                               opacity = 0.4, layerId = "highlighted")
      }else{

        leafletProxy("map") %>% addPolygons(data = toPlot$zones[toPlot$z$geo_code == id,]
                                            , color = "white"
                                            , opacity = 0.4
                                            , layerId = "highlighted")

      }
    })
  })

  # Updates the Local Authority if the map is moved
  # over another region with data
  observe({
    if(file.exists(file.path(helper$dataDir, 'isolated'))) return()
    newRegion <- findRegion()
    dataDir <- file.path(dataDirRoot, newRegion)

    if(!is.null(newRegion) && helper$dataDir != dataDir && file.exists(dataDir)){
      region$current <- newRegion
      leafletProxy("map")  %>% clearGroup(., "cents")
      helper$dataDir <<- dataDir
      toPlot <<- loadData(dataDir)
      if(input$freeze) # If we change the map data then lines should not be frozen to the old map data
        updateCheckboxInput(session, "freeze", value = F)
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
             'route'= {
               plotLines(., toPlot$rQuiet, input$nos_lines, routePopup, "quieter_route", getLineColour("quieter_route"))
               plotLines(., toPlot$rFast, input$nos_lines, routePopup,"faster_route",  getLineColour("faster_route"))
             },
             'd_route'= plotLines(., toPlot$rFast, input$nos_lines, routePopup,"faster_route",  getLineColour("faster_route")),
             'rnet' = plotLines(., toPlot$rnet, input$nos_lines, networkRoutePopup, "route_network", getLineColour("route_network"))
      )
    }
    if(input$line_type == 'rnet')
      updateSliderInput(session, inputId = "nos_lines", min = 25, max= 50, step = 25, label = "Percent (%) of Network")
    else
      updateSliderInput(session, inputId = "nos_lines", max= 100, step = 5,  label = "Number of Lines")

    # Needed to force lines to be redrawn when scenario, zone or base map changes
    paste(input$scenario, input$map_base, region$current)
  })

  # This function updates the zones and the lines
  observe({
    region$current
    leafletProxy("map")  %>%  clearGroup(., "zones") %>% clearGroup(., "centers") %>%
      addPolygons(.,  data = toPlot$zones
                  , weight = 2
                  , fillOpacity = transpRate()
                  , opacity = 0.2
                  , fillColor = getColourRamp(zcols, toPlot$zones[[zoneData()]])
                  , color = "black"
                  , group = "zones"
                  , popup = zonePopup(toPlot$zones, input$scenario, zoneAttr())
                  , layerId = paste0(toPlot$zones[['geo_code']], '-', "zones")) %>%
      addCircleMarkers(., data = toPlot$cents, radius = circleRadius(), color = "black", group = "centers",
                       popup = zonePopup(toPlot$cents, input$scenario, zoneAttr()))

    # Change the lines in isolation from the zones - should replicate previous observe
    isolate({
      leafletProxy("map") %>% {
        switch(input$line_type,
               'straight' = plotLines(., toPlot$l, input$nos_lines, straightPopup, "straight_line", getLineColour("straight_line")),
               'route'= {
                 plotLines(., toPlot$rQuiet, input$nos_lines, routePopup, "quieter_route", getLineColour("quieter_route"))
                 plotLines(., toPlot$rFast, input$nos_lines, routePopup,"faster_route",  getLineColour("faster_route"))
               },
               'd_route'= plotLines(., toPlot$rFast, input$nos_lines, routePopup,"faster_route",  getLineColour("faster_route")),
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

  # These are redundant as there is currently no option to visualize the scenario increase
  lineAttr <- reactive({
    if(input$scenario == 'olc') 'olc' else 'slc'
  })
  zoneAttr <- reactive({
    if(input$scenario == 'olc') 'olc' else 'slc'
  })

  lineData <- reactive({
    dataFilter(input$scenario, lineAttr())
  })

  zoneData <- reactive({
    dataFilter(input$scenario, zoneAttr())
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
                   , group = groupName
                   , popup = popupFn(sorted_l, input$scenario)
                   , layerId = paste0(sorted_l[['id']], '-', groupName))
  }

  mapTileUrl <- reactive({
    switch(input$map_base,
           'roadmap' = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",
           'satellite' = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           'IMD' =  "http://tiles.oobrien.com/imd2015_eng/{z}/{x}/{y}.png"
    )
  })
  output$citeHtml <- renderUI({
    HTML(paste('Ver', a(repo_sha, href= paste0("https://github.com/npct/pct-shiny/tree/", repo_sha), target='_blank'),
               'released under a', a('GNU AGP licence', href= "licence.html", target='_blank'),
               'and funded by the', a('DfT', href = "https://www.gov.uk/government/organisations/department-for-transport", target="_blank")
    ))
  })


  output$map = renderLeaflet(
    leaflet() %>%
      addTiles(., urlTemplate = mapTileUrl(),
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
               Routing <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a> |
               Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
               options=tileOptions(opacity = ifelse(input$map_base == "IMD", 0.3, 1),
                                   maxZoom = ifelse(input$map_base == "IMD", 14, 18),
                                   reuseTiles = T)) %>%
      addCircleMarkers(., data = toPlot$cents, radius = circleRadius(), color = "black", group = "cents") %>%
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
                     "Census 2011: Commuter cyclists"
    )

    # Set the labelling of Y-axis to bold
    par(font.lab = 2)

    # Barplot the data in vertical manner
    barplot(height = rep(1, 4), names.arg = round(matrix(m, nrow=4,ncol=1)),
            col = zone_col, horiz=TRUE, xlab = "", ylab = ylabel, space = 0, axes = FALSE)
  })


  output$IMDLegend <- renderPlot({
    myLab <- c("Most deprived decile", "2nd", "3rd", "4th", "5th",
               "6th", "7th", "8th", "9th", "Least deprived decile",
               "Data missing", "Data not available")

    myLab <- rev(myLab)

    myColors <- c("#a50026","#d73027", "#f46d43","#fdae61","#fee08b",
                  "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850",
                  "#006837", "#aaaaaa", "#dddddd")

    myColors <- rev(myColors)

    # Set the labelling of Y-axis to bold
    par(font.lab = 2, mar=c(0.0,5.8,0.0,1.0))

    bp <- barplot(rep(1,12), beside = TRUE, col = myColors,
                  ylab = "IMD From 2015\nIndex of Multiple Deprivation", horiz = T, axes = F)

    text(0, bp, myLab, cex=0.8, pos=4, font=2, col = "black")
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
    linesToPlot <- toPlot$ldata@data[,unname(lineColNames)]
    DT::datatable(linesToPlot, options = list(pageLength = 10), colnames = lineColNames) %>%
      formatRound(columns = names(numericLineColNames), digits=2)
  })

  output$zonesDataTable <- DT::renderDataTable({
    if(is.null(toPlot$zones@data)){
      return()
    }
    zonesToPlot <- toPlot$zones@data[,unname(zoneColNames)]
    DT::datatable(zonesToPlot, options = list(pageLength = 10), colnames = zoneColNames) %>%
      formatRound(columns = names(numericZoneColNames), digits=2)
  })
  output$downloadData <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("lines", "geojson", sep = ".")
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Bug in writeOGR that there can be no "." in the file name
      fileNoDot <- unlist(strsplit(file, ".", fixed = T))[1]
      writeOGR(toPlot$ldata, dsn = fileNoDot, layer = "", driver='GeoJSON', overwrite_layer= T)
      file.rename(fileNoDot, file)
    }
  )
})
