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
data_dir_root <- file.path(shiny_root, '..', 'pct-data')
# packages required
cran_pkgs <- c("shiny", "RColorBrewer", "httr", "rgdal", "rgeos", "leaflet", "DT", "shinyjs")

on_production <- grepl('^/var/shiny/pct-shiny', getwd())

data_sha <- as.character(readLines(file.path(shiny_root, "data_sha")))

if(!on_production){
  source(file.path(shiny_root, "scripts", "init.R"))
  init_dev_env(data_dir_root, data_sha, cran_pkgs, shiny_root)
}

repo_sha <- as.character(readLines(file.path(shiny_root, "repo_sha")))

lapply(cran_pkgs, library, character.only = T)

# Functions
source(file.path(shiny_root, "pct-shiny-funs.R"), local = T)
regions <- readOGR(dsn = file.path(shiny_root, "regions.geojson"), layer = "OGRGeoJSON")
regions <- spTransform(regions, CRS("+init=epsg:4326 +proj=longlat"))

# # # # # # # #
# shinyServer #
# # # # # # # #
shinyServer(function(input, output, session){
    # To set initialize to_plot
  observe({
    region$current
    to_plot$l <<- readRDS(file.path(region$data_dir, "l.Rds"))
    to_plot$zones <<-  readRDS(file.path(region$data_dir, "z.Rds"))
    to_plot$cents <<-   readRDS(file.path(region$data_dir, "c.Rds"))

    to_plot$l@data <<- plyr::arrange(to_plot$l@data, id)

    to_plot$rnet <<- readRDS(file.path(region$data_dir, "rnet.Rds"))
    to_plot$rnet$id <<- 1:nrow(to_plot$rnet)

    to_plot$r_fast <<- readRDS(file.path(region$data_dir, "rf.Rds" ))
    to_plot$r_fast@data <<- cbind(to_plot$r_fast@data[!(names(to_plot$r_fast) %in% names(to_plot$l))], to_plot$l@data)
    to_plot$r_quiet <<- readRDS(file.path(region$data_dir, "rq.Rds"))
    to_plot$r_quiet@data <<- cbind(to_plot$r_quiet@data[!(names(to_plot$r_quiet) %in% names(to_plot$l))], to_plot$l@data)
    # Add rqincr column to the quiet data
    to_plot$r_quiet@data$rqincr <<- to_plot$r_quiet@data$length / to_plot$r_fast@data$length
    isolate(region$replot <- !region$replot)
  })

  region <- reactiveValues(current = starting_city, data_dir = file.path(data_dir_root, starting_city), replot = F )

  # For all plotting data
  to_plot <- NULL
  # For any other persistent values
  helper <- NULL

  helper$e_lat_lng <- ""

  # Select and sort lines within a bounding box - given by flows_bb()
  sort_lines <- function(lines, sort_by, nos){
    if(!sort_by %in% names(lines)) return(NULL)
    poly <- flows_bb()
    if(is.null(poly)) return(NULL)
    poly <- spTransform(poly, CRS(proj4string(lines)))
    keep <- gContains(poly, lines,byid=TRUE )
    if(all(!keep)) return(NULL)
    lines_in_bb <- lines[drop(keep), ]
    # Sort by the absolute values
    lines_in_bb[ tail(order(abs(lines_in_bb[[sort_by]])), nos), ]
  }

  # Finds the Local Authority shown inside the map bounds
  find_region <- function(current_region){
    bb <- map_bb()
    if(is.null(bb)) return(NULL)
    regions_bb_intersects <- gIntersects(bb, regions, byid=T)
    # return NULL if centre is outside the shapefile
    if(all(drop(!regions_bb_intersects))) return(NULL)

    current_region_visible <- current_region %in% tolower(regions[drop(regions_bb_intersects), ]$Region)
    if(current_region_visible) return(NULL)

    regions_map_center_in <- gContains(regions, gCentroid(bb, byid=T), byid=T)
    if(all(drop(!regions_map_center_in))) return(NULL)
    tolower(regions[drop(regions_map_center_in), ]$Region[1])
  }

  attrs_zone <- c("Scenario Level of Cycling (SLC)" =    "slc",
                 "Scenario Increase in Cycling (SIC)" = "sic")

  observe({
    output$m_output <- renderUI({
      model_file <- file.path(data_dir_root, region$current, "model-output.html")
      if (file.exists(model_file))
        includeHTML(model_file)
      else
        HTML("<strong>No model output files are available for this region</strong>")
    })
  })

  observe({ # For highlighting the clicked line
    event <- input$map_shape_click
    if (is.null(event) || event$id == "highlighted")
      return()
    e_lat_lng <- paste0(event$lat,event$lng)

    # Fix bug when a line has been clicked then the click event is
    # re-emmited when the map is moved
    if( e_lat_lng == helper$e_lat_lng)
      return()
    helper$e_lat_lng <<- e_lat_lng

    isolate({
      id_group_name <- unlist(strsplit(event$id, "-"))
      id <- id_group_name[1]
      group_name <- id_group_name[2]

      if (event$group == "centres"){
        addPolygons(leafletProxy("map"), data = to_plot$zones[to_plot$z$geo_code == id,],
                    fill = F,
                    color = get_line_colour("centres") ,
                    opacity = 0.7,
                    layerId = "highlighted")
      } else if (event$group == "zones"){
        addPolygons(leafletProxy("map"), data = to_plot$zones[to_plot$z$geo_code == id,],
                    fill = FALSE,
                    color = "black",
                    opacity = 0.7 ,
                    layerId = "highlighted")
      } else {
        line <- switch(group_name,
                       'straight_line' = to_plot$l[to_plot$l$id == id,],
                       'faster_route' = to_plot$r_fast[to_plot$r_fast$id == id,],
                       'quieter_route' = to_plot$r_quiet[to_plot$r_quiet$id == id,],
                       'route_network' = to_plot$rnet[to_plot$rnet$id == id,]
        )
        if (!is.null(line))
          addPolylines(leafletProxy("map"), data = line, color = "white",
                       opacity = 0.4, layerId = "highlighted")
      }
    })
  })

  # Updates the Local Authority if the map is moved
  # over another region with data
  observe({
    if(file.exists(file.path(region$data_dir, 'isolated'))) return()
    new_region <- find_region(region$current)
    new_data_dir <- file.path(data_dir_root, new_region)
    if(!is.null(new_region) && region$data_dir != new_data_dir && file.exists(new_data_dir) && !file.exists(file.path(new_data_dir, 'isolated'))){
      region$current <- new_region
      region$data_dir <- new_data_dir
      if(input$freeze) # If we change the map data then lines should not be frozen to the old map data
        updateCheckboxInput(session, "freeze", value = F)
    }
  })

  # Plot if lines change
  observe({
    # Needed to force lines to be redrawn when scenario, zone or base map changes
    input$scenario
    input$map_base
    region$replot
    input$show_zones

    leafletProxy("map")  %>% clearGroup(., "straight_line") %>%
      clearGroup(., "quieter_route") %>% clearGroup(., "faster_route") %>% clearGroup(., "route_network") %>%
      removeShape(., "highlighted")

    leafletProxy("map") %>% {
      switch(input$line_type,
             'straight' = plot_lines(., to_plot$l, input$nos_lines, straight_popup, "straight_line", get_line_colour("straight_line")),
             'route'= {
               plot_lines(., to_plot$r_quiet, input$nos_lines, route_popup, "quieter_route", get_line_colour("quieter_route"))
               plot_lines(., to_plot$r_fast, input$nos_lines, route_popup,"faster_route",  get_line_colour("faster_route"))
             },
             'd_route'= plot_lines(., to_plot$r_fast, input$nos_lines, route_popup,"faster_route",  get_line_colour("faster_route")),
             'rnet' = plot_lines(., to_plot$rnet, input$nos_lines, network_route_popup, "route_network", get_line_colour("route_network"))
      )
    }
    if(input$line_type == 'rnet')
      updateSliderInput(session, inputId = "nos_lines", min = 10, max= 50, step = 20, label = "Percent (%) of Network")
    else
      updateSliderInput(session, inputId = "nos_lines", min = 1, max = 200, step = 1,  label = "N. Lines (most cycled)")

  })
  get_zone_multiplier <- function(zoom){ zoom^4/8200 }

  # This function updates the zones and the lines
  observe({
    if(is.null(input$map_zoom) ) return()
    region$replot
    input$map_base
    zoom_multiplier <- get_zone_multiplier(input$map_zoom)
    if(input$map_zoom < 11 || input$line_type == 'none')
      hideGroup(leafletProxy("map"), "centres")
    else
      showGroup(leafletProxy("map"), "centres")
  })

  observe({
    region$replot
    input$map_base
    show_zone_popup <- input$line_type == 'none'
    popup <- if(show_zone_popup) zone_popup(to_plot$zones, input$scenario, zone_attr())
    leafletProxy("map")  %>% clearGroup(., "zones") %>% clearGroup(., "region_name") %>%
      addPolygons(.,  data = to_plot$zones
                  , weight = 2
                  , fillOpacity = transp_rate()
                  , opacity = 0.2
                  , fillColor = get_colour_ramp(zcols, to_plot$zones[[zone_data()]])
                  , color = "black"
                  , group = "zones"
                  , popup = popup
                  , options = pathOptions(clickable = show_zone_popup)
                  , layerId = paste0(to_plot$zones[['geo_code']], '-', "zones")) %>%
      addCircleMarkers(., radius=0, lat=0, lng=0, group = "region_name", fillOpacity= 0, layerId = region$current) %>%
      addCircleMarkers(., data = to_plot$cents, radius = to_plot$cents$all / mean(to_plot$cents$all) * 2 + 1,
                       color = get_line_colour("centres"), group = "centres", opacity = 0.5,
                       popup = centroid_popup(to_plot$cents, input$scenario, zone_attr())) %>%
      # Hide and Show line layers, so that they are displayed as the top layer in the map.
      # Leaflet's function bringToBack() or bringToFront() (see http://leafletjs.com/reference.html#path)
      # don't seem to exist for R
      # By default hide the centroids
      hideGroup(., "centres") %>%
      {
        switch(isolate(input$line_type),
               'straight' = hideGroup(., "straight_line") %>% showGroup(., "straight_line"),
               'route'= {
                 hideGroup(., "quieter_route") %>% showGroup(., "quieter_route")
                 hideGroup(., "faster_route") %>% showGroup(., "faster_route")
               },
               'd_route' = hideGroup(., "faster_route") %>% showGroup(., "faster_route"),
               'rnet' = hideGroup(., "route_network") %>% showGroup(., "route_network")
        )
      }

      # Display centroids when zoom level is greater than 11 and lines are selected
      if (isolate(input$map_zoom) >= 11 && isolate(input$line_type) != 'none')
        showGroup(leafletProxy("map"), "centres")
  })

  transp_rate <- reactive({
    if (input$show_zones) 0.5 else 0.0
  })

  line_attr <- reactive({
    if(input$scenario == 'olc') 'olc'
    else if (input$line_type != 'rnet') input$line_order
    else 'slc'
  })

  zone_attr <- reactive({
    if(input$scenario == 'olc') 'olc' else 'slc'
  })

  line_data <- reactive({
    data_filter(input$scenario, line_attr())
  })

  zone_data <- reactive({
    data_filter(input$scenario, zone_attr())
  })

  # Reactive function for the lines data
  # 1) Called when other than 'none' is selected for the Cycling Flows
  # 2) Also called when freeze lines is unchecked and the user navigates the map
  # 3) Or when the user changes the Top Lines slider
  plot_lines_data <- reactive({
    (input$line_type != 'none' && ((!input$freeze && !is.null(input$map_bounds)) || input$nos_lines > 0)) && (line_data() %in% names(to_plot$l@data))
  })

  # Returns the map bounding box
  map_bb <- reactive({
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
  flows_bb <- reactive({
    if(!input$freeze || is.null(helper$bb)){
      helper$bb <<- map_bb()
    }
    helper$bb
  })

  plot_lines <- function(m, lines, nos, popup_fn, group_name, color){
    if (group_name == "route_network") {
      nos <- nos / 100 * nrow(lines)
      min <- 1
      max <- 20
    } else {
      min <- 5
      max <- 12
    }

    line_opacity <- 0.8
    if (group_name == 'quieter_route' || group_name == 'faster_route')
      line_opacity <- 0.5

    sorted_l <- sort_lines(lines, line_data(), nos)
    to_plot$ldata <<- sorted_l
    if(is.null(sorted_l))
      m
    else{
      addPolylines(m, data = sorted_l, color = color
                   # Plot widths proportional to attribute value
                   , weight = normalise(sorted_l[[line_data()]], min = min, max = max)
                   , opacity = line_opacity
                   , group = group_name
                   , popup = popup_fn(sorted_l, input$scenario)
                   , layerId = paste0(sorted_l[['id']], '-', group_name))
    }
  }

  map_tile_url <- reactive({
    switch(input$map_base,
           'roadmap' = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",
           'satellite' = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           'IMD' =  "http://tiles.oobrien.com/imd2015_eng/{z}/{x}/{y}.png",
           'opencyclemap' = "https://c.tile.thunderforest.com/cycle/{z}/{x}/{y}.png",
           'hilliness' = "http://{s}.tiles.wmflabs.org/hillshading/{z}/{x}/{y}.png"
    )
  })
  output$cite_html <- renderUI({
    HTML(paste('Ver', a(repo_sha, href= paste0("https://github.com/npct/pct-shiny/tree/", repo_sha), target='_blank'),
               'released under a', a('GNU AGP licence', href= "licence.html", target='_blank'),
               'and funded by the', a('DfT', href = "https://www.gov.uk/government/organisations/department-for-transport", target="_blank")
    ))
  })

  output$zone_data_links <- renderUI({
    HTML(
      make_download_link("z", "zones", region$current)
    )
  })

  output$line_data_links <- renderUI({
    HTML(paste("Straight lines",
               make_download_link("l", "lines", region$current),
               br(),
               "Fast routes",
               make_download_link("rf", "fast_routes", region$current, c('Rds', 'geojson')),
               br(),
               "Quiet routes",
               make_download_link("rq", "quiet_routes", region$current, c('Rds', 'geojson')),
               br(),
               "Route Network",
               make_download_link("rnet", "route_network", region$current, c('Rds', 'geojson'))
    ))
  })

  output$map = renderLeaflet(
    leaflet() %>%
      addTiles(., urlTemplate = map_tile_url(),
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
               Routing <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a> |
               Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
               options=tileOptions(opacity = ifelse(input$map_base == "IMD", 0.3, 1),
                                   maxZoom = ifelse(input$map_base == "IMD", 14, 18), reuseTiles = T)) %>%
      {
        if (input$map_base == 'IMD'){
            addTiles(., urlTemplate = "http://tiles.oobrien.com/shine_urbanmask_dark/{z}/{x}/{y}.png",
              options=tileOptions(opacity = 0.3, maxZoom = 14, reuseTiles = T))
            addTiles(., urlTemplate = "http://tiles.oobrien.com/shine_labels_cdrc/{z}/{x}/{y}.png",
              options=tileOptions(opacity = 0.3, maxZoom = 14, reuseTiles = T))
        }else .

      } %>%
      addCircleMarkers(., data = to_plot$cents, radius = 0, group = "centres", opacity = 0.0) %>%
      mapOptions(zoomToLimits = "first")
  )

  output$legend_cycling_potential <- renderPlot({
    region$replot
    # Create quantiles out of the zone data
    m <- quantile(to_plot$zones@data[[zone_data()]], probs=seq.int(0,1, length.out=4))

    # Create a zone colour based on the value of data
    zone_col <- get_colour_ramp(zcols, m)

    # Set a full form of the scenario as a label
    ylabel <- "Number of Cycle Commuters"

    # Set the labelling of Y-axis and font to bold, alter font size
    par(font = 2, font.lab = 2, cex = 0.95, mar=c(0.0,5.0,0.0,1.0))

    # Barplot the data in vertical manner
    barplot(height = rep(1, 4), names.arg = round(matrix(m, nrow=4,ncol=1)),
            col = zone_col, horiz=TRUE, xlab = "", ylab = ylabel, space = 0, axes = FALSE)
  })


  output$imd_legend <- renderPlot({
    my_lab <- c("Most deprived decile", "2nd", "3rd", "4th", "5th",
               "6th", "7th", "8th", "9th", "Least deprived decile",
               "Data missing", "Data not available")

    my_lab <- rev(my_lab)

    my_colors <- c("#a50026","#d73027", "#f46d43","#fdae61","#fee08b",
                  "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850",
                  "#006837", "#aaaaaa", "#dddddd")

    my_colors <- rev(my_colors)

    # Set the labelling of Y-axis to bold
    par(font.lab = 2, mar=c(0.0,5.8,0.0,1.0))

    bp <- barplot(rep(1,12), beside = TRUE, col = my_colors,
                  ylab = "IMD From 2015\nIndex of Multiple Deprivation", horiz = T, axes = F)

    text(0, bp, my_lab, cex=0.8, pos=4, font=2, col = "black")
  })

  output$lines_datatable <- DT::renderDataTable({
    # Only render lines data when any of the Cycling Flows is selected by the user
    if(!plot_lines_data()){
      # Set the warning message that no lines have been selected by the user
      output$warning_message <- renderUI(HTML("<strong>No lines selected: </strong> Lines must be displayed on map"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    # Empty the warning message - as some lines have been selected by the user
    output$warning_message <- renderUI("")

    # Reuse the lines data stored in the ldata session variable
    lines_to_plot <- to_plot$ldata@data[,unname(line_col_names)]
    DT::datatable(lines_to_plot, options = list(pageLength = 10), colnames = line_col_names) %>%
      formatRound(columns = names(numeric_line_col_names), digits=2)
  })

  output$zones_data_table <- DT::renderDataTable({
    if(is.null(to_plot$zones@data)){
      return()
    }
    zones_to_plot <- to_plot$zones@data[,unname(zone_col_names)]
    DT::datatable(zones_to_plot, options = list(pageLength = 10), colnames = zone_col_names) %>%
      formatRound(columns = names(numeric_zone_col_names), digits=2)
  })

  shinyjs::onclick("toggle_panel", shinyjs::toggle(id = "input_panel", anim = FALSE))
  shinyjs::onclick("toggle_legend", shinyjs::toggle(id = "zone_legend", anim = FALSE))
  shinyjs::onclick("toggle_map_legend", shinyjs::toggle(id = "map_legend", anim = FALSE))

  observe({
    if (input$map_base == 'IMD')
      shinyjs::hide("zone_legend")
    else
      shinyjs::show("zone_legend")
  })
})
