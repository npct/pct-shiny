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
zcols <- "RdYlBu" # for colourbrewer scale (see get_colour_ramp in pct-shiny-funs.R)
shiny_root <- file.path("..", "..")
# expect pct-data as a sibling of pct-shiny
data_dir_root <- file.path(shiny_root, '..', 'pct-data')

# Packages, only reguarly used packages are loaded into the global space
# the others must be installed but are used with the package prefix, e.g. DT::
available_locally_pkgs <- c("shiny", "leaflet", "sp")
must_be_installed_pkgs <- c("rgdal", "rgeos", "shinyjs", "dplyr", "readr", "geojsonio", "DT")

on_server <- grepl('^/var/shiny/pct-shiny', getwd())

data_sha <- as.character(readLines(file.path(shiny_root, "data_sha")))

# Create a df to store LSOA legend information
lsoa_legend_df <- data.frame(
  colours = c("#9C9C9C", "#FFFF73", "#AFFF00", "#00FFFF", "#30B0FF", "#2E5FFF", "#0000FF", "#FF00C5"),
  labels = c( "1-9", "10-49", "50-99", "100-249", "250-499", "500-999", "1000-1999", "2000+" )
)


if(!on_server){
  source(file.path(shiny_root, "scripts", "init.R"))
  init_dev_env(data_dir_root, data_sha, c(available_locally_pkgs, must_be_installed_pkgs), shiny_root)
}

installed <- must_be_installed_pkgs %in% installed.packages()
if(length(must_be_installed_pkgs[!installed]) > 0){
  stop(paste(c("Missing packages:", must_be_installed_pkgs[!installed]), collapse = " "))
}

# Check if we are on the production server (npt followed by any number of digits (only) is a prod machine)
production_branch <- grepl("npt\\d*$", Sys.info()["nodename"])

repo_sha <- as.character(readLines(file.path(shiny_root, "repo_sha")))

lapply(available_locally_pkgs, library, character.only = T)

# Functions
source(file.path(shiny_root, "pct-shiny-funs.R"), local = T)

# Static files
regions <- rgdal::readOGR(dsn = file.path(shiny_root, "regions_www/regions.geojson"), layer = "OGRGeoJSON")
regions <- spTransform(regions, CRS("+init=epsg:4326 +proj=longlat"))
codebook_l = readr::read_csv(file.path(shiny_root, "static", "codebook_lines.csv"))
codebook_z = readr::read_csv(file.path(shiny_root, "static", "codebook_zones.csv"))
codebook_r = readr::read_csv(file.path(shiny_root, "static", "codebook_routes.csv"))
codebook_rnet = readr::read_csv(file.path(shiny_root, "static", "codebook_rnet.csv"))

# # # # # # # #
# shinyServer #
# # # # # # # #
shinyServer(function(input, output, session){
  # To set initialize to_plot
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (is.na(region$current)) {
      region$current <- if(isTRUE(query[['r']] %in% regions$Region)){
        query[['r']]
      } else if (exists("starting_region")) {
        starting_region
      } else {
        "west-yorkshire"
      }
      region$data_dir <- file.path(data_dir_root, region$current)
      region$all_trips <- dir.exists(file.path(data_dir_root, region$current , 'all-trips'))
    }

    session$sendCustomMessage("regionchange", region$current)
    region$data_dir
    region$repopulate_region

    to_plot$straight_line <<- readRDS(file.path(region$data_dir, "l.Rds"))
    to_plot$zones <<- readRDS(file.path(region$data_dir, "z.Rds"))
    to_plot$cents <<- readRDS(file.path(region$data_dir, "c.Rds"))

    to_plot$route_network <<- readRDS(file.path(region$data_dir, "rnet.Rds"))
    to_plot$route_network$id <<- 1:nrow(to_plot$route_network)

    to_plot$faster_route <<- readRDS(file.path(region$data_dir, "rf.Rds" ))
    to_plot$faster_route@data <<- cbind(
      to_plot$faster_route@data[!(names(to_plot$faster_route) %in% names(to_plot$straight_line))],
      to_plot$straight_line@data)
    to_plot$quieter_route <<- readRDS(file.path(region$data_dir, "rq.Rds"))
    to_plot$quieter_route@data <<- cbind(
      to_plot$quieter_route@data[!(names(to_plot$quieter_route) %in% names(to_plot$straight_line))],
      to_plot$straight_line@data)

    # Add rqincr column to the quiet data
    to_plot$quieter_route@data$rqincr <<- to_plot$quieter_route@data$length / to_plot$faster_route@data$length

    region$all_trips <- dir.exists(file.path(data_dir_root, region$current , 'all-trips'))

    region$repopulate_region <- T
  })

  region <- reactiveValues(current = NA, data_dir = NA, repopulate_region = F, all_trips = NA)
  show_no_lines <- c("none", "lsoa_base_map")

  observe({
    output$production_branch <- renderText({ifelse(production_branch, "true", "false")})
    # If a region does not have an 'all-trips'directory, hide the trip panel
    if (!region$all_trips){
      # hide trip_panel
      shinyjs::hide("trip_panel")
    }else{
      # show trip_panel
      shinyjs::show("trip_panel")
    }
  })

  observe({
    # Create a reactive expression on the type of trips dropdown menu
    input$trip_type

    # Check if the data folder of a specific region contains a subfolder called 'all-trip'
    # If it does, only then load 'all-trip' data or load defaul commute data
    if (region$all_trips){
      if (showing_all_trips()){

        update_labels("all")
        region$data_dir <- file.path(data_dir_root, isolate(region$current), 'all-trips')
      }
      else{

        update_labels("commute")
        region$data_dir <- file.path(data_dir_root, isolate(region$current))
      }
      region$repopulate_region <- F
    }else{
      update_labels("commute")
    }
  })

  update_labels <- function(type_of_commute){

    if (type_of_commute == "all"){
      # Update the name of the scenarios when user switches to 'all-trip'
      local_scenarios <- c("Current travel patterns" = "olc",
                           "Government Target" = "govtarget",
                           "Gender equality" = "gendereq",
                           "Go Dutch" = "dutch",
                           "Ebikes" = "ebike")
      updateSelectInput(session, "scenario", choices = local_scenarios, selected = input$scenario)

      # Update the names of the sorting options for lines
      local_attrs_zone <- c("Number of cycle trips"    = "slc",
                            "Increase in Cycling" = "sic",
                            "HEAT Value"          = "slvalue_heat",
                            "CO2 reduction"       = "sico2")

      updateSelectInput(session, "line_order", choices = local_attrs_zone, selected = input$line_order)

    }else{
      # In case the user switches back to 'commute' revert the names of the scenarios
      local_scenarios <- c("Census 2011 Cycling" = "olc",
                           "Government Target" = "govtarget",
                           "Gender equality" = "gendereq",
                           "Go Dutch" = "dutch",
                           "Ebikes" = "ebike")
      updateSelectInput(session, "scenario", choices = local_scenarios, selected = input$scenario)

      # Revert the names of the sorting options for lines
      local_attrs_zone <- c("Number of cyclists"    = "slc",
                            "Increase in Cycling" = "sic",
                            "HEAT Value"          = "slvalue_heat",
                            "CO2 reduction"       = "sico2")
      updateSelectInput(session, "line_order", choices = local_attrs_zone, selected = input$line_order)
    }
  }

  # For all plotting data
  to_plot <- NULL
  # For any other persistent values
  helper <- NULL

  helper$e_lat_lng <- ""

  # Select and sort lines within a bounding box - given by flows_bb()
  sort_lines <- function(lines, group_name, nos){
    if(group_name %in% show_no_lines) return(NULL)
    if(!line_data() %in% names(lines)) return(NULL)
    # If other than route network lines are selected, subset them by the bounding box
    if (group_name != "route_network"){
      poly <- flows_bb()
      if(is.null(poly)) return(NULL)
      poly <- spTransform(poly, CRS(proj4string(lines)))
      lines_in_bb <- lines[poly, ]
      # Sort by the absolute values
      lines_in_bb[ tail(order(abs(lines_in_bb[[line_data()]])), nos), ]
    }else{
      # For the route network, just sort them according to the percentage of display
      # Sort by the absolute values
      nos <- nos / 100 * nrow(lines)
      lines[ tail(order(abs(lines[[line_data()]])), nos), ]
    }

  }

  # Finds the Local Authority shown inside the map bounds
  find_region <- function(current_region){
    bb <- map_bb()
    if(is.null(bb)) return(NULL)
    regions_bb_intersects <- rgeos::gIntersects(bb, regions, byid=T)
    # return NULL if centre is outside the shapefile
    if(all(drop(!regions_bb_intersects))) return(NULL)

    current_region_visible <- current_region %in% tolower(regions[drop(regions_bb_intersects), ]$Region)
    if(current_region_visible) return(NULL)

    regions_map_center_in <- rgeos::gContains(regions, rgeos::gCentroid(bb, byid=T), byid=T)
    if(all(drop(!regions_map_center_in))) return(NULL)
    tolower(regions[drop(regions_map_center_in), ]$Region[1])
  }

  attrs_zone <- c("Scenario Level of Cycling (SLC)" =    "slc",
                  "Scenario Increase in Cycling (SIC)" = "sic")

  # Read model-output.html, if it exists, for the loaded region
  observe({
    output$m_output <- renderUI({
      model_file <- file.path(data_dir_root, data_dir(), "model-output.html")
      #model_file <- file.path(region$data_dir, "model-output.html")
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
        line <- to_plot[[group_name]][to_plot[[group_name]]$id == id,]
        if (!is.null(line))
          addPolylines(leafletProxy("map"), data = line, color = "white",
                       opacity = 0.4, layerId = "highlighted")
      }
    })
  })

  # Updates the Local Authority if the map is moved
  # over another region with data
  observe({
    new_region <- find_region(region$current)
    if(is.null(new_region)) return()

    new_region_all_trips <- dir.exists(file.path(data_dir_root, new_region , 'all-trips'))
    # Check if the new_region is not null, and contains 'all-trips' subfolder
    new_data_dir <- ifelse (new_region_all_trips,
                            file.path(data_dir_root, new_region, 'all-trips'),
                            file.path(data_dir_root, new_region))

    if(region$data_dir != new_data_dir && file.exists(new_data_dir) && !file.exists(file.path(new_data_dir, 'isolated'))){
      region$current <- new_region
      region$data_dir <- new_data_dir
      region$repopulate_region <- F
      if(input$freeze) # If we change the map data then lines should not be frozen to the old map data
        updateCheckboxInput(session, "freeze", value = F)
    }
  })

  # Plot if lines change
  observe({
    # Needed to force lines to be redrawn when scenario, zone or base map changes
    input$scenario
    input$map_base
    region$data_dir
    input$show_zones
    region$repopulate_region

    leafletProxy("map")  %>% clearGroup(., c("straight_line", "quieter_route", "faster_route", "route_network")) %>%
      removeShape(., "highlighted")

    if(input$line_type == 'routes') {
      to_plot$ldata <<- sort_lines(to_plot$faster_route, input$line_type, input$nos_lines)
      plot_lines(leafletProxy("map"), sort_lines(to_plot$quieter_route, input$line_type, input$nos_lines), "quieter_route")
      plot_lines(leafletProxy("map"), to_plot$ldata, "faster_route")
    } else {
      to_plot$ldata <<- sort_lines(to_plot[[input$line_type]], input$line_type, input$nos_lines)
      plot_lines(leafletProxy("map"), to_plot$ldata, input$line_type)
    }

    if(input$line_type == 'route_network')
      updateSliderInput(session, inputId = "nos_lines", min = 10, max= 50, step = 20, label = "Percent (%) of Network")
    else{
      if (input$line_order == "slc")
        updateSliderInput(session, inputId = "nos_lines", min = 1, max = 200, step = 1,  label = "Top N Lines (most cycled)")
      else
        updateSliderInput(session, inputId = "nos_lines", min = 1, max = 200, step = 1,  label = "Top N Lines")
    }

  })

  # This code displays centroids if zoom level is greater than 11 and lines are displayed
  observe({
    if(is.null(input$map_zoom) ) return()
    region$repopulate_region
    input$map_base
    if(input$map_zoom < 11 || input$line_type %in% show_no_lines)
      hideGroup(leafletProxy("map"), "centres")
    else
      showGroup(leafletProxy("map"), "centres")
  })


  # Displays zone popups when no lines are selected
  observe({
    region$repopulate_region
    input$map_base
    line_type <- isolate(input$line_type)

    clearGroup(leafletProxy("map"), c("zones", "centres"))
    if(input$show_zones) {
      show_zone_popup <- input$line_type %in% show_no_lines
      popup <- if(show_zone_popup) zone_popup(to_plot$zones, input$scenario, zone_attr(), showing_all_trips())
      addPolygons(leafletProxy("map"),  data = to_plot$zones,
                  weight = 2,
                  fillOpacity = transp_rate(),
                  opacity = 0.2,
                  fillColor = get_colour_ramp(zcols, to_plot$zones[[zone_data()]]/to_plot$zones$all),
                  color = "black",
                  group = "zones",
                  popup = popup,
                  options = pathOptions(clickable = show_zone_popup),
                  layerId = paste0(to_plot$zones[['geo_code']], '-', "zones"))
    }
    addCircleMarkers(leafletProxy("map"), data = to_plot$cents, radius = normalise(to_plot$cents$all, min = 1, max = 8),
                     color = get_line_colour("centres"), group = "centres", opacity = 0.5,
                     popup = centroid_popup(to_plot$cents, input$scenario, zone_attr(), showing_all_trips()))
    # Hide and Show line layers, so that they are displayed as the top layer in the map.
    # Leaflet's function bringToBack() or bringToFront() (see http://leafletjs.com/reference.html#path)
    # don't seem to exist for R
    # By default hide the centroids
    leafletProxy("map") %>% hideGroup(., "centres") %>%
    {
      if(!line_type %in% show_no_lines) {
        switch(line_type,
               'routes'= {
                 hideGroup(., c("quieter_route", "faster_route") ) %>% showGroup(., c("quieter_route", "faster_route"))
               },
               hideGroup(., line_type) %>% showGroup(., line_type)
        )
      }
    }

    # Display centroids when zoom level is greater than 11 and lines are selected
    if (isTRUE(isolate(input$map_zoom) >= 11 && !line_type %in% show_no_lines))
      showGroup(leafletProxy("map"), "centres")
  })


  # Return the right directory name based on type of trips
  data_dir <- reactive({
    if (region$all_trips && input$trip_type == 'All')
      paste(region$current, "all-trips", sep = "/")
    else
      region$current
  })

  # Set transparency of zones to 0.5 when displayed, otherwise 0
  transp_rate <- reactive({
    if (input$show_zones) 0.5 else 0.0
  })

  # Identify suffix of lines variables
  line_attr <- reactive({
    if(input$scenario == 'olc') 'olc'
    else if (input$line_type != 'route_network') input$line_order
    else 'slc'
  })

  showing_all_trips <- reactive({ isTRUE(input$trip_type == 'All') })

  # Identify suffix of zones variables
  zone_attr <- reactive({
    if(input$scenario == 'olc') 'olc' else 'slc'
  })

  # Identify complete name of lines variable
  line_data <- reactive({
    data_filter(input$scenario, line_attr())
  })

  # Identify complete name of zones variable
  zone_data <- reactive({
    data_filter(input$scenario, zone_attr())
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

  # Set freeze checkbox to false when lines are route_network, otherwise to true
  # Also disable freeze checkbox for route_network
  observe({
    # Build a reactive expression for lines
    input$line_type
    # Also when user moves to a new region
    region$repopulate_region

    if (! input$line_type %in% show_no_lines){
      if (input$line_type == "rnet"){
        updateCheckboxInput(session, "freeze", value = T)
        disable("freeze")
      }
      else if (input$line_type != "rnet" && isolate(input$freeze)){
        updateCheckboxInput(session, "freeze", value = F)
        enable("freeze")
      }
    }
  })

  # Adds polylines on the map, depending on types and number of lines
  plot_lines <- function(m, sorted_l, group_name){
    if(is.null(sorted_l)) return()

    if (group_name == "route_network") {
      min <- 1
      max <- 20
    } else {
      min <- 5
      max <- 12
    }

    line_opacity <- 0.8
    popup_fun_name <- paste0(group_name, "_popup")

    if (group_name == 'quieter_route' || group_name == 'faster_route') {
      popup_fun_name <- "routes_popup"
      line_opacity <- 0.5
    }
    popop_fun <- get(popup_fun_name)
    addPolylines(m, data = sorted_l, color = get_line_colour(group_name),
                 # Plot widths proportional to attribute value
                 # Remove NAs from the weights
                 weight = normalise(sorted_l[[line_data()]][!is.na(sorted_l[[line_data()]]) ], min = min, max = max),
                 opacity = line_opacity,
                 group = group_name,
                 popup = popop_fun(sorted_l, input$scenario, showing_all_trips()),
                 layerId = paste0(sorted_l[['id']], '-', group_name))

  }
  # Updates map tile according to the selected map base
  map_tile_url <- reactive({
    switch(input$map_base,
           'roadmap' = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
           'satellite' = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           'IMD' =  "http://tiles.oobrien.com/imd2015_eng/{z}/{x}/{y}.png",
           'opencyclemap' = "https://c.tile.thunderforest.com/cycle/{z}/{x}/{y}.png",
           'hilliness' = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}"
    )
  })

  observe({
    input$map_base
    if (input$line_type == "lsoa_base_map"){
      urlTemplate <- paste0("http://npttile.vs.mythic-beasts.com/", input$scenario, "/{z}/{x}/{y}.png")

      leafletProxy("map") %>%
        addTiles(., urlTemplate = urlTemplate, layerId = "lsoa_base_map",
                 options=tileOptions(maxNativeZoom = 13, reuseTiles = T, tms = T)) %>%
        addLegend("topleft", layerId= "lsoa_leg", colors = lsoa_legend_df$colours,
                  labels = lsoa_legend_df$labels,
                  title = "Cyclists on route network",
                  opacity = 0.5
        )
    } else {
      leafletProxy("map") %>% removeTiles(., layerId = "lsoa_base_map") %>% removeControl("lsoa_leg")
    }
  })

  # Set map attributes
  output$cite_html <- renderUI({
    HTML(paste('Ver', a(repo_sha, href= paste0("https://github.com/npct/pct-shiny/tree/", repo_sha), target='_blank'),
               'released under a', a('GNU Affero GPL', href= "../www/licence.html", target='_blank'),
               'and funded by the', a('DfT', href = "https://www.gov.uk/government/organisations/department-for-transport", target="_blank")
    ))
  })

  output$line_codebook <- renderUI({
    a("Codebook", href = paste(
      "https://cdn.rawgit.com/npct/pct-shiny", repo_sha, "static", "codebook_lines.csv", sep = "/"),
      title="This explains the variable names in the downloadable data",
      onclick="ga('send', 'event', 'download', 'l_codebook');", target='_blank'
    )
  })

  output$route_codebook <- renderUI({
    a("Codebook", href = paste(
      "https://cdn.rawgit.com/npct/pct-shiny", repo_sha, "static", "codebook_routes.csv", sep = "/"),
      title="This explains the variable names in the downloadable data",
      onclick="ga('send', 'event', 'download', 'route_codebook');", target='_blank'
    )
  })

  output$route_codebook_quiet <- renderUI({
    a("Codebook", href = paste(
      "https://cdn.rawgit.com/npct/pct-shiny", repo_sha, "static", "codebook_routes.csv", sep = "/"),
      title="This explains the variable names in the downloadable data",
      onclick="ga('send', 'event', 'download', 'route_codebook');", target='_blank'
    )
  })

  output$route_network_codebook <- renderUI({
    a("Codebook", href = paste(
      "https://cdn.rawgit.com/npct/pct-shiny", repo_sha, "static", "codebook_rnet.csv", sep = "/"),
      title="This explains the variable names in the downloadable data",
      onclick="ga('send', 'event', 'download', 'route_network_codebook');", target='_blank'
    )
  })

  output$zone_codebook <- renderUI({
    a("Codebook", href = paste(
      "https://cdn.rawgit.com/npct/pct-shiny", repo_sha, "static", "codebook_zones.csv", sep = "/"),
      title="This explains the variable names in the downloadable data",
      onclick="ga('send', 'event', 'download', 'zones_codebook');", target='_blank'
    )
  })

  # Initialize the leaflet map
  output$map = renderLeaflet(
    leaflet() %>%
      addTiles(., urlTemplate = map_tile_url(),
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
               Routing <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a> |
               Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
               options=tileOptions(opacity = ifelse(input$map_base == "IMD", 0.3, 1),
                                   minZoom = 7,
                                   maxZoom = ifelse(input$map_base == "IMD", 14, 18), reuseTiles = T)) %>%
                                   {
                                     if (input$map_base == 'IMD'){
                                       addTiles(., urlTemplate = "http://tiles.oobrien.com/shine_urbanmask_dark/{z}/{x}/{y}.png",
                                                options=tileOptions(opacity = 0.3, minZoom = 7, maxZoom = 14, reuseTiles = T))
                                       addTiles(., urlTemplate = "http://tiles.oobrien.com/shine_labels_cdrc/{z}/{x}/{y}.png",
                                                options=tileOptions(opacity = 0.3, minZoom = 7, maxZoom = 14, reuseTiles = T))
                                     }else .
                                   } %>%
      addCircleMarkers(., data = to_plot$cents, radius = 0, group = "centres", opacity = 0.0) %>%
      mapOptions(zoomToLimits = "first")
  )

  # Adds map legend
  observe({
    input$map_base
    leafletProxy("map") %>% removeControl(layerId = "zone_leg")
    title <- ifelse(showing_all_trips(), "% trips cycled", "% cycling to work")
    if (input$show_zones) {
      leafletProxy("map") %>%
        addLegend("topleft", layerId = "zone_leg", colors = get_colour_palette(zcols, 10),
                  labels = c("0-1%",
                             "2-3%",
                             "4-6%",
                             "7-9%",
                             "10-14%",
                             "15-19%",
                             "20-24%",
                             "25-29%",
                             "30-39%",
                             "40%+"),
                  title = title,
                  opacity = 0.5
        )
    }
  })

  # Creates legend as a barplot for IMD map base
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

  observe({
    region$repopulate_region
    # Creates data for the lines datatable
    output$lines_datatable <- DT::renderDataTable({
      # Reactive values that must trigger a tabel update
      region$repopulate_region
      input$line_type

      # Only render lines data when any of the Cycling Flows is selected by the user
      plot_lines_data <- !is.null(to_plot$ldata) && !input$line_type %in% show_no_lines &&
        (!is.null(input$map_bounds)) && input$nos_lines > 0 && (line_data() %in% names(to_plot$ldata@data))
      if(!plot_lines_data){
        # Set the warning message that no lines have been selected by the user
        output$warning_message <- renderUI(HTML("<strong>No lines selected: </strong> Lines must be displayed on map </br>"))
        # Return an empty data.frame
        return(data.frame(File=character()))
      }

      # When route network is selected, show 'no lines available
      if(input$line_type == 'route_network'){
        # Set the warning message that no lines have been selected by the user
        output$warning_message <- renderUI(HTML("<strong>No lines available </strong> </br>"))
        # Return an empty data.frame
        return(data.frame(File=character()))
      }

      # Empty the warning message - as some lines have been selected by the user
      output$warning_message <- renderUI("")

      # Reuse the lines data stored in the ldata session variable
      lines_to_plot <- to_plot$ldata@data[,unname(line_col_names)]
      decimal_line_cols <- which(vapply(lines_to_plot, function(x) { is.numeric(x) && as.integer(x) != x }, FUN.VALUE = logical(1)))
      DT::datatable(lines_to_plot, options = list(pageLength = 10), colnames = line_col_names, rownames = FALSE) %>%
        DT::formatRound(columns = decimal_line_cols, digits=2)
    })

    # Creates data for the zones datatable
    output$zones_data_table <- DT::renderDataTable({
      # Reactive values that must trigger a tabel update
      region$repopulate_region

      if(is.null(to_plot$zones@data)){
        return()
      }

      zones_to_plot <- to_plot$zones@data[,unname(zone_col_names)]
      decimal_zone_cols <- which(vapply(zones_to_plot, function(x) { is.numeric(x) && as.integer(x) != x }, FUN.VALUE = logical(1)))
      DT::datatable(zones_to_plot, options = list(pageLength = 10),
                    colnames = zone_col_names, rownames = FALSE) %>%
        DT::formatRound(columns = decimal_zone_cols, digits=2)
    })

    output$download_l_csv <- downloadHandler(
      filename = function() { "lines.csv"  },
      content = function(file) { write.csv(signif_sdf(to_plot$straight_line)@data[codebook_l$`Variable name`], file = file) }
    )

    output$download_z_csv <- downloadHandler(
      filename = function() { "zones.csv"  },
      content = function(file) { write.csv(signif_sdf(to_plot$zones)@data[codebook_z$`Variable name`], file = file) }
    )

    output$download_z_geojson <- downloadHandler(
      filename = function() { "zones.geojson"  },
      content = function(file) { geojsonio::geojson_write(signif_sdf(to_plot$zones[codebook_z$`Variable name`]), file = file) }
    )

    output$download_l_geojson <- downloadHandler(
      filename = function() { "lines.geojson"  },
      content = function(file) { geojsonio::geojson_write(signif_sdf(to_plot$straight_line[codebook_l$`Variable name`]), file = file) }
    )

    output$download_rf_geojson <- downloadHandler(
      filename = function() { "routes_fast.geojson"  },
      content = function(file) { geojsonio::geojson_write(signif_sdf(to_plot$faster_route[codebook_r$`Variable name`]), file = file) }
    )

    output$download_rq_geojson <- downloadHandler(
      filename = function() { "routes_quiet.geojson"  },
      content = function(file) { geojsonio::geojson_write(signif_sdf(to_plot$quieter_route[codebook_r$`Variable name`]), file = file) }
    )

    output$download_rnet_geojson <- downloadHandler(
      filename = function() { "routes_network.geojson"  },
      content = function(file) { geojsonio::geojson_write(signif_sdf(to_plot$route_network[codebook_rnet$`Variable name`]), file = file) }
    )

    output$download_l_rds <- downloadHandler(
      filename = function() { "lines.Rds"  },
      content = function(file) { saveRDS(to_plot$straight_line[codebook_l$`Variable name`], file = file) }
    )

    output$download_rf_rds <- downloadHandler(
      filename = function() { "routes_fast.Rds"  },
      content = function(file) { saveRDS(to_plot$faster_route[codebook_r$`Variable name`], file = file) }
    )

    output$download_rq_rds <- downloadHandler(
      filename = function() { "routes_quiet.Rds"  },
      content = function(file) { saveRDS(to_plot$quieter_route[codebook_r$`Variable name`], file = file) }
    )

    output$download_rnet_rds <- downloadHandler(
      filename = function() { "routes_network.Rds"  },
      content = function(file) { saveRDS(to_plot$route_network[codebook_rnet$`Variable name`], file = file) }
    )

    output$download_z_rds <- downloadHandler(
      filename = function() { "zones.Rds"  },
      content = function(file) { saveRDS(to_plot$zones[codebook_z$`Variable name`], file = file) }
    )

  })

  # Hide/show panels on user-demand
  shinyjs::onclick("toggle_panel", shinyjs::toggle(id = "input_panel", anim = FALSE))
  shinyjs::onclick("toggle_trip_menu", shinyjs::toggle(id = "trip_menu", anim = FALSE))
  shinyjs::onclick("toggle_map_legend", shinyjs::toggle(id = "map_legend", anim = FALSE))

  # Function to add a layers control for the routes, so that users can easily select quiet routes
  observe({
    input$line_type
    if (input$line_type == 'routes'){
      leafletProxy("map") %>% addLayersControl(
        position = c("bottomright"),
        overlayGroups = c("quieter_route", "faster_route"),
        options = layersControlOptions(collapsed = T)
      )
    }else
      leafletProxy("map") %>% removeLayersControl()
  })
})
