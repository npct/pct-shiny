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
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

# # # # #
# Setup #
# # # # #

## Functions
source("pct_shiny_funs.R", local = T)

## Packages (Only regularly used packages are loaded into the global space, the others must be installed but are used with the package prefix, e.g. DT::)
available_locally_pkgs <- c("shiny", "leaflet", "sp")
must_be_installed_pkgs <- c("rgdal", "rgeos", "shinyjs", "dplyr", "readr", "geojsonio", "DT")

## Path directories to load data (expect regional data as a sibling of interface_root)
interface_root <- file.path("..", "..")
data_regional_root <-  file.path(interface_root, '..', 'pct-outputs-regional-R')
data_national_root <-  file.path(interface_root, '..', 'pct-outputs-national')

## Regional sha file [Anna question: next 7 lines added by me, wasn't clear to me otherwise how read the latest master branch? Or was the intention to set manually?]
gitArgs_reg <- c("--git-dir", file.path(data_regional_root, ".git"), "rev-parse", "--short", "HEAD", ">", file.path(interface_root, "outputs_regional_sha"))
if (.Platform$OS.type == "windows"){
  shell(paste(append("git", gitArgs_reg), collapse = " "), wait = T)
} else {
  system2("git", gitArgs_reg, wait = T)
}
outputs_regional_sha <- as.character(readLines(file.path(interface_root, "outputs_regional_sha")))

## Check if working on server and if not initiate environment (including packages)
on_server <- grepl("shiny", Sys.info()["user"])
if (!on_server) {
  source(file.path(interface_root, "scripts", "init.R"))
  init_dev_env(interface_root, data_regional_root, outputs_regional_sha,c(available_locally_pkgs, must_be_installed_pkgs))
}

repo_sha <-  as.character(readLines(file.path(interface_root, "repo_sha")))

# Apply local packages, and check correct packages installed
lapply(available_locally_pkgs, library, character.only = T)
installed <- must_be_installed_pkgs %in% installed.packages()
if (length(must_be_installed_pkgs[!installed]) > 0) {
  stop(paste(c(
    "Missing packages:", must_be_installed_pkgs[!installed]
  ), collapse = " "))
}

# Save current sha, required for files to be downloaded
download_sha <- data.frame(repo_sha = repo_sha)

## Check if we are on the production [live] server (npt followed by any number of digits (only) is a prod machine)
production_branch <- grepl("npt\\d*$", Sys.info()["nodename"])

## Load region boundaries
regions <- rgdal::readOGR(file.path(interface_root, "regions_www/pct_regions_highres.geojson"))
regions$region_name <- as.character(regions$region_name)

## Define zone colours from colourbrewer + number of bins and breaks used
# Set minimum just below zero, otherwise zero is unassigned
zcolourscale <-  "RdYlBu"
zbins_commute <- 10
zbreaks_commute = c(-0.001, 1.5, 3.5, 6.5, 9.5, 14.5, 19.5, 24.5, 29.5, 39.5,100) / 100
zbins_school <- 11
zbreaks_school = c(-0.001, 1.5, 3.5, 6.5, 9.5, 14.5, 19.5, 24.5, 29.5, 39.5, 49.5, 100) / 100

## Create a df to store LSOA legend information
# ANNA NOTE: CURRENTLY WRITTEN FOR COMMUTE - ULTIMATELY MAKE VARIABLE BY PURPOSE
lsoa_legend_df <- data.frame(
  colours = c("#9C9C9C", "#FFFF73", "#AFFF00", "#00FFFF",
              "#30B0FF", "#2E5FFF", "#0000FF", "#FF00C5"),
  labels = c("1-9", "10-49", "50-99", "100-249",
             "250-499", "500-999", "1000-1999", "2000+")
)

# # # # # # # #
# shinyServer #
# # # # # # # #
shinyServer(function(input, output, session) {

  ##############
  # FUNCTIONS TO CUSTOMISE RIGHT HAND MENU BY PURPOSE/SCENARIO [NB changes here may need to be made to UI too!]
  ##############
  ## Define purposes and geographies available depending on region
  update_purposegeog <- function(purposes_present, geographies_present) {
    # Identify locally available purposes and update list accordingly
    local_purposes <- c(
      "Commuting"  = "commute" ,
      "School travel"  = "school" ,
      "All trips"  = "alltrips"
    )
    local_purposes <- local_purposes[purposes_present]
    updateSelectInput(session, "purpose", choices = local_purposes, selected = input$purpose)

    # Identify locally available geographies
    local_geographies <- c(
      "Middle Super Output Area"  = "msoa" ,
      "Lower Super Output Area"  = "lsoa"
    )
    local_geographies <- local_geographies[geographies_present]
    # Update geographies available, and set to default if the currently-selected geography does not exist
    if (region$geography %in% local_geographies) {
      updateSelectInput(session, "geography", choices = local_geographies, selected = region$geography)
    } else {
      updateSelectInput(session, "geography", choices = local_geographies)
    }
  }

  ## Define scenarios line types and line orders available, and their labels, depending on purpose
    update_labels <- function(purpose, geography) {

    if (purpose == "commute") {
      local_scenarios <- c(
        "Census 2011 Cycling" = "olc",
        "Government Target"   = "govtarget",
        "Gender equality"     = "gendereq",
        "Go Dutch"            = "dutch",
        "Ebikes"              = "ebike"
      )
      if (geography == "msoa") {
        local_line_types <- c("None"             = "none",
                              "Straight Lines"         = "straight_lines",
                              "Fast Routes"            = "routes_fast",
                              "Fast & Quieter Routes"  = "routes",
                              "Route Network (MSOA)"   = "route_network",
                              "Route Network (LSOA)"   = "lsoa_base_map"
        )
      } else if (geography == "lsoa") {
        local_line_types <- c("None"             = "none",
                              "Straight Lines"         = "straight_lines",
                              "Fast Routes"            = "routes_fast",
                              "Fast & Quieter Routes"  = "routes",
                              "Route Network (LSOA)"   = "lsoa_base_map"
        )
      }

      local_line_order <- c(
        "Number of cyclists"  = "slc",
        "Increase in cyclists" = "sic",
        "HEAT Value"          = "slvalue_heat",
        "CO2 reduction"       = "sico2"
      )
    } else if (purpose == "school") {
      local_scenarios <- c(
        "Current travel patterns" = "olc",
        "Government Target"       = "govtarget",
        "Go Dutch"                = "dutch"
      )
      local_line_types <- c("None"                   = "none",
                            "Route Network (LSOA)"   = "route_network"
      )
      local_line_order <- c(
        "Number of cycle trips" = "slc",
        "Increase in cyclists"   = "sic"
      )

    } else if (purpose == "alltrips") {
      local_scenarios <- c(
        "Current travel patterns" = "olc",
        "Government Target"       = "govtarget",
        "Gender equality"         = "gendereq",
        "Go Dutch"                = "dutch",
        "Ebikes"                  = "ebike"
      )
      local_line_types <- c("None"                   = "none",
                            "Straight Lines"         = "straight_lines",
                            "Fast Routes"            = "routes_fast",
                            "Fast & Quieter Routes"  = "routes",
                            "Route Network (MSOA)"   = "route_network"
      )
      local_line_order <- c(
        "Number of cycle trips" = "slc",
        "Increase in Cycling"   = "sic",
        "HEAT Value"            = "slvalue_heat",
        "CO2 reduction"         = "sico2"
      )

    }

    # Update line options available, and set to default if the currently-selected option does not exist
    if (input$scenario %in% local_scenarios) {
      updateSelectInput(session, "scenario", choices = local_scenarios, selected = input$scenario)
    } else {
      updateSelectInput(session, "scenario", choices = local_scenarios)
    }

    if (input$line_type %in% local_line_types) {
      updateSelectInput(session, "line_type", choices = local_line_types, selected = input$line_type)
    } else {
      updateSelectInput(session, "line_type", choices = local_line_types)
    }

    if (input$line_order %in% local_line_order) {
      updateSelectInput(session, "line_order", choices = local_line_order, selected = input$line_order)
    } else {
      updateSelectInput(session, "line_order", choices = local_line_order)
    }

  }

  ##############
  # Initialise region and to_plot, and update right hand menu by region/purpose
  ##############

  ## Create region, to_plot and (for persistent geographical values) helper
  region <- reactiveValues(current = NA, data_dir = NA, geography = NA, repopulate_region = F, purposes_present = NA)
  to_plot <- NULL
  helper <- NULL
  helper$e_lat_lng <- ""

  ## Set  values of region
  observe({
    region$current
    region$data_dir
    region$geography
    region$repopulate_region
    region$purposes_present
    input$purpose

    # Identify region
    query <- parseQueryString(session$clientData$url_search)
    if (is.na(region$current)) {
      region$current <- if (isTRUE(query[['r']] %in% regions$region_name)) {
        query[['r']]
      } else {
        "isle-of-wight"
      }
    }

    # Notify browser to update URL to reflect new region
    session$sendCustomMessage("regionchange", region$current)

    # Define region geography, forcing a default in cases where the geography is not available
    if (input$purpose =="commute") {
      if (input$geography %in% c("msoa", "lsoa")) {
        region$geography <<- input$geography
      } else {
        region$geography <<- "msoa"
      }
    } else if (input$purpose =="school") {
      if (input$geography %in% c("lsoa")) {
        region$geography <<- input$geography
      } else {
        region$geography <<- "lsoa"
      }
    } else if (input$purpose =="alltrips") {
      if (input$geography %in% c("msoa")) {
        region$geography <<- input$geography
      } else {
        region$geography <<- "msoa"
      }
    }

    # Set data_dir
    region$data_dir <<- file.path(data_regional_root, input$purpose, region$geography, region$current)

    # Identify that region repopulation has happened
    region$repopulate_region <<- T

    # Identify purposes and geographies available in region
    purposes_list <- c("commute", "school", "alltrips")
    region$purposes_present <<- (dir.exists(file.path(data_regional_root, purposes_list, "msoa", region$current)) | dir.exists(file.path(data_regional_root, purposes_list, "lsoa", region$current)))
    geographies_list <- c("msoa", "lsoa")
    region$geographies_present <<- dir.exists(file.path(data_regional_root, input$purpose, geographies_list, region$current))
    update_purposegeog(region$purposes_present, region$geographies_present)

    # Identify the centre of the current region, save in to_plot
    to_plot$center_dim <<- rgeos::gCentroid(regions[regions$region_name == region$current, ], byid = TRUE)@coords

    # Load data to to_plot (if data exists - this varies by purpose/geography)
    if (file.exists(file.path(region$data_dir, "z.Rds"))) {
      to_plot$zones <<- readRDS(file.path(region$data_dir, "z.Rds"))
    } else {
      to_plot$zones <<- NULL
    }

    if (file.exists(file.path(region$data_dir, "c.Rds"))) {
      to_plot$centroids <<- readRDS(file.path(region$data_dir,  "c.Rds"))
    } else {
      to_plot$centroids <<- NULL
    }

    if (file.exists(file.path(region$data_dir, "l.Rds"))) {
      to_plot$straight_lines <<- readRDS(file.path(region$data_dir, "l.Rds"))
    } else {
      to_plot$straight_lines <<- NULL
    }

    if (file.exists(file.path(region$data_dir, "rf.Rds"))) {
      to_plot$routes_fast <<- readRDS(file.path(region$data_dir, "rf.Rds"))
    } else {
      to_plot$routes_fast <<- NULL
    }

    if (file.exists(file.path(region$data_dir, "rq.Rds"))) {
      to_plot$routes_quieter <<- readRDS(file.path(region$data_dir, "rq.Rds"))
      # Merge in scenario data for quiet routes - don't want this in download but need for line sorting
      # ANNA SELF: COULD TRY TO DO WITH ID INSTEAD
      to_plot$routes_quieter@data <<- cbind(
        to_plot$routes_quieter@data[!(names(to_plot$routes_quieter) %in% names(to_plot$straight_lines))],
        to_plot$straight_lines@data)
      # Add is_quiet column to identify quieter, as opposed to faster, data - used in routes pop-up
      to_plot$routes_quieter@data$is_quiet <<- T
    } else {
      to_plot$routes_quieter <<- NULL
    }

    if (file.exists(file.path(region$data_dir, "rnet.Rds"))) {
      to_plot$route_network <<- readRDS(file.path(region$data_dir, "rnet.Rds"))
      to_plot$route_network$id <<- to_plot$route_network$local_id
   } else {
     to_plot$route_network <<- NULL
   }

  })

  ## Update labels according to purpose
  # NB don't have as part of above 'observes' otherwise those re-run when scenario changes, even though data all the same
  observe({
    update_labels(input$purpose, region$geography)
  })


  ##############
  # Define BB
  ##############
  ## Returns the map bounding box [default lat/long PCT projection]
  map_bb <- reactive({
    if (is.null(input$map_bounds)) {
      return (NULL)
    }
    lat <- c(input$map_bounds$west, input$map_bounds$east, input$map_bounds$east, input$map_bounds$west)
    lng <- c(input$map_bounds$north, input$map_bounds$north, input$map_bounds$south, input$map_bounds$south)
    c1 <- cbind(lat, lng)
    r1 <- rbind(c1, c1[1,])
    bounds <- SpatialPolygons(list(Polygons(list(Polygon(r1)), 'bb')),
                              proj4string = CRS("+init=epsg:4326 +proj=longlat"))
    proj4string(bounds) = CRS("+init=epsg:4326 +proj=longlat")
    bounds
  })

  ## Updates the bounding box (bb) to the current map bb unless the map is frozen (freeze lines on)
  flows_bb <- reactive({
    if (!input$freeze || is.null(helper$bb)) {
      helper$bb <<- map_bb()
    }
    helper$bb
  })


  ##############
  # Select, sort and plot lines
  ##############
  ## Identify suffix + complete name of lines variables
  line_attr <- reactive({
    if (input$scenario == 'olc')
      'olc'
    else if (input$line_type != 'route_network')
      input$line_order
    else
      'slc'
  })
  line_data <- reactive({
    data_filter(input$scenario, line_attr())
  })

  ## Define when not to give option to sort by lines [NB also hard-written into ui ]
  show_no_lines <- c("none", "lsoa_base_map")

  ## Select and sort lines within flows_bb bounding box
  sort_lines <- function(lines, line_type, nos) {
    if (line_type %in% show_no_lines)
      return(NULL)
    if (!line_data() %in% names(lines))
      return(NULL)
    # If other than route network lines are selected, subset them by the bounding box
    if (line_type != "route_network") {
      poly <- flows_bb()
      if (is.null(poly))
        return(NULL)
      poly <- spTransform(poly, CRS(proj4string(lines)))
      keep <- rgeos::gIntersects(poly, lines, byid = TRUE)
      if (all(!keep))
        return(NULL)
      lines_in_bb <- lines[drop(keep),]
      # Sort by the absolute values
      lines_in_bb[tail(order(abs(lines_in_bb[[line_data()]])), nos),]
    } else{
      # For the route network, just sort them according to the percentage of display
      # Sort by the absolute values
      nos <- nos / 100 * nrow(lines)
      lines[tail(order(abs(lines[[line_data()]])), nos),]
    }
  }

  ## Adds polylines on the map, depending on types and number of lines
  plot_lines <- function(m, sorted_l, line_type) {
    if (is.null(sorted_l))
      return()

    if (line_type == "route_network") {
      min <- 1
      max <- 20
    } else {
      min <- 5
      max <- 12
    }

    line_opacity <- 0.8
    popup_fun_name <- paste0("popup_", line_type)

    if (line_type == 'routes_quieter' || line_type == 'routes_fast') {
      popup_fun_name <- "popup_routes"
      line_opacity <- 0.5
    }
    popop_fun <- get(popup_fun_name)
    addPolylines(
      m,
      data = sorted_l,
      color = get_line_colour(line_type),
      # Plot widths proportional to attribute value, removing NAs
      weight = normalise(sorted_l[[line_data()]][!is.na(sorted_l[[line_data()]])], min = min, max = max),
      opacity = line_opacity,
      group = line_type,
      popup = popop_fun(sorted_l, input$scenario, input$purpose),
      layerId = paste0(sorted_l[['id']], '-', line_type)
    )

  }

  ## Plot if lines change
  observe({
    # Needed to force lines to be redrawn when purpose, geography, scenario, zone or base map changes
    input$purpose
    region$geography
    input$scenario
    input$show_zones
    input$map_base
    input$line_type
    region$data_dir
    region$repopulate_region

    line_type <- ifelse(input$line_type == 'routes', "routes_quieter", input$line_type)
    local_lines <-  sort_lines(to_plot[[line_type]], input$line_type, input$nos_lines)

    if (is.null(to_plot$ldata) || (!is.null(to_plot$ldata) && (!identical(to_plot$ldata, local_lines) || !identical(to_plot$scenario, input$scenario)))) {
      leafletProxy("map")  %>% clearGroup(.,
                                          c("straight_lines",
                                            "routes_quieter",
                                            "routes_fast",
                                            "route_network"
                                          )) %>%
        removeShape(., "highlighted")
      to_plot$ldata <<- local_lines
      # Include current scenario in to_plot as the set of lines to plot may not change when the scenario alters, and so otherwise don't update
      to_plot$scenario <<- input$scenario
      plot_lines(leafletProxy("map"), to_plot$ldata, line_type)
      # Additionally plot fast routes on top of quieter if selected 'fast & quieter'
      if (input$line_type == 'routes') {
        plot_lines(leafletProxy("map"),sort_lines(to_plot$routes_fast, "routes_fast", input$nos_lines),"routes_fast")
      }
    }

    if (input$line_type == 'route_network')
      updateSliderInput(
        session,
        inputId = "nos_lines",
        min = 10,
        max = 50,
        step = 20,
        label = "Percent (%) of Network"
      )
    else{
      if (input$line_order == "slc")
        updateSliderInput(
          session,
          inputId = "nos_lines",
          min = 1,
          max = 200,
          step = 1,
          label = "Top N Lines (most cycled)"
        )
      else
        updateSliderInput(
          session,
          inputId = "nos_lines",
          min = 1,
          max = 200,
          step = 1,
          label = "Top N Lines"
        )
    }

  })


  ##############
  # Plot zones and  centroids
  ##############

  ## Set transparency of zones to 0.5 when displayed, otherwise 0
  transp_rate <- reactive({
    if (input$show_zones)
      0.5
    else
      0.0
  })

  ## Identify suffix + complete name  of zones  variables
  zone_attr <- reactive({
    if (input$scenario == 'olc')
      'olc'
    else
      'slc'
  })
  zone_data <- reactive({
    data_filter(input$scenario, zone_attr())
  })


  ## Displays zones and centroids
  observe({
    input$purpose
    region$geography
    input$scenario
    line_type <<- isolate(input$line_type)
    input$map_base
    region$data_dir
    region$repopulate_region

    clearGroup(leafletProxy("map"), c("zones", "centroids"))
    ## Display zones
    if (input$show_zones && !is.null(to_plot$zones)) {
      # Define bins and breaks (by purpose)
      if (input$purpose == "commute" || input$purpose=="alltrips") {
        zbins <- zbins_commute
        zbreaks <- zbreaks_commute
      } else if (input$purpose == "school") {
        zbins <- zbins_school
        zbreaks <- zbreaks_school
      }
      # Show zones when no lines are selected
      show_zone_popup <- (line_type %in% show_no_lines)
      popup <-
        if (show_zone_popup)
          popup_zones(to_plot$zones, input$scenario, input$purpose)
      addPolygons(
        leafletProxy("map"),
        data = to_plot$zones,
        weight = 2,
        fillOpacity = transp_rate(),
        opacity = 0.2,
        fillColor = get_colour_ramp(zcolourscale, zbins, (to_plot$zones[[zone_data()]] /to_plot$zones$all), zbreaks),
        color = "black",
        group = "zones",
        popup = popup,
        options = pathOptions(clickable = show_zone_popup),
        layerId = paste0(to_plot$zones[['geo_code']], '-', "zones")
      )
    }
    ## Define centroids (if exist) and display when zoom level is greater than 11 and lines are selected
    if (!is.null(to_plot$centroids)) {
      addCircleMarkers(leafletProxy("map"), data = to_plot$centroids,
                       radius = normalise(to_plot$centroids$all, min = 1, max = 8),
                       color = get_line_colour("centroids"), group = "centroids", opacity = 0.5,
                       popup = popup_centroids(to_plot$centroids, input$scenario, input$purpose),
                       layerId = paste0(to_plot$centroids[['geo_code']], '-', "centroids")
      )
      if (isTRUE((is.null(isolate(input$map_zoom))) || isolate(input$map_zoom) < 11 || (input$line_type %in% show_no_lines) || (input$line_type=="route_network"))) {
        hideGroup(leafletProxy("map"), "centroids")
      } else {
        showGroup(leafletProxy("map"), "centroids")
      }
    }

    ## Hide and then re-Show line layers, so that they are displayed as the top layer in the map.
    # NB Leaflet's function bringToBack() or bringToFront() (see https://leafletjs.com/reference.html#path) don't seem to exist for R
    leafletProxy("map") %>% {
      if(!line_type %in% show_no_lines) {

        switch(line_type,
               'routes'= {
                 hideGroup(., c("routes_quieter", "routes_fast") ) %>% showGroup(., c("routes_quieter", "routes_fast"))
               },
               hideGroup(., line_type) %>% showGroup(., line_type)
              )
      }
    }

  })


  ##############
  # Highlighting - both for regions and for lines
  ##############

  ## Creating and highlighting alternative regions (only if have the purpose in question)
  observe({
    input$map_base
    region$repopulate_region

    # Remove old region's polygons
    leafletProxy("map") %>% clearGroup(., "regions-zones")

    # Identify regions that 1) have the input purpose data and 2) are not the current region
    new_regions_with_purpose <- regions$region_name[(dir.exists(file.path(data_regional_root, input$purpose, region$geography, regions$region_name))) & (!regions$region_name %in% region$current)]

    # Add all eligible regions boundaries in the beginning , but set the opacity to a minimum
    addPolygons(
      leafletProxy("map"),
      data = regions[(regions$region_name %in% new_regions_with_purpose), ],
      weight = 0.1,
      color = "#000000",
      fillColor = "aliceblue",
      fillOpacity = 0.01,
      opacity = 0.3,
      label = paste(
        "Click to view",
        get_pretty_region_name(regions[regions$region_name %in% new_regions_with_purpose, ]$region_name)
      ),
      labelOptions = labelOptions(direction = 'auto'),
      # On highlight widen the boundary and fill the polygons
      highlightOptions = highlightOptions(
        color = 'grey',
        opacity = 0.3,
        weight = 10,
        fillOpacity = 0.6,
        bringToFront = F,
        sendToBack = TRUE
      ),
      options = pathOptions(clickable = T),
      layerId = paste("new_region", regions[regions$region_name %in% new_regions_with_purpose, ]$region_name),
      group = "regions-zones"
    )
  })

  ## Switching to highlighted regions + highlighting pop-ups
  observeEvent(input$map_shape_click, {
    # For switching to the clicked region
    event <- input$map_shape_click
    if (is.null(event) || event$id == "highlighted")
      return()
    # Check if the event$id is called from the "new_region" polygons
    if (grepl("new_region", event$id)) {
      # Split the id to identify the region name
      new_region <- strsplit(event$id, " ")[[1]][2]
      if (is.null(new_region))
        return()
      new_data_dir <- file.path(data_regional_root, input$purpose, region$geography, new_region)

      if (region$data_dir != new_data_dir &&
          file.exists(new_data_dir)) {
        region$current <- new_region
        region$data_dir <- new_data_dir
        region$repopulate_region <- F
        if (input$freeze)
          # If we change the map data then lines should not be frozen to the old map data
          updateCheckboxInput(session, "freeze", value = F)
      }
      return()
    }
    e_lat_lng <- paste0(event$lat, event$lng)

    # Fix bug when a line has been clicked then the click event is re-emmited when the map is moved
    if (e_lat_lng == helper$e_lat_lng)
      return()
    helper$e_lat_lng <<- e_lat_lng

    # Highlighting for the pop-ups
    isolate({
      id_line_type <- unlist(strsplit(event$id, "-"))
      id <- id_line_type[1]
      line_type <- id_line_type[2]

      if (event$group == "centroids") {
        addPolygons(
          leafletProxy("map"),
          data = to_plot$centroids[to_plot$c$geo_code == id, ],
          fill = F,
          color = get_line_colour("centroids") ,
          opacity = 0.7,
          layerId = "highlighted"
        )
      } else if (event$group == "zones") {
        addPolygons(
          leafletProxy("map"),
          data = to_plot$zones[to_plot$z$geo_code == id, ],
          fill = FALSE,
          color = "black",
          opacity = 0.7 ,
          layerId = "highlighted"
        )
      } else {
        line <- to_plot[[line_type]][to_plot[[line_type]]$id == id, ]
        if (!is.null(line))
          addPolylines(
            leafletProxy("map"),
            data = line,
            color = "white",
            opacity = 0.4,
            layerId = "highlighted"
          )
      }
    })
  })


  ##############
  # Rasters +  basemaps
  ##############

  ## LSOA layer + legend
  ##ANNA NOTE [NB in future need to make this purpose + geography specific]
  observe({
    # region$repopulate_region

    if (input$line_type == "lsoa_base_map") {
      urlTemplate <- paste0("https://npttile.vs.mythic-beasts.com/",input$scenario,"/{z}/{x}/{y}.png")

      leafletProxy("map") %>%
        addTiles(
          .,
          urlTemplate = urlTemplate,
          layerId = "lsoa_base_map",
          group = "lsoa_base_map",
          options = tileOptions(
            maxNativeZoom = 13,
            reuseTiles = T,
            tms = T
          )
        ) %>%
        addLegend(
          "topleft",
          layerId = "lsoa_leg",
          colors = lsoa_legend_df$colours,
          labels = lsoa_legend_df$labels,
          title = "Cyclists on route network",
          opacity = 0.5
        )
    } else {
      leafletProxy("map") %>% removeTiles(., layerId = "lsoa_base_map") %>% removeControl("lsoa_leg")
    }
  })

  ## Updates map tile according to the selected map base
  map_tile <- reactive({
    switch(
      input$map_base,
      'roadmap' = list(url = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png", zoom = 18),
      'satellite' = list(url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", zoom = 18),
      'IMD' = list(url = "https://maps.cdrc.ac.uk/tiles/imd2015_eng/{z}/{x}/{y}.png", zoom = 14),
      'opencyclemap' = list(url = "https://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=feae177da543411c9efa64160305212d", zoom = 18),
      'hilliness' = list(url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}", zoom = 13)
    )
  })


  ##############
  # Map attribute outputs
  ##############

  ## Initialize the leaflet map
  output$map <- renderLeaflet(
    leaflet() %>%
      # Centroids loaded invisibly to tell it the extent of the map
      # Anna note: centroids do not exist for some purposes like schools - if ever want to *start* in schools layer need to change this to zones
      # addCircleMarkers( .,
      #   data = to_plot$centroids,
      #   radius = 0,
      #   group = "centroids",
      #   opacity = 0.0
      # ) %>%
      setView(.,
        lng = to_plot$center_dim[1, 1],
        lat = to_plot$center_dim[1, 2],
        zoom = 10
      ) %>%
      mapOptions(zoomToLimits = "never")
  )

  ## Attribution statement bottom right + define the map base
  observe({
    input$map_base
    region$current
    leafletProxy("map") %>% addTiles(
      .,
      urlTemplate = map_tile()$url,
      layerId = "background",
      attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
      Routing <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a> |
      Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
      options = tileOptions(
        opacity = 1,
        minZoom = 7,
        reuseTiles = T,
        maxZoom = 18,
        maxNativeZoom = map_tile()$zoom
      )
    ) %>%
      clearGroup(., "imd_extras")
    if (input$map_base == 'IMD') {
      imdTileOptions <-
        tileOptions(
          opacity = 1,
          minZoom = 7,
          reuseTiles = T,
          maxNativeZoom = 14
        )
      leafletProxy("map") %>%
        addTiles(.,
                 urlTemplate = "https://maps.cdrc.ac.uk/tiles/imd2014_wal/{z}/{x}/{y}.png",
                 group = "imd_extras",
                 options = imdTileOptions) %>%
        addTiles(.,
                urlTemplate = "https://maps.cdrc.ac.uk/tiles/shine_urbanmask_dark/{z}/{x}/{y}.png",
                 group = "imd_extras",
                 options = imdTileOptions) %>%
        addTiles(.,
                 urlTemplate = "https://maps.cdrc.ac.uk/tiles/shine_labels_cdrc/{z}/{x}/{y}.png",
                 group = "imd_extras",
                 options = imdTileOptions)

    }
    leafletProxy("map") %>% hideGroup(., "lsoa_base_map") %>% showGroup(., "lsoa_base_map")
  })


  ## Map version info - text in bottom left
  output$cite_html <- renderUI({
    HTML(paste('Version', a(repo_sha, href = paste0("https://github.com/npct/pct-shiny/tree/", repo_sha), target = '_blank'),
      'released under a', a('GNU Affero GPL', href = "../www/licence.html", target = '_blank'), 'and funded by the',
      a('DfT', href = "https://www.gov.uk/government/organisations/department-for-transport", target = "_blank")
    ))
  })


  ## Adds map legend for zones
  observe({
    input$purpose

    # Define the legend title
    if (input$purpose == "commute") { legend_title <- "% cycling to work"}
    else if (input$purpose == "school") { legend_title <- "% cycling to school" }
    else if (input$purpose == "alltrips") { legend_title <- "% trips cycled" }

    leafletProxy("map") %>% removeControl(layerId = "zone_leg")
    if (input$purpose == "commute" || input$purpose == "alltrips") {
      title <- legend_title
      if (input$show_zones) {
        leafletProxy("map") %>%
          addLegend(
            "topleft",
            layerId = "zone_leg",
            colors = get_colour_palette(zcolourscale, 10),
            labels = c("0-1%", "2-3%", "4-6%", "7-9%",
                      "10-14%", "15-19%", "20-24%",
                      "25-29%", "30-39%", "40%+"),
            title = title,
            opacity = 0.5
          )
      }
    } else if (input$purpose == "school") {
      title <- legend_title
      if (input$show_zones) {
        leafletProxy("map") %>%
          addLegend(
            "topleft",
            layerId = "zone_leg",
            colors = get_colour_palette(zcolourscale, 11),
            labels = c("0-1%", "2-3%", "4-6%", "7-9%",
                       "10-14%", "15-19%", "20-24%",
                       "25-29%", "30-39%", "40-49%", "50%+"),
            title = title,
            opacity = 0.5
          )
      }
    }
  })

  ## Creates legend as a barplot for IMD map base
  output$imd_legend <- renderPlot({
    my_lab <- c(
      "Most deprived decile", "2nd", "3rd", "4th", "5th",
      "6th", "7th", "8th", "9th", "Least deprived decile"
      )

    my_lab <- rev(my_lab)

    my_colors <-
      c(
        "#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b",
        "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850", "#006837"
      )

    my_colors <- rev(my_colors)

    # Set the labelling of Y-axis to bold
    par(font.lab = 2, mar = c(0.0, 5.8, 0.0, 1.0))

    bp <- barplot(
      rep(1, 10),
      beside = TRUE,
      col = my_colors,
      ylab = "Index of Multiple \n Deprivation",
      horiz = T,
      axes = F
    )

    text(
      0,
      bp,
      my_lab,
      cex = 0.8,
      pos = 4,
      font = 2,
      col = "black"
    )
  })

  ## Hide/show panels on user-demand
  shinyjs::onclick("toggle_panel", shinyjs::toggle(id = "input_panel", anim = FALSE))
  shinyjs::onclick("toggle_imd_legend", shinyjs::toggle(id = "imd_legend", anim = FALSE))

  ## Function to add a layers control for the routes, so that users can easily select quiet routes
  # NB: currently this resets every time scenario changes or lines get redrawn because of map zoom - possibly to revisit
  observe({
    input$line_type

    if (input$line_type == 'routes') {
      leafletProxy("map") %>% addLayersControl(
        position = c("bottomright"),
        overlayGroups = c("routes_fast", "routes_quieter"),
        options = layersControlOptions(collapsed = T)
      )
    } else if (input$line_type != 'routes') {
      leafletProxy("map") %>% showGroup(c("routes_fast", "routes_quieter")) %>% removeLayersControl()
    }
  })

  ## Read region_stats.html, if it exists, for the loaded region
  output$region_stats <- renderUI({
    input$purpose
    input$geography
    region$current

    region_stats_file <-
      file.path("../tabs/region_stats", input$purpose, input$geography, region$current,
                "region_stats.html")
    if (file.exists(region_stats_file))
      includeHTML(region_stats_file)
    else
      HTML("<strong>No statistics available</strong>")
  })

  ## Read regional data download files
  # This file updates whenever there is a change to input$region
  output$download_region_current <- renderUI({
    html <- includeHTML(file.path("..", "..", "non_www", "tabs", "download_region.html"))
    html <- gsub("<!--region_name-->", get_pretty_region_name(region$current), html)
    gsub("<!--region_url-->", region$current, html)
  })

  ## Create the national data download files
  output$download_national_current <- renderUI({
    includeHTML(file.path("..", "..", "non_www", "tabs", "download_national.html"))
  })

  ##############
  ## Data tables (may remove?)
  ##############

  # ## Creates data for the lines datatable
  # output$lines_datatable <- DT::renderDataTable({
  #   # Reactive values that must trigger a table update
  #   region$repopulate_region
  #   input$line_type
  #
  #   # Only render lines data when any of the Cycling Flows is selected by the user
  #   plot_lines_data <-
  #     !is.null(to_plot$ldata) && !input$line_type %in% show_no_lines &&
  #     (!is.null(input$map_bounds)) &&
  #     input$nos_lines > 0 && (line_data() %in% names(to_plot$ldata@data))
  #   if (!plot_lines_data) {
  #     # Set the warning message that no lines have been selected by the user
  #     output$warning_message <-
  #       renderUI(HTML(
  #         "<strong>No lines selected: </strong> Lines must be displayed on map </br>"
  #       ))
  #     # Return an empty data.frame
  #     return(data.frame(File = character()))
  #   }
  #
  #   # When route network is selected, show 'no lines available
  #   if (input$line_type == 'route_network') {
  #     # Set the warning message that no lines have been selected by the user
  #     output$warning_message <-
  #       renderUI(HTML("<strong>No lines available </strong> </br>"))
  #     # Return an empty data.frame
  #     return(data.frame(File = character()))
  #   }
  #
  #   # Empty the warning message - as some lines have been selected by the user
  #   output$warning_message <- renderUI("")
  #
  #   # Reuse the lines data stored in the ldata session variable
  #   lines_to_plot <- to_plot$ldata@data[, unname(line_col_names)]
  #   decimal_line_cols <-
  #     which(vapply(lines_to_plot, function(x) {
  #       is.numeric(x) && as.integer(x) != x
  #     }, FUN.VALUE = logical(1)))
  #   DT::datatable(
  #     lines_to_plot,
  #     options = list(pageLength = 10),
  #     colnames = line_col_names,
  #     rownames = FALSE
  #   ) %>%
  #     DT::formatRound(columns = decimal_line_cols, digits = 2)
  # })
  #
  # ## Creates data for the zones datatable
  # output$zones_data_table <- DT::renderDataTable({
  #   # Reactive values that must trigger a tabel update
  #   region$repopulate_region
  #
  #   if (is.null(to_plot$zones@data)) {
  #     return()
  #   }
  #
  #   zones_to_plot <- to_plot$zones@data[, unname(zone_col_names)]
  #   decimal_zone_cols <-
  #     which(vapply(zones_to_plot, function(x) {
  #       is.numeric(x) && as.integer(x) != x
  #     }, FUN.VALUE = logical(1)))
  #   DT::datatable(
  #     zones_to_plot,
  #     options = list(pageLength = 10),
  #     colnames = zone_col_names,
  #     rownames = FALSE
  #   ) %>%
  #     DT::formatRound(columns = decimal_zone_cols, digits = 2)
  # })

})
