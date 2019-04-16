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
must_be_installed_pkgs <- c("rgdal", "rgeos", "shinyjs")

## Path directories to load data (expect regional data as a sibling of interface_root)
interface_root <- file.path("..", "..")
data_regional_root <-  file.path(interface_root, '..', 'pct-outputs-regional-R')

outputs_regional_sha <- as.character(readLines(file.path(interface_root, "outputs_regional_sha")))

## Check if working on server and if not initiate environment (including packages)
on_server <- grepl("shiny", Sys.info()["user"])
if (!on_server) {
  source(file.path(interface_root, "scripts", "init.R"))
  init_dev_env(interface_root, data_regional_root, outputs_regional_sha,c(available_locally_pkgs, must_be_installed_pkgs))
}

repo_sha <-  as.character(readLines(file.path(interface_root, "repo_sha")))

# Check if we are on the production server (npt followed by any number of digits (only) is a prod machine)
is_prod <- grepl("npt\\d*$", Sys.info()["nodename"])

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
# NB: CURRENTLY WRITTEN FOR COMMUTE - ULTIMATELY MAKE VARIABLE BY PURPOSE
lsoa_legend_df <- data.frame(
  colours = c("#9C9C9C", "#FFFF73", "#AFFF00", "#00FFFF",
              "#30B0FF", "#2E5FFF", "#0000FF", "#FF00C5"),
  labels = c("1-9", "10-49", "50-99", "100-249",
             "250-499", "500-999", "1000-1999", "2000+")
)
loaded_data <- list()
loaded_data_accessed <- list()

# # # # # # # #
# shinyServer #
# # # # # # # #
shinyServer(function(input, output, session) {

  input_purpose <- reactive({
    if(is.null(input$purpose)) {
      "commute"
    } else {
      input$purpose
    }
  })

  ##############
  # FUNCTIONS TO CUSTOMISE RIGHT HAND MENU BY PURPOSE/SCENARIO [NB changes here may need to be made to UI too!]
  ##############
  ## Define purposes and geographies available depending on region
  update_purposegeog <- function(purposes_present, geographies_present) {
    # Identify locally available purposes and update list accordingly
    if(!is.null(input$purpose)){
      local_purposes <- c(
        "Commuting"  = "commute" ,
        "School travel"  = "school" ,
        "All trips"  = "alltrips"
      )[purposes_present]

      # Remove all trips for prod branch
      if (is_prod)
        local_purposes <- local_purposes[local_purposes != 'alltrips']


      if(input$purpose %in% local_purposes) {
        selected_purpose <- input$purpose
      } else {
        selected_purpose <- local_purposes[1]
      }
      updateSelectInput(session, "purpose", choices = local_purposes, selected = selected_purpose)
    }
    # Identify locally available geographies
    local_geographies <- c(
      "Middle Super Output Area"  = "msoa" ,
      "Lower Super Output Area"  = "lsoa"
    )[geographies_present]

    # Update geographies available, and set to default if the currently-selected geography does not exist
    if (region$geography %in% local_geographies) {
      selected_geog <- region$geography
    } else {
      selected_geog <- local_geographies[1]
    }
    updateSelectInput(session, "geography", choices = local_geographies, selected = selected_geog)
  }

  ## Define scenarios line types and line orders available, and their labels, depending on purpose
  update_labels <- function(purpose, geography) {

    if (purpose == "commute") {
      local_scenarios <- c(
        "Census 2011 Cycling" = "olc",
        "Government Target (equity)"   = "govtarget",
        "Government Target (near market)"   = "govnearmkt",
        "Gender equality"     = "gendereq",
        "Go Dutch"            = "dutch",
        "Ebikes"              = "ebike"
      )
      if (geography == "msoa") {
        local_line_types <- c("None"             = "none",
                              "Straight Lines"         = "straight_lines",
                              "Fast Routes"            = "routes_fast",
                              "Fast & quieter Routes"  = "routes",
                              "Route Network (MSOA)"   = "route_network",
                              "Route Network (LSOA)"   = "lsoa_base_map"
        )
      } else if (geography == "lsoa") {
        local_line_types <- c("None"             = "none",
                              "Straight Lines"         = "straight_lines",
                              "Fast Routes"            = "routes_fast",
                              "Fast & quieter Routes"  = "routes",
                              "Route Network (LSOA)"   = "lsoa_base_map"
        )
      }

      local_line_order <- c(
        "Number of cyclists"   = "slc",
        "Increase in cyclists" = "sic",
        "Reduction in deaths"  = "sideath_heat",
        "Reduction in CO2"     = "sico2"
      )
    } else if (purpose == "school") {
      local_scenarios <- c(
        "School Census 2011"      = "olc",
        "Government Target (equity)"       = "govtarget",
        "Go Dutch"                = "dutch"
      )
      local_line_types <- c("None"  = "none",
                            "Route Network (LSOA, clickable)"   = "route_network",
                            "Route Network (LSOA, image)" = "route_network_tile"
      )
      local_line_order <- c(
        "Number of cycle trips" = "slc",
        "Increase in cyclists"   = "sic"
      )

    } else if (purpose == "alltrips") {
      local_scenarios <- c(
        "Current travel patterns" = "olc",
        "Government Target (equity)"       = "govtarget",
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
        "Increase in cycling"   = "sic",
        "Reduction in deaths"   = "sideath_heat",
        "Reduction in CO2"      = "sico2"
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
  # Initialise region, and update right hand menu by region/purpose
  ##############

  ## Create region and (for persistent geographical values) helper
  region <- reactiveValues(current = NA, data_dir = NA, geography = NA, repopulate_region = F, purposes_present = NA, plot = NULL)
  helper <- NULL
  helper$e_lat_lng <- ""
  helper$old_geog <- ""
  helper$old_purpose <- ""

  load_data <- function(base_path, filename, purpose, str_lines = NULL){
    filepath <- file.path(base_path, filename)

    while (length(loaded_data) > 100) { # Rm objects from the list when too many (by time last accessed)
      loaded_data[[names(loaded_data_accessed[loaded_data_accessed == min(unlist(loaded_data_accessed))])]] <<- NULL
    }
    if (file.exists(filepath)) {
      loaded_data_accessed[[filepath]] <<- Sys.time()
      if (is.null(loaded_data[[filepath]])) {
        rds <- readRDS(filepath)
        if(filename == "rnet.Rds") {
          if(!is.null(rds)){
            rds$id <- rds$local_id
          }
        }
        if(filename == "rq.Rds"){
          # Merge in scenario data for quiet routes - don't want this in download but need for line sorting
          rds@data <- cbind(
            rds@data[!(names(rds) %in% names(str_lines))],
            str_lines@data)
          # Add is_quiet column to identify quieter, as opposed to faster, data - used in routes pop-up
          rds@data$is_quiet <- T
        }

        loaded_data[[filepath]] <<- rds
      } else {
        loaded_data[[filepath]]
      }
    } else {
      NULL
    }
  }

  ## Set  values of region
  observe({
    shinyjs::showElement(id = "loading")
    if(is.null(input$geography)){
      return()
    }
    region$current
    region$data_dir
    region$geography
    region$repopulate_region
    region$purposes_present
    input$purpose

    # Identify region from URL or use a default
    if (is.na(region$current)) {
      query <- parseQueryString(session$clientData$url_search)
      region$current <- if (isTRUE(query[['r']] %in% regions$region_name)) {
        query[['r']]
      } else {
        "isle-of-wight"
      }
    }

    # Notify browser to update URL to reflect new region
    session$sendCustomMessage("regionchange", region$current)

    # Define region geography, forcing a default in cases where the geography is not available
    switch(input_purpose(),
           "commute"= {
             if (input$geography %in% c("msoa", "lsoa")) {
               region_geo_change_to <- input$geography
             } else {
               region_geo_change_to <- "msoa"
             }
           },
           "school"= { region_geo_change_to <- "lsoa" },
           "alltrips"= { region_geo_change_to <- "msoa" }
    )

    # Only trigger geography changes if required.
    if (!identical(region_geo_change_to, region$geography)) {
      region$geography <- region_geo_change_to
    }

    # Set data_dir
    region$data_dir <- file.path(data_regional_root, input_purpose(), region$geography, region$current)

    # Identify that region repopulation has happened
    region$repopulate_region <- T

    # Identify purposes and geographies available in region
    purposes_list <- c("commute", "school", "alltrips")
    new_purpose <- (dir.exists(file.path(data_regional_root, purposes_list, "msoa", region$current)) | dir.exists(file.path(data_regional_root, purposes_list, "lsoa", region$current)))
    # Remove alltrips from new_purpose for the production branch, even if the data directory exists
    new_purpose <- ifelse(is_prod, new_purpose[new_purpose != "alltrips"], new_purpose)
    if(!identical(new_purpose,region$purposes_present)){
      region$purposes_present <- new_purpose
    }
    geographies_list <- c("msoa", "lsoa")
    region$geographies_present <- dir.exists(file.path(data_regional_root, input_purpose(), geographies_list, region$current))

    # Identify the centre of the current region, save in region$plot

    isolate({
      region$plot$center_dim <- rgeos::gCentroid(regions[regions$region_name == region$current, ], byid = TRUE)@coords

      # Load data to region$plot (if data exists - this varies by purpose/geography)
      region$plot$zones <- load_data(region$data_dir, "z.Rds", input_purpose())
      region$plot$centroids <- load_data(region$data_dir, "c.Rds", input_purpose())
      region$plot$destinations <- load_data(region$data_dir, "d.Rds", input_purpose())
      region$plot$straight_lines <- load_data(region$data_dir, "l.Rds", input_purpose())
      region$plot$routes_fast <- load_data(region$data_dir, "rf.Rds", input_purpose())
      region$plot$route_network <- load_data(region$data_dir, "rnet.Rds", input_purpose())
      region$plot$routes_quieter <- load_data(region$data_dir, "rq.Rds", input_purpose(), region$plot$straight_lines)

      # For confidentiality we have replaced exact numbers with NAs but they cause havoc with the interface.
      # This replaces the NAs with the mean values.
      if (input_purpose() == "school") {
        columns_na <- c("all", "bicycle", "foot", "car")

        z_na_const <- 1.5
        d_na_const <- 3
        rnet_na_const <- 1.5

        idx <- is.na(region$plot$zones@data[,columns_na])
        region$plot$zones@data[,columns_na][idx] <- z_na_const

        idx <- is.na(region$plot$zones@data[,school_na("govtarget")$na])
        region$plot$zones@data[,school_na("govtarget")$na][idx] <- z_na_const +
          region$plot$zones@data[,school_na("govtarget")$base][idx]

        idx <- is.na(region$plot$zones@data[,school_na("dutch")$na])
        region$plot$zones@data[,school_na("dutch")$na][idx] <- z_na_const +
          region$plot$zones@data[,school_na("dutch")$base][idx]

        idx <- is.na(region$plot$destinations@data[,columns_na])
        region$plot$destinations@data[,columns_na][idx] <- d_na_const

        idx <- is.na(region$plot$destinations@data[,school_na("govtarget")$na])
        region$plot$destinations@data[,school_na("govtarget")$na][idx] <- d_na_const +
          region$plot$destinations@data[,school_na("govtarget")$base][idx]

        idx <- is.na(region$plot$destinations@data[,school_na("dutch")$na])
        region$plot$destinations@data[,school_na("dutch")$na][idx] <- d_na_const +
          region$plot$destinations@data[,school_na("dutch")$base][idx]

        region$plot$route_network@data[is.na(region$plot$route_network@data)] <- rnet_na_const
      }
    })
    shinyjs::hideElement(id = "loading")
  }, priority = 3)


  # Only requred to run if the region changes (as that affects purpose) or the purpose changes (as that affects geographies)
  observe({
    shinyjs::showElement(id = "loading")
    region$current
    input_purpose()
    isolate({
      update_purposegeog(region$purposes_present, region$geographies_present)
    })
    shinyjs::hideElement(id = "loading")
  }, priority =  2)

  ## Update labels according to purpose
  # NB don't have as part of above 'observes' otherwise those re-run when scenario changes, even though data all the same
  observe({
    shinyjs::showElement(id = "loading")
    # massive hack to return early if the geography and purpose haven't actually changed
    if (helper$old_geog == region$geography && helper$old_purpose == input_purpose()) {
      return()
    }
    helper$old_purpose <<- input_purpose()
    helper$old_geog <<- region$geography
    update_labels(input_purpose(), region$geography)
    shinyjs::hideElement(id = "loading")
  }, priority = 1)


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
  show_no_lines <- c("none", "lsoa_base_map", "route_network_tile")

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
      # Sort low-to-high for reduction in deaths (can't use absolute values as no. deaths can be a positive number, i.e. health disbenefit)
      if (grepl(c("sideath_heat"), line_data())) {
        lines_in_bb[tail(order(lines_in_bb[[line_data()]], decreasing = T), nos),]
      } else {
        # sort by absolute values for remainder of things, which all have zero as higher or lower limit
        lines_in_bb[tail(order(abs(lines_in_bb[[line_data()]])), nos),]
      }
    } else {
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
      popup = popop_fun(sorted_l, input$scenario, input_purpose()),
      layerId = paste0(sorted_l[['id']], '-', line_type)
    )

  }

  ## Plot if lines change
  observe({
    shinyjs::showElement(id = "loading")

    line_type <- ifelse(input$line_type == 'routes', "routes_quieter", input$line_type)
    local_lines <-  sort_lines(region$plot[[line_type]], input$line_type, input$nos_lines)

    # Filter out zero lines for scenario in question from route network
    if (input$line_type == "route_network") {
      if (input$scenario == 'olc') {
       local_lines <- local_lines[local_lines$bicycle>0,]
      } else if (input$scenario == 'govtarget') {
        local_lines <- local_lines[local_lines$govtarget_slc>0,]
      } else if (input$scenario == 'govnearmkt') {
        local_lines <- local_lines[local_lines$govnearmkt_slc>0,]
      } else if (input$scenario == 'gendereq') {
        local_lines <- local_lines[local_lines$gendereq_slc>0,]
      } else {
        local_lines <- local_lines[local_lines$dutch_slc>0,] # always >0 for both
      }
    }

    if (is.null(region$plot$ldata) || (!is.null(region$plot$ldata) && (!identical(region$plot$ldata, local_lines) || !identical(region$plot$scenario, input$scenario)))) {
      leafletProxy("map")  %>% clearGroup(.,
                                          c("straight_lines",
                                            "routes_quieter",
                                            "routes_fast",
                                            "route_network"
                                          )) %>%
        removeShape(., "highlighted")
      isolate({
      region$plot$ldata <- local_lines
      # Include current scenario in region$plot as the set of lines to plot may not change when the scenario alters, and so otherwise don't update
      region$plot$scenario <- input$scenario
      })
      plot_lines(leafletProxy("map"), region$plot$ldata, line_type)
      # Additionally plot fast routes on top of quieter if selected 'fast & quieter'
      if (input$line_type == 'routes') {
        plot_lines(leafletProxy("map"), sort_lines(region$plot$routes_fast, "routes_fast", input$nos_lines),"routes_fast")
      }
    }

    if (input$line_type == 'route_network') {
      updateSliderInput(
        session,
        inputId = "nos_lines",
        min = 10,
        max = 90,
        step = 20,
        label = "Percent (%) of Network"
      )
    } else {
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
    shinyjs::hideElement(id = "loading")
  }, priority = - 10)


  ##############
  # Plot zones and centroids
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


  ## Display zones
  observe({
    line_type <- isolate(input$line_type)
    region$repopulate_region

    clearGroup(leafletProxy("map"), c("zones"))
    ## Display zones
    if (input$show_zones && !is.null(region$plot$zones)) {
      # Define bins and breaks (by purpose)
      if (input_purpose() == "school") {
        zbins <- zbins_school
        zbreaks <- zbreaks_school
      } else {
        zbins <- zbins_commute
        zbreaks <- zbreaks_commute
      }
      # Show zones when no lines are selected
      show_zone_popup <- (line_type %in% show_no_lines)
      popup <-
        if (show_zone_popup)
          popup_zones(region$plot$zones, input$scenario, input_purpose())
      addPolygons(
        leafletProxy("map"),
        data = region$plot$zones,
        weight = 2,
        fillOpacity = transp_rate(),
        opacity = 0.2,
        fillColor = get_colour_ramp(zcolourscale, zbins, (region$plot$zones[[zone_data()]] /region$plot$zones$all), zbreaks),
        color = "black",
        group = "zones",
        popup = popup,
        options = pathOptions(clickable = show_zone_popup),
        layerId = paste0(region$plot$zones[['geo_code']], '-', "zones")
      )
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

  ## Define centroids
  observe({
    input$line_type
    input$map_zoom

    clearGroup(leafletProxy("map"), c("centroids"))
    # Define centroids (if exist) and display when zoom level is greater or equal to 11 and lines are selected
    if (!is.null(region$plot$centroids)) {
      addCircleMarkers(leafletProxy("map"), data = region$plot$centroids,
                       radius = normalise(region$plot$centroids$all, min = 1, max = 8),
                       color = get_line_colour("centroids"), group = "centroids", opacity = 0.5,
                       popup = popup_centroids(region$plot$centroids, input$scenario, input_purpose()),
                       layerId = paste0(region$plot$centroids[['geo_code']], '-', "centroids")
      )
      if (isTRUE((is.null(input$map_zoom)) || input$map_zoom < 11 || (input$line_type %in% show_no_lines) || (input$line_type=="route_network"))) {
        hideGroup(leafletProxy("map"), "centroids")
      } else {
        showGroup(leafletProxy("map"), "centroids")
      }
    }
  })

  ## Define destinations
  observe({
    input$line_type
    input$map_zoom

    clearGroup(leafletProxy("map"), c("destinations"))
    # Define destinations (if exist) and display when zoom level is greater or equal to 11 and lines are selected
    if (!is.null(region$plot$destinations)) {
      addCircleMarkers(leafletProxy("map"), data = region$plot$destinations,
                       radius = normalise(region$plot$destinations$all, min = 1, max = 8),
                       color = get_line_colour("destinations"), group = "destinations", opacity = 0.5,
                       popup = popup_destinations(region$plot$destinations, input$scenario, input_purpose()),
                       layerId = paste0(region$plot$destinations[['urn']], '-', "destinations")
      )
      if (isTRUE((is.null(input$map_zoom)) || input$map_zoom < 11 )) {
        hideGroup(leafletProxy("map"), "destinations")
      } else {
        showGroup(leafletProxy("map"), "destinations")
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
    new_regions_with_purpose <- regions$region_name[(dir.exists(file.path(data_regional_root, input_purpose(), region$geography, regions$region_name))) & (!regions$region_name %in% region$current)]

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
      new_data_dir <- file.path(data_regional_root, input_purpose(), region$geography, new_region)

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
          data = region$plot$centroids[region$plot$c$geo_code == id, ],
          fill = F,
          color = get_line_colour("centroids") ,
          opacity = 0.7,
          layerId = "highlighted"
        )
      } else if (event$group == "zones") {
        addPolygons(
          leafletProxy("map"),
          data = region$plot$zones[region$plot$z$geo_code == id, ],
          fill = FALSE,
          color = "black",
          opacity = 0.7 ,
          layerId = "highlighted"
        )
      } else {
        line <- region$plot[[line_type]][region$plot[[line_type]]$id == id, ]
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
  ## NB in future need to make this purpose + geography specific
  observe({
    shinyjs::showElement(id = "loading")
    if (input$line_type %in% c("lsoa_base_map", "route_network_tile")) {
      urlTemplate <- paste("https://npttile.vs.mythic-beasts.com", input_purpose(), "v2", input$scenario,"{z}/{x}/{y}.png", sep= "/")
      leafletProxy("map") %>%
        addTiles(
          .,
          urlTemplate = urlTemplate,
          layerId = "lsoa_base_map",
          group = "lsoa_base_map",
          options = tileOptions(
            maxNativeZoom = 15,
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
    shinyjs::hideElement(id = "loading")
  })

  ## Updates map tile according to the selected map base
  map_tile <- reactive({
    switch(
      input$map_base,
      'roadmap' = list(url = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png", zoom = 18),
      'satellite' = list(url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", zoom = 18),
      'IMD' = list(url = "https://cdrc-maps.liv.ac.uk/tiles/imd2015_eng/{z}/{x}/{y}.png", zoom = 14),
      'opencyclemap' = list(url = "https://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=feae177da543411c9efa64160305212d", zoom = 18),
      'hilliness' = list(url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}", zoom = 13)
    )
  })


  ##############
  # Map attribute outputs
  ##############

  ## Initialize the leaflet map
  output$map <- renderLeaflet(
    isolate(
      leaflet() %>%
        setView(.,
                lng = region$plot$center_dim[1, 1],
                lat = region$plot$center_dim[1, 2],
                zoom = 10
        ) %>%
        mapOptions(zoomToLimits = "never")
    )
  )

  ## Attribution statement bottom right + define the map base
  observe({
    region$current
    tileOpts <- tileOptions(
      opacity = 1,
      minZoom = 7,
      reuseTiles = T,
      maxZoom = 18,
      maxNativeZoom = map_tile()$zoom
    )

    leafletProxy("map") %>% addTiles(
      .,
      urlTemplate = map_tile()$url,
      layerId = "background",
      attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
      Routing <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a> |
      Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
      options = tileOpts
    ) %>%
      clearGroup(., "imd_extras")
    if (input$map_base == 'IMD') {
      leafletProxy("map") %>%
        addTiles(.,
                 urlTemplate = "https://cdrc-maps.liv.ac.uk/tiles/imd2014_wal/{z}/{x}/{y}.png",
                 group = "imd_extras",
                 options = tileOpts) %>%
        addTiles(.,
                urlTemplate = "https://cdrc-maps.liv.ac.uk/tiles/shine_urbanmask_dark/{z}/{x}/{y}.png",
                 group = "imd_extras",
                 options = tileOpts) %>%
        addTiles(.,
                 urlTemplate = "https://cdrc-maps.liv.ac.uk/tiles/shine_labels_cdrc/{z}/{x}/{y}.png",
                 group = "imd_extras",
                 options = tileOpts)

    }
    leafletProxy("map") %>% hideGroup(., "lsoa_base_map") %>% showGroup(., "lsoa_base_map")
    shinyjs::hide(id = "loading")
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
    input_purpose()

    # Define the legend title
    switch(input_purpose(),
           "commute" =  { legend_title <- "% cycling to work"},
           "school" = { legend_title <- "% cycling to school" },
           "alltrips" = { legend_title <- "% trips cycled" }
    )
    leafletProxy("map") %>% removeControl(layerId = "zone_leg")

    if (input_purpose() == "school") {
      if (input$show_zones) {
        leafletProxy("map") %>%
          addLegend(
            "topleft",
            layerId = "zone_leg",
            colors = get_colour_palette(zcolourscale, 11),
            labels = c("0-1%", "2-3%", "4-6%", "7-9%",
                       "10-14%", "15-19%", "20-24%",
                       "25-29%", "30-39%", "40-49%", "50%+"),
            title = legend_title,
            opacity = 0.5
          )
      }
    } else {
      if (input$show_zones) {
        leafletProxy("map") %>%
          addLegend(
            "topleft",
            layerId = "zone_leg",
            colors = get_colour_palette(zcolourscale, 10),
            labels = c("0-1%", "2-3%", "4-6%", "7-9%",
                       "10-14%", "15-19%", "20-24%",
                       "25-29%", "30-39%", "40%+"),
            title = legend_title,
            opacity = 0.5
          )
      }
    }
  })

  ## Creates legend as a barplot for IMD map base
  output$imd_legend <- renderPlot({
    my_lab <- c(
      "Most deprived tenth", "2nd", "3rd", "4th", "5th",
      "6th", "7th", "8th", "9th", "Least deprived tenth"
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
    input_purpose()
    input$geography
    region$current

    region_stats_file <-
      file.path("../tabs/region_stats", input_purpose(), input$geography, region$current,
                "region_stats.html")
    if (file.exists(region_stats_file))
      includeHTML(region_stats_file)
    else
      HTML("<strong>No statistics available</strong>")
  })

  ## Read regional data download files
  # This file updates whenever there is a change to input$region
  output$download_region_current <- renderUI({
    input_purpose()

    html <- includeHTML(file.path("..", "..", "non_www", "tabs", input_purpose(), "download_region.html"))
    html <- gsub("<!--region_name-->", get_pretty_region_name(region$current), html)
    gsub("<!--region_url-->", region$current, html)
  })

  ## Create the national data download files
  output$download_national_current <- renderUI({
    input_purpose()

    includeHTML(file.path("..", "..", "non_www", "tabs", input_purpose(), "download_national.html"))
  })
})
