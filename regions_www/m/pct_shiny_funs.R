#################
# Name, number and colour functions
#################

## Region names [NB copy of this also in pct-scripts/00_setup_and_funs - if modify here, modify there too]
get_pretty_region_name <- function(region_name_in, the = T){
  regions_thes <- c(
    "isle-of-wight" = "the Isle of Wight",
    "north-east" = "the North East",
    "west-midlands" = "the West Midlands"
  )
  region_name <- gsub("(\\b[[:alpha:]])", "\\U\\1", region_name_in, perl=TRUE)
  region_name <- gsub("-", " ", region_name)
  region_name <- gsub("(Of|And|The) ", "\\L\\1 ", region_name, perl=TRUE)

  if(the) {
    ifelse(!is.na(regions_thes[region_name_in]), regions_thes[region_name_in], region_name)
  } else {
    region_name
  }
}

## Return a named list with the possible NAs columns and the base column to replace them,
## i.e. govtarget_slc NAs should be replaced with 3 + govtarget_sic
school_na <- function(scenario){
  list(
    na   = c(paste0(scenario, "_slc"), paste0(scenario, "_slw"), paste0(scenario, "_sld")),
    base = c(paste0(scenario, "_sic"), paste0(scenario, "_siw"), paste0(scenario, "_sid"))
  )
}

## Normalise the data ready for plotting (used for centroid sizes and line widths)
normalise <- function(values, min = 0, max = 1){
  # Consider absolute values
  values <- abs(values)
  if(length(values) == 1) { return(( max + min)/2.0) }
  min + ((max-min) * (values - min(values))/diff(range(values)))
}


## Create Values such that rounded to XDP if >0 and <0.5.  [NB can be NaN if denominator zero]
round_dp <- function(expression, small_threshold = 0.05, large_threshold = 0.5){
  small <- is.finite(expression) & expression > -small_threshold & expression < small_threshold
  medium <- is.finite(expression) & ((expression <= -small_threshold & expression > -large_threshold) | (expression >= small_threshold & expression < large_threshold))
  large <- is.finite(expression) & (expression <= -large_threshold | expression >= large_threshold)
  other <- is.nan(expression) | is.na(expression)
  expression[small] <- round(expression[small], 2)
  expression[medium] <- round(expression[medium], 1)
  expression[large] <- round(expression[large])
  expression[other] <- "-"
  expression
}

round_percent <- function(expression){
  round_dp(expression * 100, small_threshold = 0.0, large_threshold = 0.5)
}

## Define and apply the colours of lines
line_and_colour_df <- data.frame(
  line_type = c("destinations", "centroids", "straight_lines", "routes_fast", "routes_quieter", "route_network"),
  line_colour = c("darkblue", "maroon", "maroon", "purple", "darkgreen", "blue")
)

get_line_colour <- function(line_type){
  line_and_colour_df$line_colour[line_and_colour_df$line_type == line_type]
}


## Create customised colour scales
get_colour_palette <- function(colourscale, bins = 10){
  # Manually modify to be 'standard 10 plus one extra' for 11 levels
  if (colourscale == "RdYlBu" && bins == 11) {
    local_palette <- RColorBrewer::brewer.pal(n = 10, name = colourscale)
    extra_colour <- "#2d004b"
    local_palette <- append(local_palette, extra_colour)
  } else {
    local_palette <- RColorBrewer::brewer.pal(n = bins, name = colourscale)
  }
  # Replace #e0f3f8 with #c6dbef for colourbrewer "RdYlBu"
  if (colourscale == "RdYlBu") {
    local_palette <- gsub(pattern = "#E0F3F8", replacement = "#C6DBEF", x = local_palette)
  }
  local_palette
}


## Generate a series of colours based on the input values + breaks
get_colour_ramp <- function(colourscale, bins = 10, values, breaks) {
  get_colour_palette(colourscale, bins)[cut(x = values, breaks = breaks)]
}


#################
# LABELLING FUNCTIONS FOR MAP INTERFACE
#################

# Get scenario name
scenario_name_df <- data.frame(
  sc_s_name = c("olc", "govtarget","gendereq","dutch", "ebike"),
  sc_f_name_commute  = c("Census 2011 Cycling", "Government Target", "Gender equality", "Go Dutch", "Ebikes"),
  sc_f_name_school   = c("School Census 2011", "Government Target", "Gender equality", "Go Dutch", "Ebikes"),
  sc_f_name_alltrips = c("Current travel patterns", "Government Target", "Gender equality", "Go Dutch", "Ebikes")
)

get_scenario_name <- function(scenario_name, purpose){
  switch(purpose,
         "commute"  = scenario_name_df$sc_f_name_commute[scenario_name_df$sc_s_name == scenario_name],
         "school"   = scenario_name_df$sc_f_name_school[scenario_name_df$sc_s_name == scenario_name],
         "alltrips" = scenario_name_df$sc_f_name_alltrips[scenario_name_df$sc_s_name == scenario_name])
}

# Get names in pop-ups (variable by purpose)
text_zone_header <- function(purpose) {
  switch(purpose,
         "commute"  = "All residents living in zone",
         "school"   = "All school children living in zone",
         "alltrips" = "All trips starting in zone")
}

text_destination_header <- function(purpose) {
  switch(purpose,
         "school"   = "All children attending this school")
}

text_all <- function(purpose) {
  switch(purpose,
         "commute"  = "Total commuters: &nbsp;",
         "school"   = "Total school children: &nbsp;",
         "alltrips" = "Total weekly no. trips: &nbsp;")
}

text_cycle_baseline <- function(purpose){
  if(purpose=="commute" | purpose=="school") {"Cyclists (baseline): &nbsp; "}
  else if(purpose=="alltrips") {"Cycle trips/wk (baseline): &nbsp;"}
}

text_cycle_scenario <- function(purpose){
  gsub("baseline", "scenario", text_cycle_baseline(purpose))
}

text_cycle_change <- function(purpose){
  if(purpose=="commute" | purpose=="school") {"Change in cyclists: &nbsp; "}
  else if(purpose=="alltrips") {"Change in cycle trips/wk: &nbsp;"}
}

text_drive_baseline <- function(purpose){
  switch(purpose,
         "commute"  = "Drivers (baseline): &nbsp; ",
         "school"   = "Driven by car (baseline): &nbsp;",
         "alltrips" = "Car trips/wk (baseline): &nbsp;")
}

text_drive_change <- function(purpose){
  switch(purpose,
         "commute"  = "Change in drivers: &nbsp; ",
         "school"   = "Change in driven by car: &nbsp;",
         "alltrips" = "Change in car trips/wk: &nbsp;")
}

text_cycle_interzone <- function(purpose){
  if(purpose=="commute")
    list("cycle" = "Between-zone cyclists*", "*" = "* selected cyclists: see Region Stats tab")
  else if(purpose=="school")
    list("cycle" = "Cyclists* ", "*" = "* selected cyclists: see Region Stats tab")
  else if(purpose=="alltrips")
    list("cycle" = "Between-zone cycle trips/wk* ", "*" = "* selected cycle trips")
}


############
# POP-UP FUNCTIONS
############

# Define variable name from scenario name + variable type
# NB for olc defaults to bicycle, i.e. assumes this is always what you want to sort by at baseline
data_filter <- function(scenario, type){
  ifelse(scenario == "olc", "bicycle", paste(scenario, type, sep = "_"))
}

# Return red if the data is negative
negative_red <- function(data, scenario, type){
  ifelse(round_dp(data[[data_filter(scenario, type)]]) < 0, "red", "inherit")
}

# Identify small cells in schools layer
school_smallcell <- function(expression, return_tf = F, d = F){
  if (d) {
    smallcell <- (expression > 0 & expression <= 5)  # Upper bound to suppress cells if 5 in schools [d], otherwise 2 [z, rnet]
    expression[smallcell] <- "1 to 5"
  } else {
    smallcell <- (expression > 0 & expression <= 2)
    expression[smallcell] <- "1 or 2"
  }
  if (return_tf) {
    smallcell
  } else {
    expression
  }
}



############
# POP UP FOR STRAIGHT LINES IN HTML TABLE
############
popup_straight_lines <- function(data, scenario, purpose){

  font_colour <- negative_red(data, scenario, "sivalue_heat")

  # BASELINE TABLE
  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
 	<th>", get_scenario_name(scenario, purpose), "</th>
  <tbody>
    <tr>
      <td>", data$geo_name1 , " - ", data$geo_name2, "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
      <td>",  data$bicycle, " (", round_percent(data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car_driver, " (", round_percent(data$car_driver / data$all), "%) </td>
    </tr>
    <tr>
      <td> Distance (km): &nbsp; </td>
      <td>", round(data$e_dist_km, 1), "</td>
    </tr>
  </tbody>
</table>
")

  } else {

    paste0("
<table class = 'htab'>
  <th> Scenario: ", get_scenario_name(scenario, purpose), "</th>
  <tbody>
    <tr>
      <td>", data$geo_name1 , " - ", data$geo_name2, "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
      <td>", data$bicycle, " (", round_percent(data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_cycle_scenario(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "slc")]]), " (", round_percent(data[[data_filter(scenario, "slc")]] / data$all), "%) </td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3),
             " (&pound;" , round(data[[data_filter(scenario, "sivalue_heat")]]), ")
      </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[data_filter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
    <tr>
      <td> Distance (km): &nbsp; </td>
      <td>", round(data$e_dist_km, 1), "</td>
    </tr>
  </tbody>
</table>")
  }
}


############
# ROUTE POPUP FUNCTION
############
popup_routes <- function(data, scenario, purpose){

  # Identify which of the fast/quiet routes are the quiet routes
  ifelse(("is_quiet" %in% colnames(data@data)), route_type <-'quieter', route_type <-'fast')

  if (route_type == 'fast') {

  font_colour <- negative_red(data, scenario, "sivalue_heat")
    if(scenario == 'olc') {
      paste0("
<table class = 'htab'>
  <th>", get_scenario_name(scenario, purpose), " (baseline) </th>
  <tbody>
    <tr>
      <td>", data$geo_name1 , " - ", data$geo_name2, "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
      <td>", data$bicycle, " (", round_percent(data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car_driver, " (", round_percent(data$car_driver / data$all), "%) </td>
    </tr>
    <tr>
      <td> Fast route distance (km): &nbsp; </td>
      <td>", round(data$rf_dist_km, 1), "</td>
    </tr>
    <tr>
      <td> Hilliness (av. gradient, %): &nbsp; </td>
      <td>", round(data$rf_avslope_perc, 1), "</td>
    </tr>
  </tbody>
</table>
")

    } else {

      paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario, purpose), " </th>
  <tbody>
    <tr>
        <td>", data$geo_name1 , " - ", data$geo_name2, "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
      <td>", data$bicycle, " (", round_percent(data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_cycle_scenario(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "slc")]]), " (", round_percent(data[[data_filter(scenario, "slc")]] / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3),
             " (&pound;" , round(data[[data_filter(scenario, "sivalue_heat")]]), ")
      </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[data_filter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
    <tr>
      <td> Fast route distance (km): &nbsp; </td>
      <td>", round(data$rf_dist_km, 1), "</td>
    </tr>
    <tr>
      <td> Hilliness (av. gradient, %): &nbsp; </td>
      <td>", round(data$rf_avslope_perc, 1), "</td>
    </tr>
  </tbody>
</table>
")
    }
  } else {

    # Create a local variable to distinguish baseline veusus scenarios (since otherwise same pop-up for both)
    routes_quieter_label <- paste("Scenario: ", get_scenario_name(scenario, purpose))
    if(scenario == 'olc')
      routes_quieter_label <-  paste(routes_quieter_label, " (baseline)")

    # Create rqincr_val in a local variable, as a percent.  Append '+' sign with values greater than zero
    rqincr_val <- round( ((100 * data$dist_rq_rf) - 100), 1)
    rqincr_val <- ifelse(rqincr_val > 0, paste0("+", rqincr_val), rqincr_val)

    paste0("
<table class = 'htab'>
  <th> ", routes_quieter_label, " </th>
  <tbody>
    <tr>
        <td>", data$geo_name1 , " - ", data$geo_name2, "</td>
    </tr>
    <tr>
      <td> Quieter route distance (km): &nbsp; </td>
      <td>", round(data$rq_dist_km, 1), "</td>
    </tr>
    <tr>
    <td> Distance compared w. fastest route (%): &nbsp; </td>
      <td>", rqincr_val , "</td>
    </tr>
    <tr>
      <td> Hilliness (av. gradient, %): &nbsp; </td>
      <td>", round(data$rf_avslope_perc, 1), "</td>
    </tr>
  </tbody>
</table>
")
  }
}

############
# ROUTE NETWORK POPUP FUNCTION
############
popup_route_network <- function(data, scenario, purpose){

  if (purpose %in% c("commute")) {
    if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <th>", get_scenario_name(scenario, purpose), " (baseline)</th>
  <tbody>
    <tr>
      <td>", text_cycle_interzone(purpose)[["cycle"]] ," (baseline): &nbsp; </td>
      <td>", data$bicycle, "</td>
    </tr>
    <tr>
      <td>", text_cycle_interzone(purpose)[["*"]] ," </td>
     </tr>
  </tbody>
</table>
")
    } else {
    paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario, purpose), "</th>
  <tbody>
    <tr>
      <td>", text_cycle_interzone(purpose)[["cycle"]] ," (baseline): &nbsp; </td>
      <td>", data$bicycle, "</td>
    </tr>
    <tr>
      <td>", text_cycle_interzone(purpose)[["cycle"]] ," (scenario): &nbsp; </td>
      <td>", round(data[[data_filter(scenario, 'slc')]]), "</td>
    </tr>
    <tr>
      <td> Ratio (scenario / baseline): &nbsp; </td>
      <td>", round(data[[data_filter(scenario, 'slc')]] / data$bicycle, 2 ), "</td>
    </tr>
    <tr>
      <td>", text_cycle_interzone(purpose)[["*"]] ,"</td>
    </tr>
  </tbody>
</table>
")
    }
  } else if (purpose == "school") {
    if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <th>", get_scenario_name(scenario, purpose), " (baseline)</th>
  <tbody>
    <tr>
      <td>", text_cycle_interzone(purpose)[["cycle"]] ," (baseline): &nbsp; </td>
      <td>", school_smallcell(data$bicycle), "</td>
    </tr>
    <tr>
      <td>", text_cycle_interzone(purpose)[["*"]] ," </td>
    </tr>
  </tbody>
</table>
")

    } else {
paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario, purpose), "</th>
  <tbody>
    <tr>
      <td>", text_cycle_interzone(purpose)[["cycle"]] ," (baseline): &nbsp; </td>
      <td>", school_smallcell(data$bicycle), "</td>
    </tr>
    <tr>
      <td>", text_cycle_interzone(purpose)[["cycle"]] ," (scenario): &nbsp; </td>
      <td>", ifelse(school_smallcell(data$bicycle, return_tf = T),school_smallcell(round(data[[data_filter(scenario, 'slc')]])),round(data[[data_filter(scenario, 'slc')]])), "</td>
    </tr>
    <tr>
      <td> Ratio (scenario / baseline): &nbsp; </td>
      <td>", ifelse(school_smallcell(data$bicycle, return_tf = T), "-", round(data[[data_filter(scenario, 'slc')]] / data$bicycle, 2 )), "</td>
    </tr>
    <tr>
      <td>", text_cycle_interzone(purpose)[["*"]] ,"</td>
    </tr>
  </tbody>
</table>
")
    }
  }
}

############
# ZONE POPUP
############
popup_zones <- function(data, scenario, purpose){

  if (purpose %in% c("commute", "alltrips")) {
  font_colour <- negative_red(data, scenario, "sivalue_heat")
  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <thead>
    <tr>
     <th>", text_zone_header(purpose), "</th>
    </tr>
    <tr>
     <th>", get_scenario_name(scenario, purpose), " (baseline)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>", data$geo_name, " (",data$geo_code,")", "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
      <td>", data$bicycle, " (", round_percent(data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car_driver, " (", round_percent(data$car_driver/data$all), "%) </td>
    </tr>
  </tbody>
</table>")

  } else {

    paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th>", text_zone_header(purpose),"</th>
    </tr>
    <tr>
      <th>  Scenario: ", get_scenario_name(scenario, purpose), " </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>", data$geo_name, " (",data$geo_code,")", "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
      <td>", data$bicycle, " (", round_percent(data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_cycle_scenario(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, 'slc')]]), " (", round_percent(data[[data_filter(scenario, "slc")]] / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3),
             " (&pound;" , round(data[[data_filter(scenario, "sivalue_heat")]]), ")
      </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[data_filter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
  </tbody>
</table>")
  }

  } else if (purpose == "school") {
    font_colour <- negative_red(data, scenario, "simmet")

    if(scenario == 'olc') {
      paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th>", text_zone_header(purpose), "</th>
    </tr>
    <tr>
      <th>", get_scenario_name(scenario, purpose), " (baseline)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>", data$geo_name, " (",data$geo_code,")", "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", school_smallcell(data$all), "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
     <td>", school_smallcell(data$bicycle), ifelse(school_smallcell(data$bicycle, return_tf = T), "" , paste0(" (", round_percent(data$bicycle / data$all) , "%)" )), "</td>
     </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
     <td>", school_smallcell(data$car), ifelse(school_smallcell(data$car, return_tf = T), "" , paste0(" (", round_percent(data$car / data$all) , "%)" )), "</td>

     </tr>
  </tbody>
</table>")
    } else {
     paste0("
<table class = 'htab'>
  <thead>
    <tr>
     <th>", text_zone_header(purpose),"</th>
    </tr>
    <tr>
      <th>  Scenario: ", get_scenario_name(scenario, purpose), " </th>
    </tr>
  </thead>
  <tbody>
    <tr>
     <td>", data$geo_name, " (",data$geo_code,")", "</td>
    </tr>
    <tr>
     <td>", text_all(purpose), "</td>
     <td>", school_smallcell(data$all), "</td>
    </tr>
    <tr>
     <td>", text_cycle_baseline(purpose), "</td>
     <td>", school_smallcell(data$bicycle), ifelse(school_smallcell(data$bicycle, return_tf = T), "" , paste0(" (", round_percent(data$bicycle / data$all) , "%)" )), "</td>
   </tr>
   <tr>
     <td>", text_cycle_change(purpose), "</td>
     <td>", round_dp(data[[data_filter(scenario, "sic")]]), "</td>
   </tr>
   <tr>
     <td>", text_drive_change(purpose), "</td>
     <td>", round_dp(data[[data_filter(scenario, "sid")]]), "</td>
   </tr>
   <tr>
    <td> Change (% change) in active  &nbsp; </td>
    <td></td>
   </tr>
   <tr>
    <td> &nbsp; &nbsp; &nbsp; travel mMETs/child/week: &nbsp; </td>
    <td style= 'color:", font_colour , "' >", round_dp(data[[data_filter(scenario, "simmet")]], 0.05, 100),
            " (", round_percent((data[[data_filter(scenario, "simmet")]]/(data$baseline_at_mmet))), "%)", "</td>
   </tr>
   <tr>
     <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
     <td>", round(data[[data_filter(scenario, "sico2")]] / 1000,1), "</td>
   </tr>
  </tbody>
</table>")
    }
  }
}

############
# CENTROID POPUP
############
popup_centroids <- function(data, scenario, purpose){
  font_colour <- negative_red(data, scenario, "sivalue_heat")
 if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th>Within zone travel</th>
    </tr>
    <tr>
      <th>", get_scenario_name(scenario, purpose), " (baseline) </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>", data$geo_name, " (",data$geo_code,")", "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
      <td>", data$bicycle, " (", round_percent(data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car_driver, " (", round_percent(data$car_driver / data$all), "%) </td>
    </tr>
  </tbody>
</table>
")

  } else {

    paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th>Within zone travel</th>
    </tr>
    <tr>
      <th> Scenario: ", get_scenario_name(scenario, purpose), "</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>", data$geo_name, " (",data$geo_code,")", "</td>
    </tr>
    <tr>      <td>", text_all(purpose), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
      <td>", data$bicycle, " (", round_percent(data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_cycle_scenario(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "slc")]]), " (", round_percent(data[[data_filter(scenario, "slc")]] / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3),
             " (&pound;" ,round(data[[data_filter(scenario, "sivalue_heat")]]), ")
      </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[data_filter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
  </tbody>
</table>")
  }
}

############
# DESTINATION POPUP
############
popup_destinations <- function(data, scenario, purpose){

  font_colour <- negative_red(data, scenario, "simmet")
  if(scenario == 'olc') {
    paste0("
 <table class = 'htab'>
   <thead>
    <tr>
      <th>", text_destination_header(purpose),"</th>
    </tr>
    <tr>
      <th>", get_scenario_name(scenario, purpose), " (baseline) </th>
    </tr>
   </thead>
   <tbody>
    <tr>
      <td>", data$schoolname, "</td>
    </tr>
    <tr>
      <td>", "(urn=",data$urn,")", "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", school_smallcell(data$all, d = T), "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
     <td>", school_smallcell(data$bicycle, d = T), ifelse(school_smallcell(data$bicycle, return_tf = T, d = T), "" , paste0(" (", round_percent(data$bicycle / data$all) , "%)")), "</td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
     <td>", school_smallcell(data$car, d = T), ifelse(school_smallcell(data$car, return_tf = T, d = T), "" , paste0(" (", round_percent(data$car / data$all) , "%)")), "</td>
    </tr>
   </tbody>
 </table>
  ")

  } else {

    paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th>", text_destination_header(purpose),"</th>
    </tr>
    <tr>
      <th>  Scenario: ", get_scenario_name(scenario, purpose), " </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>", data$schoolname, "</td>
    <tr>
    <tr>
      <td>", " (urn=",data$urn,")", "</td>
    </tr>
    <tr>
      <td>", text_all(purpose), "</td>
      <td>", school_smallcell(data$all, d = T), "</td>
    </tr>
    <tr>
      <td>", text_cycle_baseline(purpose), "</td>
     <td>", school_smallcell(data$bicycle, d = T), ifelse(school_smallcell(data$bicycle, return_tf = T, d = T), "" , paste0(" (", round_percent(data$bicycle / data$all) , "%)")), "</td>
    </tr>
    <tr>
      <td>", text_cycle_change(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "sic")]]), "</td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round_dp(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change (% change) in active  &nbsp; </td>
      <td></td>
    </tr>
    <tr>
     <td> &nbsp; &nbsp; &nbsp; travel mMETs/child/week: &nbsp; </td>
     <td style= 'color:", font_colour , "' >", round_dp(data[[data_filter(scenario, "simmet")]], 0.05, 100),
         " (", round_percent((data[[data_filter(scenario, "simmet")]]/(data$baseline_at_mmet))), "%)", "</td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[data_filter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
  </tbody>
</table>")

  }
  }

