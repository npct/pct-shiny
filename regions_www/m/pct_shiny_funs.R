#################
# Name, number and colour functions
#################

## Region names [NB copy of this also in pct-scripts/00_setup_and_funs - if modify here, modify there too]
get_pretty_region_name <- function(region_name, the = T){
  if (the == T) {
    region_name <- gsub("isle-of-wight", "the-isle-of-wight", region_name, perl=TRUE)
    region_name <- gsub("north-east", "the-north-east", region_name, perl=TRUE)
    region_name <- gsub("west-midlands", "the-west-midlands", region_name, perl=TRUE)
  }
  region_name <- gsub("(^|-)([[:alpha:]])", " \\U\\2", region_name, perl=TRUE)
  region_name <- gsub("(Of|And|The) ", "\\L\\1 ", region_name, perl=TRUE)
  region_name
}


## Normalise the data ready for plotting (used for centroid sizes and line widths)
normalise <- function(values, min = 0, max = 1){
  # Consider absolute values
  values <- abs(values)
  if(length(values) == 1) { return(( max + min)/2.0) }
  min + ((max-min) * (values - min(values))/diff(range(values)))
}


## Create percentages such that rounded to 1DP if >0% and <0.5%.  [NB can be NaN if denominator zero]
round_percent <- function(data, expression){
 value <- round(100 * expression)
  for(i in 1:length(data)) {
    if(!is.na(expression[i]) && !is.nan(expression[i]) && expression[i]>0 && expression[i]<0.005) {
      value[i] <- round(100 * expression[i], 1)
    }
    if (is.nan(value[i]) || is.na(value[i])) { value[i] <- "-" }
  }
 value
}


## Define and apply the colours of lines
line_and_colour_df <- data.frame(
  line_type = c("centroids", "straight_lines", "routes_fast", "routes_quieter", "route_network"),
  line_colour = c("maroon", "maroon", "purple", "darkgreen", "blue")
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
  sc_f_name_commute = c("Census 2011 Cycling", "Government Target", "Gender equality", "Go Dutch", "Ebikes"),
  sc_f_name_school = c("School Census 2011", "Government Target", "Gender equality", "Go Dutch", "Ebikes"),
  sc_f_name_alltrips = c("Current travel patterns", "Government Target", "Gender equality", "Go Dutch", "Ebikes")
)

get_scenario_name <- function(scenario_name, purpose){
  if (purpose=="commute") {scenario_name_df$sc_f_name_commute[scenario_name_df$sc_s_name == scenario_name]}
  else if (purpose=="school") {scenario_name_df$sc_f_name_school[scenario_name_df$sc_s_name == scenario_name]}
  else if (purpose=="alltrips") {scenario_name_df$sc_f_name_alltrips[scenario_name_df$sc_s_name == scenario_name]}
}

# Get names in pop-ups (variable by purpose)
text_zone_header <- function(purpose) {
  if(purpose=="commute") {"All residents living in zone"}
  else if(purpose=="school") {"All children living in zone"}
  else if(purpose=="alltrips") {"All trips starting in zone"}
}

text_all <- function(purpose) {
  if(purpose=="commute") {"Total commuters: &nbsp;"}
  else if(purpose=="school") {"Total children: &nbsp;"}
  else if(purpose=="alltrips") {"Total weekly no. trips: &nbsp;"}
}

text_cycle_baseline <- function(purpose){
  if(purpose=="commute" | purpose=="school") {"Cyclists (baseline): &nbsp; "}
  else if(purpose=="alltrips") {"Cycle trips/wk (baseline): &nbsp;"}
}

text_cycle_scenario <- function(purpose){
  if(purpose=="commute" | purpose=="school") {"Cyclists (scenario): &nbsp; "}
  else if(purpose=="alltrips") {"Cycle trips/wk (scenario): &nbsp;"}
}

text_drive_baseline <- function(purpose){
  if(purpose=="commute") {"Drivers (baseline): &nbsp; "}
  else if(purpose=="school") {"Driven by car (baseline): &nbsp;"}
  else if(purpose=="alltrips") {"Car trips/wk (baseline): &nbsp;"}
}

text_drive_change <- function(purpose){
  if(purpose=="commute") {"Change in drivers: &nbsp; "}
  else if(purpose=="school") {"Change in driven by car: &nbsp;"}
  else if(purpose=="alltrips") {"Change in car trips/wk: &nbsp;"}
}

text_cycle_interzone <- function(purpose){
  if(purpose=="commute" | purpose=="school")
    list("cycle" = "Between-zone cyclists*", "*" = "* selected cyclists: see Model Output tab")
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

# Create a new variable called font_colour which changes into red colour when change in death/yr is negative
positive_red <- function(dataset, scenario, type){
  dataset@data$font_colour <- ifelse(round(dataset[[data_filter(scenario, type)]]) <0, "red", "black")
}


############
# POP UP FOR STRAIGHT LINES IN HTML TABLE
############
popup_straight_lines <- function(data, scenario, purpose){

  # Create a new variable called font_colour which changes into red colour when change in death/yr is negative
  data@data$font_colour <- ifelse(round(data[[data_filter(scenario, "sivalue_heat")]]) <0, "red", "black")

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
      <td>",  data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car_driver, " (", round_percent(data, data$car_driver / data$all), "%) </td>
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
      <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_cycle_scenario(purpose), "</td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", round_percent(data, data[[data_filter(scenario, "slc")]] / data$all), "%) </td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", data$font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3),
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

  # Create a new variable called font_colour which changes into red colour when change in death/yr is negative
  data@data$font_colour <- ifelse(round(data[[data_filter(scenario, "sivalue_heat")]]) <0, "red", "black")

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
      <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car_driver, " (", round_percent(data, data$car_driver / data$all), "%) </td>
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
      <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_cycle_scenario(purpose), "</td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", round_percent(data, data[[data_filter(scenario, "slc")]] / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", data$font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3),
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
}

############
# ZONE - COMMUTE
############
popup_zones <- function(data, scenario, purpose){

  if (purpose %in% c("commute", "alltrips")) {

  # Create a new variable called font_colour which changes into red colour when change in death/yr is negative
  data@data$font_colour <- ifelse(round(data[[data_filter(scenario, "sivalue_heat")]]) < 0, "red", "black")

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
      <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car_driver, " (", round_percent(data, data$car_driver/data$all), "%) </td>
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
      <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_cycle_scenario(purpose), "</td>
      <td>", round(data[[data_filter(scenario, 'slc')]]), " (", round_percent(data, data[[data_filter(scenario, "slc")]] / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", data$font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3),
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
      <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
      <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car, " (", round_percent(data, data$car/data$all), "%) </td>
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
     <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
   </tr>
   <tr>
     <td>", text_cycle_scenario(purpose), "</td>
     <td>", round(data[[data_filter(scenario, 'slc')]]), " (", round_percent(data, data[[data_filter(scenario, "slc")]] / data$all) , "%) </td>
   </tr>
  </tbody>
</table>")
    }
  }
}

############
# CENTROID
############
popup_centroids <- function(data, scenario, purpose){

  # Create a new variable called font_colour which changes into red colour when change in death/yr is negative
  data@data$font_colour <- ifelse(round(data[[data_filter(scenario, "sivalue_heat")]]) <0, "red", "black")

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
      <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_baseline(purpose), "</td>
      <td>", data$car_driver, " (", round_percent(data, data$car_driver / data$all), "%) </td>
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
      <td>", data$bicycle, " (", round_percent(data, data$bicycle / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_cycle_scenario(purpose), "</td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", round_percent(data, data[[data_filter(scenario, "slc")]] / data$all) , "%) </td>
    </tr>
    <tr>
      <td>", text_drive_change(purpose), "</td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", data$font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3),
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


#################
# DATA DOWNLOAD LINKS
#################

# NB: functions previously had a 'download_name' used to set the download name, but this did not work as links are not same-origin (https://stackoverflow.com/questions/33909763/download-attribute-with-a-file-name-not-working)
# NB2: cdn & sha download does not seem to work with LFS files, hence just sending the latest master version of the national/regional downloads

download_link_national_data <- function(file, purpose, geography, formats = c('Rds', 'geojson', 'csv', 'tif')){
  base_url = paste("https://github.com/npct/pct-outputs-national/raw/master", purpose, geography, sep = "/")
  all_links <- ""
  for(i in 1:length(formats)){
    format <- formats[i]
    all_links <- paste(
      all_links, a(format,
                   href= paste0(base_url, "/", file, ".", format),
                   target='_blank',
                   download = paste0(file, ".", format),
                   onclick= paste0("ga('send', 'event', 'download', '", file , '.' , format, "');")
      )
    )
  }
  all_links
}

download_link_region_data <- function(file, purpose, geography, region, formats = c('Rds', 'geojson', 'csv', 'tif')){
  all_links <- ""
  for(i in 1:length(formats)){
    format <- formats[i]
    if(format == 'Rds') {
      base_url = paste("https://github.com/npct/pct-outputs-regional-R/raw/master", purpose, geography, region, sep = "/")
    } else {
#      base_url = paste("https://github.com/npct/pct-outputs-regional-notR/raw/master", purpose, geography, region, sep = "/")
      base_url = paste("https://github.com/npct/pct-outputs-regional/raw/master", purpose, geography, region, sep = "/")
    }
    all_links <- paste(
      all_links, a(format,
                   href= paste0(base_url, "/", file, ".", format),
                   target='_blank',
                   download = paste0(file, ".", format),
                   onclick= paste0("ga('send', 'event', 'download', '", file , '.' , format, "');")
      )
    )
  }
  all_links
}

download_link_codebook <- function(file, purpose, repo_sha){
  base_url = paste("https://cdn.rawgit.com/npct/pct-shiny", repo_sha, "regions_www/www/static/02_codebooks", purpose, sep = "/")
  all_links <- ""
  all_links <- paste(
      all_links, a("Codebook" ,
                   href= paste0(base_url, "/", file, ".csv"),
                   target='_blank',
                   download = paste0(file, ".csv"),
                   onclick= paste0("ga('send', 'event', 'download', '", file , '.csv', "', '", repo_sha ,"' );")
                   )
      )
  all_links
}

# REMOVE STYLE FROM HTML PAGE (APPLY TO MARKDOWN-CREATED HTML)
remove_unused_tags = function(x){
  style_starts = grep("<style", x)
  style_ends = grep("</style", x)
  # Remove lines ONLY when the 'style' tag exists
  if ((length(style_starts) != 0 && length(style_ends) != 0))
    x <- x[-(style_starts:style_ends)]

  script_starts = grep("<script", x)
  script_ends = grep("</script", x)
  # Remove lines ONLY when the 'style' tag exists
  if ((length(script_starts) != 0 && length(script_ends) != 0))
    x <- x[-(script_starts:script_ends)]

  x
}


# # CREATING ON-SCREEN TABLE FOR DATA DOWNLOAD PAGE [may no longer be needed?]
# col_names_line <- c(
#   "Start and end zones"                          = "id",
#   "Total no. commuters"                          = "all",
#   "No. cyclists in Census 2011"                  = "bicycle",
#   "No. car drivers in Census 2011"               = "car_driver",
#   "No. cyclists in Government Target"            = "govtarget_slc",
#   "Change in deaths/year in Government Target"   = "govtarget_sideath_heat",
#   "Change in CO2/year (kg) in Government Target" = "govtarget_sico2",
#   "No. cyclists in Gender Equality"              = "gendereq_slc",
#   "Change in deaths/year in Gender Equality"     = "gendereq_sideath_heat",
#   "Change in CO2/year (kg) in Gender Equality"   = "gendereq_sico2",
#   "No. cyclists in Go Dutch"                     = "dutch_slc",
#   "Change in deaths/year in Go Dutch"            = "dutch_sideath_heat",
#   "Change in CO2/year (kg) in Go Dutch"          = "dutch_sico2",
#   "No. cyclists in Ebikes"                       = "ebike_slc",
#   "Change in deaths/year in Ebikes"              = "ebike_sideath_heat",
#   "Change in CO2/year (kg) in Ebikes"            = "ebike_sico2"
# )
#
# col_names_zone <- c(
#   "Zone code"                                    = "geo_code",
#   "Zone name"                                    = "geo_name",
#   "Total no. commuters"                          = "all",
#   "No. cyclists in Census 2011"                  = "bicycle",
#   "No. car drivers in Census 2011"               = "car_driver",
#   "No. cyclists in Government Target"            = "govtarget_slc",
#   "Change in deaths/year in Government Target"   = "govtarget_sideath_heat",
#   "Change in CO2/year (kg) in Government Target" = "govtarget_sico2",
#   "No. cyclists in Gender Equality"              = "gendereq_slc",
#   "Change in deaths/year in Gender Equality"     = "gendereq_sideath_heat",
#   "Change in CO2/year (kg) in Gender Equality"   = "gendereq_sico2",
#   "No. cyclists in Go Dutch"                     = "dutch_slc",
#   "Change in deaths/year in Go Dutch"            = "dutch_sideath_heat",
#   "Change in CO2/year (kg) in Go Dutch"          = "dutch_sico2",
#   "No. cyclists in Ebikes"                       = "ebike_slc",
#   "Change in deaths/year in Ebikes"              = "ebike_sideath_heat",
#   "Change in CO2/year (kg) in Ebikes"            = "ebike_sico2"
# )