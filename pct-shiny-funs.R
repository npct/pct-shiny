sc_name_df <- data.frame(
  sc_f_name = c("Census 2011 Cycling", "Government Target", "Gender equality", "Go Dutch", "Ebikes"),
  sc_s_name = c("olc", "govtarget","gendereq","dutch", "ebike")
)

get_scenario_name <- function(sc_name, all_trips){
  if (sc_name == "olc" && all_trips){
    "Current travel patterns"
  } else {
    sc_name_df$sc_f_name[sc_name_df$sc_s_name == sc_name]
  }
}

total_commuters <- function(all_trips) {
  ifelse(all_trips, "Total weekly no. trips:", "Total commuters:")
}

cyclists_baseline <- function(all_trips){
  ifelse(all_trips, "Cycle trips/wk (baseline): &nbsp;", "Cyclists (baseline): &nbsp; ")
}

cyclists_scenario <- function(all_trips){
  ifelse(all_trips, "Cycle trips/wk (scenario): &nbsp;", "Cyclists (scenario): &nbsp; ")
}

cyclists_change <- function(all_trips){
  ifelse(all_trips, "Change in cycle trips/wk: &nbsp;", "Change in cyclists: &nbsp; ")
}

driver_baseline <- function(all_trips){
  ifelse(all_trips, "Car trips/wk: &nbsp;", "Drivers (baseline): &nbsp; ")
}

driver_change <- function(all_trips){
  ifelse(all_trips, "Change in car trips/wk: &nbsp;", "Change in drivers: &nbsp; ")
}
cyclists_interzone <- function(all_trips){
  if(all_trips)
    list("cycle" = "Between-zone cycle trips/wk* ", "*" = "* selected cycle trips")
  else
    list("cycle" = "Between-zone cyclists*", "*" = "* selected cyclists: see Model Output tab")
}

is_decimal <- function(x) {
  is.numeric(x) && x %% 1 != 0
}

signif_sdf <- function(sdf, digits = 3) {
  # Replace NAs with zeros
  sdf@data[ is.na(sdf@data) ] <- 0
  nums <- vapply(sdf@data, is_decimal, FUN.VALUE = T)

  sdf@data[,nums] <- signif(sdf@data[,nums], digits = digits)
  sdf
}

# Data Frame which contains the links of lines and their colours
line_and_colour_df <- data.frame(
  line_type = c("straight_line", "quieter_route", "faster_route", "route_network", "centres"),
  line_colour = c("maroon","darkgreen","purple","blue", "maroon")
)

get_line_colour <- function(line_type){
  line_and_colour_df$line_colour[line_and_colour_df$line_type == line_type]
}

zone_fill_breaks = c(0, 1.5, 3.5, 6.5, 9.5, 14.5, 19.5, 24.5, 29.5, 39.5, 100) / 100  # The bins used for the scale

make_download_link <- function(file, download_name, region, formats = c('Rds', 'geojson', 'csv')){
  base_url = paste("https://cdn.rawgit.com/npct/pct-data", data_sha, region, sep = "/")
  all_links <- ""
  for(i in 1:length(formats)){
    format <- formats[i]
    all_links <- paste(
      all_links, a(format,
                   href= paste0(base_url, "/", file, ".", format),
                   target='_blank',
                   download = paste0(download_name, ".", format),
                   onclick= paste0("ga('send', 'event', 'download', '", file , '.' , format, "', '", data_sha ,"' );")
      )
    )
  }
  all_links
}

line_col_names <- c(
  "Start and end zones"                          = "id",
  "Total no. commuters"                          = "all",
  "No. cyclists in Census 2011"                  = "bicycle",
  "No. car drivers in Census 2011"               = "car_driver",
  "No. cyclists in Government Target"            = "govtarget_slc",
  "Change in deaths/year in Government Target"   = "govtarget_sideath_heat",
  "Change in CO2/year (kg) in Government Target" = "govtarget_sico2",
  "No. cyclists in Gender Equality"              = "gendereq_slc",
  "Change in deaths/year in Gender Equality"     = "gendereq_sideath_heat",
  "Change in CO2/year (kg) in Gender Equality"   = "gendereq_sico2",
  "No. cyclists in Go Dutch"                     = "dutch_slc",
  "Change in deaths/year in Go Dutch"            = "dutch_sideath_heat",
  "Change in CO2/year (kg) in Go Dutch"          = "dutch_sico2",
  "No. cyclists in Ebikes"                       = "ebike_slc",
  "Change in deaths/year in Ebikes"              = "ebike_sideath_heat",
  "Change in CO2/year (kg) in Ebikes"            = "ebike_sico2"
)

zone_col_names <- c(
  "Zone code"                                    = "geo_code",
  "Zone name"                                    = "geo_label",
  "Total no. commuters"                          = "all",
  "No. cyclists in Census 2011"                  = "bicycle",
  "No. car drivers in Census 2011"               = "car_driver",
  "No. cyclists in Government Target"            = "govtarget_slc",
  "Change in deaths/year in Government Target"   = "govtarget_sideath_heat",
  "Change in CO2/year (kg) in Government Target" = "govtarget_sico2",
  "No. cyclists in Gender Equality"              = "gendereq_slc",
  "Change in deaths/year in Gender Equality"     = "gendereq_sideath_heat",
  "Change in CO2/year (kg) in Gender Equality"   = "gendereq_sico2",
  "No. cyclists in Go Dutch"                     = "dutch_slc",
  "Change in deaths/year in Go Dutch"            = "dutch_sideath_heat",
  "Change in CO2/year (kg) in Go Dutch"          = "dutch_sico2",
  "No. cyclists in Ebikes"                       = "ebike_slc",
  "Change in deaths/year in Ebikes"              = "ebike_sideath_heat",
  "Change in CO2/year (kg) in Ebikes"            = "ebike_sico2"
)

scenarios_names <- c(
  "olc"      = "Cyclists (recorded)",
  "slc"      = "Cyclists (scenario)"
)

# Normalise the data ready for plotting
normalise <- function(values, min = 0, max = 1){
  # Consider absolute values
  values <- abs(values)
  if(length(values) == 1) return(( max + min)/2.0)
  min + max * (values - min(values))/diff(range(values))
}

# Create own colour function which replaces #e0f3f8 with #c6dbef
get_colour_palette <- function(color, bins = 10){
  local_palette <- RColorBrewer::brewer.pal(n = bins, name = color)
  # Replace #e0f3f8 with #c6dbef
  local_palette <- gsub(pattern = "#E0F3F8", replacement = "#C6DBEF", x = local_palette)
  local_palette
}

# Generate a series of colours based on the input range
get_colour_ramp <- function(colors, values) {
  get_colour_palette(colors)[cut(x = values, breaks = zone_fill_breaks)]
}

data_filter <- function(scenario, type){
  ifelse(scenario == "olc", "bicycle", paste(scenario, type, sep = "_"))
}

# Popup function for straight line data in html table
straight_line_popup <- function(data, scenario, all_trips){

  # Create a new variable called font_colour which changes into red colour when change in death/yr is negative
  data@data$font_colour <- ifelse(round(data[[data_filter(scenario, "sivalue_heat")]]) <0, "red", "black")

  # Store olc bicycle values in a local variable
  olc_bicycle_percentage <- 100 * data$bicycle / data$all

  # Create a new variable for cyclists, showing 1 decimal place for values less than 0.5
  data@data$olc_bicycle_percentage <- ifelse((olc_bicycle_percentage > 0 & olc_bicycle_percentage < 0.5), round(olc_bicycle_percentage, 1), round(olc_bicycle_percentage))

  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
 	<th>", get_scenario_name(scenario, all_trips), "</th>
  <tbody>
    <tr>
      <td>", data$geo_label1 , " - ", data$geo_label2, "</td>
    </tr>
    <tr>
      <td>", total_commuters(all_trips), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", cyclists_baseline(all_trips), "</td>
      <td>",  data$bicycle, " (", data$olc_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", driver_baseline(all_trips), "</td>
      <td>", data$car_driver, " (", round(100 * data$car_driver / data$all), "%) </td>
    </tr>
    <tr>
      <td> Distance (km): </td>
      <td>", round(data$dist, 1), "</td>
    </tr>
  </tbody>
</table>
")
  } else {


    # Store scenario bicycle values in a local variable
    scenario_bicycle_percentage <- 100 * data[[data_filter(scenario, "slc")]] / data$all

    # Create a new variable for cyclists, showing 1 decimal place for values less than 0.5
    data@data$scenario_bicycle_percentage <- ifelse((scenario_bicycle_percentage > 0 & scenario_bicycle_percentage < 0.5), round(scenario_bicycle_percentage, 1), round(scenario_bicycle_percentage))

    # scenarios table
    # Please align HTML!
    paste0("
<table class = 'htab'>
  <th> Scenario: ", get_scenario_name(scenario, all_trips), "</th>
  <tbody>
    <tr>
      <td>", data$geo_label1 , " - ", data$geo_label2, "</td>
    </tr>
    <tr>
      <td>", total_commuters(all_trips), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", cyclists_baseline(all_trips), "</td>
      <td>", data$bicycle, " (", data$olc_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", cyclists_scenario(all_trips), "</td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", data$scenario_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", driver_change(all_trips), "</td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", data$font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3), " (&pound;" ,
            round(data[[data_filter(scenario, "sivalue_heat")]]), ")
      </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[data_filter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
    <tr>
      <td> Distance (km):</td>
      <td>", round(data$dist, 1), "</td>
    </tr>
  </tbody>
</table>")
  }
}

route_type_label <- NULL
route_type_label[['fastest']] <- 'Direct'
route_type_label[['quietest']] <- 'Quiet'

#
none_popup <- function(){
  # noop
}

# Route popup function
routes_popup <- function(data, scenario, all_trips){

  ifelse(("rqincr" %in% colnames(data@data)), route_type <-'quiet', route_type <-'fast')

  # Create a new variable called font_colour which changes into red colour when change in death/yr is negative
  data@data$font_colour <- ifelse(round(data[[data_filter(scenario, "sivalue_heat")]]) <0, "red", "black")


  # Store olc bicycle values in a local variable
  olc_bicycle_percentage <- 100 * data$bicycle / data$all

  # Create a new variable for cyclists, showing 1 decimal place for values less than 0.5
  data@data$olc_bicycle_percentage <- ifelse((olc_bicycle_percentage > 0 & olc_bicycle_percentage < 0.5), round(olc_bicycle_percentage, 1), round(olc_bicycle_percentage))


  if (route_type=='fast') {
    if(scenario == 'olc') {
      paste0("
<table class = 'htab'>
  <th>", get_scenario_name(scenario, all_trips), " (baseline) </th>
  <tbody>
    <tr>
      <td>", data$geo_label1 , " - ", data$geo_label2, "</td>
    </tr>
    <tr>
      <td>", total_commuters(all_trips), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", cyclists_baseline(all_trips), "</td>
      <td>",  data$bicycle, " (", data$olc_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", driver_baseline(all_trips), "</td>
      <td>", data$car_driver, " (", round(100 * data$car_driver / data$all), "%) </td>
    </tr>
    <tr>
      <td> Route Distance (km): </td>
      <td>", round(data$dist_fast, 1), "</td>
    </tr>
    <tr>
      <td> Hilliness (av. gradient, %): &nbsp; </td>
      <td>", round(100*data$av_incline, 1), "</td>
    </tr>
  </tbody>
</table>
")

    } else {


      # Store scenario bicycle values in a local variable
      scenario_bicycle_percentage <- 100 * data[[data_filter(scenario, "slc")]] / data$all

      # Create a new variable for cyclists, showing 1 decimal place for values less than 0.5
      data@data$scenario_bicycle_percentage <- ifelse((scenario_bicycle_percentage > 0 & scenario_bicycle_percentage < 0.5), round(scenario_bicycle_percentage, 1), round(scenario_bicycle_percentage))


      paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario, all_trips), " </th>
  <tbody>
    <tr>
        <td>", data$geo_label1 , " - ", data$geo_label2, "</td>
    </tr>
    <tr>
      <td>", total_commuters(all_trips), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", cyclists_baseline(all_trips), "</td>
      <td>", data$bicycle, " (", data$olc_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", cyclists_scenario(all_trips), "</td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", data$scenario_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", driver_change(all_trips), "</td>
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
      <td> Route distance (km):</td>
      <td>", round(data$dist_fast, 1), "</td>
    </tr>
    <tr>
      <td> Hilliness (av. gradient, %):</td>
      <td>", round(100*data$av_incline, 1), "</td>
    </tr>
  </tbody>
</table>
")
    }
  } else {
    # Create a local variable to distinguish baseline with scenarios
    quiet_route_label <- paste("Scenario: ", get_scenario_name(scenario, all_trips))
    if(scenario == 'olc')
      quiet_route_label <-  paste(quiet_route_label, " (baseline)")

    # Keep rqincr in a local variable
    val <- data$rqincr
    # Replace NAs with 0
    val[is.na(val)] <- 0
    # Convert it into percent
    val <- round( ((100 * val) - 100), 1)
    # Append '+' sign with values greater than zero
    val <- ifelse(val > 0, paste0("+", val), val)

    paste0("
<table class = 'htab'>
  <th> ", quiet_route_label, " </th>
  <tbody>
    <tr>
        <td>", data$geo_label1 , " - ", data$geo_label2, "</td>
    </tr>
    <tr>
      <td> Route distance (km): </td>
      <td>", round(data$dist_quiet, 1), "</td>
    </tr>
    <tr>
    <td> Distance compared w. fastest route (%): &nbsp; </td>
      <td>", val , "</td>
    </tr>
    <tr>
      <td> Hilliness (av. gradient, %): &nbsp; </td>
      <td>", round(100*data$av_incline, 1), "</td>
    </tr>
  </tbody>
</table>
")  #both olc& scenario
  }
}


# Network Route popup function
route_network_popup <- function(data, scenario, all_trips){
  ############ % increase in distance vs. fastest route ONLY FOR QUIETEST ??

  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <th>", get_scenario_name(scenario, all_trips), " (baseline)</th>
  <tbody>
    <tr>
      <td>", cyclists_interzone(all_trips)[["cycle"]] ," (baseline): &nbsp; </td>
      <td>", data$bicycle, "</td>
    </tr>
    <tr>
      <td>", cyclists_interzone(all_trips)[["*"]] ," </td>
     </tr>
  </tbody>
</table>
")

  } else {

    paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario, all_trips), "</th>
  <tbody>
    <tr>
      <td>", cyclists_interzone(all_trips)[["cycle"]] ," (baseline): &nbsp; </td>
      <td>", data$bicycle, "</td>
    </tr>
    <tr>
      <td>", cyclists_interzone(all_trips)[["cycle"]] ," (scenario): &nbsp; </td>
      <td>", round(data[[data_filter(scenario, 'slc')]]), "</td>
    </tr>
    <tr>
      <td> Ratio (scenario / baseline): &nbsp; </td>
      <td>", round(data[[data_filter(scenario, 'slc')]] / data$bicycle, 2 ), "</td>
    </tr>
    <tr>
      <td>", cyclists_interzone(all_trips)[["*"]] ,"</td>
    </tr>


  </tbody>
</table>
")
  }
}

####ZONE = ANYWHERE INSIDE THE AREA but the centroid
zone_popup <- function(data, scenario, zone, all_trips){
  zone_filter_name <- scenarios_names[zone]

  # Create a new variable called font_colour which changes into red colour when change in death/yr is negative
  data@data$font_colour <- ifelse(round(data[[data_filter(scenario, "sivalue_heat")]]) <0, "red", "black")

  # Store olc bicycle values in a local variable
  olc_bicycle_percentage <- 100 * data[[data_filter('olc', zone)]] /data$all

  # Create a new variable for cyclists, showing 1 decimal place for values less than 0.5
  data@data$olc_bicycle_percentage <- ifelse((olc_bicycle_percentage > 0 & olc_bicycle_percentage < 0.5), round(olc_bicycle_percentage, 1), round(olc_bicycle_percentage))


  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <th>", get_scenario_name(scenario, all_trips), " (baseline)</th>
  <tbody>
    <tr>
      <td> Zone: </td>
      <td>", data$geo_label, "</td>
    </tr>
    <tr>
      <td>", total_commuters(all_trips), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", cyclists_baseline(all_trips), "</td>
      <td>",  round(data[[data_filter('olc', zone)]] ), " (", data$olc_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", driver_baseline(all_trips), "</td>
      <td>", data$car_driver, " (", round(100* data$car_driver/data$all), "%) </td>
    </tr>
  </tbody>
</table>")

  } else {


    # Store scenario bicycle values in a local variable
    scenario_bicycle_percentage <- 100 * data[[data_filter(scenario, "slc")]] / data$all

    # Create a new variable for cyclists, showing 1 decimal place for values less than 0.5
    data@data$scenario_bicycle_percentage <- ifelse((scenario_bicycle_percentage > 0 & scenario_bicycle_percentage < 0.5), round(scenario_bicycle_percentage, 1), round(scenario_bicycle_percentage))


    paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario, all_trips), " </th>
  <tbody>
    <tr>
      <td> Zone: </td>
      <td>", data$geo_label, "</td>
    </tr>
    <tr>
      <td>", total_commuters(all_trips), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", cyclists_baseline(all_trips), "</td>
      <td>", round(data[[data_filter('olc', zone)]] ), " (", data$olc_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", cyclists_scenario(all_trips), "</td>
      <td>", round(data[[data_filter(scenario, 'slc')]]), " (", data$scenario_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td> ", cyclists_change(all_trips), " </td>
      <td>", (round(round(data[[data_filter(scenario, 'slc')]]) - round(data[[data_filter('olc', zone)]]))) , "</td>
    </tr>
    <tr>
      <td>", driver_change(all_trips), "</td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td style= 'color:", data$font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3), " (&pound;" ,
          round(data[[data_filter(scenario, "sivalue_heat")]]), ")
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

####CENTROID
centroid_popup <- function(data, scenario, zone, all_trips){
  zone_filter_name <- scenarios_names[zone]

  # Create a new variable called font_colour which changes into red colour when change in death/yr is negative
  data@data$font_colour <- ifelse(round(data[[data_filter(scenario, "sivalue_heat")]]) <0, "red", "black")


  # Store olc bicycle values in a local variable
  olc_bicycle_percentage <- 100 * data$bicycle / data$all

  # Create a new variable for cyclists, showing 1 decimal place for values less than 0.5
  data@data$olc_bicycle_percentage <- ifelse((olc_bicycle_percentage > 0 & olc_bicycle_percentage < 0.5), round(olc_bicycle_percentage, 1), round(olc_bicycle_percentage))


  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th> Within zone flows </th>
    </tr>
    <tr>
      <th>", get_scenario_name(scenario, all_trips), " (baseline) </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td> Zone: </td>
      <td>", data$geo_label, "</td>
    </tr>
    <tr>
      <td>", total_commuters(all_trips), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", cyclists_baseline(all_trips), "</td>
      <td>", data$bicycle, " (", data$olc_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", driver_baseline(all_trips), "</td>
      <td>", data$car_driver, " (", round(100 * data$car_driver / data$all), "%) </td>
    </tr>
  </tbody>
</table>
")

  } else {


    # Store scenario bicycle values in a local variable
    scenario_bicycle_percentage <- 100 * data[[data_filter(scenario, "slc")]] / data$all

    # Create a new variable for cyclists, showing 1 decimal place for values less than 0.5
    data@data$scenario_bicycle_percentage <- ifelse((scenario_bicycle_percentage > 0 & scenario_bicycle_percentage < 0.5), round(scenario_bicycle_percentage, 1), round(scenario_bicycle_percentage))


    paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th>Within zone flows</th>
    </tr>
    <tr>
      <th> Scenario: ", get_scenario_name(scenario, all_trips), "</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td> Zone: </td>
      <td>", data$geo_label, "</td>
    </tr>
    <tr>
      <td>", total_commuters(all_trips), "</td>
      <td>", data$all, "</td>
    </tr>
    <tr>
      <td>", cyclists_baseline(all_trips), "</td>
      <td>", data$bicycle, " (", data$olc_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", cyclists_scenario(all_trips), "</td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", data$scenario_bicycle_percentage , "%) </td>
    </tr>
    <tr>
      <td>", driver_change(all_trips), "</td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
           <td style= 'color:", data$font_colour , "' >", round(data[[data_filter(scenario, "sideath_heat")]], 3), " (&pound;" ,
           round(data[[data_filter(scenario, "sivalue_heat")]]), ")
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
