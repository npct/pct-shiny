
sc_name_df <- data.frame(
  sc_f_name = c("Census 2011 Cycling", "Government Target", "Gender equality", "Go Dutch", "Ebikes"),
  sc_s_name = c("olc","govtarget","gendereq","dutch", "ebike")
)

# Data Frame which contains the links of lines and their colours
line_and_colour_df <- data.frame(
  line_type = c("straight_line", "quieter_route", "faster_route", "route_network", "centres"),
  line_colour = c("maroon","turquoise","purple","blue", "maroon")
)

get_line_colour <- function(line_type){
  line_and_colour_df$line_colour[line_and_colour_df$line_type == line_type]
}

make_download_link <- function(file, download_name, region){
  formats <- c('Rds', 'geojson', 'csv')
  base_url = paste("https://cdn.rawgit.com/npct/pct-data", data_sha, region, sep = "/")
  all_links <- ""
  for(i in 1:length(formats)){
    format <- formats[i]
    all_links <- paste(
      all_links, a(format,
                   href= paste0(base_url, "/", file, ".", format), target='_blank', download = paste0(download_name, ".", format))
    )
  }
  all_links
}

get_scenario_name <- function(sc_name){
  sc_name_df$sc_f_name[sc_name_df$sc_s_name == sc_name]
}

numeric_line_col_names <- c(
  "Average Slope"                     = "avslope",
  "Straight Line Distance (km)"       = "dist",
  "Cirquity"                          = "cirquity",
  "Fastest Route Distance (km)"       = "dist_fast",
  "Quietest Route Distance (km)"      = "dist_quiet",
  "Cycling Observed (%)"              = "clc",
  "Cyclists at Government target"     = "govtarget_slc",
  "Increase at Government target"     = "govtarget_sic",
  "Cyclists at gender equality"       = "gendereq_slc",
  "Increase at gender equality"       = "gendereq_sic",
  "Cyclists at Dutch levels"          = "dutch_slc",
  "Increase at Dutch levels"          = "dutch_sic",
  "Cyclists at Ebikes scenario"       = "ebike_slc",
  "Increase at Ebikes scenario"       = "ebike_sic"
)

line_col_names <- c(
  "Start and end zones"               = "id",
  "All commutes"                      = "All",
  "Rail"                              = "Rail",
  "Bus"                               = "Bus",
  "Car"                               = "Car_driver",
  "Bicycle"                           = "Bicycle",
  numeric_line_col_names
)

numeric_zone_col_names <- c(
  "Average slope"                     = "avslope",
  "Cyclists in census"                = "Bicycle",
  "Cyclists at govenment target"      = "govtarget_slc",
  "Increase at govenment target"      = "govtarget_sic",
  "Cyclists at gender equality"       = "gendereq_slc",
  "Increase at gender equality"       = "gendereq_sic",
  "Cyclists at Dutch levels"          = "dutch_slc",
  "Increase at Dutch levels"          = "dutch_sic",
  "Cyclists at Ebike scenario"        = "ebike_slc",
  "Increase at Ebike scenario"        = "ebike_sic"
)

zone_col_names <- c(
  "Geo code"                          = "geo_code",
  "Geo label"                         = "geo_label",
  numeric_zone_col_names
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

# Generate a series of colours based on the input range
get_colour_ramp <- function(colors, values) {
  v <- normalise(values)
  x <- colorRamp(colors)(v)
  x[is.na(x)] <- 1
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

data_filter <- function(scenario, type){
  ifelse(scenario == "olc", "Bicycle", paste(scenario, type, sep = "_"))
}

# Popup function for straight line data in html table
straight_popup <- function(data, scenario){

  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
 	<th> Census 2011 cycling (baseline) </th>
  <tbody>
    <tr>
      <td>", data$geo_label_o , " - ", data$geo_label_d, "</td>
    </tr>
    <tr>
      <td> Total commuters: </td>
      <td>", data$All, "</td>
    </tr>
    <tr>
      <td> Cyclists (baseline): &nbsp; </td>
      <td>",  data$Bicycle, " (", round(100 * data$Bicycle / data$All), "%) </td>
    </tr>
    <tr>
      <td> Drivers (baseline): &nbsp; </td>
      <td>", data$Car_driver, " (", round(100 * data$Car_driver / data$All), "%) </td>
    </tr>
    <tr>
      <td> Distance (km): </td>
      <td>", round(data$dist, 1), "</td>
    </tr>
  </tbody>
</table>
")

  } else {

    # scenarios table
    # Please align HTML!
    paste0("
<table class = 'htab'>
  <th> Scenario: ", get_scenario_name(scenario), "</th>
  <tbody>
    <tr>
      <td>", data$geo_label_o , " - ", data$geo_label_d, "</td>
    </tr>
    <tr>
      <td> Total commuters: </td>
      <td>", data$All, "</td>
    </tr>
    <tr>
      <td> Cyclists (baseline): </td>
      <td>", data$Bicycle, " (", round(100 * data$Bicycle / data$All), "%) </td>
    </tr>
    <tr>
      <td> Cyclists (scenario): </td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", round(100*data[[data_filter(scenario, "slc")]] / data$All), "%) </td>
    </tr>
    <tr>
      <td> Change in drivers: </td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td>", round(data[[data_filter(scenario, "sideath_heat")]], 3), " (&pound;" ,
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

# Route popup function
route_popup <- function(data, scenario){

  ifelse(("rqincr" %in% colnames(data@data)), route_type <-'quiet', route_type <-'fast')

  if (route_type=='fast') {
    if(scenario == 'olc') {
      paste0("
<table class = 'htab'>
  <th> Census 2011 cycling (baseline) </th>
  <tbody>
    <tr>
      <td>", data$geo_label_o , " - ", data$geo_label_d, "</td>
    </tr>
    <tr>
      <td> Total commuters: </td>
      <td>", data$All, "</td>
    </tr>
    <tr>
      <td> Cyclists (baseline): &nbsp; </td>
      <td>",  data$Bicycle, " (", round(100 * data$Bicycle / data$All), "%) </td>
    </tr>
    <tr>
      <td> Drivers (baseline): &nbsp; </td>
      <td>", data$Car_driver, " (", round(100 * data$Car_driver / data$All), "%) </td>
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

      paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario), " </th>
  <tbody>
    <tr>
        <td>", data$geo_label_o , " - ", data$geo_label_d, "</td>
    </tr>
    <tr>
      <td> Total commuters: </td>
      <td>", data$All, "</td>
    </tr>
    <tr>
      <td> Cyclists (baseline): </td>
      <td>", data$Bicycle, " (", round(100 * data$Bicycle / data$All), "%) </td>
    </tr>
    <tr>
      <td> Cyclists (scenario): </td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", round(100*data[[data_filter(scenario, "slc")]] / data$All), "%) </td>
    </tr>
    <tr>
      <td> Change in drivers: </td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td>", round(data[[data_filter(scenario, "sideath_heat")]], 3), " (&pound;" ,
          round(data[[data_filter(scenario, "sivalue_heat")]]), ")
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
    quiet_route_label <- paste("Scenario: ", get_scenario_name(scenario))
    if(scenario == 'olc')
      quiet_route_label <- "Census 2011 cycling (baseline)"


    paste0("
<table class = 'htab'>
  <th> ", quiet_route_label, " </th>
  <tbody>
    <tr>
        <td>", data$geo_label_o , " - ", data$geo_label_d, "</td>
    </tr>
    <tr>
      <td> Route distance (km): </td>
      <td>", round(data$length, 1), "</td>
    </tr>
    <tr>
    <td> Distance compared w. fastest route (%): &nbsp; </td>
      <td>", round((100*data$rqincr-100), 1), "</td>
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
network_route_popup <- function(data, scenario){
  ############ % increase in distance vs. fastest route ONLY FOR QUIETEST ??

  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <th> Census 2011 cycling (baseline)</th>
  <tbody>
    <tr>
      <td> Between-zone cyclists (baseline): &nbsp; </td>
      <td>", data$Bicycle, "</td>
    </tr>
  </tbody>
</table>
")

  } else {

    paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario), "</th>
  <tbody>
    <tr>
      <td> Between-zone cyclists (baseline): &nbsp; </td>
      <td>", data$Bicycle, "</td>
    </tr>
    <tr>
      <td> Between-zone cyclists (scenario): &nbsp; </td>
      <td>", round(data[[data_filter(scenario, 'slc')]]), "</td>
    </tr>
  </tbody>
</table>
")
  }
}

####ZONE = ANYWHERE INSIDE THE AREA but the centroid
zone_popup <- function(data, scenario, zone){
  zone_filter_name <- scenarios_names[zone]

  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <th> Census 2011 cycling (baseline)</th>
  <tbody>
    <tr>
      <td> Zone: </td>
      <td>", data$geo_label, "</td>
    </tr>
    <tr>
      <td> Total commuters: </td>
      <td>", data$All, "</td>
    </tr>
    <tr>
      <td> Cyclists (baseline): &nbsp; </td>
      <td>",  round(data[[data_filter('olc', zone)]] ), " (", round(100*data[[data_filter('olc', zone)]] /data$All), "%) </td>
    </tr>
    <tr>
      <td> Drivers (baseline): &nbsp; </td>
      <td>", data$Car, " (", round(100* data$Car/data$All), "%) </td>
    </tr>
  </tbody>
</table>")

  } else {

    paste0("
<table class = 'htab'>
  <th>  Scenario: ", get_scenario_name(scenario), " </th>
  <tbody>
    <tr>
      <td> Zone: </td>
      <td>", data$geo_label, "</td>
    </tr>
    <tr>
      <td> Total commuters: </td>
      <td>", data$All, "</td>
    </tr>
    <tr>
      <td> Cyclists (baseline): &nbsp; </td>
      <td>",  round(data[[data_filter('olc', zone)]] ), " (", round(100*data[[data_filter('olc', zone)]] /data$All), "%) </td>
    </tr>
    <tr>
      <td> Cyclists (scenario): </td>
      <td>", round(data[[data_filter(scenario, 'slc')]]), " (", round(100*data[[data_filter(scenario, "slc")]] / data$All), "%) </td>
    </tr>
    <tr>
      <td> Change in cyclists: </td>
      <td>", (round(round(data[[data_filter(scenario, 'slc')]]) - round(data[[data_filter('olc', zone)]]))) , "</td>
    </tr>
    <tr>
      <td> Change in drivers: </td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td>", round(data[[data_filter(scenario, "sideath_heat")]], 3), " (&pound;",
            round(data[[data_filter(scenario, "sivalue_heat")]]), ")</td>
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
centroid_popup <- function(data, scenario, zone){
  zone_filter_name <- scenarios_names[zone]

  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th> Within zone flows </th>
    </tr>
    <tr>
      <th> Census 2011 cycling (baseline) </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td> Zone: </td>
      <td>", data$geo_label, "</td>
    </tr>
    <tr>
      <td> Total commuters: </td>
      <td>", data$All, "</td>
    </tr>
    <tr>
      <td> Cyclists (baseline): &nbsp; </td>
      <td>",  data$Bicycle, " (", round(100 * data$Bicycle / data$All), "%) </td>
    </tr>
    <tr>
      <td> Drivers (baseline): &nbsp; </td>
      <td>", data$Car_driver, " (", round(100 * data$Car_driver / data$All), "%) </td>
    </tr>
  </tbody>
</table>
")

  } else {

    paste0("
<table class = 'htab'>
  <thead>
    <tr>
      <th>Within zone flows</th>
    </tr>
    <tr>
      <th> Scenario: ", get_scenario_name(scenario), "</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td> Zone: </td>
      <td>", data$geo_label, "</td>
    </tr>
    <tr>
      <td> Total commuters: </td>
      <td>", data$All, "</td>
    </tr>
    <tr>
      <td> Cyclists (baseline): </td>
      <td>", data$Bicycle, " (", round(100 * data$Bicycle / data$All), "%) </td>
    </tr>
    <tr>
      <td> Cyclists (scenario): </td>
      <td>", round(data[[data_filter(scenario, "slc")]]), " (", round(100*data[[data_filter(scenario, "slc")]] / data$All), "%) </td>
    </tr>
    <tr>
      <td> Change in drivers: </td>
      <td>", round(data[[data_filter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td>", round(data[[data_filter(scenario, "sideath_heat")]], 3), " (&pound;",
           round(data[[data_filter(scenario, "sivalue_heat")]]), ") </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[data_filter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
  </tbody>
</table>")

  }
}
