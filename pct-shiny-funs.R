# Data Frame which contains the full and short forms of scenarios
scNameDF <- data.frame(
  scFName = c("Census 2011 Cycling", "Government Target", "Gender equality", "Go Dutch", "Ebikes"),
  scSName = c("olc","govtarget","gendereq","dutch", "ebike")
)

# Data Frame which contains the links of lines and their colours
lineAndColourDF <- data.frame(
  lineType = c("straight_line", "quieter_route", "faster_route", "route_network", "centres"),
  lineColour = c("maroon","turquoise","purple","blue", "maroon")
)

getLineColour <- function(lineType){
  lineAndColourDF$lineColour[lineAndColourDF$lineType == lineType]
}

getScenarioName <- function(scName){
  scNameDF$scFName[scNameDF$scSName == scName]
}

numericLineColNames <- c(
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

lineColNames <- c(
  "Start and end zones"               = "id",
  "All commutes"                      = "All",
  "Rail"                              = "Rail",
  "Bus"                               = "Bus",
  "Car"                               = "Car_driver",
  "Bicycle"                           = "Bicycle",
  numericLineColNames
)

numericZoneColNames <- c(
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

zoneColNames <- c(
  "Geo code"                          = "geo_code",
  "Geo label"                         = "geo_label",
  numericZoneColNames
)

scenariosNames <- c(
  "olc"      = "Cyclists (recorded)",
  "slc"      = "Cyclists (scenario)"
)

# Normalise the data ready for plotting
normalise <- function(values, min = 0, max = 1){
  min + max * (values - min(values))/diff(range(values))
}

# Generate a series of colours based on the input range
getColourRamp <- function(colors, values) {
  v <- normalise(values)
  x <- colorRamp(colors)(v)
  x[is.na(x)] <- 1
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

dataFilter <- function(scenario, type){
  ifelse(scenario == "olc", "Bicycle", paste(scenario, type, sep = "_"))
}

# Popup function for straight line data in html table
straightPopup <- function(data, scenario){

  # Get ID which contains both source and target destination
  # Split the list into two parts
  # id[1] contains source and id[2] contains target
  id <- (unlist(strsplit(data$id, " ")))

  if(scenario == 'olc') {
    paste0("
<table class = 'htab'>
 	<th> Census 2011 cycling (baseline) </th>
  <tbody>
    <tr>
      <td>", id[1] , " - ", id[2], "</td>
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
  <th> Scenario: ", getScenarioName(scenario), "</th>
  <tbody>
    <tr>
      <td>", id[1] , " - ", id[2], "</td>
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
      <td>", round(data[[dataFilter(scenario, "slc")]]), " (", round(100*data[[dataFilter(scenario, "slc")]] / data$All), "%) </td>
    </tr>
    <tr>
      <td> Change in drivers: </td>
      <td>", round(data[[dataFilter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td>", round(data[[dataFilter(scenario, "sideath_heat")]], 3), " (&pound;" ,
            round(data[[dataFilter(scenario, "sivalue_heat")]]), ")
      </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[dataFilter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
    <tr>
      <td> Distance (km):</td>
      <td>", round(data$dist, 1), "</td>
    </tr>
  </tbody>
</table>")
  }
}

routeTypeLabel <- NULL
routeTypeLabel[['fastest']] <- 'Direct'
routeTypeLabel[['quietest']] <- 'Quiet'

# Route popup function
routePopup <- function(data, scenario){
  # Get ID which contains both source and target destination
  # Split the list into two parts
  # id[1] contains source and id[2] contains target
  id <- (unlist(strsplit(data$id, " ")))

  ifelse(("rqincr" %in% colnames(data@data)), routeType <-'quiet', routeType <-'fast')

  if (routeType=='fast') {
    if(scenario == 'olc') {
      paste0("
<table class = 'htab'>
  <th> Census 2011 cycling (baseline) </th>
  <tbody>
    <tr>
      <td>", id[1] , " - ", id[2], "</td>
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
      <td> Hilliness (av. gradient): &nbsp; </td>
      <td>", round(100*data$av_incline, 1), "&#176;</td>
    </tr>
  </tbody>
</table>
")

    } else {

      paste0("
<table class = 'htab'>
  <th>  Scenario: ", getScenarioName(scenario), " </th>
  <tbody>
    <tr>
        <td>", id[1] , " - ", id[2], "</td>
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
      <td>", round(data[[dataFilter(scenario, "slc")]]), " (", round(100*data[[dataFilter(scenario, "slc")]] / data$All), "%) </td>
    </tr>
    <tr>
      <td> Change in drivers: </td>
      <td>", round(data[[dataFilter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td>", round(data[[dataFilter(scenario, "sideath_heat")]], 3), " (&pound;" ,
          round(data[[dataFilter(scenario, "sivalue_heat")]]), ")
    </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[dataFilter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
    <tr>
      <td> Route distance (km):</td>
      <td>", round(data$dist_fast, 1), "</td>
    </tr>
    <tr>
      <td> Hilliness (av. gradient):</td>
      <td>", round(100*data$av_incline, 1), "&#176;</td>
    </tr>
  </tbody>
</table>
")
    }
  } else {
    # Create a local variable to distinguish baseline with scenarios
    quietRouteLabel <- paste("Scenario: ", getScenarioName(scenario))
    if(scenario == 'olc')
      quietRouteLabel <- "Census 2011 cycling (baseline)"


    paste0("
<table class = 'htab'>
  <th> ", quietRouteLabel, " </th>
  <tbody>
    <tr>
      <td> Route distance (km): </td>
      <td>", round(data$length, 1), "</td>
    </tr>
    <tr>
    <td> % increase in distance vs. Fast route: &nbsp; </td>
      <td>", round((100*data$rqincr-100), 1), "%</td>
    </tr>
    <tr>
      <td> Hilliness (av. gradient): &nbsp; </td>
      <td>", round(100*data$av_incline, 1), "&#176;</td>
    </tr>
  </tbody>
</table>
")  #both olc& scenario
  }
}


# Network Route popup function
networkRoutePopup <- function(data, scenario){
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
  <th>  Scenario: ", getScenarioName(scenario), "</th>
  <tbody>
    <tr>
      <td> Between-zone cyclists (baseline): &nbsp; </td>
      <td>", data$Bicycle, "</td>
    </tr>
    <tr>
      <td> Between-zone cyclists (scenario): &nbsp; </td>
      <td>", round(data[[dataFilter(scenario, 'slc')]]), "</td>
    </tr>
  </tbody>
</table>
")
  }
}

####ZONE = ANYWHERE INSIDE THE AREA but the centroid
zonePopup <- function(data, scenario, zone){
  zone_filter_name <- scenariosNames[zone]

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
      <td>",  round(data[[dataFilter('olc', zone)]] ), " (", round(100*data[[dataFilter('olc', zone)]] /data$All), "%) </td>
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
  <th>  Scenario: ", getScenarioName(scenario), " </th>
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
      <td>",  round(data[[dataFilter('olc', zone)]] ), " (", round(100*data[[dataFilter('olc', zone)]] /data$All), "%) </td>
    </tr>
    <tr>
      <td> Cyclists (scenario): </td>
      <td>", round(data[[dataFilter(scenario, 'slc')]]), " (", round(100*data[[dataFilter(scenario, "slc")]] / data$All), "%) </td>
    </tr>
    <tr>
      <td> Change in cyclists: </td>
      <td>", (round(round(data[[dataFilter(scenario, 'slc')]]) - round(data[[dataFilter('olc', zone)]]))) , "</td>
    </tr>
    <tr>
      <td> Change in drivers: </td>
      <td>", round(data[[dataFilter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td>", round(data[[dataFilter(scenario, "sideath_heat")]], 3), " (&pound;",
            round(data[[dataFilter(scenario, "sivalue_heat")]]), ")</td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[dataFilter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
  </tbody>
</table>")
  }
}

####CENTROID
centroidPopup <- function(data, scenario, zone){
  zone_filter_name <- scenariosNames[zone]

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
      <th> Scenario: ", getScenarioName(scenario), "</th>
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
      <td>", round(data[[dataFilter(scenario, "slc")]]), " (", round(100*data[[dataFilter(scenario, "slc")]] / data$All), "%) </td>
    </tr>
    <tr>
      <td> Change in drivers: </td>
      <td>", round(data[[dataFilter(scenario, "sid")]]), "</td>
    </tr>
    <tr>
      <td> Change in deaths/yr: &nbsp; </td>
      <td>", round(data[[dataFilter(scenario, "sideath_heat")]], 3), " (&pound;",
           round(data[[dataFilter(scenario, "sivalue_heat")]]), ") </td>
    </tr>
    <tr>
      <td> Change in CO<sub>2</sub>e (t/yr): &nbsp;</td>
      <td>", round(data[[dataFilter(scenario, "sico2")]] / 1000,1), "</td>
    </tr>
  </tbody>
</table>")

  }
}
