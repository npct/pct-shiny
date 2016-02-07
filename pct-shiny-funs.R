# Data Frame which contains the links of lines and their colours
lineAndColourDF <- data.frame(
  lineType = c("straight_line", "quieter_route", "faster_route", "route_network"),
  lineColour = c("maroon","turquoise","purple","blue")
)

getLineColour <- function(lineType){
  lineAndColourDF$lineColour[lineAndColourDF$lineType == lineType]
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
  "Cyclists at govenment target"      = "base_slc",
  "Increase at govenment target"      = "base_sic",
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
  "olc"      = "Cyclists (observed)",
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

tableStart <- '<table><tbody>'
tableEnd <- '</table></tbody>'
tableCommon <- '<tr>
<td> Total commutes </td>
<td> %s </td>
</tr>
<tr>
<td> Cycle commutes </td>
<td> %s (%s&#37;) </td>
</tr>
<tr>
<td> Cycle commutes (scenario) </td>
<td> %s </td>
</tr>
<tr>
<td> Increase in commutes &nbsp; </td>
<td> %s </td>
</tr>
'

# Popup function for straight line data in html table
straightPopup <- function(data, scenario){
  paste(
    tableStart,
    sprintf(paste0(tableCommon, '<tr>
                     <td> Distance (km) </td>
                     <td> %s </td>
                     </tr>'), data$All, data$Bicycle, round(100*data$clc), round(data[[dataFilter(scenario, "slc")]]), round(data[[dataFilter(scenario, "sic")]]), round(data$dist, 1)
    ),
    tableEnd
  )
}

routeTypeLabel <- NULL
routeTypeLabel[['fastest']] <- 'Direct'
routeTypeLabel[['quietest']] <- 'Quiet'

# Route popup function
routePopup <- function(data, scenario){
  paste(
    tableStart,
    sprintf(paste(tableCommon,'<tr>
                  <td>Route Distance</td>
                  <td>%s km</td>
                  </tr><tr>
                  <td>Av. Route Time</td>
                  <td>%s min</td>
                  </tr><tr>
                  </tr><tr>
                  <td>Hilliness (average gradient)</td>
                  <td>%s&#37;</td>
                  </tr><tr>
                  <td>Route Type</td>
                  <td>%s</td>
                  </tr>'),
            data$All, data$Bicycle, round(100*data$clc), round(data[[dataFilter(scenario, "slc")]]),
            round(data[[dataFilter(scenario, "sic")]]), round(data$length, 1),
            round(data$time / 60, 1), round(data$av_incline * 100, 1),
            routeTypeLabel[[data$plan[1]]]),
    tableEnd
  )
}

# Network Route popup function
networkRoutePopup <- function(data, scenario){
  paste(
    tableStart,
    sprintf(paste('<tr>
                  <td>Observed: </td>
                  <td>&nbsp;%s cyclists</td>
                  </tr><tr>
                  <td>Scenario: </td>
                  <td>&nbsp;%s cyclists</td>
                  </tr>'),
            data$Bicycle, round(data[[dataFilter(scenario, "slc")]])),
    tableEnd
    )
}

zonePopup <- function(data, scenario, zone){
  zone_filter_name <- scenariosNames[zone]
  paste(
    tableStart,
    sprintf("
            <tr>
            <td>Zone: </td>
            <td>%s</td>
            </tr><tr>
            <td>%s: </td>
            <td>%s </td>
            </tr>
            <tr>
            <td>&#37; who drive: &nbsp</td>
            <td>%s </td>
            </tr>
            <tr>
            <td>Hilliness: &nbsp</td>
            <td>%s&deg;</td>
            </tr>", data$MSOA11NM, zone_filter_name,
            round(data[[dataFilter(scenario, zone)]], 2 ),
            round(100*data$base_olcarusers,1), round(data$avslope, 2)),
    tableEnd)
}
