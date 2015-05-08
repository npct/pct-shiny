# Normalise the data ready for plotting
normalise <- function(values, min = 0, max = 1){
  min + max * (values - min(values))/diff(range(values))
}

# Generate a series of colours based on the input range
getColourRamp <- function(colors, values) {
  v <- normalise(values)
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

dataFilter <- function(scenario, attr){
  paste(scenario, attr, sep = "_")
}
tableStart <- '<table><tbody>'
tableEnd <- '</table></tbody>'
tableCommon <- '<tr>
    <td> Total commutes </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> Observed (OLC) </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> Scenario (SLC) </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> Increase (SLC - OLC) &nbsp; </td>
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
    </tr>'), data$All, data$Bicycle, round(data[[dataFilter(scenario, "slc")]]), round(data[[dataFilter(scenario, "sic")]]), round(data$dist, 1)
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
                <td>Route Type</td>
                <td>%s</td>
                </tr>'),
            data$All, data$Bicycle, round(data[[dataFilter(scenario, "slc")]]), round(data[[dataFilter(scenario, "sic")]]), round(data$length, 1), round(data$time / 60, 1), routeTypeLabel[[data$plan[1]]]),
    tableEnd
  )
}

zonePopup <- function(data, scenario, zone){
  zone_filter_name <- toupper(zone)
  paste(
    tableStart,
    sprintf("
    <tr>
      <td>Zone: </td>
      <td>%s</td>
    </tr><tr>
      <td>%s: </td>
      <td>%s </td>
    </tr><tr>
      <td>Hilliness: &nbsp</td>
      <td>%s&deg;</td>
    </tr>", data$geo_code, zone_filter_name, round(data[[dataFilter(scenario, zone)]], 2 ), round(data$avslope, 2)),
    tableEnd)
}

