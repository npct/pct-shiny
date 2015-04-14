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

# Popup function for straight line data in html table
straightPopup <- function(data, scenario){
  sprintf('
    <table>
    <tbody>
    <tr>
    <td> Total n. commutes </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> OLC </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> SLC </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> SIC (SLC - CLC) </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> Euclidean Distance (km) &nbsp; </td>
    <td> %s </td>
    </tr>
    </tbody>
    </table>', data$All, data$Bicycle, round(data[[dataFilter(scenario, "slc")]]), round(data[[dataFilter(scenario, "sic")]]), round(data$dist, 1)
  )
}

routeTypeLabel <- NULL
routeTypeLabel[['fastest']] <- 'Direct'
routeTypeLabel[['quietest']] <- 'Quiet'

# Route popup function
routePopup <- function(data, scenario){
  sprintf('<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd><dt>Type of Route</dt><dd>%s</dd></dl>',
    round(data$length, 1), round(data$base_olc / data$All * 100, 2), routeTypeLabel[[data$plan[1]]])
}

zonePopup <- function(data, scenario, zone){
  zone_filter_name <- toupper(zone)
   sprintf("
<table>
  <tbody>
    <tr>
      <td>Zone: </td>
      <td>%s</td>
    </tr><tr>
      <td>%s: </td>
      <td>%s </td>
    </tr><tr>
      <td>Hilliness:  </td>
      <td>%s&deg;</td>
    </tr>
  </tbody>
</table>", data$geo_code, zone_filter_name, round(data[[dataFilter(scenario, zone)]], 2 ), round(data$avslope, 2))
}

