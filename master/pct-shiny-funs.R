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

attrWithScenario <- function(attr, scenario){
  paste(scenario, attr, sep = "_")
}

# Popup function for straight line data in html table
straightPopup <- function(data){
  sprintf('
    <table>
    <tbody>
    <tr>
    <td> Total n. commutes </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> N. cycle </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> PLC (model scenario) </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> ECP (PCL - N. Cycle) </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> Euclidean Distance (km) &nbsp; </td>
    <td> %s </td>
    </tr>
    </tbody>
    </table>', data$All, data$Bicycle, round(data$base_plc, 1), round(data$base_ecp, 1), round(data$dist, 1)
  )
}

# Route popup function
routePopup <- function(data){
  sprintf('<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd><dt>Type of Route</dt><dd>%s</dd></dl>',
    round(data$length, 1), round(data$base_clc / data$All * 100, 2), data$plan[1])
}

zonePopup <- function(data, zone){
  # Create a zone filter variable by concatenating the word 'base' with the zone input variable (for instance base_clc)
  zone_filter <- paste("base", zone, sep = "_")
  # Create a new name for the zone variable by making an upper case title out of it
  zone_filter_name <- toupper(zone)

   sprintf("
<table>
  <tbody>
    <tr>
      <td>Zone: </td>
      <td>%s</td>
    </tr><tr>
      <td>%s: </td>
      <td>%s%% </td>
    </tr><tr>
      <td>Hilliness:  </td>
      <td>%s&deg;</td>
    </tr>
  </tbody>
</table>", data$geo_code, zone_filter_name, round(data[[zone_filter]] * 100, 2 ), round(data$avslope, 2))
}

