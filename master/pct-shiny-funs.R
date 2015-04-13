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
    </table>', data$All, data$Bicycle, round(data$base_slc, 1), round(data$base_sic, 1), round(data$dist, 1)
  )
}

routeTypeLabel <- NULL
routeTypeLabel[['fastest']] <- 'Direct'
routeTypeLabel[['quietest']] <- 'Quiet'

# Route popup function
routePopup <- function(data){
  sprintf('<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd><dt>Type of Route</dt><dd>%s</dd></dl>',
    round(data$length, 1), round(data$base_olc / data$All * 100, 2), routeTypeLabel[[data$plan[1]]])
}

zonePopup <- function(data, scenario, zone){
  # Return when olc is set, while zone attribute list is being updated for non-baseline scenarios (like ebike etc)
  if (scenario != "base" && zone == "olc"){
    return()
  }
  # Create a zone filter variable by concatenating the word 'base' with the zone input variable (for instance base_olc)
  zone_filter <- paste(scenario, zone, sep = "_")
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
      <td>%s </td>
    </tr><tr>
      <td>Hilliness:  </td>
      <td>%s&deg;</td>
    </tr>
  </tbody>
</table>", data$geo_code, zone_filter_name, round(data[[zone_filter]], 2 ), round(data$avslope, 2))
}

