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
    <td> N. Cycle </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> CLC (%% who cycle) </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> PLC (%%) </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> ECP (N.) </td>
    <td> %s </td>
    </tr>
    <tr>
    <td> Euclidean Distance (km) &nbsp; </td>
    <td> %s </td>
    </tr>
    </tbody>
    </table>', data$All.x, data$Bicycle.x, round(data$base_clc * 100, 1), round(data$base_plc * 100, 1), round(data$base_ecp, 1), round(data$dist, 1)
  )
}

# Route popup function
routePopup <- function(data){
  sprintf('<dl><dt>Distance </dt><dd>%s km</dd><dt>Journeys by bike</dt><dd>%s%%</dd><dt>Type of Route</dt><dd>%s</dd></dl>',
    round(data$length * 1000000, 1), round(data$clc * 10, 2), data$plan[1])
}

zonePopup <- function(data){
   sprintf("
<table>
  <tbody>
    <tr>
      <td>Zone: </td>
      <td>%s</td>
    </tr><tr>
      <td>CLC: </td>
      <td>%s%% </td>
    </tr><tr>
      <td>Hilliness:</td>
      <td>%s&deg;</td>
    </tr>
  </tbody>
</table>", data$geo_code, round(data$base_clc * 100, ), round(data$avslope, 2))
}

