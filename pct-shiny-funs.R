# Data Frame which contains the links of lines and their colours
lineAndColourDF <- data.frame(
  lineType = c("straight_line", "quieter_route", "faster_route", "route_network"),
  lineColour = c("maroon","turquoise","purple","blue")
)

subSup <- function(html){
  html <- sub('£', "&pound;", html)
  html <- sub('_\\{(.*)\\}', "<sub>\\1</sub>", html) # converts _{ab} into <sub>ab</sub>
  sub('^\\{(.*)\\}', "<sup>\\1</sup>", html) # converts ^{ab} into <sup>ab</sup>
}

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

  popupTable <- NULL

  if(scenario == 'olc') {

    # SLC and SIC NOT IN 'olc' scenario
    baseTable <- knitr::kable(data.frame(
      Attribute =c("Total commuters:\t",
                   "Cyclists (baseline):\t",
                   "Drivers (baseline):\t",
                   "Distance (km):\t"),
      Value =c("%s" ,"%s (%s%%)" , "%s (%s%%)", "%s"  )),
      format="html", col.names=NULL)

    scenario <- 'base'
    popupTable <- sprintf(baseTable,
                           data$All, data$Bicycle, round(100 * data$Bicycle / data$All),
                           data$Car_driver, round(100 * data$Car_driver / data$All),round(data$dist, 1))
  }

  else {


    #scenarios table
    scenarioTable <- knitr::kable(data.frame(
      Attribute = c("Total commuters:\t", "Cyclists (baseline):\t", "Cyclists (scenario):\t",
                    "Change in cyclists:\t", "Change in drivers:\t", "Change in deaths/yr:\t",
                    "Change in deaths (£/yr):", "Change in CO_{2}e (t/yr):\t",
                    "Distance (km):\t"),
      Value =    c("%s", "%s (%s%%)" , "%s (%s%%)",
                   "%s", "%s", "%s",
                   "  %s", "  %s ", "%s")),
      format="html", col.names=NULL)

    popupTable <- sprintf(subSup(scenarioTable),
                           data$All, data$Bicycle, round(100 * data$Bicycle / data$All),     #baseline & %
                           round(data[[dataFilter(scenario, "slc")]]),  round(100*data[[dataFilter(scenario, "slc")]]/ data$All),  # slc, slc%
                           round(data[[dataFilter(scenario, "sic")]]), round(data[[dataFilter(scenario, "sid")]]),  #changes sic, sid
                           round(data[[dataFilter(scenario, "sideath_heat")]],4), round(data[[dataFilter(scenario, "sivalue_heat")]],1),   #heat, heat value
                           round(data[[dataFilter(scenario, "sico2")]]/1000,1),  round(data$dist, 1)  )    #co2, distance
  }
}

routeTypeLabel <- NULL
routeTypeLabel[['fastest']] <- 'Direct'
routeTypeLabel[['quietest']] <- 'Quiet'

# Route popup function
routePopup <- function(data, scenario){

  popupTable<- NULL

  ifelse(("rqincr" %in% colnames(data@data)), routeType <-'quiet', routeType <-'fast')

  if (routeType=='quiet') {

    quietRouteTable <- knitr::kable(data.frame(
      Attribute = c("Route distance (km):\t",
                    "%% increase in distance vs. Fast route:  \t",
                    "Hilliness (av. gradient):\t"),
      Value =     c("%s ", "%s%%" , "%s%%" )),
      format="html", col.names=NULL)

    popupTable <- sprintf(quietRouteTable,
                           round(data$length, 1),
                          (round((100*data$rqincr-100), 1)),
                           round(100*data$av_incline, 1))
    #both olc& scenario
  }

  if (routeType=='fast') {


    # SLC and SIC NOT IN 'olc' scenario
    fastRouteTable <- knitr::kable(data.frame(
      Attribute =c("Total commuters:\t",
                   "Cyclists (baseline):\t",
                   "Drivers (baseline):\t",
                   "Route distance (km):\t",
                   "Hilliness (av. gradient):\t"),
      Value =c("%s" ,"%s (%s%%)" , "%s (%s%%)","%s ", "%s%%")), format="html", col.names=NULL)

    if(scenario == 'olc') {
      scenario <- 'base'

      popupTable <- sprintf(fastRouteTable,
                             data$All, data$Bicycle, round(100 * data$Bicycle / data$All),
                             data$Car_driver, round(100 * data$Car_driver/ data$All),
                             round(data$dist_fast, 1), round(100*data$av_incline, 1) )

    } #olc

    else {

      #scenarios table
      scenarioFastRouteTable <- knitr::kable(data.frame(
        Attribute = c("Total commuters:\t", "Cyclists (baseline):\t", "Cyclists (scenario):\t",
                      "Change in cyclists:\t", "Change in drivers:\t", "Change in deaths/yr:\t",
                      "Change in deaths (£/yr):", "Change in CO_{2}e (t/yr):\t",
                      "Route distance (km):\t",
                      "Hilliness (av. gradient):\t"),


        Value =    c("%s", "%s (%s%%)" , "%s (%s%%)",
                     "%s", "%s", "%s",
                     "  %s", "  %s ","%s ", "%s%%")), format="html", col.names=NULL)


      popupTable <- sprintf(subSup(scenarioFastRouteTable),
                             data$All, data$Bicycle, round(100 * data$Bicycle / data$All),      # olc, olc%
                             round(data[[dataFilter(scenario, "slc")]]),round(100*data[[dataFilter(scenario, "slc")]]/ data$All),    # slc, slc%
                             round(data[[dataFilter(scenario, "sic")]]), round(data[[dataFilter(scenario, "sid")]]),  #change: sic, sid
                             round(data[[dataFilter(scenario, "sideath_heat")]],1), round(data[[dataFilter(scenario, "sivalue_heat")]],1),   #heat, heat values
                             round(data[[dataFilter(scenario, "sico2")]]/1000,1), round(data$dist_fast, 1), round(100*data$av_incline, 1)   )

    }

  }  #fast route

  popupTable

}


# Network Route popup function
networkRoutePopup <- function(data, scenario){

  popupTable <- NULL

  ############ % increase in distance vs. fastest route ONLY FOR QUIETEST ??

  if(scenario == 'olc') {

    popupTable <- sprintf (paste0 (knitr::kable(data.frame(
      Attribute = c("Between-zone cyclists (baseline):\t",
                    "Within-zone cyclists (baseline):\t"),
      Value =     c("%s", "up to    %s"  )), format="html", col.names=NULL)),
      data$Bicycle, 'NA'   )

  }

  else {

    popupTable <- sprintf (paste0(knitr::kable(data.frame(
      Attribute = c("Between-zone cyclists (baseline):\t",
                    "Within-zone cyclists (baseline):\t",
                    "Between-zone cyclists (scenario):\t",
                    "Within-zone cyclists (scenario):\t"),

      Value =     c("%s", "%s", "%s", "%s" )), format="html", col.names=NULL)),
      data$Bicycle,'NA',round(data[[dataFilter(scenario, 'slc')]]), 'NA')
  }

  popupTable
}

####ZONE = ANYWHERE INSIDE THE AREA but the centroid
zonePopup <- function(data, scenario, zone){
  popupTable <- NULL

  zone_filter_name <- scenariosNames[zone]

  if(scenario == 'olc') {

    t1 <- knitr::kable(data.frame(
      Attribute = c("Zone:\t ", "Total commuters:\t ","Cyclists (baseline):\t","Drivers (baseline):\t" ),
      Value =     c("%s", "%s" , "%s (%s%%)"  , "%s (%s%%)")), format="html", col.names=NULL)


    popupTable <-sprintf(t1,
                          data$geo_label, data$All,round(data[[dataFilter('olc', zone)]] ),round(100*data[[dataFilter('olc', zone)]] /data$All),
                          data$Car, round(100* data$Car/data$All) )


  }

  else {

    t1 <- knitr::kable(data.frame(
      Attribute = c("Zone:\t",
                    "Total commuters:\t",
                    "Cyclists (baseline):\t",
                    "Cyclists (scenario):\t",
                    "Change in no. cyclists:\t",
                    "Change in no. drivers:\t",
                    "Change in deaths/yr:\t",
                    "Change in deaths (£/yr):\t",
                    "Change in CO_{2}e (t/yr):\t"),
      Value =     c("%s", " %s " , "%s (%s%%)"  , "%s (%s%%)", "%s", "%s", "%s", "%s", "%s")), format="html", col.names=NULL)


    popupTable <-sprintf(subSup(t1),
                          data$geo_label,
                          data$All,
                          round(data[[dataFilter('olc', zone)]] ),
                          round(100*data[[dataFilter('olc', zone)]] /data$All),
                          round(data[[dataFilter(scenario, 'slc')]]),
                          round(100*data[[dataFilter(scenario, 'slc')]]/data$All),
                          round(data[[dataFilter(scenario, "sic")]]),
                          round(data[[dataFilter(scenario, "sid")]]),
                          round(data[[dataFilter(scenario, "sideath_heat")]],4),
                          round(data[[dataFilter(scenario, "sivalue_heat")]],4),
                          round(data[[dataFilter(scenario, "sico2")]]/1000,1))


  }

  popupTable

}


####CENTROID
centroidPopup <- function(data, scenario, zone){
  popupTable <- NULL

  zone_filter_name <- scenariosNames[zone]

  if(scenario == 'olc') {

    t1 <- knitr::kable(data.frame(
      Attribute = c("Zone:\t ", "Total commuters:\t ","Cyclists (baseline):\t","Drivers (baseline):\t" ),
      Value =     c("%s", "%s" , "%s (%s%%)"  , "%s (%s%%)")), format="html", col.names=NULL)


    popupTable <-sprintf(t1,
                         data$geo_code, data$all,data$bicycle, round(100*data$bicycle / data$all),
                         data$Car, round(100* data$Car/data$all) )


  }

  else {

    t1 <- knitr::kable(data.frame(
      Attribute = c("Zone:\t",
                    "Total commuters:\t",
                    "Cyclists (baseline):\t",
                    "Cyclists (scenario):\t",
                    "Change in no. cyclists:\t",
                    "Change in no. drivers:\t",
                    "Change in deaths/yr:\t",
                    "Change in deaths (£/yr):\t",
                    "Change in CO_{2}e (t/yr):\t"),
      Value =     c("%s", " %s " , "%s (%s%%)"  , "%s (%s%%)", "%s", "   %s", "%s", "%s", "%s")), format="html", col.names=NULL)


    popupTable <-sprintf(subSup(t1),
                         data$geo_code,
                         data$all,
                         round(data$bicycle, 2),
                         round(100*data$bicycle / data$all),
                         round(data[[dataFilter(scenario, 'slc')]]),
                         round(100*data[[dataFilter(scenario, 'slc')]]/data$all),
                         round(data[[dataFilter(scenario, "sic")]]),
                         round(data[[dataFilter(scenario, "sid")]]),
                         format(round(data[[dataFilter(scenario, "sideath_heat")]],4), scientific = F),
                         round(data[[dataFilter(scenario, "sivalue_heat")]],1),
                         round(data[[dataFilter(scenario, "sico2")]]/1000,1))


  }

  popupTable

}
