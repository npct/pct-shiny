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

tableStart <- '<table><tbody>'

tableEnd <- '</table></tbody>'

# SLC and SIC NOT IN 'olc' scenario
tableOLC <- knitr::kable(data.frame(
    Attribute =c("Total commuters:\t", "Cyclists (baseline):\t", "Drivers (baseline):\t"),
    Value =c("%s" ,"%s (%s%%)" , "%s (%s%%)"  )), format="html", col.names=NULL)

#scenarios table
tableCommon <- knitr::kable(data.frame(
    Attribute = c("Total commuters:\t", "Cyclists (baseline):\t", "Cyclists (scenario):\t",
  "Change in no. cyclists:\t", "Change in no. drivers:\t", "Change in no. deaths/year:\t",
  "Value of deaths avoided (pounds/year):\t ", "Change in CO2e (Tonnes/year):\t"),


  Value =    c("%s", "%s (%s%%)" , "%s (%s%%)",
                "%s", "%s", "%s",
            " %s", "%s T.")), format="html", col.names=NULL)


# Popup function for straight line data in html table
straightPopup <- function(data, scenario){

  if(scenario == 'olc') {
     scenario <- 'base'
     tableInterm <- sprintf(paste0(tableOLC, '<tr>
                     <td> Distance (km):               </td>
                     <td> %s  </td>
                     </tr>'),
                      data$All, data$Bicycle, round(100 * data$Bicycle / data$All),
                      data$Car_driver, round(100 * data$Car_driver / data$All),round(data$dist, 1))
                      }

  else {
    tableInterm <- sprintf(paste0(tableCommon, '<tr>
                    <td> Distance (km):               </td>
                    <td> %s  </td>
                    </tr>'),
                     data$All, data$Bicycle, round(100 * data$Bicycle / data$All),     #baseline & %
                     round(data[[dataFilter(scenario, "slc")]]),  round(100*data[[dataFilter(scenario, "slc")]]/ data$All),  # slc, slc%
                     round(data[[dataFilter(scenario, "sic")]]), round(data[[dataFilter(scenario, "sid")]]),  #changes sic, sid
                     round(data[[dataFilter(scenario, "sideath_heat")]],4), round(data[[dataFilter(scenario, "sivalue_heat")]],1),   #heat, heat value
                     round(data[[dataFilter(scenario, "sico2")]]/1000,1),  round(data$dist, 1)  )    #co2, distance
            }

    paste(tableStart,tableInterm,tableEnd)

}

routeTypeLabel <- NULL
routeTypeLabel[['fastest']] <- 'Direct'
routeTypeLabel[['quietest']] <- 'Quiet'

# Route popup function
routePopup <- function(data, scenario){

ifelse(("rqincr" %in% colnames(data@data)), routeType <-'quiet', routeType <-'fast')

    if (routeType=='quiet') {

        blob <- knitr::kable(data.frame(
            Attribute = c("Route Distance (km):\t", "Hilliness (av. gradient):\t","%% incr. distance vs fastest:\t"),
            Value =     c("%s "            , "%s %%" , "%s %%" )), format="html", col.names=NULL)

        tableInterm <- sprintf(paste0(blob),
                               round(data$length, 1),
                               round(100*data$av_incline, 1),
                               (round(100*data$rqincr, 1)-100)
        )  #both olc& scenario
    }

    if (routeType=='fast') {

        blob <- knitr::kable(data.frame(
            Attribute = c("Route Distance (km):\t", "Hilliness (av. gradient):\t"),
            Value =     c("%s  "            , "%s %%"  )), format="html", col.names=NULL)


        if(scenario == 'olc') {
            scenario <- 'base'

            tableInterm <- sprintf(paste0(tableOLC,blob),
                                   data$All, data$Bicycle, round(100 * data$Bicycle / data$All),
                                   data$Car_driver, round(100 * data$Car_driver/ data$All),
                                   round(data$dist_fast, 1), round(100*data$av_incline, 1) )

        } #olc

        else {

            tableInterm <- sprintf(paste0(tableCommon,blob),
                                   data$All, data$Bicycle, round(100 * data$Bicycle / data$All),      # olc, olc%
                                   round(data[[dataFilter(scenario, "slc")]]),round(100*data[[dataFilter(scenario, "slc")]]/ data$All),    # slc, slc%
                                   round(data[[dataFilter(scenario, "sic")]]), round(data[[dataFilter(scenario, "sid")]]),  #change: sic, sid
                                   round(data[[dataFilter(scenario, "sideath_heat")]],1), round(data[[dataFilter(scenario, "sivalue_heat")]],1),   #heat, heat values
                                   round(data[[dataFilter(scenario, "sico2")]]/1000,1), round(data$dist_fast, 1), round(100*data$av_incline, 1)   )

        }

    }  #fast route

    paste(tableStart,tableInterm,tableEnd)

}


# Network Route popup function
networkRoutePopup <- function(data, scenario){

############ % increase in distance vs. fastest route ONLY FOR QUIETEST ??

  if(scenario == 'olc') {

    tableInterm <- sprintf (paste0 (knitr::kable(data.frame(
      Attribute = c("Between-zone cyclists (baseline):\t", "Within-zone cyclists (baseline):\t"),
      Value =     c("%s"                                 , "up to    %s"  )), format="html", col.names=NULL)),
                    data$Bicycle, 'NA')

                    }

      else {

          tableInterm <- sprintf (paste0(knitr::kable(data.frame(
              Attribute = c("Between-zone cyclists (baseline):\t", "Within-zone cyclists (baseline):\t",
                            "Between-zone cyclists (scenario):\t",  "Within-zone cyclists (scenario):\t"),

             Value =     c("%s", "up to %s", "%s", "up to   %s" )), format="html", col.names=NULL)),
                        data$Bicycle,'NA', round(data[[dataFilter(scenario, "slc")]]),  'NA'  )


                    }


    paste(
      tableStart,
      tableInterm,
      tableEnd
    )

}

####ZONE = ANYWHERE INSIDE THE AREA but the centroid
zonePopup <- function(data, scenario, zone){

  zone_filter_name <- scenariosNames[zone]

  if(scenario == 'olc') {

    t1 <- knitr::kable(data.frame(
        Attribute = c("Zone:\t ", "Total commuters:\t ","Cyclists (baseline):\t","Drivers (baseline):\t" ),
        Value =     c("%s", "%s" , "%s (%s%%)"  , "%s (%s%%)")), format="html", col.names=NULL)


    tableInterm <-sprintf(t1,
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
                        "Change in no. drivers:\t" ),
          Value =     c("%s", " %s " , "%s (%s%%)"  , "%s (%s%%)", "%s", "%s")), format="html", col.names=NULL)


      tableInterm <-sprintf(t1,
                            data$geo_label,
                            data$All,
                            round(data[[dataFilter('olc', zone)]] ),
                            round(100*data[[dataFilter('olc', zone)]] /data$All),
                            round(data[[dataFilter(scenario, 'slc')]]),
                            round(100*data[[dataFilter(scenario, 'slc')]]/data$All),
                            round(data[[dataFilter(scenario, "sic")]]),
                            round(data[[dataFilter(scenario, "sid")]])       )

  }


  paste(tableStart,
        tableInterm,
        tableEnd)

}
