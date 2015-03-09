library(shiny)
library(leaflet)
fluidPage(
  titlePanel("FixMyPath"),
  sidebarLayout(
    sidebarPanel("Controls", width = 3
        ,selectInput("scenario", "Scenario:",
          c("Baseline" =           "ba"
            ,"Gender equality" =   "ge"
            ,"Go Dutch" =          "gd"
            ,"Electric bicycles" = "eb")
          , selected = "ba")
                 ,selectInput("line_type", "Representation of cycling desire lines",
                              c("None" = "none"
                                ,"Straight Lines" = "straight"
                                ,"Fastest & Quietest Routes" = "route")
                              , selected = "none")
                 ,conditionalPanel(
                   condition = "input.line_type != 'none'"
                   ,selectInput("line_attr", "Line attribute to display:",
                                c("Current Level Cycling (CLC)" =       "clc"
                                  ,"Potential Level of Cycling (PLC)" = "plc"
                                  ,"Extra Cycling Potential (ECP)" =    "ecp")
                                , selected = "current")
                   ,sliderInput("nos_lines"
                                , label = "Number of lines (n) negative to show the bottom n, postive the top"
                                , min = -10, max = 10, value = 10)
                 )
                 ,selectInput("zone_type", "Type of zones:",
                              c("None" = "none"
                                ,"Medium Super Output Area (MSOA)" = "msoa"
                                ,"Super Output Area (SOA)" = "routes")
                              , selected = "msoa")
                 ,conditionalPanel(
                   condition = "input.zone_type != 'none'"
                   ,selectInput("zone_attr", "Zone attribute:",
                                c("Current Level of Cycling (CLC)" =     "clc"
                                  ,"Potential Level of Cycling (PLC)" =  "plc"
                                  ,"Extra Cycling Potential (ECP)" =     "ecp"
                                  ,"Average distance to work" =          "dis")
                                , selected = "current")
                 )
                 ,selectInput("feature", "Additional Features"
                              , choices =  c("None" = "none"
                                             ,"Cycle Parking" = "cycleparking"
                                             ,"Collisions involving cyclists" = "collisions"
                                             ,"Bike Shops" = "bikeshops")
                 )
    ),
    mainPanel("Welcome to",
              a(href = "http://geo8.webarch.net:3838/pct-shiny/master/", "fixMyPath"),
              p("fixMyPath is a shiny app written to facilitate better bicycle path planning in Leeds, the UK and eventually the world. If you'd like to get involved, please check-out, test and contribute-to the fully reproducible code..."),
              a(href = "https://github.com/npct/pct-shiny/tree/master/master", strong("HERE!"), target="_blank"),
              leafletOutput('map', height = 600)),
  ))
