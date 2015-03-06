library(shiny)
library(leaflet)
fluidPage(
  titlePanel("FixMyPath"),
  sidebarLayout(
    sidebarPanel("Controls", width = 3
                 ,selectInput("line_type", "Type of lines:",
                              c("None" = "none"
                                ,"Stright Lines" = "straight"
                                ,"Fastest & Quietest Routes" = "route")
                              , selected = "none")
                 ,conditionalPanel(
                   condition = "input.line_type != 'none'"
                   ,selectInput("line_scenarios", "Scenario of line:",
                                c("Current Flow" = "clc"
                                  ,"Extra Cycling Potential (Baseline)" = "ecp"
                                  ,"Potential Level of Cycling (Baseline)" = "plc"
                                  ,"Extra Cycling Potential (Dutch)" = "plc_dutch"
                                  ,"Potential Level of Cycling (Dutch)" = "plc_dutch")
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
                   ,selectInput("zone_scenarios", "Scenario of zones:",
                                c("Current Rate" = "current"
                                  ,"Extra Cycling Potential (Baseline)" = "ecp_base"
                                  ,"Potential Level of Cycling (Baseline)" = "plc_base"
                                  ,"Distance to work" = "plc_dutch")
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
              a(href = "https://robinlovelace.shinyapps.io/fixMyPath/", "fixMyPath"),
              p("fixMyPath is a shiny app written to facilitate better bicycle path planning in Leeds, the UK and eventually the world. If you'd like to get involved, please check-out, test and contribute-to the fully reproducible code..."),
              a(href = "https://github.com/nikolai-b/hackMyRoute/tree/master/R/fixMyPath", strong("HERE!"), target="_blank"),
              leafletOutput('map', height = 600)),
  ))
