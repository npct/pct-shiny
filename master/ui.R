library(shiny)
library(leaflet)

scenarios <- c("Baseline" =          "ba"
              ,"Gender equality" =   "ge"
              ,"Go Dutch" =          "gd"
              ,"Electric bicycles" = "eb")

line_types <- c("None" = "none"
            ,"Straight Lines" = "straight"
            ,"Fastest & Quietest Routes" = "route")

zone_types <- c("None" = "none"
           ,"Medium Super Output Area (MSOA)" = "msoa"
           ,"Super Output Area (SOA)" = "routes")

attrs <- c("Current Level Cycling (CLC)" =       "clc"
          ,"Potential Level of Cycling (PLC)" = "plc"
          ,"Extra Cycling Potential (ECP)" =    "ecp")

features <- c("None" = "none"
              ,"Cycle Parking" = "cycleparking"
              ,"Collisions involving cyclists" = "collisions"
              ,"Bike Shops" = "bikeshops")

shinyUI(navbarPage("Infrastructure planning tool", id="nav"
                   ,tabPanel("Interactive map"
                            ,leafletOutput("map", width="auto", height="1000")
                            ,absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 330, height = "auto"
                                           ,selectInput("scenario", "Scenario:", scenarios)
                                           ,selectInput("line_type", "Representation of cycling desire lines", line_types, selected = "none")
                                           ,conditionalPanel(condition = "input.line_type != 'none'"
                                                             ,selectInput("line_attr", "Line attribute to display:", attrs, selected = "current")
                                                             ,sliderInput("nos_lines", label = "Number of lines (n) negative to show the bottom n, postive the top"
                                                                          , min = -10, max = 10, value = 10)
                                           )
                                           ,selectInput("zone_type", "Type of zones:", zone_types, selected = "msoa")
                                           ,conditionalPanel(condition = "input.zone_type != 'none'"
                                                             ,selectInput("zone_attr", "Zone attribute:", attrs, selected = "current")
                                           )
                                           ,selectInput("feature", "Additional Features", features)
                            )
                   )
))
