library(shiny)
library(leaflet)

scenarios <- c("Baseline" =           "base"
               ,"Gender equality" =   "gendereq"
               ,"Go Dutch" =          "dutch"
               ,"Electric bicycles" = "ebike")

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
                             ,leafletOutput("map", width="auto", height="600")
                             ,absolutePanel(
                               cursor = "move"
                               ,id = "controls"
                               ,class = "panel panel-default"
                               ,fixed = TRUE
                               ,top = 50
                               ,right = 0
                               ,width = 180
                               ,height = "auto"
                               ,style = "opacity: 0.9"
                               ,selectInput("scenario", "Scenario:", scenarios)
                               ,selectInput("line_type", "Representation of cycling desire lines", line_types, selected = "none")
                               ,conditionalPanel(condition = "input.line_type != 'none'"
                                                 ,checkboxInput("freeze", "Freeze Scope", value = TRUE)
                                                 ,selectInput("line_attr", "Line attribute to display:", attrs, selected = "current")
                                                 ,sliderInput("nos_lines", label = "Number of lines to show (top n)", 5, 500, value = 5, step = 5)
                               )
                               ,selectInput("zone_type", "Type of zones:", zone_types, selected = "msoa")
                               ,conditionalPanel(condition = "input.zone_type != 'none'"
                                                 ,selectInput("zone_attr", "Zone attribute:", attrs, selected = "current")
                               )
                               ,selectInput("feature", "Additional Features", features)
                             )
                   )
))
