library(shiny)
library(leaflet)

scenarios <- c("Baseline" =           "base"
               ,"Gender equality" =   "gendereq"
               ,"Go Dutch" =          "dutch"
               ,"Electric bicycles" = "ebike")

line_types <- c("None" = "none"
                ,"Straight Lines" = "straight"
                ,"Direct Route" = "d_route"
                ,"Direct & Quiet Routes" = "route")

attrs <- c("Current Level Cycling (CLC)" =       "clc"
           ,"Potential Level of Cycling (PLC)" = "plc"
           ,"Extra Cycling Potential (ECP)" =    "ecp")

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
                               ,selectInput("zone_attr", "Zone Attribute:", attrs, selected = "current")
                               ,selectInput("line_type", "Cycling Flows", line_types, selected = "none")
                               ,conditionalPanel(condition = "input.line_type != 'none'"
                                                 ,checkboxInput("freeze", "Freeze Lines", value = TRUE)
                                                 ,selectInput("line_attr", "Line attribute to display:", attrs, selected = "current")
                                                 ,sliderInput("nos_lines", label = "Number of lines to show (top n)", 5, 500, value = 5, step = 5)
                               )

                             )
                             ,absolutePanel(
                               cursor = "move"
                               ,id = "legend"
                               ,class = "panel panel-default"
                               ,fixed = TRUE
                               ,top = 150
                               ,left = 30
                               ,width = 200
                               ,height = "auto"
                               ,style = "opacity: 0.9"
                               ,plotOutput("legendCyclingPotential", height = 300)
                               )

                   )
))
