library(shiny)
library(leaflet)

scenarios <- c("Baseline" =           "base"
               ,"Gender equality" =   "gendereq"
               ,"Go Dutch" =          "dutch"
               ,"Electric bicycles" = "ebike")

line_types <- c("None" = "none"
                ,"Straight Lines" = "straight"
                ,"Fastest Route" = "d_route"
                ,"Fastest Route & Quiet Routes" = "route")

attrs <- c("Observed Level Cycling (OLC)" =       "olc"
           ,"Scenario-based Level of Cycling (PLC)" = "slc"
           ,"Scenario-based Increase in Cycling (ECP)" =    "sic")

shinyUI(navbarPage("Infrastructure planning tool", id="nav"
                   ,header = tags$head(includeScript("google-analytics.js"))
                   ,tabPanel("Interactive map"
                             , helpText("This tool is ONLY for testing purposes. Not all the data is intended to be correct.")
                             ,leafletOutput("map", width="auto", height="600")
                             ,absolutePanel(
                               cursor = "move"
                               ,id = "controls"
                               ,class = "panel panel-default"
                               ,fixed = TRUE
                               ,top = 110
                               ,right = 20
                               ,width = 180
                               ,height = "auto"
                               ,style = "opacity: 0.9"
                               ,selectInput("scenario", "Scenario:", scenarios)
                               ,selectInput("zone_attr", "Zone Attribute:", attrs, selected = "current")
                               ,selectInput("line_type", "Cycling Flows", line_types, selected = "none")
                               ,conditionalPanel(condition = "input.line_type != 'none'"
                                                 ,checkboxInput("freeze", "Freeze Lines", value = TRUE)
                                                 ,selectInput("line_attr", "Line attribute to display:", attrs, selected = "current")
                                                 ,sliderInput("nos_lines", label = "Number of lines to show (top n)", 1, 50, value = 5)
                               )

                             )
                             ,absolutePanel(
                               cursor = "default"
                               ,id = "legend"
                               ,class = "panel panel-default"
                               ,fixed = TRUE
                               ,top = 150
                               ,left = 30
                               ,width = 100
                               ,style = "opacity: 0.7"
                               ,plotOutput("legendCyclingPotential", width = "100%")
                               )

                   )
))
