library(shiny)
library(shinyBS)
library(leaflet)

scenarios <- c("Baseline" =           "base"
               ,"Gender equality" =   "gendereq"
               ,"Go Dutch" =          "dutch"
               ,"Electric bicycles" = "ebike")

line_types <- c("None" = "none"
                ,"Straight Lines" = "straight"
                ,"Fastest Route" = "d_route"
                ,"Fastest Route & Quiet Routes" = "route")

attrs <- c("Observed Level Cycling (OLC)" = "olc"
               ,"Scenario-based Level of Cycling (SLC)" =    "slc"
               ,"Scenario-based Increase in Cycling (SIC)" = "sic")


shinyUI(navbarPage("Infrastructure planning tool", id="nav"
                   ,header = tags$head(includeScript("google-analytics.js"))
                   ,tabPanel("Interactive map"
                             , helpText("Warning: this tool is under development. Its outputs may change as the model is refined.")
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
                               ,bsTooltip(id = "scenario", title = "Select a Scenario",placement = "left", trigger = "hover")
                               ,selectInput("zone_attr", "Zone Attribute:", attrs, selected = "current")
                               ,bsTooltip(id = "zone_attr", title = "Select the zone level attribute",placement = "left", trigger = "hover")
                               ,selectInput("line_type", "Cycling Flows", line_types, selected = "none")
                               ,bsTooltip(id = "line_type", title = "Select the Cycling Flow",placement = "left", trigger = "hover")
                               ,conditionalPanel(condition = "input.line_type != 'none'"
                                                 ,checkboxInput("freeze", "Freeze Lines", value = TRUE)
                                                 ,bsTooltip(id = "freeze", title = "Checked: Fixed lines <br> Unchecked: Dyanamic Lines ",placement = "left", trigger = "hover")
                                                 ,selectInput("line_attr", "Line attribute to display:", attrs, selected = "current")
                                                 ,bsTooltip(id = "line_attr", title = "Select the line attribute",placement = "left", trigger = "hover")
                                                 ,sliderInput("nos_lines", label = "Number of lines to show (top n)", 1, 50, value = 5)
                                                 ,bsTooltip(id = "nos_lines", title = "Select number of lines to be displayed",placement = "left", trigger = "hover")
                               )

                             )
                             ,bsTooltip(id = "legend", title = "Scenario-specific quartiles <br> of Cycling Level",
                                        placement = "centre", trigger = "hover")
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
