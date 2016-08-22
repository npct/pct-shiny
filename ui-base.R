#     This is UI base that runs on every connected client
#
#     Copyright (C) 2016 Nikolai Berkoff, Ali Abbas and Robin Lovelace
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Affero General Public License as
#     published by the Free Software Foundation, either version 3 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Affero General Public License for more details.
#
#     You should have received a copy of the GNU Affero General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(shiny)
library(leaflet)
library(shinyjs)

scenarios <- c("Census 2011 Cycling" = "olc",
               "Government Target" = "govtarget",
               "Gender equality" = "gendereq",
               "Go Dutch" = "dutch",
               "Ebikes" = "ebike")

line_types <- c("None" = "none",
                "Straight Lines" = "straight",
                "Fast Routes" = "d_route",
                "Fast & Quiet Routes" = "route",
                "Route Network" = "rnet")

attrs_zone <- c("Level of Cycling"    = "slc",
               "Increase in Cycling" = "sic",
               "HEAT Value"          = "slvalue_heat",
               "CO2 reduction"       = "sico2")

map_base_attrs <- c("Roadmap (Black & White)"   = "roadmap",
                    "Roadmap (OpenCycleMap)" = "opencyclemap",
                    "Satellite" = "satellite",
                    "Index of Deprivation" = "IMD",
                    "Hilliness" = "hilliness")

on_production <- grepl('^/var/shiny/pct-shiny', getwd())

shinyUI(
  navbarPage(
    title = "Propensity to Cycle Tool",
    id="nav",
    tabPanel(
      "Map",
      useShinyjs(),
      div(
        class="outer",
        tags$head(
          if(on_production) includeScript("www/assets/google-analytics.js"),
          includeScript("www/assets/extra.js"),
          includeCSS("www/stylesheet.css"),
          includeHTML(file.path("regions_www", "favicon.html"))
        ),
        br(),
        leafletOutput("map", width="100%", height="95%"),
        absolutePanel(
          id = "controls", class = "panel panel-default",
          fixed = TRUE,  top = 110,  right = 20, width = 180,
          height = "auto",  style = "opacity: 0.9",
          tags$div(title="Show/Hide Panel",
                   a(id = "toggle_panel", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
          ),
          div(
            id = "input_panel",
            tags$div(title="Scenario (see FAQ tab)",
                     selectInput("scenario", "Scenario:", scenarios, selectize = F)
            ),
            tags$div(title="Shows the cycling flow between the centres of zones",
                     selectInput("line_type", "Cycling Flows", line_types, selected = "none", selectize = F)
            ),
            tags$div(title="Shows the cycling flow between the centres of zones",
                     checkboxInput("show_zones", "Show Zones", value = T)
            ),
            conditionalPanel(
              condition = "input.line_type != 'none'",
              tags$div(title="Untick to update lines when you move the map",
                       checkboxInput("freeze", "Freeze Lines", value = TRUE)
              ),
              tags$div(title="Flows to show",
                       sliderInput("nos_lines", label = "N. Lines (most cycled)", 1, 200, value = 30, ticks = F)
              ),
              conditionalPanel(
                condition = "input.line_type != 'rnet' && input.scenario != 'olc'",
                tags$div(title="Order the top flows by",
                         selectInput("line_order", "Order lines/flows by", attrs_zone, selected = "slc", selectize = F)
                )
              )
            ),
            tags$div(title="Change base of the map",
                        selectInput("map_base", "Map Base:", map_base_attrs, selectize = F)
            )
          )
        ),
        absolutePanel(
          cursor = "default", id = "legend", class = "panel panel-default",
          top = 350, left = 25, height = 30, width = 100,
          style = "opacity: 0.7",
          tags$div(title="Show/Hide zone legend",
                   a(id = "toggle_legend", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
          ),
          div(id = "zone_legend",
              tags$div(title="Scenario-specific quartiles of cycling level",
                       selectInput("triptype", label = "Trip data", choices = c("Commuting", "Education (unavailable)", "Shopping (unavailable)", "All"), selected = "Commute data", selectize = T),
                       conditionalPanel(
                         condition = "input.map_base != 'c'",
                         plotOutput("legend_cycling_potential", width = "100%", height = 200)
                       )
              )
          )
        ),

        conditionalPanel(
          condition = "input.map_base == 'IMD'",
          absolutePanel(
            cursor = "auto", id = "legend", class = "panel panel-default",
            bottom = 235, left = 5, height = 20, width = 225, draggable = TRUE,
            style = "opacity: 0.7",
            tags$div(title="Show/Hide map legend",
                     a(id = "toggle_map_legend", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
            ),
            div(id = "map_legend",
                tags$div(title="Index of Multiple Deprivation",
                   plotOutput("imd_legend", width = "100%", height = 180)

                )
            )
          )
        ),
        tags$div(id="cite",
                 htmlOutput("cite_html")
        )
      )
    ),
    tabPanel("Lines",
             br(),
             br(),
             HTML("This tab shows a selection of the Census 2011 and scenario data generated for selected between-zone
                  lines in the chosen region (see Model Output tab for details of the lines included). The full csv
                  dataset contains further details concerning mode share in Census 2011; the cycling, walking and driving
                  levels in each scenario; and the associated health and carbon impacts. You can download the full csv
                  dataset for the lines and routes here, alongside geographic and attribute data to be read by R (.Rds)
                  or GIS programs such as QGIS (.geojson)"),
             htmlOutput("line_data_links"),
             uiOutput("warning_message"),
             DT::dataTableOutput("lines_datatable")
    ),
    tabPanel("Zones",
             br(),
             br(),
             HTML("This tab shows a selection of the Census 2011 and scenario data generated
                    for all zones in the chosen region. The full csv dataset contains further
                    details concerning mode share in Census 2011; the cycling, walking and driving
                    levels in each scenario; and the associated health and carbon impacts.
                    You can download the full csv dataset for the zones here, alongside geographic
                    and attribute data to be read by R (.Rds) or GIS programs such as QGIS (.geojson):"),
             htmlOutput("zone_data_links"),
             DT::dataTableOutput("zones_data_table")
    ),
    tabPanel("Model Output",
             htmlOutput("m_output")
    ),
    tabPanel("About",
             includeHTML(file.path("regions_www", "about_body.html"))
    ),
    tabPanel("Manual",
             includeHTML(file.path("regions_www", "manual_body.html"))
    )
  )
)
