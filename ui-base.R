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

attrsZone <- c("Scenario Level of Cycling (SLC)"    = "slc",
               "Scenario Increase in Cycling (SIC)" = "sic")

map_base_attrs <- c("Roadmap (Black & White)"   = "roadmap",
                    "Roadmap (OpenCycleMap)" = "opencyclemap",
                    "Satellite" = "satellite",
                    "Index of Deprivation" = "IMD",
                    "Hilliness" = "hilliness")

shinyUI(
  navbarPage(
    title = "National Propensity to Cycle Tool (Prototype)",
    id="nav",
    tabPanel(
      "Map",
      div(
        class="outer",
        tags$head(
          includeScript("assets/google-analytics.js"),
          includeScript("assets/extra.js"),
          includeCSS("www/stylesheet.css")
        ),
        br(),
        leafletOutput("map", width="100%", height="95%"),
        absolutePanel(
          id = "controls", class = "panel panel-default",
          fixed = TRUE,  top = 110,  right = 20, width = 180,
          height = "auto",  style = "opacity: 0.9",
          tags$div(title="Show/Hide Panel",
                   a(id = "togglePanel", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
          ),
          div(
            id = "input_panel",
            tags$div(title="Scenario (see FAQ tab)",
                     selectInput("scenario", "Scenario:", scenarios, selectize = F)
            ),
            tags$div(title="Shows the cycling flow between the centres of zones",
                     selectInput("line_type", "Cycling Flows", line_types, selected = "none", selectize = F)
            ),
            conditionalPanel(
              condition = "input.line_type != 'none'",
              tags$div(title="Untick to update lines when you move the map",
                       checkboxInput("freeze", "Freeze Lines", value = TRUE)
              ),
              tags$div(title="Flows to show",
                       sliderInput("nos_lines", label = "N. Lines (most cycled)", 1, 200, value = 30, ticks = F)
              )
            ),
            tags$div(title="Change base of the map",
                        selectInput("map_base", "Map Base:", map_base_attrs, selectize = F)
            ),
            conditionalPanel(
              condition = "input.line_type != 'none'",
              tags$div(downloadButton('downloadData', 'geojson'))
            )
          )
        ),
        absolutePanel(
          cursor = "default", id = "legend", class = "panel panel-default",
          top = 100, left = 25, height = 30, width = 100,
          style = "opacity: 0.7",
          tags$div(title="Show/Hide zone legend",
                   a(id = "toggleLegend", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
          ),
          div(id = "zone_legend",
              tags$div(title="Scenario-specific quartiles of cycling level",
                       selectInput("triptype", label = "Trip data", choices = c("Commuting", "Education (unavailable)", "Shopping (unavailable)"), selected = "Commute data", selectize = T),
                       conditionalPanel(
                         condition = "input.map_base != 'c'",
                         plotOutput("legendCyclingPotential", width = "100%", height = 200)
                       )
              )
          )
        ),

        conditionalPanel(
          condition = "input.map_base == 'IMD'",
          absolutePanel(
            cursor = "default", id = "legend", class = "panel panel-default",
            bottom = 235, left = 5, height = 20, width = 225, draggable = TRUE,
            style = "opacity: 0.7",
            tags$div(title="Show/Hide map legend",
                     a(id = "toggleMapLegend", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
            ),
            div(id = "map_legend",
                tags$div(title="Index of Multiple Deprivation",
                   plotOutput("IMDLegend", width = "100%", height = 180)

                )
            )
          )
        ),
        tags$div(id="cite",
                 htmlOutput("citeHtml")
        )
      )
    ),
    tabPanel("Lines",
             br(),
             br(),
             helpText("This tab shows the underlying data of the Cycling Flows (straight or otherwise)"),
             uiOutput("warningMessage"),
             DT::dataTableOutput("linesDatatable")
    ),
    tabPanel("Zones",
             br(),
             br(),
             helpText(HTML("This tab shows the underlying data of the Zones </br>
                             <strong>Data Source: </strong> We are using the 2011 Census data for England and Wales.
                             It contains origin-destination data on workplace flows. For more information, please see the
                             <a target='_blank' href = \"https://www.nomisweb.co.uk/census/2011/wu03ew\">source</a>")),
             DT::dataTableOutput("zonesDataTable")

    ),
    tabPanel("Model Output",
             htmlOutput("moutput")
    ),
    tabPanel("About",
             includeHTML(file.path("static", "about_in_shiny.html"))
    ),
    tabPanel("FAQs",
             includeHTML(file.path("static", "FAQs.html"))
    )
  )
)
