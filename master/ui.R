library(shiny)
library(leaflet)

scenarios <- c("Census 2011 Cycling" = "olc",
               "Government Target" =   "cdp",
               "Gender equality" =   "gendereq",
               "Go Dutch" =          "dutch",
               "Ebikes (prototype)" = "ebike")

attrsLine <- c("Scenario Level of Cycling (SLC)" =    "slc",
               "Scenario Increase in Cycling (SIC)" = "sic")

line_types <- c("None" = "none",
                "Straight Lines" = "straight",
                "Fastest Route" = "d_route",
                "Fastest Route & Quiet Routes" = "route")

attrsZone <- c(attrsLine, c("None" = "none"))

map_base_attrs <- c("Black and White" = "bw",
                    "OpenCycleMap" =    "c")

shinyUI(
  navbarPage(
    "Cycling Planning Tool", id="nav",
    tabPanel(
      "Interactive map",
      div(
        class="outer",
        tags$head(
          includeScript("../master/google-analytics.js"),
          includeScript("../master/extra.js"),
          tags$style(type="text/css"),
          tags$link(rel = "stylesheet", type = "text/css", href ="stylesheet.css")
        ),
        br(),
        leafletOutput("map", width="100%", height="100%"),
        absolutePanel(
          id = "controls", class = "panel panel-default",
          fixed = TRUE,  top = 110,  right = 20, width = 180,
          height = "auto",  style = "opacity: 0.9",
          tags$div(title="Show/Hide Panel",
            a(id = "togglePanel", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
          ),
          div(
            id = "input_panel",
            tags$div(title="Scenario details can be seen in the More Information tab",
                     selectInput("scenario", "Scenario:", scenarios)
            ),
            conditionalPanel(
              condition = "input.scenario != 'olc'",
              tags$div(title="Set zone colours depending on the cycling level",
                selectInput("zone_attr", "Zone Attribute:", attrsZone)
              )
            ),
            tags$div(title="Shows the cycling flow between the centres of zones",
              selectInput("line_type", "Cycling Flows", line_types, selected = "none")
            ),
            conditionalPanel(
              condition = "input.line_type != 'none'",
              tags$div(title="Ticked: flows are independent of the map boundary (zoom and position), Unticked: flows update depending on the map boundary",
                checkboxInput("freeze", "Freeze Lines", value = TRUE)
              ),
              conditionalPanel(
                condition = "input.advanced",
                tags$div(title = "Flow attribute to display",
                  selectInput("line_attr", "Flow attribute to display:", attrsLine)
                )
              ),
              tags$div(title="Flows to show (top n)",
                sliderInput("nos_lines", label = "Flows to show (top n)", 1, 50, value = 5)
              )
            ),
            tags$div(title="Displays advanced options",
              checkboxInput('advanced', 'Advanced Controls')
            )
          )
        ),
        absolutePanel(
          cursor = "default", id = "legend", draggable = T, class = "panel panel-default",
          top = 100, left = 25, height = 30, width = 100,
          style = "opacity: 0.7",
          tags$div(title="Show/Hide zone legend",
            a(id = "toggleLegend", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
          ),
          div(id = "zone_legend",
              tags$div(title="Scenario-specific quartiles of cycling level",
                selectInput("triptype", label = "Trip data", choices = c("Commuting", "Education (unavailable)", "Shopping (unavailable)"), selected = "Commute data"),
                plotOutput("legendCyclingPotential", width = "100%", height = 350)
              )
          )
        ),
        conditionalPanel(
          condition = "input.advanced",
          absolutePanel(
            cursor = "default", id = "map_base_panel", class = "panel panel-default",
            bottom = 0, left = 100, width = 250, height = 50,
            style = "opacity: 0.9",
            tags$div(title="Change base of the map",
              tags$div(class = "rbox",
                radioButtons("map_base", "Map Base:", map_base_attrs, inline = TRUE)
              )
            )
          )
        )
      ),
      tags$div(id="cite",
               'This is a prototype tool released under an', a('MIT licence', href= "/licence.html"), '.'
      )
    ),
    tabPanel("Lines Data",
             br(),
             br(),
             helpText("This tab shows the underlying data of the Cycling Flows (straight or otherwise)"),
             uiOutput("warningMessage"),
             DT::dataTableOutput("linesDatatable")
    ),
    tabPanel("Zones Data",
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
    tabPanel("More information",
            includeHTML("help.html")
    )
  )
)
