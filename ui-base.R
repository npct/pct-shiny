library(shiny)
library(leaflet)

scenarios <- c("Census 2011 Cycling" = "olc",
               "Government Target" =   "cdp",
               "Gender equality" =   "gendereq",
               "Go Dutch" =          "dutch",
               "Ebikes (prototype)" = "ebike")

line_types <- c("None" = "none",
                "Straight Lines" = "straight",
                "Fastest Route" = "d_route",
                "Fastest Route & Quiet Routes" = "route",
                "Route Network" = "rnet")

attrsZone <- c("Scenario Level of Cycling (SLC)"    = "slc",
               "Scenario Increase in Cycling (SIC)" = "sic")

map_base_attrs <- c("Roadmap"   = "roadmap",
                    "Satellite" = "satellite",
                    "IMD" = "IMD")

shinyUI(
  navbarPage(
    title = "National Propensity to Cycle Tool (Prototype)",
    id="nav",
    tabPanel(
      "Interactive map",
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
            tags$div(title="Scenario details can be seen in the Help tab",
                     selectInput("scenario", "Scenario:", scenarios)
            ),
            tags$div(title="Shows the cycling flow between the centres of zones",
                     selectInput("line_type", "Cycling Flows", line_types, selected = "none")
            ),
            conditionalPanel(
              condition = "input.line_type != 'none'",
              tags$div(title="Untick to allow lines to update when you move the map",
                       checkboxInput("freeze", "Freeze Lines", value = TRUE)
              ),
              tags$div(title="Flows to show",
                       sliderInput("nos_lines", label = "Number of Lines", 1, 100, value = 10)
              )
            ),
            tags$div(title="Change base of the map",
                     tags$div(class = "rbox",
                              radioButtons("map_base", "Map Base:", map_base_attrs)
                     )
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
                       selectInput("triptype", label = "Trip data", choices = c("Commuting", "Education (unavailable)", "Shopping (unavailable)"), selected = "Commute data"),
                       conditionalPanel(
                         condition = "input.map_base != 'c'",
                         plotOutput("legendCyclingPotential", width = "100%", height = 300)
                       )
              )
          )
        ),

        conditionalPanel(
          condition = "input.map_base == 'IMD'",
          absolutePanel(
            cursor = "default", id = "legend", class = "panel panel-default",
            bottom = 200, right = 5, height = 20, width = 215,
            style = "opacity: 0.7",
            tags$div(title="Show/Hide map legend",
                     a(id = "toggleMapLegend", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
            ),
            div(id = "map_legend",
                tags$div(title="imd",
                   plotOutput("IMDLegend", width = "100%", height = 180)

                )
            )
          )
        ),
        tags$div(id="cite",
                 'This is a prototype released under the', a('GNU AGP licence', href= "licence.html", target='_blank'), 'and funded by the', a('DfT', href = "https://www.gov.uk/government/organisations/department-for-transport", target="_blank")
        )
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
    tabPanel("About",
             includeHTML(file.path("static", "more-info.html"))
    ),
    tabPanel("How to use it",
             includeMarkdown(file.path("static", "helpmd.md"))
    ),
    tabPanel("FAQs",
             includeHTML(file.path("static", "FAQs.html"))
    )
  )
)
