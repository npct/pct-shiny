library(shiny)
library(shinyBS)
library(leaflet)

scenarios <- c("Census 2011 Cycling" = "olc",
               "Government Target" =   "base",
               "Gender equality" =   "gendereq",
               "Go Dutch" =          "dutch",
               "Electric bicycles" = "ebike")

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
    "Infrastructure planning tool",
    id="nav",
    header = tags$head(includeScript("../master/google-analytics.js")),
    tabPanel(
      "Interactive map",
      helpText("Warning: this tool is under development. Its outputs may change as the model is refined."),
      leafletOutput("map", width="auto", height="600"),
      absolutePanel(
        draggable = TRUE,
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        top = 110,
        right = 20,
        width = 180,
        height = "auto",
        style = "opacity: 0.9",
        selectInput("scenario", "Scenario:", scenarios),
        conditionalPanel(
          condition = "input.scenario != 'olc'",
          selectInput("zone_attr", "Zone Attribute:", attrsZone)
        ),
        selectInput("line_type", "Cycling Flows", line_types, selected = "none"),
        conditionalPanel(
          condition = "input.line_type != 'none'",
          checkboxInput("freeze", "Freeze Lines", value = TRUE),
          conditionalPanel(
            condition = "input.advanced",
            selectInput("line_attr", "Flow attribute to display:", attrsLine)
          ),
          sliderInput("nos_lines", label = "Flows to show (top n)", 1, 50, value = 5)
        ),
        checkboxInput('advanced', 'Advanced Controls')
      ),
        absolutePanel(
          cursor = "default",
          id = "legend",
          draggable = TRUE,
          class = "panel panel-default",
          fixed = TRUE,
          top = 150,
          left = 30,
          width = 100,
          style = "opacity: 0.7",
          plotOutput("legendCyclingPotential", width = "100%"),
          bsTooltip("legendCyclingPotential", "", placement = "right", options = list(container = "body"))
      ),

      absolutePanel(
        cursor = "default",
        id = "credits",
        class = "panel panel-default",
        fixed = TRUE,
        bottom = 5,
        right = 20,
        width = 250,
        bsCollapse(id = "credits",
          bsCollapsePanel("Route Information", HTML("We are using an API from OpenStreetMap to calculate routes.
                          Please see more information:
                          <a target='_blank' href = \"https://wiki.openstreetmap.org/wiki/Relation:route#Cycle_routes_.28also_mountain_bike.29\">Cycle Route</a>"),
                          style = "success")
        )
      ),

      conditionalPanel(
        condition = "input.advanced",
          absolutePanel(
            cursor = "default",
            id = "map_base_panel",
            class = "panel panel-default",
            bottom = 5,
            left = 20,
            width = 300,
            style = "opacity: 0.7",
            radioButtons("map_base", "Map Base:", map_base_attrs, inline = TRUE)
          )
      )
    ),
    tabPanel("Lines Data",
             helpText("This tab shows the underlying data of the Cycling Flows (straight or otherwise)"),
             uiOutput("warningMessage"),
             dataTableOutput("linesDatatable")
    ),
    tabPanel("Zones Data",
             helpText(HTML("This tab shows the underlying data of the Zones </br>
                            <strong>Data Source: </strong> We are using the 2011 Census data for England and Wales.
                            It contains origin-destination data on workplace flows. For more information, please see the
                            <a target='_blank' href = \"https://www.nomisweb.co.uk/census/2011/wu03ew\">source</a>")),
             dataTableOutput("zonesDataTable")
    ),
    tabPanel("Help",
             helpText("This tab contains information and screenshots on how to use the tool better."),
             includeHTML("help.html")
    )
  )
)
