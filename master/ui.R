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
        cursor = "inherit",
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        top = 110,
        right = 20,
        width = 180,
        height = "auto",
        style = "opacity: 0.9",
        selectInput("scenario", "Scenario:", scenarios),
        bsTooltip(
          id = "scenario",
          title = "<strong>Census 2011 Cycling</strong> Based on the Census 2011 data</br><strong>Government Target</strong> Scenario based increase in cycling levels</br><strong>Gender equality</strong> equal number of women and men cycling</br><strong>Go Dutch</strong> similar cycling rates as Holland</br><strong>Electric Bikes</strong> more electric which increases the distance people are willing to cycle",
          placement = "left",
          trigger = "hover"
        ),
        conditionalPanel(
          condition = "input.scenario != 'olc'",
          tipify(
            selectInput("zone_attr", "Zone Attribute:", attrsZone),
            title = "Set zone colours depending on the cycling level:</br><strong>SCL</strong> Scenario (number model predicts)</br><strong>SIC</strong> Scenario Increase (change between observed and scenario)",
            placement = "left", trigger = "hover")
        ),
        tipify(selectInput("line_type", "Cycling Flows", line_types, selected = "none"),
               title = "Shows the cycling flow between the centres of zones using:<strong></br>Straight lines</br>Fastest cycle routes</br>Fastest and quietest routes</strong>",
               placement = "left", trigger = "hover"),
        conditionalPanel(
          condition = "input.line_type != 'none'",
          tipify(checkboxInput("freeze", "Freeze Lines", value = TRUE),
                 title = "<strong>Ticked</strong> the flows are independent of the map boundary (zoom and position)</br><strong>Unticked</strong> the flows update depending on the map boundary",
                 placement = "left", trigger = "hover", options = list(container = "body")),
          conditionalPanel(
            condition = "input.advanced",
            tipify(selectInput("line_attr", "Flow attribute to display:", attrsLine),
                   title = "Filter the routes/lines by:</br><strong>OCL</strong> Observed (census data)[on baseline only]</br><strong>SCL</strong> Scenario (number model predicts)</br><strong>SIC</strong> Scenario Increase (change between observed and scenario)",
                   placement = "left", trigger = "hover")
          ),
          tipify(sliderInput("nos_lines", label = "Flows to show (top n)", 1, 50, value = 5),
                 title = "Display the top n flows based on the selected flow attribute.</br>Thicker flows means higher attribute level",
                 placement = "left", trigger = "hover")
        ),
        checkboxInput('advanced', 'Advanced Controls')
      ),
      tipify(
        absolutePanel(
          cursor = "default",
          id = "legend",
          class = "panel panel-default",
          fixed = TRUE,
          top = 150,
          left = 30,
          width = 100,
          style = "opacity: 0.7",
          plotOutput("legendCyclingPotential", width = "100%")
        ),
        title = "Scenario-specific quartiles</br>of Cycling Level",
        placement = "centre", trigger = "hover"
      ),
      conditionalPanel(
        condition = "input.advanced",
        tipify(
          absolutePanel(
            cursor = "default",
            id = "map_base_panel",
            class = "panel panel-default",
            bottom = 5,
            left = 20,
            width = 300,
            style = "opacity: 0.7",
            radioButtons("map_base", "Map Base:", map_base_attrs, inline = TRUE)
          ),
          title = "Changing base of the map",
          placement = "centre", trigger = "hover"
        )
      )
    ),
    tabPanel("Lines Data",
             helpText("This tab shows the underlying data of the Cycling Flows (straight or otherwise)"),
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
