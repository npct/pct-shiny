library(shiny)
library(shinyBS)
library(leaflet)

scenarios <- c("Baseline" =           "base",
               "Gender equality" =   "gendereq",
               "Go Dutch" =          "dutch",
               "Electric bicycles" = "ebike")

line_types <- c("None" = "none",
                "Straight Lines" = "straight",
                "Fastest Route" = "d_route",
                "Fastest Route & Quiet Routes" = "route")

attrs <- c("Observed Level Cycling (OLC)" = "olc",
           "Scenario-based Level of Cycling (SLC)" =    "slc",
           "Scenario-based Increase in Cycling (SIC)" = "sic")


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
          title = "<strong>Baseline</strong> is the model based on the current cycling levels</br><strong>Gender equality</strong> assumes the number of women and men cycling evens out</br><strong>Go Dutch</strong> assumes a similar cycling rates as Holland</br><strong>Electric Bikes</strong> assumes that ownership and use of electric bicycles increases dramatically, increasing the distance people are willing to regularly cycle",
          placement = "left",
          trigger = "hover"
        ),
        tipify(
          selectInput("zone_attr", "Zone Attribute:", attrs, selected = "current"),
          title = "Alters to colours of the zones depending on the cycling level:</br><strong>OCL</strong> Observed (census data)[on baseline only]</br><strong>SCL</strong> Scenario (number model predicts)</br><strong>SIC</strong> Scenario Increase (change between observed and scenario)",
          placement = "left", trigger = "hover"),
        tipify(selectInput("line_type", "Cycling Flows", line_types, selected = "none"),
               title = "Shows the cycling flow between the centres of zones using:<strong></br>Straight lines</br>Fastest cycle routes</br>Fastest and quietest routes</strong>",
               placement = "left", trigger = "hover"),
        conditionalPanel(
          condition = "input.line_type != 'none'",
          tipify(checkboxInput("freeze", "Freeze Lines", value = TRUE),
                 title = "<strong>Ticked</strong> the flows are independent of the map boundary (zoom and position)</br><strong>Unticked</strong> the flows update depending on the map boundary",
                 placement = "left", trigger = "hover", options = list(container = "body")),
          tipify(selectInput("line_attr", "Flow attribute to display:", attrs, selected = "current"),
                 title = "Filter the routes by:</br><strong>OCL</strong> Observed (census data)[on baseline only]</br><strong>SCL</strong> Scenario (number model predicts)</br><strong>SIC</strong> Scenario Increase (change between observed and scenario)",
                 placement = "left", trigger = "hover"),
          tipify(sliderInput("nos_lines", label = "Flows to show (top n)", 1, 20, value = 5),
                 title = "Display the top n flows based on the selected flow attribute</br>thicker flows means higher attribute level",
                 placement = "left", trigger = "hover")
        )
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
      )
    ),
    tabPanel("Help",
             helpText("This tab contains information and screenshots on how to use the tool better.")
             ,includeHTML("help.html")
    )
  )
)
