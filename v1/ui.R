library(shiny)
library(shinyBS)
library(leaflet)

# Text for the 'Scenario' dropbox menu
# Modifiable Text: Please modify the text on the left hand side to change in the UI
scenarios <- c("Baseline" =           "base",
               "Gender equality" =   "gendereq",
               "Go Dutch" =          "dutch",
               "Electric bicycles" = "ebike")

# Text for the 'Cycling Flows' dropdown menu
# Modifiable Text: Please modify the text on the left hand side to change in the UI
line_types <- c("None" = "none",
                "Straight Lines" = "straight",
                "Fastest Route" = "d_route",
                "Fastest Route & Quiet Routes" = "route")

# Text for the 'Zone Attribute' dropdown menu
# Modifiable Text: Please modify the text on the left hand side to change in the UI
attrs <- c("Census 2011 Cycling" = "olc",
           "Scenario Cycling Level (SCL)" =    "slc",
           "Scenario Increase in Cycling (SIC)" = "sic")

# Text for the 'Map Based' radiobox
# Modifiable Text: Please modify the text on the left hand side to change in the UI
map_base_attrs <- c("Black and White" = "bw",
                  "OpenCycleMap" =    "c")

shinyUI(
  navbarPage(
    # Modifiable Text: Text to display at the Navigation Bar
    "Infrastructure planning tool",
    id="nav",
    header = tags$head(includeScript("../master/google-analytics.js")),
    tabPanel(
      # Modifiable Text: Text to display at the tab
      "Interactive map",
      # Modifiable Text: Warning sign for the application
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
          # Modifiable Text: Tooltip for the 'Scenario' dropbox menu
          title = "<strong>Baseline</strong> model based on the current cycling levels</br><strong>Gender equality</strong> equal number of women and men cycling</br><strong>Go Dutch</strong> similar cycling rates as Holland</br><strong>Electric Bikes</strong> use of electric bicycles increases, increasing the distance people are willing to cycle",
          placement = "left",
          trigger = "hover"
        ),
        tipify(
          # Title for the 'Scenario' dropbbox menu
          # Modifiable Text: Only 'Zone Attribute:' is modifiable
          selectInput("zone_attr", "Zone Attribute:", attrs, selected = "current"),
          # Modifiable Text: Tooltip for the 'Zone Attribute' dropbox menu
          title = "Set zone colours depending on the cycling level:</br><strong>OCL</strong> Observed (census data)[on baseline only]</br><strong>SCL</strong> Scenario (number model predicts)</br><strong>SIC</strong> Scenario Increase (change between observed and scenario)",
          placement = "left", trigger = "hover"),
        tipify(
          # Title for the 'Cycling Flows' dropbbox menu
          # Modifiable Text: Only 'Cycling Flows' is modifiable
          selectInput("line_type", "Cycling Flows", line_types, selected = "none"),
          # Modifiable Text: Tooltip for the 'Cycling Flows' dropbox menu
          title = "Shows the cycling flow between the centres of zones using:<strong></br>Straight lines</br>Fastest cycle routes</br>Fastest and quietest routes</strong>",
          placement = "left", trigger = "hover"),
        conditionalPanel(
          condition = "input.line_type != 'none'",
          tipify(
            # Title for the 'Freeze Lines' checkbox menu
            # Modifiable Text: Only 'Freeze Lines' is modifiable
            checkboxInput("freeze", "Freeze Lines", value = TRUE),
            # Modifiable Text: Tooltip for the 'Freeze Lines' checkbox menu
            title = "<strong>Ticked</strong> the flows are independent of the map boundary (zoom and position)</br><strong>Unticked</strong> the flows update depending on the map boundary",
            placement = "left", trigger = "hover", options = list(container = "body")),
          tipify(
            # Title for the 'Flow attribute to display' dropbox menu
            # Modifiable Text: Only 'Flow attribute to display' is modifiable
            selectInput("line_attr", "Flow attribute to display:", attrs, selected = "current"),
            # Modifiable Text: Tooltip for the 'Flow attribute to display' dropbox menu
            title = "Filter the routes by:</br><strong>OCL</strong> Observed (census data)[on baseline only]</br><strong>SCL</strong> Scenario (number model predicts)</br><strong>SIC</strong> Scenario Increase (change between observed and scenario)",
            placement = "left", trigger = "hover"),
          tipify(
            # Title for the 'Flows to show (top n)' dropbox menu
            # Modifiable Text: Only 'Flows to show (top n)' is modifiable
            sliderInput("nos_lines", label = "Flows to show (top n)", 1, 20, value = 5),
            # Modifiable Text: Tooltip for the 'Flows to show (top n)' dropbox menu
            title = "Display the top n flows based on the selected flow attribute.</br>Thicker flows means higher attribute level",
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
        # Tooltip for the legend
        # Modifiable Text: Tooltip for the legend
        title = "Scenario-specific quartiles</br>of Cycling Level",
        placement = "centre", trigger = "hover"
      ),

      tipify(
        absolutePanel(
          cursor = "default",
          id = "map_base_panel",
          class = "panel panel-default",
          bottom = 5,
          left = 10,
          width = 300,
          style = "opacity: 0.7",
          radioButtons("map_base", "Map Base:", map_base_attrs, inline = TRUE)
        ),
        # Tooltip for the Map Base radiobox
        # Modifiable Text: Tooltip for the Map Base
        title = "Changing base of the map",
        placement = "centre", trigger = "hover"
      )
    ),
    tabPanel(
        # Title of the tab
        # Modifiable Text
        "Help",
        # Description shown in the tab
        # Modifiable
        helpText("This tab contains information and screenshots on how to use the tool better."),
        includeHTML("help.html")
    )
  )
)
