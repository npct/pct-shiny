library(shiny)
library(leaflet)

fluidPage(
  titlePanel("Spatial Lines"),
  sidebarLayout(
    sidebarPanel("User input", width = 3
                 , selectInput("lines", "Select Lines", choices = c("Top 10", "Top 50", "Bottom 10", "Bottom 50"))
                 , radioButtons("scenario", label = "Scenario", choices = list("Current level of cycling (clc)" = "clc",
                                                                               "Potential level of cycling (plc)" = "plc",
                                                                               "Extra cycling potential (ecp)" = "ecp"), selected = "clc")
                 , checkboxInput("show_zones", label = "Show zone boundaries", value = FALSE)
                 , sliderInput("transp", label = "Transparency of boundaries", min = 0, max = 1, value = 0.7)

    ),
    mainPanel("Welcome to",
              a(href = "https://usr110.shinyapps.io/spatial-lines-test/", "Spatial Linest Test", target="_blank"),
              br(a(href = "https://github.com/usr110/pct/blob/master/shiny-test/spatial-lines-test/", strong("See the code here!"), target="_blank")),
              leafletOutput('map', height = 600),
              tags$head(tags$script(src="leg.js")))
  ))