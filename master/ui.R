library(shiny)
library(leaflet)
fluidPage(
  titlePanel("FixMyPath"),
  sidebarLayout(
    sidebarPanel("User input", width = 3
                 # ,checkboxGroupInput("display", label = "Display", choices = c("zones", "centroids", "some-lines", "all-lines"), selected = "zones"),
                 ,checkboxInput("transp_zones", label = "Transparency of zone boundaries", value = FALSE)
                 # ,sliderInput("transp_cents", label = "Transparency of centroids", min = 0, max = 1, value = 0.3)
                 ,sliderInput("transp_fast", label = "Transparency of paths", min = 0, max = 1, value = 0.7)
                 # , selectInput("colour", "Colour of paths", choices = c("red", "blue", "black"))
                 , selectInput("feature", "Features", choices = c("none", "cycleparking", "collisions", "bikeshops"))

    ),
    mainPanel("Welcome to",
              a(href = "https://robinlovelace.shinyapps.io/fixMyPath/", "fixMyPath"),
              p("fixMyPath is a shiny app written to facilitate better bicycle path planning in Leeds, the UK and eventually the world. If you'd like to get involved, please check-out, test and contribute-to the fully reproducible code..."),
              a(href = "https://github.com/nikolai-b/hackMyRoute/tree/master/R/fixMyPath", strong("HERE!"), target="_blank"),
              leafletOutput('map', height = 600)),
  ))