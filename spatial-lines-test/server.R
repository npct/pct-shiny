library(shiny)
library(leaflet)
library(sp)

# Load line data
l <- readRDS("../data/l.Rds")
# Load MSOA data
leeds <- readRDS("../data/leeds-msoas-simple.Rds")

function(input, output, session) {

  observe({
    # Select which lines are to be shown - when user selects the scenario
    choice <- ""
    if (input$scenario == 1){
      choice <- "clc"
    }else if (input$scenario == 2){
      choice <- "plc"
    }else if (input$scenario == 3){
      choice <- "ecp"
    }

    ## Tail of Head of sorting (if it is TOP then it is sorted by head, otherwise by tail)
    torh <- 0
    func <- head

    if (input$lines == "Top 10"){
      torh <- 10
      func <- head
    }else if (input$lines == "Top 50"){
      torh <- 50
      func <- head
    }else if (input$lines == "Bottom 10"){
      torh <- 10
      func <- tail
    }else if (input$lines == "Bottom 50"){
      torh <- 50
      func <- tail
    }

    # Calculate the filtered data based on order function (head or tail) and how many lines are shown
    filter_data <- l[ func(order(l[[choice]]), (torh)), ]

    map <- leaflet() %>%
      addTiles(urlTemplate = "http://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png") %>%
      addPolylines(data = filter_data, opacity = 1, color = "red") %>%
      mapOptions(zoomToLimits = "first")
    output$map = renderLeaflet(map
                               %>%
      # Add polygones (of MSOA boundaries) ONLY when the checkbox transp_zones is CHECKED
      {
        if (input$show_zones)
          addPolygons(. , data = leeds
                      , fillOpacity = 0.4
                      , opacity = (input$transp)*.4
                      , fillColor = leeds$color_pcycle
          )
        else .
      })
  })
}

