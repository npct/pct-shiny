library(shiny)
library(leaflet)
library(sp)

# Load line data
l <- readRDS("../data/l.Rds")
# Load MSOA data
leeds <- readRDS("../data/leeds-msoas-simple.Rds")

function(input, output, session) {

  observe({

    ## Initialize how many lines to be sorted by zero
    torh <- 0
    ## Tail or Head for sorting (if it is TOP then it is sorted by head, otherwise by tail)
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
    filter_data <- l[ func(order(l[[input$scenario]]), (torh)), ]

    map <- leaflet() %>%
      addTiles(urlTemplate = "http://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png") %>%
      addPolylines(data = filter_data,
                   # Add opacity to the lines - ranging from 0 to 0.5 in descending order
                   opacity = sort(runif(torh, min = 0, max = 0.5), decreasing = TRUE),
                   color = "red") %>%
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

