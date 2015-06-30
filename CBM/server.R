library(shiny)
library(DT)
library(sqldf)
if (!require(rCharts)) {
  install_github('rCharts', 'ramnathv')
  library(rCharts)
}
library(reshape2)
library(dplyr)

#source("data-processing.R")
pd <- idata$total_mmet
scdata <- NULL
pdl <- NULL

shinyServer(function(input, output, session){

  plotTables <- reactive({
    (input$scenario != 'none')
  })

  plotDataTable<- reactive({
    if (input$scenario == 't')
      data <- tdata
    else
      data <- idata
    if (input$ag != 'All'){
      data <- subset(data, age_group %in% input$ag)
    }
    if (input$gender != 3)
      data <- subset(data, Sex_B01ID %in% input$gender)
    data[is.na(data)] <- 0
    pd <<- data
  })

  output$plotMode <- renderPlot({
    plotDataTable()
    if (!is.null(pd)){
      if (input$scenario == 't'){
#         pm <- rPlot(x = pd$MainMode_reduced, y = pd$MainMode_reduced_val, data = pd, type = "bar") # , y = pd$MainMode_reduced
#         pm$set(dom = 'plotMode')
#         return(pm)


        par(mar = c(10, 4, 5, 5) + 0.2)
        barplot(height=table(pd$MainMode_reduced, pd$MainMode_reduced_val), las=2,
                cex.names=1, col=1:length(unique(pd$MainMode_reduced)),
                main = "Mode of Travel (Trip Based Dataset)")
      }
      else{
        max_val <- max(pd$total_mmet)
        if (max_val < 60){
          hist(pd$total_mmet, xlab = "Total Marginal MET", main = "Total Marginal MET (Individual Based Dataset)",
               breaks = c(seq(min(pd$total_mmet), ceiling(max(pd$total_mmet)), by = 5), max(pd$total_mmet)))
        }
        else{
          hist(pd$total_mmet, xlab = "Total Marginal MET", main = "Total Marginal MET (Individual Based Dataset)",
               breaks = c(seq(min(pd$total_mmet), 60, by = 5),max(pd$total_mmet)), xlim = c(min(pd$total_mmet), 60), right=FALSE)
        }
      }
    }

  })


  output$plotBaseline <- renderPlot({
#     plotDataTable()
    if (!is.null(tdata)){
      if (input$scenario == 't'){
        par(mar = c(10, 4, 5, 5) + 0.2)
        barplot(height=table(tdata$MainMode_reduced, tdata$MainMode_reduced_val), las=2,
                cex.names=1, col=1:length(unique(tdata$MainMode_reduced)),
                main = "Baseline: Mode of Travel")
      }
      else{
        hist(idata$total_mmet, xlab = "Total Marginal MET", main = "Baseline: Total Marginal MET",
             breaks = c(seq(min(idata$total_mmet), 60, by = 5),max(idata$total_mmet)), xlim = c(min(idata$total_mmet), 60), right=FALSE)
      }
    }

  })

  generateScenarioTable<- reactive({

    lMS <- input$inMS
    lTDR <- input$inTDR
    lEB <- input$inEB
    lEQ <- input$inEQ

    data <- sdata
    if (lMS != "All")
      data <- subset(data, MS == lMS)# & TDR == lTDR & equity == lEQ & ebike == lEB)
    if (lTDR != "All")
      data <- subset(data, TDR == lTDR)
    if (lEQ != "All")
      data <- subset(data, equity == lEQ)
    if (lEB != "All")
      data <- subset(data, ebike == lEB)

    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    # data[order(Age),]
    scdata <<- data
  })

  #   Solid
  #   ShortDash
  #   ShortDot
  #   ShortDashDot
  #   ShortDashDotDot
  #   Dot
  #   Dash
  #   LongDash
  #   DashDot
  #   LongDashDot
  #   LongDashDotDot

  genericPlot <- function(var){
    if (input$inTDR == "All" & input$inMS == "All"){

      h1 <- Highcharts$new()
      h1$chart(type = "spline")
      # types of charts: http://api.highcharts.com/highcharts#plotOptions
      h1$yAxis(title = list(text = var))
      h1$xAxis(title = list(text = '# of Scenarios'))

      sub1 <- subset(scdata, TDR == 0.7)
      h1$series(data = sub1[[var]], name = "TDR 0.7")
      sub1 <- subset(scdata, TDR == 0.8)
      h1$series(data = sub1[[var]], name = "TDR 0.8")
      sub1 <- subset(scdata, TDR == 0.9)
      h1$series(data = sub1[[var]], name = "TDR 0.9")
      sub1 <- subset(scdata, TDR == 1.0)
      h1$series(data = sub1[[var]], name = "TDR 1.0")

#       sub1 <- subset(scdata, ebike == 0)
#       h1$series(data = sub1[[var]], name = "Ebike 0")
#       sub1 <- subset(scdata, ebike == 1)
#       h1$series(data = sub1[[var]], name = "Ebike 1")
#       sub1 <- subset(scdata, equity == 0)
#       h1$series(data = sub1[[var]], name = "Equity 0")
#       sub1 <- subset(scdata, equity == 1)
#       h1$series(data = sub1[[var]], name = "Equity 1")

      h1$exporting(enabled = T)
      #h1$show('inline', include_assets = TRUE, cdn = TRUE)
#       h1$tooltip( formatter = "#! function() { return 'x: '     + this.point.x +
#                                                 'y: '    + this.point.y  +
#                                                 'name: '  + this.point.group; } !#")
      #h1$set(dom = 'plotCycPercent')
      return(h1)

    }else{

      h1 <- Highcharts$new()
      h1$chart(type = "spline")
      h1$series(data = scdata[[var]], name = paste("TDR", input$inTDR), dashStyle = "longdash")
      h1$yAxis(title = list(text = var))
      h1$xAxis(title = list(text = '# of Scenarios'))
      h1$exporting(enabled = T)
      return(h1)
    }
  }

  output$plotCycPercent <- renderChart({
    generateScenarioTable()
    h <- genericPlot("cyclists.perc")
    h$set(dom = 'plotCycPercent')
    return (h)
  })

  output$plotCO2R <- renderChart({
    generateScenarioTable()
    h <- genericPlot("CO2R.perc")
    h$set(dom = 'plotCO2R')
    return (h)

  })

  output$plotCarAccess <- renderChart({
    generateScenarioTable()
    h <- genericPlot("nocar.caraccess")
    h$set(dom = 'plotCarAccess')
    return (h)
  })

})
