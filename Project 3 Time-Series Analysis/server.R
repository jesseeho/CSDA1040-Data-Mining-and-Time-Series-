#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(forecast)
library(ggplot2)
library(lubridate)


#sales <- sales[order(sales$Date),]

#ts_sales <- ts((sales %>%
             #select(-Date))[,1], freq=365.25/7, start=2010+142/365.25)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Filter data
  filter_data <- reactive({
    if (input$store==0) {
      sales <- df
    } else {
      sales <- df %>%
        filter(Store == input$store)
    }
    sales
  })
   
  # Summarize data
  
  # Plotdata
  plotdata <- reactive({
    fdata <- filter_data()
    pdata <- fdata[input$Variable]
    pdata
  })
  
  
  
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #   
  # })
  output$summary <- renderPlot({
    pd <- filter_data()
    
    if (input$Variable == "Weekly_Sales") {
      pd %>%
        select(Date, Weekly_Sales) %>%
        group_by(Date) %>%
        summarise(Sales = sum(Weekly_Sales)) %>%
        ggplot(mapping = aes(y= Sales)) +
        geom_boxplot()
    } else {
      
      pd %>%
        select(x=input$Variable, Weekly_Sales) %>%
        group_by(x) %>%
        summarise(Sales = sum(Weekly_Sales)) %>%
        top_n(20) %>%
        ungroup() %>%
        mutate(x = reorder(x, Sales)) %>%
        ggplot(aes(x, Sales)) +
        geom_col(show.legend = FALSE) +
        coord_flip()
    }
  })
    
  output$ts1 <- renderPlot({
    pdata <- filter_data()
    s1 <- pdata %>%
      select(Date, Weekly_Sales) %>%
      group_by(Date) %>%
      summarise(Weekly_Sales = sum(Weekly_Sales))
    
    s2 <- s1 %>%
      select(Weekly_Sales)
    
    
    b <- ts(s2, frequency=365.25/7)
    if (input$Viz == "Plot Time-series") {
      plot(b)
    }
    if (input$Viz == "Time-series Display") {
      tsdisplay(b, points = TRUE)
    }
    if (input$Viz == "Decompose") {
      f <- decompose(b)
      #plot(f)
      
      #plot(f$figure, type="b", xaxt="n", xlab="")
      # get names of weeks in yyyyww format 
      weekNames <- year(s1$Date)*100+week(s1$Date)
      
      axis(1, at=1:143, labels=weekNames, las=2)
      
      plot(f)
      
    }
    if (input$Viz == "Smoothing") {
      tags$div("Under construction")
      # f <- decompose(b)
      # # HoltWinter Function
      # b.pre <- HoltWinters(f)
      # 
      # plot(b.pre)""
    }
    if (input$Viz == "Arima") {
      fita <- arima(b, order=c(1,0,0))
      #summary(fita)
      fore <- predict(fita, n.ahead=104)
      U <- fore$pred + 2*fore$se
      L <- fore$pred - 2*fore$se
      
      ts.plot(b, fore$pred, U, L, col=c(1,2,4,4), lty=c(1,1,2,2))
      legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2), cex=0.5)
      
    }
    if (input$Viz == "Auto-Arima") {
      fit3 <- auto.arima(b, ic="bic")
      #fit3
      #accuracy(fit3)
      #summary(fit3)
      
      fore3 <- predict(fit3, n.ahead=104)
      U3 <- fore3$pred + 2*fore3$se
      L4 <- fore3$pred - 2*fore3$se
      ts.plot(b, fore3$pred, U3, L4, col=c(1,2,4,4), lty=c(1,1,2,2))
      
    }
    if (input$Viz == "Forecasting") {
      # bestfit <- list(aicc=Inf)
      # harmonics <- fourier(b, K=13)
      # fit <- auto.arima(b, xreg=harmonics, seasonal=FALSE)
      # newharmonics <- fourier(gas, K = 13, h = 156)
      # fc <- forecast(fit, xreg = newharmonics)
      # autoplot(fc)
      # #fc <- forecast(bestfit, xreg=fourier(gas, K=13, h=143))
      # plot(fc)
      
      gastbats <- tbats(b)
      fc2 <- forecast(gastbats, h=10)
      options(scipen=999)
      plot(fc2, ylab="sales per week")
      
      p <- plot(fc2, ylab="sales per week")
    }
  })
    
        
    #x <- summary(sales$Weekly_Sales)
    #boxplot(x, col="sky blue", border="purple")
    #summary(sales)
    #str(sales)
    #summary(sales)
  
  
})
