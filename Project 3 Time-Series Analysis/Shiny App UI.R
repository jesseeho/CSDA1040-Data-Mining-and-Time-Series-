#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(dplyr)

# Load Data
#df = read.csv('sales data-set.csv', header=TRUE)

# Read RDS
features_df <- readRDS('features.rds')
sales_df <- readRDS('sales.rds')
stores_df <- readRDS('stores.rds')

sales_df$Date <- as.Date(sales_df$Date, format = "%d/%m/%Y")
features_df$Date <- as.Date(features_df$Date, format = "%d/%m/%Y")
str(sales_df)

summary(sales_df)

df <- sales_df %>%
  inner_join(stores_df, by="Store") %>%
  inner_join(select(features_df, -c(MarkDown1, MarkDown2, MarkDown3, 
                                    MarkDown4, MarkDown5)), 
             by=c("Store", "Date", "IsHoliday")) 

sales <- df


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Time-series Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        contition = "$('html').hasClass('shiny-busy')",
        tags$div("Loading...", id="loadmessage")
      ),
      selectInput(
        'series', label="Time-Series by:",
        c("Sales", "Temperature", "Fuel Price", "CPI", "Unemployment")
      ),
      sliderInput(
        'store', label="Store #",
        min=0, max=45, step =1, value=0
      ), br(),
      tags$div("Store = 0, represents All Stores")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id='mytab',
                  tabPanel('Summary', value='summary',
                           selectInput("Variable", "Column", 
                                       choices = colnames(sales)[-1]), br(),
                           
                           tags$div("Top 20 based on Total Sales"), 
                           plotOutput("summary")),
                  tabPanel('Time-Series 1', value='ts1', 
                           selectInput("Viz", "Visualize",
                                       choices = c("Plot Time-series"
                                                   , "Time-series Display"
                                                   , "Decompose"
                                                   , "Smoothing"
                                                   , "Arima"
                                                   , "Auto-Arima"
                                                   , "Forecasting"
                                                   )),
                           plotOutput("ts1"))
                  )
       
    )
  )
))
