#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library(datasets)

source("C:\Users\Jessee\Desktop\York University\CSDA1040\Project 1 Market Analysis/market_basket_analysis_v3Full.R",local = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(pageWithSidebar(
  headerPanel("Cleaned Dataset - Market_Basket_Analysis"),
  
    sidebarPanel(

    selectInput("dataset", "Choose a dataset:", 
                  choices = c("trans", "train", "test")),  
  
      radioButtons("type", "Choose the fuction from bellow:",
                 list("Summary" = "summ",
                      "Histogram" = "Hist",
                      "Table" = "table",
                      "Frequency Plot" = "freqp",
                      "Summary Rules (apriori (supt:0.001, confidence:0.25))" = "sumrl1",
                      "Inspect Rules(apriori (supt:0.001, confidence:0.25))" = "insr",
                      "Inspect Rules Filter (apriori (supt:0.001, confidence:0.25))" = "rulf1"
                      )),
    br(),
    
    sliderInput("n", 
              "Support:", 
                value = 1,
                min = 0, 
                max = 2)
 ),
    sliderInput("n", 
             "Confidence:", 
             value = 1,
             min = 0, 
             max = 2)
),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    conditionalPanel(
      condition = "input.type == 'summ'",
      verbatimTextOutput("summaryp")
    ),
    conditionalPanel(
      condition =  "input.type == 'Hist'",
      plotOutput("histtp")
    ),
    conditionalPanel(
      condition = "input.type == 'table'",
      dataTableOutput('table1p')
    ),
    conditionalPanel(
      condition =  "input.type == 'freq'",
      plotOutput("freqp")
    ),
    conditionalPanel(
      condition = "input.type == 'sumrl1'",
      verbatimTextOutput("sumrl1p")
    ),
    conditionalPanel(
      condition = "input.type == 'insr'",
      verbatimTextOutput("insrp")
    ),
    conditionalPanel(
      condition = "input.type == 'rulf1'",
      verbatimTextOutput("rulf1p")
    )
    
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- reactive({  
    dist <- switch(input$dataset,
                   trans = trans,
                   train = train,
                   test = test,
                   trans)
})
  

  # Generate a summary of the data
  output$summaryp <- renderPrint({
    summary(data())
  })

  output$histtp <- renderPlot({
    itemFrequencyPlot(trans, topN=20, type="absolute")
  })
  
  # Generate an HTML table view of the data
  output$table1p = renderDataTable({
   
  })
  
  output$freqp <- renderPlot({
    itemFrequencyPlot(trans, topN=20, type="absolute")
  })
  
  output$sumrl1p <- renderPrint({
    summary(rules)
  })
  
  output$insrp <- renderPrint({
    inspect(rules)
  })
  
  output$rulf1p <- renderPrint({
    inspect(rules_filter)
  })
}
# Run the application 

shinyApp(ui = ui, server = server)

