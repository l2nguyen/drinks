# Load packages ---
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

bcl <- read.csv("data/bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("priceInput", "Price:", min = 0, max = 200,
                   value = c(50,150), step = 5, pre="$"),
      
      
      uiOutput("typeOutput"),
      
      uiOutput("countryOutput")
  ),
    
    mainPanel(
      h3(textOutput("textHead")),
      downloadButton("download", "Download results"),
      br(),
      plotOutput("coolplot"),
      br(), br(),
      DT::dataTableOutput("results")
    )
  )
)

server <- function(input, output) {

  filtered <- reactive({
    
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    bcl %>%
    filter(Price >= input$priceInput[1],
           Price <= input$priceInput[2],
           Type == input$typeInput,
           Country == input$countryInput
    )
  })
  
  output$textHead <- renderText({
    numResult <- nrow(filtered())
    if (is.null(numResult)) {
      numResult <- 0
    }
    
    paste("Search result: ", numResult, " items")
    
  })  
  
  output$coolplot <- renderPlot({
    
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram() + 
      labs(title = "Alcohol Content Histogram", x="Alcohol Content (%)")
  })
  
  output$results <- DT::renderDataTable({
    filtered()
    })
  
  output$download <- downloadHandler(
    filename = function() {
      "bcl-results.csv"
    },
    content = function(file) {
      write.csv(filtered(), file)
      })
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
    })
  
  output$typeOutput <- renderUI({
    selectInput("typeInput", "Product Type",
    choices = sort(unique(bcl$Type)),
    multiple = TRUE,
    selected = "WINE")
    })
  
}

shinyApp(ui = ui, server = server)
