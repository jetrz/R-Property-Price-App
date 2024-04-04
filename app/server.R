library(shiny)
source("func/analysis.R")

# Define server logic
shinyServer(function(input, output) {
  output$plotArea <- renderPlot({
    # Check if the button was clicked
    input$plotButton
    # This line ensures that the plot is only rendered after the button is clicked
    isolate({
      HDBMeanPriceByArea()
    })
  })
})
