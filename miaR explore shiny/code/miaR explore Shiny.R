library(tidyverse)
library(shiny)

runExample("01_hello")

hts <- Heights

ui <- fluidPage(
  titlePanel("miaR - explore Shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "number of bins",
                  min = 1,
                  max = 50,
                  value = 30)),
    mainPanel(
      plotOutput(outputId = "distPlot")))
  )



server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- hts$heights
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "lightblue", border = "darkblue",
         xlab = "heights (cm)",
         ylab = "count",
         main = "histogram of 100 female heights")})
  }

shinyApp(ui = ui, server = server)












