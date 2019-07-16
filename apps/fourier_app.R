#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
    textAreaInput("caption", "Insert expression for function f(t) here:", "", width = "1000px"),
    numericInput("piece.range.from", "Piece range from:", -1),
    numericInput("piece.range.to", "To:", 1),
    sliderInput("pieces", label = "Pieces:",
                min = -5, max = 5, value = c(-1,1), step = 1),
    sliderInput("resolution", label = "Graph resolution:",
                min = 20, max = 1000, value = 100, step = 20),
    actionButton("go", "Go"),
   # verbatimTextOutput("value"),
    plotOutput("plot")
    
    
)
server <- function(input, output) {
   # output$value <- renderText({ input$pieces })
    randomVals <- eventReactive(input$go, {
        runif(input$resolution)
    })
    
    output$plot <- renderPlot({
        hist(randomVals())
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
