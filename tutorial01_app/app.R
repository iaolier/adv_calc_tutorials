#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

plot.piecewise.fnc = function(.piece.range, .FUN, ..., .pieces = c(-1, 0, 1), .resolution = 101) {
    period = diff(.piece.range)  #FIXME: to throw an error if NEGATIVE
    repetitions = rep(.pieces * period, each = .resolution)
    Ts = (.piece.range[2] - .piece.range[1])/.resolution
    n.T = seq(from = round(-.resolution/2), to = round(.resolution/2), length.out = .resolution)
    t.T = n.T * Ts
    t = t.T + repetitions
    Ft = .FUN((t-repetitions))
    
    data.plot = tibble(time = t, Ft = Ft)
    
    ggplot(data.plot, aes(x = time, y = Ft)) + geom_line()
}

plot.fourier.series = function(.T, .a0, .an.fun, .bn.fun, .t.range, .n.max = 10, .resolution = 101) {
    Ts = (.t.range[2] - .t.range[1])/.resolution
    k = seq(from = round(-.resolution/2), to = round(.resolution/2), length.out = .resolution)
    t.vector = k * Ts
    ft = map_dbl(t.vector, function(t) {
        map_dbl(1:.n.max, function(n) {
            .an.fun(n) * cos(2*pi*n*t/.T) + .bn.fun(n) * sin(2*pi*n*t/.T)
        }) %>% reduce(`+`)
    })
    ft = ft + a0/2
    data.plot = tibble(time = t.vector, Ft = ft)
    ggplot(data = data.plot, aes(x = time, y = Ft)) + geom_line()
}


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
   piecewise.fncs <- eventReactive(input$go, {
        plot.piecewise.fnc(.piece.range = c(input$piece.range.from, input$piece.range.to), .FUN = function(t){
            ifelse(t<0, 0, t)
        }, .pieces = seq(input$pieces[1], input$pieces[2]), .resolution = input$resolution)
    })
    
    output$plot <- renderPlot({
        piecewise.fncs()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
