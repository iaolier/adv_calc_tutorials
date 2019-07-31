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
    
    ggplot(data.plot, aes(x = time, y = Ft)) + geom_line() + theme_classic()
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
    ft = ft + .a0/2
    data.plot = tibble(time = t.vector, Ft = ft)
    ggplot(data = data.plot, aes(x = time, y = Ft)) + geom_line() + theme_classic()
}




ui <- navbarPage("Fourier analysis",
    tabPanel("Piecewise functions",
        textAreaInput("capt.func", "Insert expression for function f(t) here:", "function(t) ifelse(t<=-3, -t-3, ifelse(t>-3 & t<=0, t+3, ifelse(t>0 & t<=3, -2*t+3, t-6)))", width = "500px"),
        numericInput("piece.range.from", "Piece range from:", -5),
        numericInput("piece.range.to", "To:", 8),
        sliderInput("pieces", label = "Pieces:",
            min = -5, max = 5, value = c(-1,1), step = 1),
        sliderInput("resolution", label = "Graph resolution:",
            min = 20, max = 1000, value = 100, step = 20),
        actionButton("go", "Go"),
        plotOutput("plot.pw"),
        downloadButton("DownloadPlotPW", "Download")
    ),
    tabPanel("Fourier series",
        numericInput("fs.a0", "a0:", 0),
        textAreaInput("fs.an", "Insert expression for an:", "function(n) 1/n", width = "500px"),
        textAreaInput("fs.bn", "Insert expression for bn:", "function(n) 1/n", width = "500px"),
        numericInput("fs.period", "Period (T):", 1),
        numericInput("fs.trange.from", "t range from:", -1),
        numericInput("fs.trange.to", "To:", 1),
        sliderInput("fs.nmax", label = "maximum n:",
            min = 1, max = 50, value = 1, step = 1),
        sliderInput("fs.resolution", label = "Graph resolution:",
            min = 20, max = 1000, value = 100, step = 20),
        actionButton("fs.go", "Go"),
        plotOutput("plot.fs"),
        downloadButton("DownloadPlotFS", "Download")
    )
)

server <- function(input, output) {
   piecewise.fncs <- eventReactive(input$go, {
       pw.fn = eval(parse(text = input$capt.func))
        plot.piecewise.fnc(.piece.range = c(input$piece.range.from, input$piece.range.to), 
                           .FUN = pw.fn, .pieces = seq(input$pieces[1], input$pieces[2]), 
                           .resolution = input$resolution)
    })
    
    output$plot.pw <- renderPlot({
        piecewise.fncs()
    })
    
    fs.fncs <- eventReactive(input$fs.go, {
        fs.anfn = eval(parse(text = input$fs.an))
        fs.bnfn = eval(parse(text = input$fs.bn))
        plot.fourier.series(.T = input$fs.period, .a0 = input$fs.a0, 
                            .an.fun = fs.anfn, .bn.fun = fs.bnfn, 
                            .t.range = c(input$fs.trange.from, input$fs.trange.to), 
                            .n.max = input$fs.nmax, .resolution = input$fs.resolution)
    })
    output$plot.fs <- renderPlot({
        fs.fncs()
    })
    
    output$DownloadPlotPW <- downloadHandler(
      filename = function() {
        paste("Plot.pw-", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        ggsave(piecewise.fncs(), filename = file)
      }
    )
    
    output$DownloadPlotFS <- downloadHandler(
      filename = function() {
        paste("Plot.fs-", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        ggsave(fs.fncs(), filename = file)
      }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
