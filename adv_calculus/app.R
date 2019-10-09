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
library(plotly)
library(ggquiver)

plot.scalar.field.fnc = function(.X.range, .Y.range, .FUN, ..., .resolution = 101) {
  TXs = (.X.range[2] - .X.range[1])/.resolution
  TYs = (.Y.range[2] - .Y.range[1])/.resolution
  k = seq(from = round(-.resolution/2), to = round(.resolution/2), length.out = .resolution)
  X.vector = k * TXs
  Y.vector = k * TYs
  z = outer(X.vector, Y.vector, .FUN)
  plot_ly(x = X.vector, y = Y.vector, z = z) %>% add_surface()
}

plot.contour.fnc = function(.X.range, .Y.range, .FUN, ..., .resolution = 101) {
  TXs = (.X.range[2] - .X.range[1])/.resolution
  TYs = (.Y.range[2] - .Y.range[1])/.resolution
  k = seq(from = round(-.resolution/2), to = round(.resolution/2), length.out = .resolution)
  X.vector = k * TXs
  Y.vector = k * TYs
  z = outer(X.vector, Y.vector, .FUN)
  plot_ly(x = X.vector, y = Y.vector, z = z) %>% add_contour()
}

plot.vector.field.fnc = function(.X.range, .Y.range, .x.fnc, .y.fnc, ..., .resolution = 101) {
  TXs = (.X.range[2] - .X.range[1])/.resolution
  TYs = (.Y.range[2] - .Y.range[1])/.resolution
  expand.grid(x=seq(.X.range[1],.X.range[2],TXs), 
              y=seq(.Y.range[1],.Y.range[2],TYs)) %>%
    mutate(u = .x.fnc(x,y), v = .y.fnc(x,y), magnitud = sqrt(u^2+v^2)) %>%
    ggplot(aes(x=x,y=y,u=u,v=v, colour = magnitud)) +
    geom_quiver() + theme_classic()
}

plot.param.curves.fnc = function(.t.range, .x.fnc, .y.fnc, .z.fnc, ..., .resolution = 101) {
  Ts = (.t.range[2] - .t.range[1])/.resolution
  k = seq(from = round(-.resolution/2), to = round(.resolution/2), length.out = .resolution)
  t.vector = k * Ts
  X = .x.fnc(t.vector)
  Y = .y.fnc(t.vector)
  Z = .z.fnc(t.vector)
  plot_ly(x = X, y = Y, z = Z, type = 'scatter3d', mode = 'lines',
          line = list(width = 6, color = t.vector, colorscale = 'Viridis'))
}

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




ui <- navbarPage("Advanced Calculus",
                 tabPanel("Scalar fields",
                  sidebarLayout(
                    sidebarPanel(
                      textAreaInput("sf.capt.func", "Insert expression for function z=f(x,y) here:", "function(x,y) x*sin(y)", width = "500px"),
                      numericInput("sf.x.range.from", "X range from:", -5),
                      numericInput("sf.x.range.to", "To:", 8),
                      numericInput("sf.y.range.from", "Y range from:", -5),
                      numericInput("sf.y.range.to", "To:", 8),
                      sliderInput("sf.resolution", label = "Graph resolution:",
                                  min = 20, max = 1000, value = 100, step = 20),
                      actionButton("sf.go", "Go")
                    ),        
                    mainPanel(
                      plotlyOutput("plot.sf")
                    )        
                  ),
                          
                          
                 ),
                 tabPanel("Contours",
                  sidebarLayout(
                    sidebarPanel(
                      textAreaInput("co.capt.func", "Insert expression for function z=f(x,y) here:", "function(x,y) x*sin(y)", width = "500px"),
                      numericInput("co.x.range.from", "X range from:", -5),
                      numericInput("co.x.range.to", "To:", 8),
                      numericInput("co.y.range.from", "Y range from:", -5),
                      numericInput("co.y.range.to", "To:", 8),
                      sliderInput("co.resolution", label = "Graph resolution:",
                                  min = 20, max = 1000, value = 100, step = 20),
                      actionButton("co.go", "Go")
                    ),
                    mainPanel(
                      plotlyOutput("plot.co")
                    )
                  ),
                          
                          
                 ),
                 tabPanel("2D vector fields",
                  sidebarLayout(
                    sidebarPanel(
                      textAreaInput("vf.u.capt.func", "Assuming F=u(x,y)i + v(x,y)j. Insert expression for function u(x,y) here:", "function(x,y) x*sin(y)", width = "500px"),
                      textAreaInput("vf.v.capt.func", "... and expression for function v(x,y) here:", "function(x,y) x*cos(y)", width = "500px"),
                      numericInput("vf.x.range.from", "X range from:", -5),
                      numericInput("vf.x.range.to", "To:", 8),
                      numericInput("vf.y.range.from", "Y range from:", -5),
                      numericInput("vf.y.range.to", "To:", 8),
                      sliderInput("vf.resolution", label = "Graph resolution:",
                                  min = 5, max = 50, value = 10, step = 5),
                      actionButton("vf.go", "Go")
                    ),
                    mainPanel(
                      plotOutput("plot.vf")
                    )
                  ),
                          
                          
                 ),
                 tabPanel("Parametric curves",
                  sidebarLayout(
                    sidebarPanel(
                      numericInput("pc.t.range.from", "t range from:", 0),
                      numericInput("pc.t.range.to", "To:", 3),
                      textAreaInput("pc.x.capt.func", "Insert expression for function x(t) here:", "function(t) t", width = "500px"),
                      textAreaInput("pc.y.capt.func", "Insert expression for function y(t) here:", "function(t) sin(pi*t)", width = "500px"),
                      textAreaInput("pc.z.capt.func", "Insert expression for function z(t) here:", "function(t) t^2", width = "500px"),
                      sliderInput("pc.resolution", label = "Graph resolution:",
                                  min = 20, max = 1000, value = 100, step = 20),
                      actionButton("pc.go", "Go")
                    ),
                    mainPanel(
                      plotlyOutput("plot.pc")
                    )
                  ),
                          
                          
                 ),
                 tabPanel("Piecewise functions",
                  sidebarLayout(
                    sidebarPanel(
                      textAreaInput("capt.func", "Insert expression for function f(t) here:", "function(t) ifelse(t<=-3, -t-3, ifelse(t>-3 & t<=0, t+3, ifelse(t>0 & t<=3, -2*t+3, t-6)))", width = "500px"),
                      numericInput("piece.range.from", "Piece range from:", -5),
                      numericInput("piece.range.to", "To:", 8),
                      sliderInput("pieces", label = "Pieces:",
                                  min = -5, max = 5, value = c(-1,1), step = 1),
                      sliderInput("resolution", label = "Graph resolution:",
                                  min = 20, max = 1000, value = 100, step = 20),
                      actionButton("go", "Go"),
                    
                    ),
                    mainPanel(
                      plotOutput("plot.pw"),
                      downloadButton("DownloadPlotPW", "Download")
                      
                    )
                  ),
                 ),
                 tabPanel("Fourier series",
                  sidebarLayout(
                    sidebarPanel(
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
                      actionButton("fs.go", "Go")
                    ),
                    mainPanel(
                      plotOutput("plot.fs"),
                      downloadButton("DownloadPlotFS", "Download")
                    )
                  )        
                          
                 ),        
                tabPanel("About",
                  titlePanel("Advanced Calculus App"),
                  h3("Authors"),
                  br(),
                  p("Ivan Olier"),
                  p("Muireann Settle"),
                  br(),
                  p("Liverpool John Moores University"),
                  p("Departament of Applied Mathematics"),
                  p("This app is aimed at supporting the 6109MATHS - Advanced Calculus module."),
                  br(),
                  p("Version 0.1, 2019")
                )
)

server <- function(input, output) {
  scalar.field.fncs <- eventReactive(input$sf.go, {
    sf.fn = eval(parse(text = input$sf.capt.func))
    plot.scalar.field.fnc(.X.range = c(input$sf.x.range.from, input$sf.x.range.to), 
                          .Y.range = c(input$sf.y.range.from, input$sf.y.range.to),
                          .FUN = sf.fn, 
                          .resolution = input$sf.resolution)
  })
  
  output$plot.sf <- renderPlotly({
    scalar.field.fncs()
  })
  
  contour.fncs <- eventReactive(input$co.go, {
    co.fn = eval(parse(text = input$co.capt.func))
    plot.contour.fnc(.X.range = c(input$co.x.range.from, input$co.x.range.to), 
                          .Y.range = c(input$co.y.range.from, input$co.y.range.to),
                          .FUN = co.fn, 
                          .resolution = input$co.resolution)
  })
  
  output$plot.co <- renderPlotly({
    contour.fncs()
  })
  
  vector.field.fncs <- eventReactive(input$vf.go, {
    vf.u.fn = eval(parse(text = input$vf.u.capt.func))
    vf.v.fn = eval(parse(text = input$vf.v.capt.func))
    plot.vector.field.fnc(.X.range = c(input$vf.x.range.from, input$vf.x.range.to), 
                          .Y.range = c(input$vf.y.range.from, input$vf.y.range.to),
                          .x.fnc = vf.u.fn, 
                          .y.fnc = vf.v.fn,
                          .resolution = input$vf.resolution)
  })
  
  output$plot.vf <- renderPlot({
    vector.field.fncs()
  })  
  
  param.curves.fncs <- eventReactive(input$pc.go, {
    pc.x.fn = eval(parse(text = input$pc.x.capt.func))
    pc.y.fn = eval(parse(text = input$pc.y.capt.func))
    pc.z.fn = eval(parse(text = input$pc.z.capt.func))
    
    plot.param.curves.fnc(.t.range = c(input$pc.t.range.from, input$pc.t.range.to),
                          .x.fnc = pc.x.fn, 
                          .y.fnc = pc.y.fn,
                          .z.fnc = pc.z.fn,
                          .resolution = input$pc.resolution)
  })
  
  output$plot.pc <- renderPlotly({
    param.curves.fncs()
  })
  
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