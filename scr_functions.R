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
  ft = ft + a0/2
  data.plot = tibble(time = t.vector, Ft = ft)
  ggplot(data = data.plot, aes(x = time, y = Ft)) + geom_line() + theme_classic()
}

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

# plot.vector.field.fnc = function(.X.range, .Y.range, .Z..FUN, ..., .resolution = 101) {
#   TXs = (.X.range[2] - .X.range[1])/.resolution
#   TYs = (.Y.range[2] - .Y.range[1])/.resolution
#   k = seq(from = round(-.resolution/2), to = round(.resolution/2), length.out = .resolution)
#   X.vector = k * TXs
#   Y.vector = k * TYs
#   z = outer(X.vector, Y.vector, .FUN)
#   plot_ly(x = X.vector, y = Y.vector, z = z) %>% add_surface()
# }

library(ggquiver)
plot.vector.field.fnc = function(.X.range, .Y.range, .x.fnc, .y.fnc, ..., .resolution = 101) {
  TXs = (.X.range[2] - .X.range[1])/.resolution
  TYs = (.Y.range[2] - .Y.range[1])/.resolution
  expand.grid(x=seq(.X.range[1],.X.range[2],TXs), 
              y=seq(.Y.range[1],.Y.range[2],TYs)) %>%
    mutate(u = .x.fnc(x), v = .y.fnc(y), magnitud = sqrt(u^2+v^2)) %>%
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
