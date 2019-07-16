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
