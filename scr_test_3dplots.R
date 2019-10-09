
library(tidyverse)
library(plotly)

p <- plot_ly(z = ~volcano) %>% add_surface()
p


plot_ly(
  type= "cone",
  x= 1, y= 1, z= 1,
  u= 0, v= 0, w= 0
) %>%
  layout(
    scene= list(
      camera= list(
        eye= list(x= -0.76, y= 1.8, z= 0.92)
      )
    )
  )

library(ggquiver)
expand.grid(x=seq(0,pi,pi/12), y=seq(0,pi,pi/12)) %>%
  ggplot(aes(x=x,y=y,u=cos(x),v=sin(y))) +
  geom_quiver()
