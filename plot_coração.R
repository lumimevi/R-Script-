library(ggplot2)
library(dplyr)
## Criando um data frame 
data = data_frame(
  t = seq(0, 2*pi, pi/60), 
  x = 16*sin(t)^3, 
  y = 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t), 
  x2 = 3*sqrt(2)*cos(t)/(sin(t)^2 + 1),
  y2 = 20 + 4*sqrt(2)*cos(t)*sin(t)/(sin(t)^2 + 1))

## Criando plot ##
plot = ggplot(data = data, aes(x, y)) + 
  geom_path(aes(group = 1)) +
  geom_polygon(aes(group = 1), fill = "#8856a7") +
  geom_text(aes(x = 0, y = 0, label = "Feliz dia dos namorados"), 
            size = 10, colour = "white")+
  geom_text(aes(x = 0, y = 15, label = "R-Ladies Natal"), 
            size = 8, colour = "#8856a7")

plot
