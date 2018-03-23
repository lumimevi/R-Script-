library(car)
library(nlme)
require(ggplot2)
dados <- read.csv("ICU.csv")
dados
plot(dados)
names(dados)
attach(dados)
qplot(Cidade,ICU,data= dados,geom=c('boxplot'),fill=Cidade, 
      xlab="Cidades", ylab="Ilha de Calor Urbana")
# Calcule as estatísticas de resumo por Cidades - contagem, média, sd:
library(dplyr)
group_by(dados, Cidade) %>%
  summarise(
    count = n(),
    mean = mean(ICU, na.rm = TRUE),
    sd = sd(ICU, na.rm = TRUE)
  )
