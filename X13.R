setwd("~/Desktop/nome da pasta")
Sys.setenv(x13_PATH='/home/milagros/Desktop/nome da pasta')
install.packages('seasonal')
require(seasonal)
checkX13()
pim <- read.csv("Dados.csv")
pim
pim.ts <- ts(pim, start = c(1998,1), end=2016,freq=12)
plot(pim.ts)
monthplot(pim.ts, col.base = 2, lty.base = 2, labels = month.abb)
ajuste <- seas(pim.ts)
ajuste
