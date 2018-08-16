
#Agrupar dados por mês em R#
library(dplyr)
library(lubridate)
set.seed(2017)
options(digits = 4)
dados<- read.table('midata.csv', header = TRUE, sep = ",")
names(dados)
attach(dados)
data<-mdy(DATAS)
media<-dados %>%group_by(month=floor_date(data, "month")) %>%summarize(variavel=mean(Manaus, na.rm = TRUE))
#Exportar resultados #
write.table(media, "saida.ods")
