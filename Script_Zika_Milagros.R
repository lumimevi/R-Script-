############# EDA - zika ###############
## Lourdes M.M. Villavicencio.
########################################
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(rworldmap)
library(tidyr)

#### dados cdc_zika ####
setwd("~/Milagros/cursosR/cdc_zika.csv")
zika <- read.csv('cdc_zika.csv',stringsAsFactors = F, header = T)
class(zika)
dim(zika)
colnames(zika)
structure(zika)
str(zika)
summary(zika)

#### pre-processamento ####
zika <- zika %>%
separate(col = "report_date", into = c("year", "month"), sep = "-") %>%
separate(col = "location", into = c("country", "state"), sep = "-") 


zika$value <- as.character(zika$value)
zika$value <- as.numeric(zika$value,na.rm=TRUE)
#organizando os meses
zika$month_name <- ""
zika$month_name[which(zika$month == "01")] <- "jan"
zika$month_name[which(zika$month == "02")] <- "fev"
zika$month_name[which(zika$month == "03")] <- "mar"
zika$month_name[which(zika$month == "04")] <- "abr"
zika$month_name[which(zika$month == "05")] <- "mai"
zika$month_name[which(zika$month == "06")] <- "jun"
zika$month_name[which(zika$month == "07")] <- "jul"
zika$month_name[which(zika$month == "11")] <- "nov"
zika$month_name[which(zika$month == "12")] <- "dez"
View(zika) #veja a tabela

zika$confirmed <- ifelse(grepl('confirmed', zika$data_field), TRUE, FALSE)
zika$mc <- ifelse(grepl('microcephaly', zika$data_field), TRUE, FALSE)

#### exerc.1 #####

#Mostre, atraves de grafico de barras, os paises com casos confirmados com zika 
country_data <- zika %>%
filter(confirmed == T & unit == 'cases' & !is.na(value)) %>%
group_by(country) %>%
summarise(num_cases = sum(value,na.rm = T)) %>%
arrange(desc(num_cases))
View(country_data)
#grafico 
library(scales)
ggplot(data=country_data,
     aes(x=reorder(country, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(country, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 12000) + 
  labs(x = "Paises", 
       y = "Numero de casos", 
       title = "Numero de casos confirmado de Zika por paÌs") +
  scale_fill_continuous(name= "numero de casos", labels=comma) +
  scale_y_continuous(labels =  comma) 
 
#### exerc.2 #####
# Construa 2 graficos de barras que mostre, por mes, as incidencias de Zika nos paises onde houve maior # incidencia de Zika no ano de 2016. 
#Colombia
colombia_2016 <- zika %>%
filter(confirmed == T & unit == 'cases' & !is.na(value)) %>%
select(year, month, country, value, month_name) %>%
filter(country == "Colombia") %>%
group_by(month, month_name) %>% 
summarise(num_cases = sum(value,na.rm = T)) %>%
arrange(desc(num_cases))
 
#Grafico Colombia
ggplot(data=colombia_2016,
        aes(x=reorder(month_name, desc(num_cases)),
            y=num_cases,
            fill=num_cases))+
   geom_bar(stat='identity') +
   geom_text(aes(x = reorder(month_name, desc(num_cases)), 
                 y = num_cases, label = num_cases), nudge_y = 12000) + 
   labs(x = "Meses", 
        y = "Numero de casos", 
        title = "Numero de casos confirmado de Zika na Colombia") +
   scale_fill_continuous(name= "numero de casos", labels=comma) +
   scale_y_continuous(labels =  comma) 
 

#Brasil
Brasil2016 <- zika %>%
  filter(confirmed == T & unit == 'cases' & !is.na(value)) %>%
  select(year, month, country, value, month_name) %>%
  filter(country == "Brazil") %>%
  group_by(month, month_name) %>% 
  summarise(num_cases = sum(value,na.rm = T)) %>%
  arrange(desc(num_cases))


#Grafico Brasil
ggplot(data=Brasil2016,
       aes(x=reorder(month_name, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(month_name, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 900) + 
  labs(x = "Meses", 
       y = "Numero de casos", 
       title = "Numero de casos confirmado de Zika no Brasil") +
  scale_fill_continuous(name= "numero de casos", labels=comma) +
  scale_y_continuous(labels =  comma) 

#### exerc.3 #####
# Mostre, atraves de Box Plot, os nuneros de casos por regi√£o no Brasil. 
# Use os estados como sendo as amostras, para cada boxplot.

region<- zika%>%
select(country, year, value,month, month_name)%>%
filter(country %in% c("Norte","Nordeste","Sudeste","Sul","Centro"))%>%
group_by(year,month, month_name, country) %>% 
summarise(num_cases = sum(value,na.rm  = T)) %>%
arrange(desc(num_cases))  

ggplot(region, aes(x=country, y=num_cases)) + 
geom_boxplot( colour = "black", fill = "#56B4E9") + 
labs(title="BoxPlot de casos de Zika por Regi„o do Brasil",x="Regi„o", y = "Numero de casos")
 

#### exerc.4 #####
# Crie um grafico de barras que mostre a incidencia total de casos confirmados no brasil por estado e por mes. 


BR_uf <- zika %>%
filter(confirmed == T & unit == 'cases' & !is.na(value)) %>%
select(year, month, month_name,state, country, state, value) %>%
filter(country == "Brazil") %>%
group_by(month_name, month,state) %>% 
drop_na(state) %>%
summarise(num_cases = sum(value,na.rm  = T)) %>%
arrange(desc(num_cases))

ggplot(BR_uf , 
       aes(x = month_name, y = num_cases, fill = state)) +
      geom_bar(stat='identity') + 
      labs(x = "Mes", 
       y = "Numero de casos", 
       title = "Numero de casos confirmados com Zika por estados do Brasil")
#Densidade
BR_uf %>% 
  ggplot(aes(x = num_cases)) +
  geom_density(color = "red", fill = "green")

 ###    
BR_uf %>% 
  ggplot(aes(x = num_cases))+
  geom_density(aes(fill = month_name),
               # adicionar transparencia
               alpha = 0.5 )


#### exerc.5 #####
# Calcule a m√©dia do aparecimento de Zika no brasil no ano de 2016, em cada estado brasileiro reportado # na base de dados do Zika Virus. Calcule, atrav√©s desses valores, a m√©dia de casos por 100 mil habitantes em cada estado.

mean_uf <- zika %>%
    select(year, month, country, state, value) %>%
    filter(country == "Brazil") %>%
    group_by(state) %>% 
    drop_na(state) %>%
    summarise(avg_state = round(mean(value, na.rm = T), digits = 0)) %>%
    mutate(avg_pop = avg_state/100000)
mean_uf

## Aplicado um test- t para duas amostras independentes
## Nordeste e Sudeste
## H0: as medias s„o iguais
## H1: as medias s„o deiferentes
## rejeitar para um p-vlor<0.05

Acre<- zika%>%
select(state, year, value,month, month_name)%>%
filter(state == "Acre" ) %>%
group_by(year,month, month_name, state) %>% 
summarise(num_cases = sum(value,na.rm  = T))%>%
arrange(desc(num_cases))

Alagoas<- zika%>%
  select(state, year, value,month, month_name)%>%
  filter(state == "Alagoas" ) %>%
  group_by(year,month, month_name, state) %>% 
  summarise(num_cases = sum(value,na.rm  = T))%>%
  arrange(desc(num_cases))

dados <- data.frame( resposta = c(Acre$num_cases,Alagoas$num_cases), 
  grupos=c(rep("Acre", 6), rep("Alagoas", 6)))

teste_t <- t.test(resposta ~ grupos, data=dados, var.equal=TRUE)
teste_t

