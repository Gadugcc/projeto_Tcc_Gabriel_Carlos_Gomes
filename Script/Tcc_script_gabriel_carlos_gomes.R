library(readr)
library(tidyverse)
library(corrplot)
library(plotly)
library(rgdal)
library(knitr)
library(tmap)
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)


### Lendo a base de dados.
report_2015 <- read.csv("Bases-csv/2015.csv")
report_2016 <- read.csv("Bases-csv/2016.csv")
report_2017 <- read.csv("Bases-csv/2017.csv")
report_2018 <- read.csv("Bases-csv/2018.csv")
report_2019 <- read.csv("Bases-csv/2019.csv")
report_2020 <- read.csv("Bases-csv/2020.csv")
report_2021 <- read.csv("Bases-csv/2021.csv")
report_2022 <-  read_csv("Bases-csv/2022.csv", 
                         col_types = cols(`Happiness score` = col_number()))



## Será usada as seguintes colunas - Overall Rank, score, Gdp per capita, Social suport
# Expectativa de vida, Liberdade para fazer escolhas, Generosidade, corruption, Region

#2015
report_2015 <- report_2015 %>% select(c("Country","Region",
                                        "Happiness.Rank",
                                        "Happiness.Score",
                                        "Economy..GDP.per.Capita.",
                                        "Health..Life.Expectancy.",
                                        "Family","Freedom",
                                        "Trust..Government.Corruption.",
                                        "Generosity"))

report_2015 <- report_2015 %>% 
  mutate(Year=2015) #Acrescentando uma coluna referente ao ano

#2016

report_2016 <- report_2016 %>% 
  select(c("Country",
           "Region",
           "Happiness.Rank",
           "Happiness.Score",
           "Economy..GDP.per.Capita.",
           "Health..Life.Expectancy.",
           "Family","Freedom",
           "Trust..Government.Corruption.",
           "Generosity"))

report_2016 <- report_2016 %>% 
  mutate(ano=2016) %>% 
  rename("Year"=11)


# 2017 

report_2017 <- report_2017 %>% 
  select(-c("Dystopia.Residual",
            "Whisker.high",
            "Whisker.low"))

# 2017 não possui a coluna Region, nesse caso, foi criada uma base que servirá 
# para realizar um inner join 

base_regiao <- report_2015 %>% 
  select(c("Country",
           "Region")) #Foi selecionada 2015 pois é a base
# que abrange mais paises

report_2017 <- left_join(x=report_2017
                         ,y=base_regiao,
                         by="Country")

report_2017 <- report_2017 %>% 
  mutate(Year=2017)


#2018

report_2018 <- report_2018 %>% 
  rename("Country"=2)

report_2018 <- left_join(x=report_2018,
                         y=base_regiao,
                         by="Country")

report_2018 <- report_2018 %>% 
  mutate("Year"=2018)

report_2018 <- report_2018 %>% 
  rename("Happiness.Rank"=1,
         "Happiness.Score"=3,
         "Economy..GDP.per.Capita."=4,
         "Family"=5,
         "Freedom"=7,
         "Trust..Government.Corruption."=9,
         "Health..Life.Expectancy."=6)

#2019

report_2019 <- report_2019 %>% 
  rename("Country"=2)

report_2019 <- left_join(x=report_2019,
                         y=base_regiao,by="Country")

report_2019 <- report_2019 %>% 
  mutate("Year"=2019)

report_2019 <- report_2019 %>% 
  rename("Happiness.Rank"=1,
         "Happiness.Score"=3,
         "Economy..GDP.per.Capita."=4,
         "Family"=5,
         'Health..Life.Expectancy.'=6,
         "Freedom"=7,
         "Trust..Government.Corruption."=9)


#2020

report_2020 <- report_2020 %>% 
  select(c("Country.name",
           "Regional.indicator",
           "Logged.GDP.per.capita",
           "Social.support",
           "Healthy.life.expectancy",
           "Ladder.score",
           "Perceptions.of.corruption",
           "Ladder.score","Generosity","Freedom.to.make.life.choices"))

report_2020 <- report_2020 %>%
  mutate(year = 2020,
         happiness.rank = c(1:153))

report_2020 <- report_2020 %>% 
  rename("Country"=1,
         "Region"=2,
         "Economy..GDP.per.Capita."=3,
         "Family"=4,
         "Health..Life.Expectancy."=5,
         "Happiness.Score"=6,
         "Trust..Government.Corruption."=7,"Freedom"=9,"Year"=10,"Happiness.Rank"=11)


# 2021

report_2021 <- report_2021 %>%
  mutate("Country"=1)

report_2021 <- report_2021 %>% 
  select(c("ï..Country.name",
           "Regional.indicator",
           "Logged.GDP.per.capita",
           "Social.support",
           "Healthy.life.expectancy",
           "Ladder.score",
           "Perceptions.of.corruption",
           "Ladder.score",
           "Generosity",
           "Freedom.to.make.life.choices"))


report_2021 <- report_2021 %>% 
  mutate(Year = 2021, 
         happiness.rank=c(1:149))

report_2021 <- report_2021 %>% rename("Country"=1,
                                      "Region"=2,
                                      "Economy..GDP.per.Capita."=3,
                                      'Family'=4,
                                      "Health..Life.Expectancy."=5,
                                      "Happiness.Score"=6,
                                      "Trust..Government.Corruption."=7,
                                      "Freedom"=9,
                                      'Year'=10,
                                      "Happiness.Rank"=11)
#2022

report_2022 <- report_2022[1:146,]

report_2022$`Happiness score` <- (report_2022$`Happiness score`)/1000

report_2022 <- report_2022 %>% 
  mutate(Year=2022)

report_2022 <- left_join(x=report_2022,
                         y=base_regiao,
                         by="Country")

# Agrupar os dados em uma base só.

base <- rbind(report_2015,
              report_2016,
              report_2017,
              report_2018,
              report_2019,
              report_2020,
              report_2021)

base$Trust..Government.Corruption. <- as.numeric(base$Trust..Government.Corruption.)

## Cada base se refere ao ano anterior, por exemplo, a base de 2021 é referente a 2020,
## ano que seu deu a descoberta do primeiro caso de covid 19, assim podemos observar a média da pontuação
## por ano

base %>% group_by(Year) %>%
  summarise(média = mean(Happiness.Score))


report_2021 %>% 
  summarise(média = mean(report_2022$`Happiness score`))

report_2022 %>% 
  summarise(média = mean(report_2022$`Happiness score`))

report_2020 %>%
  summarise(média = mean(report_2020$Happiness.Score))

# Se olharmos para a média temos um aumento dos scores ao longo dos anos inclusive durante a pandemia do novo corona virus,
# olhando os scores por região, ficamos com:

base %>% group_by(Region) %>% 
  summarise(média = mean(Happiness.Score))

# Que pode ser confirmado graficamente

base %>% 
  mutate(Region= fct_reorder(Region,Happiness.Score,median)) %>% 
  ggplot(aes(Happiness.Score))+
  geom_boxplot(aes(fill=Region))

## Analisando a correlação de person para o ano de 2021

base_cor <- base%>% 
  filter(Year==2021) %>%  
  select(-c(Country,Region,Year,Happiness.Rank))

base_cor <- na.omit(base_cor)

cor <- cor(base_cor)

corrplot(cor)



# Possui uma alta correlação principalmente entre Economy..GDP.per.Capita, Health
# life expectat, Family e Freedom.

base %>% filter(Year==2021) %>% 
  ggplot(aes(x=Economy..GDP.per.Capita.,y=Happiness.Score,color=Region)) +
  facet_grid(~Region)+geom_point()+geom_smooth(method = "lm")

#Observando o gráfico temos que países do Western Europe, possui uma alta renda 
#que leva um aumento do indice de felicidade, se olharmos em especifico para o Brasil
# Tivemos o seguinte comportamento

x <- base %>% filter(Country=="Brazil") %>% 
  group_by(Year) %>% select(c(Happiness.Score))

y <- report_2022 %>% 
  filter(Country=="Brazil") %>% 
  group_by(Year) %>%
  select(`Happiness score`)

y <- y %>% rename("Happiness.Score"=2)                

brasil <- rbind(x,y)        

brasil %>%
  ggplot(aes(Year,Happiness.Score))+
  geom_line()+
  geom_point() 

# Podendo estabelecer um comparativo com os paises da Latinos americanos e caribe que o brasil está inserido.

ameri_cariben <- base %>% 
  filter(Region== "Latin America and Caribbean") %>% 
  group_by(Year) %>% 
  select(c(Country,Happiness.Score))

ameri_cariben_2022 <- report_2022 %>% 
  filter(Region == "Latin America and Caribbean") %>% 
  select(c(Country,`Happiness score`,Year)) %>%
  rename("Happiness.Score"=2)

ameri_cariben <- rbind(ameri_cariben,ameri_cariben_2022)

ggplotly(ameri_cariben %>%
           ggplot(aes(Year,Happiness.Score,color = Country))+
           geom_line()+
           geom_point())

#Analise da diferença dos scores no periodo antes pandemico no caso na base de 2020
# em comparação a 2021 e 2022

dif_score <- base %>%
  filter(Year==2020) %>% 
  select(c(Country,Happiness.Score)) %>%
  rename("Score_2020"=2)

score_2021 <- base %>%
  filter(Year ==2021) %>% 
  select(c(Country,Happiness.Score)) %>%
  rename("Score_2021"=2)

score_2022 <- report_2022 %>%
  select(c(Country,`Happiness score`)) %>%
  rename("Score_2022"=2)

dif_score <- inner_join(dif_score,
                        score_2021,
                        by= "Country")

dif_final <- inner_join(dif_score,
                        score_2022,
                        by = "Country")

dif_final <- dif_final %>% 
  mutate("dif" = Score_2022- Score_2020,
         "dif_2021_2020" = Score_2021 - Score_2020)

#  paises que fizeram uma gestão de crise

dif_final %>% filter(dif>0) 

# Paises que tiveram uma queda

dif_final %>% filter(dif<0)

# Selicionando os paises com a diferença positiva entra 2022 e 2020,2021 e 2021

gest_crise <- dif_final %>% 
  filter(dif>0,
         dif_2021_2020>0)
# Podemos olhar por exemplo para a china que apresentou um combate eficaz a
# covid 19.

china <-base %>% 
  filter(Country=="China") %>%
  select(c(1,3,4,11))

china_2022 <- report_2022 %>% 
  filter(name=="China") %>% 
  select(c(1,2,3,13)) %>% 
  rename("Happiness.Rank"=1,'Country'=2,"Happiness.Score"=3,"Year"=4)

china <- rbind(china,china_2022)

ggplotly(  china %>% ggplot(aes(Year,Happiness.Score)) + 
             geom_point() +
             geom_line())

#

#Olhando para o brasil conseguimos notar uma queda brusca no indice, principlamente
#durante o ano de pandemia, se olharmos para o ano de 2020 que leva em consideração
# dados referente ao ano de 2019 em comparação com 2021 tivemos uma queda de mais de quase
# 0.09.

#Olhando para o mundo todo por região.

x1 <- base %>% 
  group_by(Region) %>%
  select(c(Year,Happiness.Score))

y2 <- report_2022 %>% 
  group_by(Region) %>% 
  select(c(Year,`Happiness score`))

y2 <- y2 %>%
  rename("Year"=2)

y2 <- y2 %>% 
  rename("Happiness.Score"=3)

regiao <- rbind(x1,y2)

regiao  %>% summarise(soma=sum(Happiness.Score))

regiao %>% ggplot(aes(Year,
                      Happiness.Score,
                      color=regiao$Region))+
  geom_line()+
  geom_point()



t1 <- regiao %>% 
  filter(Year ==2015) %>% 
  group_by(Region) %>% 
  summarise(média = mean(Happiness.Score)) 

t1 <- t1 %>%
  mutate(Year =2015)

t2 <-  regiao %>%
  filter(Year ==2016) %>% 
  group_by(Region) %>%
  summarise(média = mean(Happiness.Score)) %>%
  mutate(Year = 2016)

t3 <- regiao %>%
  filter(Year ==2017) %>% 
  group_by(Region) %>% 
  summarise(média = mean(Happiness.Score)) %>%
  mutate(Year = 2017)

t4 <- regiao %>% 
  filter(Year ==2018) %>% 
  group_by(Region) %>% 
  summarise(média = mean(Happiness.Score)) %>%
  mutate(Year = 2018)

t5 <- regiao %>%
  filter(Year ==2019) %>%
  group_by(Region) %>%
  summarise(média = mean(Happiness.Score)) %>%
  mutate(Year = 2019)

t6 <- regiao %>%
  filter(Year ==2020) %>% 
  group_by(Region) %>%
  summarise(média = mean(Happiness.Score)) %>% 
  mutate(Year = 2020)

t7 <- regiao %>% 
  filter(Year ==2021) %>%
  group_by(Region) %>%
  summarise(média = mean(Happiness.Score)) %>%
  mutate(Year = 2021)

t8 <- regiao %>%
  filter(Year ==2022) %>% 
  group_by(Region) %>% 
  summarise(média = mean(Happiness.Score)) %>% 
  mutate(Year = 2022)

t_final <- rbind(t1,
                 t2,
                 t3,
                 t4,
                 t5,
                 t6,
                 t7,
                 t8) %>% na.omit()

# Possivel ver a queda brusca por região

ggplotly( t_final %>%
            ggplot(aes(Year,média,color = Region))+
            geom_line()+
            geom_point())


# Obervando espacialmente.

shp_mundo <- readOGR(dsn = "Shape.file/shapefile_mundo",
                     layer = "mundo")

report_2022  <- report_2022 %>% 
  rename("name"=2)

dados_2022_mundo <- merge(x=shp_mundo,
                          y=report_2022,
                          by.x = "name",
                          by.y = "name")  



tm_shape(dados_2022_mundo) + tm_fill(col = "Happiness score")



# Analisando os top 10 paises durante o tempo

top_2015 <- report_2015[1:10,] %>% 
  select(c(1,2,3,4,11))

top_2016 <- report_2016[1:10,] %>% 
  select(c(1,2,3,4,11))

top_2017 <- report_2017[1:10,] %>% 
  select(c(1,2,3,10,11))

top_2018 <- report_2018[1:10,] %>% 
  select(c(1,2,3,10,11))

top_2019 <- report_2019[1:10,] %>% 
  select(c(1,2,3,10,11))

top_2020 <- report_2020[1:10,] %>% 
  select(c(1,2,6,10,11))

top_2021 <- report_2021[1:10,] %>% 
  select(c(1,2,10,11,6))

top_2022 <- report_2022[1:10,] %>% 
  select(c(1,2,3,14,13)) %>% rename("Happiness.Rank"=1,
                                    "Happiness.Score"=3)

top_10 <- rbind(top_2015,
                top_2016,
                top_2017,
                top_2018,
                top_2019,
                top_2020,
                top_2021,
                top_2022)


top_10[76,1] <- "Luxembourg"

table(top_10$Country)

# Se olhar considerando que nossa base considera 8 anos, desses 8 anos paises como 
# Suiça, Finlandia, Islandia, Holanda, Nova zelandia, Noruega, Dinamarca, Suecia,
# apareceram em todos os top 10 de paises mais felizes.

ggplotly( top_10 %>% 
            ggplot(aes(Year,Happiness.Score,color = Country)) + 
            geom_point()+
            geom_line())


#Outro ponto interessante que luxemburgo que aparece mais de uma vez era esperado
# pois é o pais com mais renda per capita no mundo, sendo a mesma a variável que 
# mais influência essa pontuação.

#################################################################################
#
#
# CLUSTERIZAÇÃO. 
#
################################################################################




#Removendo as colunas que não sera utilizada

report_2022_cluster <- report_2022 %>% 
  select(-c(RANK,Year,Region,
            `Whisker-high`,
            `Whisker-low`,
            `Dystopia (1.83) + residual`))

#Mudando o nome das linhas para não ter variaveis quali

rownames(report_2022_cluster) <- report_2022_cluster[,1]


