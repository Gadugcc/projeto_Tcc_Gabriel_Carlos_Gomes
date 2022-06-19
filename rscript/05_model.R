
source(here::here("rscript", "00_packages.R"))

base_reports_sqlite <-
  here::here(
    "data",
    "produced_data",             # pasta onde sera salvo
    "base_reports.SQLITE"        # o nome que o arquivo, sempre
  )                              # espeficicando a extensao .SQLITE

# abrindo a concexao com o sqlite
my_con <- dbConnect(drv = SQLite(), base_reports_sqlite)

# --- reading the table in sqlite database
base_reports <- 
  DBI::dbReadTable(
    conn = my_con,
    name = "base_reports"
    # name = dbListTables(my_con)[1]   # nome da tabela em sqlite  
  ) 

base_reports$trust_government_corruption[is.na(base_reports$trust_government_corruption)] <- 0


#######################################################################
#                                                                     #
#                                                                     #
# CLUSTER                                                             #
#                                                                     #
#                                                                     #
#######################################################################

#Olhando para a clusterização apenas no ano de 2022

paises_cluster <- 
  base_reports %>% 
  select(-c(region, happiness_rank, year)) %>% 
  mutate(country =  str_replace(country, "\\*", "")) %>% 
  group_by(country) %>% 
  summarise(
    happiness_score = mean(happiness_score),
    economy_gdp_per_capita = mean(economy_gdp_per_capita),
    family = mean(family),
    health_life_expectancy = mean(health_life_expectancy),
    freedom = mean(freedom),
    trust_government_corruption = mean(trust_government_corruption),
    generosity = mean(generosity),
  )


paises_cluster %>% tabyl(country) %>% 
  adorn_pct_formatting() %>% 
  knitr::kable()

#Omitindo as na´s.

paises_cluster <- na.omit(paises_cluster)

#Renomeando as linhas
paises_cluster <- 
  paises_cluster %>%
  column_to_rownames( var = "country")


# Padronizando as variáveis
standardized_data <- paises_cluster |>
  mutate(across(everything(), scale))

standardized_data

# Calculando a matriz de distancia usando a distância euclidiana
distance <- 
  dist(standardized_data, 
       method = "euclidean")

# Definindo o cluster a partir do metodo escolhido no caso single linkage
hc1 <- hclust(distance, method = "single")

#Plotando o dendograma
plot(hc1, cex = 0.6, hang = -1)

#Devido a quantidade de países é dificil ver a quantidade de cluster necessario.
# Olhando para o metodo de elbow, definermos 4 cluster
fviz_nbclust(standardized_data, FUN = hcut, method = "wss") + 
  geom_vline(xintercept = 4, linetype = 2)

#Usando a interpretação para imput do método não hierarquico
cluster_nh <- kmeans(standardized_data , centers =4)

#Vizualizando os cluster
fviz_cluster(cluster_nh, data = standardized_data)


#Agrupando na base original
paises_cluster <- 
  paises_cluster %>% 
  mutate(cluster = cluster_nh$cluster)


head(paises_cluster)
tail(paises_cluster)







