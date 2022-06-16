

# --- Exploratory Data Analysis

source(here::here("rscript", "02_preparing_data.R"))


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

# report_2020 <- 
#   DBI::dbReadTable(
#     conn = my_con,
#     name = "report_2020"
#     # name = dbListTables(my_con)[1]   # nome da tabela em sqlite  
#   ) 
# 
# report_2021 <- 
#   DBI::dbReadTable(
#     conn = my_con,
#     name = "report_2021"
#     # name = dbListTables(my_con)[1]   # nome da tabela em sqlite  
#   ) 

# Restore report_2022
# report_2022 <- readRDS(here::here("data", "produced_data", "report_2022.rds"))


## Cada base se refere ao ano anterior, por exemplo, a base de 2021 é referente a 2020,
## ano que seu deu a descoberta do primeiro caso de covid 19, assim podemos observar a média da pontuação
## por ano
base_reports %>% 
  group_by(year) %>%
  summarise(média = mean(happiness_score))


# Se olharmos para a média temos um aumento dos scores ao longo dos anos inclusive durante a pandemia do novo corona virus,
# olhando os scores por região, ficamos com:
base_reports %>% 
  group_by(region) %>% 
  summarise(média = mean(happiness_score))


# Porem ao se fazer uma analise observando apenas os paises que participaram durante todos os anos tiveram uma queda grande, confirmada
# graficamente

base_reports %>% 
  select(year, country, happiness_score) %>% 
  # rbind(report_2022 %>% select(year, country, happiness_score)) %>% 
  group_by(year) %>% 
  summarise(soma  = sum(happiness_score),
            media = mean(happiness_score)) %>% 
  ggplot(aes(x=year, y = soma)) +
              geom_line() +
              geom_point() +
  theme_bw()
  

# Que pode ser confirmado graficamente
base_reports %>% 
  mutate(region = fct_reorder(region, happiness_score, median)) %>% 
  ggplot(aes(happiness_score))+
  geom_boxplot(aes(fill = region))




# Analisando a correlação de person para a base toda
base_cor <- 
  base_reports %>% 
  # filter(year == 2021) %>%  
  select(!c(country, region, year, happiness_rank)) %>% 
  cor() %>% 
  corrplot()


# Possui uma alta correlação principalmente entre Economy..GDP.per.Capita, Health
# life expectat, Family e Freedom.
base_reports %>% 
  # filter(year == 2021) %>% 
  ggplot(aes(x = economy_gdp_per_capita,
             y = happiness_score,
             color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap( ~ region,
              ncol = 3) +
  scale_colour_viridis_d()
  


# --- 

# Observando o gráfico temos que países do Western Europe, possui uma alta renda 
# que leva um aumento do indice de felicidade, se olharmos em especifico para o Brasil
# Tivemos o seguinte comportamento
base_reports %>% 
  filter(country == "Brazil") %>%
  select(year, country, happiness_score) %>% 
  # rbind(report_2022 %>% filter(country == "Brazil") %>% 
  #         select(year, country, happiness_score)) %>% 
  ggplot(aes(year, happiness_score))+
  geom_line()+
  geom_point()



# Podendo estabelecer um comparativo com os paises da Latinos americanos e caribe que o brasil está inserido.
base_reports %>% 
  filter(region == "Latin America and Caribbean") %>%
  select(year, country, happiness_score) %>% 
  # rbind(report_2022 %>% filter(region == "Latin America and Caribbean") %>% select(year, country, happiness_score)) %>% 
  ggplot(aes(year, happiness_score, color = country)) +
  geom_line() + 
  geom_point() +
  scale_colour_viridis_d()


# Analise da diferença dos scores no periodo antes pandemico no caso na base de 2020
# em comparação a 2021 e 2022:

# --- paises que fizeram uma gestao de crise
base_reports %>% filter(year %in% 2020) %>% 
  select(c(country, happiness_score, year)) %>% 
  inner_join(
    base_reports %>% filter(year %in% 2021) %>% 
      select(country, happiness_score, year), 
    by = "country") %>% 
  inner_join(
    base_reports %>% filter(year %in% 2022) %>% 
      select(country, happiness_score, year), 
    by = "country") %>% 
  group_by(country) %>% 
  summarise(
    "dif_21_20" = happiness_score.y - happiness_score.x,
    "dif_22_21" = happiness_score - happiness_score.y
  ) %>% 
  filter(dif_21_20 > 0) %>% 
  arrange(desc(dif_21_20))


# -- Paises que tiveram uma queda
base_reports %>% filter(year %in% 2020) %>% 
  select(c(country, happiness_score, year)) %>% 
  inner_join(
    base_reports %>% filter(year %in% 2021) %>% 
      select(country, happiness_score, year), 
    by = "country") %>% 
  inner_join(
    base_reports %>% filter(year %in% 2022) %>% 
      select(country, happiness_score, year), 
    by = "country") %>% 
  group_by(country) %>% 
  summarise(
    "dif_21_20" = happiness_score.y - happiness_score.x,
    "dif_22_21" = happiness_score - happiness_score.y
  ) %>% 
  filter(dif_21_20 < 0) %>% 
  arrange(dif_21_20)


# Selicionando os paises com a diferença positiva entra 2022 e 2020,2021 e 2021
gest_crise <- base_reports %>% filter(year %in% 2020) %>% 
  select(c(country, happiness_score, year)) %>% 
  inner_join(
    base_reports %>% filter(year %in% 2021) %>% 
      select(country, happiness_score, year), 
    by = "country") %>% 
  inner_join(
    base_reports %>% filter(year %in% 2022) %>%
      select(country, happiness_score, year), 
    by = "country") %>% 
  group_by(country) %>% 
  summarise(
    "dif_21_20" = happiness_score.y - happiness_score.x,
    "dif_22_21" = happiness_score - happiness_score.y
  ) %>% 
  filter(dif_21_20 > 0, dif_22_21 > 0) %>% 
  arrange(country) 
  


# Podemos olhar por exemplo para a china que apresentou um combate eficaz a
# covid 19.
base_reports %>% 
  filter(country == "China") %>%
  select(year, country, happiness_score) %>% 
  # rbind(report_2022 %>% filter(country == "China") %>% 
  #         select(year, country, happiness_score)) %>% 
  ggplot(aes(year, happiness_score))+
  geom_line()+
  geom_point()



# podemos olhar o top 3 gestao de crise entre 2020-2021
gest_crise %>% arrange(desc(dif_21_20)) %>% head(3)

base_reports %>% 
  filter(country %in% c("Armenia", "Bahrain", "Croatia")) %>%
  select(year, country, happiness_score) %>% 
  ggplot(aes(year, happiness_score, color = country))+
  geom_line() +
  geom_point() +
  scale_colour_viridis_d()



# podemos olhar o top 3 gestao de crise entre 2021-2022
gest_crise %>% arrange(desc(dif_22_21)) %>% head(3)

base_reports %>% 
  filter(country %in% c("Romania", "Mozambique", "China")) %>%
  select(year, country, happiness_score) %>% 
  ggplot(aes(year, happiness_score, color = country))+
  geom_line() +
  geom_point() +
  scale_colour_viridis_d()


#Olhando para o brasil conseguimos notar uma queda brusca no indice, principlamente
#durante o ano de pandemia, se olharmos para o ano de 2020 que leva em consideração
# dados referente ao ano de 2019 em comparação com 2021 tivemos uma queda de mais de quase
# 0.09.

#Olhando para o mundo todo por região.
base_reports %>% 
  select(year, region, happiness_score) %>% 
  group_by(region) %>% 
  # summarise(soma = sum(happiness_score)) %>% 
  ggplot(aes(year, happiness_score, color = region)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis_d()
  
  
  
# Possivel ver a queda brusca por região
base_reports %>% 
    select(year, region, happiness_score) %>% 
    drop_na() %>% 
    group_by(year, region) %>% 
    summarise(mean_score = mean(happiness_score)) %>%
    ggplot(aes(year, mean_score, color = region)) +
    geom_line() +
    geom_point() +
    scale_colour_viridis_d()



  