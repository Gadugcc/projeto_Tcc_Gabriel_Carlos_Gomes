
source(here::here("rscript", "01_importing_data.R"))


## Será usada as seguintes colunas - Overall Rank, score, Gdp per capita, Social suport
# Expectativa de vida, Liberdade para fazer escolhas, Generosidade, corruption, Region

# todas as colunas tem nomes em letras minusculas


# --- ajustando o codigo

report_2015 <-      # ao inves de selecior as variaveis desejadas
  report_2015 %>%   # estou "excluindo" as indesejadas.
  select(!c(standard_error, dystopia_residual)) %>% 
  mutate(year = 2015) # Acrescentando uma coluna referente ao ano


report_2016 <- 
  report_2016 %>% 
  select(!c(upper_confidence_interval, 
            lower_confidence_interval, dystopia_residual)) %>% 
  mutate(year = 2016) %>% 
  select(country, region, happiness_rank, happiness_score,
         economy_gdp_per_capita, family, health_life_expectancy,
         freedom, trust_government_corruption, generosity, year)


report_2017 <- 
  report_2017 %>% 
  select(!c(dystopia_residual, whisker_low, whisker_high)) %>% 
  mutate(year = 2017) %>%   # 2017 não possui a coluna Region
  left_join(report_2015 %>% # realizar um inner join com 2015 
      select(c(country, region)), by="country") %>% # que abrange mais paises
  select(country, region, happiness_rank, happiness_score,
         economy_gdp_per_capita, family, health_life_expectancy,
         freedom, trust_government_corruption, generosity, year)

report_2018 <- 
  report_2018 %>% 
  mutate(year = 2018) %>% 
  rename("Happiness.Rank"                = overall_rank,
         "Country"                       = country_or_region,
         "Happiness.Score"               = score,
         "Economy..GDP.per.Capita."      = gdp_per_capita,
         "Family"                        = social_support,
         "Freedom"                       = freedom_to_make_life_choices,
         "Trust..Government.Corruption." = perceptions_of_corruption,
         "Health..Life.Expectancy."      = healthy_life_expectancy) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names) %>% 
  left_join(report_2015 %>% select(c(country, region)), by="country") %>% 
  select(country, region, happiness_rank, happiness_score,
         economy_gdp_per_capita, family, health_life_expectancy,
         freedom, trust_government_corruption, generosity, year)



report_2019 <- 
  report_2019 %>% 
  mutate("Year"=2019) %>% 
  rename("Happiness.Rank"                = overall_rank,
         "Country"                       = country_or_region,
         "Happiness.Score"               = score,
         "Economy..GDP.per.Capita."      = gdp_per_capita,
         "Family"                        = social_support,
         "Freedom"                       = freedom_to_make_life_choices,
         "Trust..Government.Corruption." = perceptions_of_corruption,
         "Health..Life.Expectancy."      = healthy_life_expectancy) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names) %>% 
  left_join(report_2015 %>% select(c(country, region)), by="country") %>% 
  select(country, region, happiness_rank, happiness_score,
         economy_gdp_per_capita, family, health_life_expectancy,
         freedom, trust_government_corruption, generosity, year)




report_2020 <- report_2020 %>% 
  mutate(year = 2020, happiness.rank = row.names(report_2020)) %>% 
  select(happiness.rank, country_name, regional_indicator, 
         logged_gdp_per_capita, social_support, 
         healthy_life_expectancy, ladder_score, 
         perceptions_of_corruption, year, generosity, 
         freedom_to_make_life_choices) %>% 
  rename("Country"= country_name,
         "Region"= regional_indicator,
         "Economy..GDP.per.Capita."= logged_gdp_per_capita,
         "Family"= social_support,
         "Health..Life.Expectancy."= healthy_life_expectancy,
         "Happiness.Score" = ladder_score,
         "Trust..Government.Corruption."= perceptions_of_corruption,
         "Freedom" = freedom_to_make_life_choices,
         "Happiness.Rank" = happiness.rank) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names) %>% 
  select(country, region, happiness_rank, happiness_score,
         economy_gdp_per_capita, family, health_life_expectancy,
         freedom, trust_government_corruption, generosity, year)





report_2021 <- 
  report_2021 %>% 
  mutate(year = 2021, happiness.rank = row.names(report_2021)) %>% 
  select(country_name, regional_indicator, logged_gdp_per_capita,
         social_support, healthy_life_expectancy,
         ladder_score, perceptions_of_corruption,
         generosity, freedom_to_make_life_choices,
         happiness.rank, year) %>% 
  rename(Country                        = country_name,
         Region                         = regional_indicator,
         Economy..GDP.per.Capita.       = logged_gdp_per_capita,
         Family                         = social_support,
         Happiness.Score                = ladder_score,
         health_life_expectancy         = healthy_life_expectancy,
         Trust..Government.Corruption.  = perceptions_of_corruption,
         Freedom                        = freedom_to_make_life_choices) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names) %>% 
  select(country, region, happiness_rank, happiness_score,
         economy_gdp_per_capita, family, health_life_expectancy,
         freedom, trust_government_corruption, generosity, year)


report_2022 <- 
  report_2022 %>% 
  slice(-n()) %>% 
  select(!c(whisker_high, whisker_low, dystopia_1_83_residual)) %>% 
  mutate(happiness_score = happiness_score/100,
         year            = 2022,
         explained_by_gdp_per_capita = explained_by_gdp_per_capita/100,
         explained_by_social_support = explained_by_social_support/100,
         explained_by_healthy_life_expectancy = explained_by_healthy_life_expectancy/100,
         explained_by_freedom_to_make_life_choices = explained_by_freedom_to_make_life_choices/100,
         explained_by_generosity = explained_by_generosity/100,
         explained_by_perceptions_of_corruption = explained_by_perceptions_of_corruption/100
         ) %>% 
  rename( happiness_rank = rank,
    Economy..GDP.per.Capita. =  "explained_by_gdp_per_capita" ,             
    Family =  "explained_by_social_support",              
    health_life_expectancy =  "explained_by_healthy_life_expectancy",     
    Freedom =  "explained_by_freedom_to_make_life_choices",
    generosity = "explained_by_generosity",                  
    Trust..Government.Corruption. = "explained_by_perceptions_of_corruption" 
  ) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names) %>% 
  left_join(report_2015 %>% select(c(country, region)), by="country") %>% 
  select(country, region, happiness_rank, happiness_score,
         economy_gdp_per_capita, family, health_life_expectancy,
         freedom, trust_government_corruption, generosity, year)
  




# --- merging dataframes

base_reports <- 
  rbind(report_2015,
        report_2016,
        report_2017,
        report_2018,
        report_2019,
        report_2020,
        report_2021,
        report_2022) %>% 
  mutate(trust_government_corruption = as.numeric(trust_government_corruption))



# skimr::skim(base_reports)

# --- salvando os dataframes
saveRDS(report_2015, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "report_2015.rds"        # o nome que o arquivo, sempre
))

saveRDS(report_2016, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "report_2016.rds"        # o nome que o arquivo, sempre
))

saveRDS(report_2017, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "report_2017.rds"        # o nome que o arquivo, sempre
))

saveRDS(report_2018, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "report_2018.rds"        # o nome que o arquivo, sempre
))

saveRDS(report_2019, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "report_2019.rds"        # o nome que o arquivo, sempre
))

saveRDS(report_2020, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "report_2020.rds"        # o nome que o arquivo, sempre
))

saveRDS(report_2021, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "report_2021.rds"        # o nome que o arquivo, sempre
))

saveRDS(report_2022, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "report_2022.rds"        # o nome que o arquivo, sempre
))

saveRDS(base_reports, here::here(
  "data",
  "produced_data",             # pasta onde sera salvo
  "base_reports.rds"        # o nome que o arquivo, sempre
))



# # ---
# # --- Exportando dados do R para o SQLite
#
# # usando a funcao here() do pacote here
# # essa funcao nos ajuda a especificar os
# # locais de trabalho, facilitando o workflow
#
# criando a base
base_reports_sqlite <-
  here::here(
    "data",
    "produced_data",             # pasta onde sera salvo
    "base_reports.SQLITE"        # o nome que o arquivo, sempre
  )                              # espeficicando a extensao .SQLITE

# abrindo a concexao com o sqlite
my_con <- dbConnect(drv = SQLite(), base_reports_sqlite)

# escrevendo/gravando/salvando o dataframe em formato sqlite
dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'report_2015',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = report_2015          # data frame que ira salvar na tabela
)

dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'report_2016',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = report_2016          # data frame que ira salvar na tabela
)

dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'report_2017',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = report_2017          # data frame que ira salvar na tabela
)

dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'report_2018',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = report_2018          # data frame que ira salvar na tabela
)

dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'report_2019',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = report_2019          # data frame que ira salvar na tabela
)

dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'report_2020',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = report_2020          # data frame que ira salvar na tabela
)

dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'report_2021',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = report_2021          # data frame que ira salvar na tabela
)

# dbRemoveTable(my_con, 'report_2022')
dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'report_2022',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = report_2022          # data frame que ira salvar na tabela
)

# dbRemoveTable(my_con, 'base_reports')
dbWriteTable(
  conn = my_con,        # sua conexao my_con
  name = 'base_reports',   # escolha o nome para a tabela, ex: 'MyDB.Ibov'
  value = base_reports          # data frame que ira salvar na tabela
)


# para ver as tabelas existentes em sua base
dbListTables(my_con)

# disconnect
dbDisconnect(my_con)



# --- clear global enviroment
rm(list=ls())

