


# Obervando espacialmente.

world_sf <- 
  readOGR(
    dsn = here::here("assets", "shape_files", "world_sf"), 
    layer = "world")


base_mean_score <- 
  base_reports %>% 
  select(year, country, happiness_score) %>% 
  rename(name = country) %>% 
  drop_na() %>% 
  group_by(name) %>% 
  summarise(mean_score = mean(happiness_score)) 


# https://bookdown.org/nicohahn/making_maps_with_r5/docs/tmap.html
plot_world_sf <-   
  merge(
  x = world_sf,
  y = base_mean_score,
  by.x = "name",
  by.y = "name") %>% 
  tm_shape() + 
  tm_polygons(col = "mean_score", 
              palette = "viridis")
  
plot_world_sf


# Analisando os top 10 paises durante o tempo

top_10 <- 
  base_reports %>% 
  filter(year == 2015) %>% head(10) %>% 
  select(year, country, happiness_score) %>% 
  rbind(base_reports %>% 
          filter(year == 2016) %>% head(10) %>% 
          select(year, country, happiness_score)) %>% 
  rbind(base_reports %>% 
          filter(year == 2017) %>% head(10) %>% 
          select(year, country, happiness_score)) %>% 
  rbind(base_reports %>% 
          filter(year == 2018) %>% head(10) %>% 
          select(year, country, happiness_score)) %>% 
  rbind(base_reports %>% 
          filter(year == 2019) %>% head(10) %>% 
          select(year, country, happiness_score)) %>% 
  rbind(base_reports %>% 
          filter(year == 2020) %>% head(10) %>% 
          select(year, country, happiness_score)) %>% 
  rbind(base_reports %>% 
          filter(year == 2021) %>% head(10) %>% 
          select(year, country, happiness_score)) %>% 
  rbind(base_reports %>% 
          filter(year == 2022) %>% head(10) %>% 
          select(year, country, happiness_score)) %>% 
 mutate(country =  str_replace(country, "\\*", ""))




# Se olhar considerando que nossa base considera 8 anos, desses 8 anos paises como 
# Suiça, Finlandia, Islandia, Holanda, Nova zelandia, Noruega, Dinamarca, Suecia,
# apareceram em todos os top 10 de paises mais felizes.
top_10 %>% 
  tabyl(country) %>% 
  adorn_pct_formatting() %>% 
  knitr::kable()


top_10 %>% 
  ggplot(aes(year, happiness_score, color = country)) + 
  geom_point() +
  geom_line() +
  scale_colour_viridis_d()


# --- 

# Outro ponto interessante que luxemburgo que aparece mais de uma vez era esperado
# pois é o pais com mais renda per capita no mundo, sendo a mesma a variável que 
# mais influência essa pontuação.


