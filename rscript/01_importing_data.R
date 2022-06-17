
source(here::here("rscript", "00_packages.R"))

# ---
# --- read the data bases:

report_2015 <- 
  read.csv(
    here::here(
      "data",
      "raw_data",
      '2015.csv'
    )
  ) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names)



report_2016 <- 
  read.csv(
    here::here(
      "data",
      "raw_data",
      '2016.csv'
    )
  ) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names)



report_2017 <- 
  read.csv(
    here::here(
      "data",
      "raw_data",
      '2017.csv'
    )
  ) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names)


report_2018 <- 
  read.csv(
    here::here(
      "data",
      "raw_data",
      '2018.csv'
    )
  ) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names)


report_2019 <- 
  read.csv(
    here::here(
      "data",
      "raw_data",
      '2019.csv'
    )
  ) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names)


report_2020 <- 
  read.csv(
    here::here(
      "data",
      "raw_data",
      '2020.csv'
    )
  ) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names)


report_2021 <- 
  read.csv(
    here::here(
      "data",
      "raw_data",
      '2021.csv'
    )
  ) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names)



report_2022 <- 
  read_delim(
  here::here(
    "data",
    "raw_data",
    '2022.csv'), 
  delim = ";", 
  escape_double = FALSE, 
  col_types = cols(RANK = col_character(),
                   `Happiness score` = col_number(),
                   `Whisker-high` = col_number(), 
                   `Whisker-low` = col_number(),
                   `Dystopia (1.83) + residual` = col_number(),
                   `Explained by: GDP per capita` = col_number(),
                   `Explained by: Social support` = col_number(),
                   `Explained by: Healthy life expectancy` = col_number(),
                   `Explained by: Freedom to make life choices` = col_number(),
                   `Explained by: Generosity` = col_number(),
                   `Explained by: Perceptions of corruption` = col_number()), 
                    trim_ws = TRUE) %>% 
  tibble::as_tibble(.name_repair = janitor::make_clean_names)
