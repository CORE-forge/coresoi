library(here)
library(readr)
library(dplyr)
library(janitor)

cod_stat <- read_delim(here("data-raw", "cod-stat.csv"), locale = locale(encoding = "latin1"), delim = ";", show_col_types = F, name_repair = make_clean_names) %>%
  select(
    codice_regione,
    denominazione_regione,
    denominazione_italiana_e_straniera,
  )

usethis::use_data(cod_stat, overwrite = TRUE)
