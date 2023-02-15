library(here)
library(readr)
library(dplyr)
library(rvest)
library(janitor)

cod_stat <- read_delim(here("data-raw", "cod-stat-23.csv"), locale = locale(encoding = "latin1"), delim = ";", show_col_types = F, name_repair = make_clean_names) %>%
  select(
    codice_regione,
    denominazione_regione,
    codice_dell_unita_territoriale_sovracomunale_valida_a_fini_statistici,
    denominazione_dell_unita_territoriale_sovracomunale_valida_a_fini_statistici,
    sigla_automobilistica,
    codice_comune_formato_alfanumerico,
    denominazione_italiana_e_straniera,
    codice_nuts2_2021_3,
    codice_nuts3_2021
  )

## scrape dati ufficiali to map from `luogo_istat` to NUTS 2 and 3
url = "https://dait.interno.gov.it/territorio-e-autonomie-locali/sut/elenco_codici_comuni.php"

elenco_cod_comuni_italiani = url %>%
  read_html() %>%
  html_table() %>%
  .[[1]] %>%
  clean_names()

merged_elenco_cod_comuni_italiani = elenco_cod_comuni_italiani %>%
  left_join(cod_stat, by = c("sigla" = "sigla_automobilistica"), multiple = "first", keep = F) %>%
  select()

usethis::use_data(merged_elenco_cod_comuni_italiani, overwrite = TRUE)
