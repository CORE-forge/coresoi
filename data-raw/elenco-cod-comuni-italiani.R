library(dplyr)
library(rvest)
library(janitor)

url = "https://dait.interno.gov.it/territorio-e-autonomie-locali/sut/elenco_codici_comuni.php"
elenco_cod_comuni_italiani = url %>%
  read_html() %>%
  html_table() %>%
  .[[1]] %>%
  clean_names()



usethis::use_data(elenco_cod_comuni_italiani, overwrite = TRUE)
