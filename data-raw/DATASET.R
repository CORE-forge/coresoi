## code to prepare `DATASET` dataset goes here

library(here)
library(tibble)
library(forcats)
load(here("data", "data_test_core.rdata"))


test_data_bndcp_core <- tibble(data_test_core) %>%
  mutate(
    nome_regione2 = fct_recode(nome_regione2,
                               "Trentino-Alto Adige" ="Trentino-Alto Adige/S\xfcdtirol",
                               "Valle d'Aosta" = "Valle d'Aosta/Vall\xe9e d'Aoste")
  )


usethis::use_data(test_data_bndcp_core, overwrite = TRUE)
