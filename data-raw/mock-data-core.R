## code to prepare `DATASET` dataset goes here

library(here)
library(tibble)
library(readr)
library(dplyr)
library(forcats)
library(tidyr)
library(stringr)
library(forcats)
library(stringr)

## ask me for this!
## I have cut if for you!
# mock_data_core <- tibble(get(load("/Users/niccolo/Desktop/mock_data_core_2.rdata"))) %>%
#   mutate(
#     luogo_istat = if_else(str_detect(luogo_istat, "\\b\\d{4}\\b"), true= paste0("00", luogo_istat), false = as.character(luogo_istat)),
#     luogo_istat = if_else(str_detect(luogo_istat, "\\b\\d{5}\\b"), true= paste0("0", luogo_istat), false = as.character(luogo_istat)),
#   ) %>%
#   saveRDS(file = here("data-raw", "mock_data_core.rds"))


mock_data_core = tibble(get(load("/Users/niccolo/Desktop/core_assets/mock_data_core_def.RData")))


# mock_data_core <- read_rds(here("data-raw", "mock_data_core.rds"))

# ## need also to join with ISTAT codes
# library(readxl)
# library(janitor)
# codici_istat = read_xlsx("/Users/niccolo/Desktop/cladag_pres_images/Elenco-codici-statistici-e-denominazioni-delle-unita-territoriali/Codici-statistici-e-denominazioni-al-30_06_2023.xlsx", .name_repair = make_clean_names) %>%
#   transmute(
#     codice_comune_formato_alfanumerico = codice_comune_formato_alfanumerico,
#     codice_nuts1_2021,
#     codice_nuts2_2021 = codice_nuts2_2021_3,
#     codice_nuts3_2021
#     )
#
# mock_data_core = mock_data_core %>%
#   mutate(luogo_istat = as.character(luogo_istat)) %>%
#   left_join(codici_istat, by = c("luogo_istat"="codice_comune_formato_alfanumerico"))




fix_encoding <- function(x) {
  Encoding(x) <- "latin1"
  return(x)
}
mock_data_core <- mock_data_core %>%
  mutate_if(is.character,fix_encoding)

usethis::use_data(mock_data_core, overwrite = TRUE)
