## code to prepare `DATASET` dataset goes here

library(here)
library(tibble)
library(readr)
library(dplyr)
library(forcats)
library(tidyr)
library(stringr)
library(forcats)

## ask me for this!
## I have cut if for you!
mock_data_core <- read_csv("/Users/niccolo/Desktop/r_projects/corebd/sample_bdncp_100000.csv") %>%
  saveRDS(file = here("data-raw", "mock_data_core.rds"))


mock_data_core <- read_rds(here("data-raw", "mock_data_core.rds")) %>%
  select(-...1, -ends_with(".y")) %>%
  rename_if(endsWith(names(.), ".x"), ~ str_remove(., ".x")) %>%
  mutate(
    provincia = fct_recode(provincia,
      "REGGIO NELL'EMILIA" = "REGGIO NELLEMILIA",
      "L'AQUILA" = "LAQUILA"
    )
  )


## preps

usethis::use_data(mock_data_core, overwrite = TRUE)
