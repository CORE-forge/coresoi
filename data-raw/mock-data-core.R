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
mock_data_core <- read_csv("/Users/niccolo/Desktop/folamour_album/sample_data.csv") %>%
  saveRDS(file = here("data-raw", "mock_data_core.rds"))


mock_data_core <- read_rds(here("data-raw", "mock_data_core.rds"))


## preps

usethis::use_data(mock_data_core, overwrite = TRUE)
