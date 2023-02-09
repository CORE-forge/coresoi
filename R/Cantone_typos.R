library(coresoi)
library(tidyverse)

mock_data_core %>% View()


mock_data_core %>%
  dplyr::mutate(
    prepost = dplyr::if_else(lubridate::ymd(
      data_pubblicazione
    ) >= "2017-06-30",
    true = "post", false = "pre"),
    prepost = forcats::as_factor(prepost),
  ) %>%
  dplyr::count( cf_amministrazione_appaltante,
                denominazione , prepost) %>%
  dplyr::group_by(cf_amministrazione_appaltante , prepost) %>%
  dplyr::summarise(
    gini = Gini(n)
  ) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = prepost, values_from = gini) %>%
  View()


  View()

mock_data_core$data_pubblicazione
