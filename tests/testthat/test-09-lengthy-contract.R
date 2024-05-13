library(dplyr)
library(purrr)

expect_row_number <- function(object, n) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$row_number <- dim(act$val)[1]
  if (act$row_number == n) {
    succeed()
    return(invisible(act$val))
  }

  message <- sprintf("%s has row_number %i, not row_number %i.", act$lab, act$row_number, n)
  fail(message)
}


expect_col_number <- function(object, n) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$col_number <- dim(act$val)[2]
  if (act$col_number == n) {
    succeed()
    return(invisible(act$val))
  }

  message <- sprintf("%s has col_number %i, not col_number %i.", act$lab, act$col_number, n)
  fail(message)
}


expect_within_range <- function(object, min, max) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$indicator_min <- min(act$val$indicator_value)
  act$indicator_max <- max(act$val$indicator_value)

  if (any(between(act$val$indicator_value, min, max))) {
    succeed()
    return(invisible(act$val))
  }

  message <- sprintf("`indicator_value` in %s does not stay in [%i - %i] indeed min-max range is [%i -  %i]", act$lab, act$indicator_min, act$indicator_max, min, max)
  fail(message)
}


expect_variability <- function(object) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$indicator_sd <- sd(act$val$indicator_value)

  if (act$indicator_sd > 0) {
    succeed()
    return(invisible(act$val))
  }

  message <- "`indicator_value` has not variability"
  fail(message)
}

mock_data_core_comp <- mock_data_core %>%
  tidyr::unnest(aggiudicatari, keep_empty = TRUE)


test_that("check `ind_9()` by contr auth are 11 columns as according to `generate_indicator_schema()`s ", {
  expect_col_number(
    ind_9(
      data = mock_data_core,
      publication_date = data_pubblicazione,
      stat_unit = cf_amministrazione_appaltante,
      eff_start = data_inizio_effettiva,
      eff_end = data_effettiva_ultimazione,
      emergency_name = "coronavirus"
    ), 11
  )
})

test_that("check if `indicator_value` by company is variable", {
  expect_variability(
    ind_9(
      data = mock_data_core_comp,
      publication_date = data_pubblicazione,
      stat_unit = codice_fiscale,
      eff_start = data_inizio_effettiva,
      eff_end = data_effettiva_ultimazione,
      emergency_name = "coronavirus"
    )
  )
})


## different scenarios
test_that("check `ind_9()` by contr auth are 11 columns as according to `generate_indicator_schema()`s ", {
  expect_col_number(
    ind_9(
      data = mock_data_core,
      publication_date = data_pubblicazione,
      stat_unit = cf_amministrazione_appaltante,
      eff_start = data_inizio_effettiva,
      eff_end = data_effettiva_ultimazione,
      emergency_name = "terremoto ischia"
    ), 11
  )
})

test_that("check if `indicator_value` by company is variable", {
  expect_variability(
    ind_9(
      data = mock_data_core_comp,
      publication_date = data_pubblicazione,
      stat_unit = codice_fiscale,
      eff_start = data_inizio_effettiva,
      eff_end = data_effettiva_ultimazione,
      emergency_name = "terrmoto centro ialia"
    )
  )
})

test_that("check if `indicator_value` by nuts3 is variable", {
  expect_variability(
    ind_9(
      data = mock_data_core,
      publication_date = data_pubblicazione,
      stat_unit = codice_nuts3_2021,
      eff_start = data_inizio_effettiva,
      eff_end = data_effettiva_ultimazione,
      emergency_name = "terrmoto centro ialia"
    )
  )
})
