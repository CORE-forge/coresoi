library(dplyr)
library(purrr)
library(tidyr)

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


# unnest data by variants
mock_data_core_ca <- unnest(mock_data_core, varianti, keep_empty = TRUE)
mock_data_core_ca_com <- unnest(mock_data_core_ca, aggiudicatari, keep_empty = TRUE)

test_that("check `ind_8()` by contr auth are 11 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_8(
        data = mock_data_core_ca_com,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        variant_date = data_approvazione_variante,
        months_win = 6,
        emergency_name = "coronavirus"
      )
    }), 11
  )
})

test_that("check `ind_8()` by company is variable", {
  expect_variability(
    suppressWarnings({
      ind_8(
        data = mock_data_core_ca_com,
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        variant_date = data_approvazione_variante,
        months_win = 6,
        emergency_name = "coronavirus"
      )
    })
  )
})


test_that("check `ind_8()` by nuts3 are 11 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_8(
        data = mock_data_core_ca_com,
        publication_date = data_pubblicazione,
        stat_unit = codice_nuts3_2021,
        variant_date = data_approvazione_variante,
        months_win = 6,
        emergency_name = "coronavirus"
      )
    }), 11
  )
})

## other scenarios
test_that("check `ind_8()` by contr auth are 11 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_8(
        data = mock_data_core_ca_com,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        variant_date = data_approvazione_variante,
        months_win = 6,
        emergency_name = "terremoto aquila"
      )
    }), 11
  )
})

test_that("check `ind_8()` by company is variable", {
  expect_variability(
    suppressWarnings({
      ind_8(
        data = mock_data_core_ca_com,
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        variant_date = data_approvazione_variante,
        months_win = 6,
        emergency_name = "terremoto ischia"
      )
    })
  )
})


test_that("check `ind_8()` by nuts3 are 11 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_8(
        data = mock_data_core_ca_com,
        publication_date = data_pubblicazione,
        stat_unit = codice_nuts3_2021,
        variant_date = data_approvazione_variante,
        months_win = 6,
        emergency_name = "terremoto centor italia"
      )
    }), 11
  )
})
