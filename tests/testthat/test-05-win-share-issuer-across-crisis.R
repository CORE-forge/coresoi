library(dplyr)
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

## it keeps expectation within range, but it also tolerates Inf when Lim denominator -> 0
expect_within_range_tolerate <- function(object, min, max) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$indicator_min <- min(act$val$indicator_value)
  act$indicator_max <- max(act$val$indicator_value)

  if (any(between(act$val$indicator_value, min, max)) | Inf) {
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




test_that("check `ind_5()` are 11 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_5(
        data = unnest(mock_data_core, aggiudicatari),
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = codice_fiscale,
        emergency_name = "Coronavirus"
      )
    }), 11
  )
})


test_that("check `ind_5()` is variable", {
  expect_variability(
    suppressWarnings({
      ind_5(
        data = unnest(mock_data_core, aggiudicatari),
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = codice_fiscale,
        emergency_name = "Coronavirus"
      )
    })
  )
})


## test with different scenarios
test_that("check `ind_5()` are 11 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_5(
        data = unnest(mock_data_core, aggiudicatari),
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = codice_fiscale,
        emergency_name = "terremoto centro italia"
      )
    }), 11
  )
})


test_that("check `ind_5()` is variable", {
  expect_variability(
    suppressWarnings({
      ind_5(
        data = unnest(mock_data_core, aggiudicatari),
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = codice_fiscale,
        emergency_name = "terremoto aquila"
      )
    })
  )
})
