library(dplyr)
library(purrr)
library(testthat)


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

# ind1 only for companies
mock_data_core_comp <- mock_data_core %>%
  tidyr::unnest(aggiudicatari, keep_empty = TRUE)

test_that("check `ind_1()` are 11 columns as according to `generate_indicator_schema()`s with `stat_unit` being **codice_fiscale**", {
  expect_col_number(
    suppressWarnings({
      ind_1(
        data = mock_data_core_comp,
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    }), 11
  )
})

test_that("check column names are as according to pre determined schema", {
  col_names <- c(
    "indicator_id", "indicator_name", "indicator_value", "aggregation_name",
    "aggregation_id", "emergency_name", "emergency_id",
    "country_id", "country_name", "indicator_last_update",
    "data_last_update"
  )

  expect_equal(
    suppressWarnings({
      names(ind_1(
        data = mock_data_core_comp,
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        emergency_name = "coronavirus",
        test_type = "fisher"
      ))
    }), col_names,
    tolerance = 0.8
  )
})



test_that("check if `indicator_value` lays inbetween min/max values according to test chosen, Barnard, (slow, only on a 30000 top obs)", {
  expect_within_range(
    suppressWarnings({
      ind_1(
        # only 30000 obs since this is time consuming
        data = mock_data_core_comp %>% dplyr::sample_n(30000),
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        emergency_name = "coronavirus",
        test_type = "barnard"
      )
    }),
    min = 0, max = 1
  )
})

test_that("check if `indicator_value` variability is not 0", {
  expect_variability(
    suppressWarnings({
      ind_1(
        # only 10000 obs since this is time consuming
        data = mock_data_core_comp %>% dplyr::sample_n(10000),
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        emergency_name = "coronavirus",
        test_type = "barnard"
      )
    })
  )
})

test_that("check if `indicator_value` variability is not 0", {
  expect_variability(
    suppressWarnings({
      ind_1(
        # only 10000 obs since this is time consuming
        data = mock_data_core_comp,
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    })
  )
})


test_that("check if `indicator_value` lays inbetween min/max values according to test chosen (Fisher)", {
  expect_within_range(
    suppressWarnings({
      ind_1(
        data = mock_data_core_comp,
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    }),
    min = 0, max = 1
  )
})

## test for different scenarios
test_that("check if `indicator_value` lays inbetween min/max values according to test chosen", {
  expect_within_range(
    suppressWarnings({
      ind_1(
        data = mock_data_core_comp,
        publication_date = data_pubblicazione,
        stat_unit = codice_fiscale,
        emergency_name = "terremoto aquila",
        test_type = "fisher"
      )
    }),
    min = 0, max = 1
  )
})




test_that("check if the indicator table, in its column `emergency_name` and `emergency_id` is coherent with the change in emergency scenario", {
  expect_equal(
    ind_1(
      data = mock_data_core_comp,
      publication_date = data_pubblicazione,
      stat_unit = codice_fiscale,
      emergency_name = "terremoto ischia",
      test_type = "fisher"
    ) %>%
      distinct(emergency_name, emergency_id) %>%
      purrr::flatten(),
    list(
      emergency_name = "Terremoto Ischia",
      emergency_id = 3
    )
  )
})
