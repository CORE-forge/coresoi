library(dplyr)

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



## test suite
## - 1 test colnames for different emergency scenarios OK
## - 2 test compliance to schema OK
## - 3 test `indicator_value` inbetween a range OK
## - 4 test number of rows choerence with grouping var
## TODO:  - 5  test diff  scenarios

test_that("check `ind_5()` are 12 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_5(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = denominazione,
        emergency_name = "Coronavirus"
      )
    }), 12
  )
})


# test_that("check column names are as according to pre determined schema", {
#   col_names <- c(
#     "indicator_id", "indicator_name", "indicator_value", "aggregation_name",
#     "aggregation_id", "aggregation_type", "emergency_id", "emergency_name",
#     "country_id", "country_name", "indicator_last_update",
#     "data_last_update"
#   )
#
#   expect_equal(
#     suppressWarnings({
#       names(ind_5(
#         data = mock_data_core,
#         publication_date = data_pubblicazione,
#         stat_unit = cf_amministrazione_appaltante,
#         winners = denominazione,
#         emergency_name = "Coronavirus"
#       ))
#     }), col_names
#   )
# })



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen, tolerating Inf,", {
  expect_within_range_tolerate(
    suppressWarnings({
      ind_5(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        winners = denominazione,
        emergency_name = "Coronavirus"
      )
    }),
    min = 0, max = 10
  )
})


test_that("check if the number of rows is coherent with the aggregation level (`cf_amministrazione_appaltante`), `ind_5` has only one target stat_unit", {
  expect_row_number(
    suppressWarnings({
      ind_5(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = denominazione,
        emergency_name = "Coronavirus"
      )
    }),
    n = 194
  )
})

## test with different scenarios

test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen in a changed scenario i.e. Terremoto Aquila", {
  expect_within_range_tolerate(
    suppressWarnings({
      ind_5(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = denominazione,
        emergency_name = "terremoto aquila"
      )
    }),
    min = 0, max = 10
  )
})



test_that("check if the number of rows is coherent with the aggregation level (`provincia`) with a different emergency scenario", {
  expect_row_number(
    suppressWarnings({
      ind_5(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = denominazione,
        emergency_name = "terremoto aquila"
      )
    }),
    n = 604 # qui diverso perchè c'è filtro su cpv per 33, mi aspetto meno dati
  )
})


test_that("check if the indicator table, in its column `emergency_name` and `emergency_id` is coherent with the change in emergency scenario", {
  expect_equal(
    suppressWarnings({
      ind_5(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = denominazione,
        emergency_name = "terremoto ischia"
      ) %>%
        distinct(emergency_name, emergency_id) %>%
        flatten()
    }),
    list(
      emergency_name = "Terremoto Ischia",
      emergency_id = 3
    )
  )
})


## expect to have less rows or equal rows when cpvs are filtered
test_that("check if the number of rows when indicator is filtered out by cpv is loweer than the one with more cpvs on it (i.e. the defualt)", {
  expect_lte(
    suppressWarnings({
      nrow(ind_5(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        winners = denominazione,
        emergency_name = "Coronavirus",
        cpvs = c(33, 34, 38, 39, 41, 44, 65, 85)
      ))
    }), expected = nrow(ind_5(
      data = mock_data_core,
      publication_date = data_pubblicazione,
      stat_unit = cf_amministrazione_appaltante,
      winners = denominazione,
      emergency_name = "Coronavirus"
    ))
  )
})

