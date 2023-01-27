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

## test suite
## - 1 test colnames for different emergency scenarios OK
## - 2 test compliance to schema OK
## - 3 test `indicator_value` inbetween a range OK
## - 4 test number of rows choerence with grouping var


test_that("check `ind_8()` are 12 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_8(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        id_variants = id_variante,
        emergency_name = "coronavirus"
      )
    }), 12
  )
})


##  need to invert column orded (Je ne sais quoi)
test_that("check column names are as according to pre determined schema", {
  col_names <- c(
    "indicator_id", "indicator_name", "indicator_value", "aggregation_name",
    "aggregation_id", "aggregation_type", "emergency_id", "emergency_name",
    "country_id", "country_name", "indicator_last_update",
    "data_last_update"
  )

  expect_equal(
    suppressWarnings({
      names(ind_8(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        id_variants = id_variante,
        emergency_name = "coronavirus"
      ))
    }), col_names
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen", {
  expect_within_range(
    suppressWarnings({
      ind_8(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        id_variants = id_variante,
        emergency_name = "coronavirus"
      )
    }),
    min = 0, max = 1
  )
})




test_that("check if the number of rows is coherent with the aggregation level (`provincia`)", {
  expect_row_number(
    suppressWarnings({
      ind_8(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        id_variants = id_variante,
        emergency_name = "coronavirus"
      )
    }),
    n = 108
  )
})

# TODO: we should specify scenario and compute prepost, instead of specifying `outbreak_starting_date`
# that' beacause we do not want the user to select emergency scenario based on their
# date, instead this should happen right inside a function
#
# test_that("check if the number of rows is coherent with the aggregation level (`provincia`) during Covid Emergency", {
#
#   expect_row_number(
#     suppressWarnings({
#       ind_8(
#         data = mock_data_core,
#         publication_date = data_pubblicazione,
#         stat_unit = provincia,
#         id_variants = id_variante,
#         emergency_name = "coronavirus"
#       )
#     }), n = 109)
# })
#
#

test_that("check if the number of rows is coherent with the aggregation level (`cf_amministrazione_appaltante`)", {
  expect_row_number(
    suppressWarnings({
      ind_8(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        id_variants = id_variante,
        emergency_name = "coronavirus"
      )
    }),
    n = 3227
  )
})


## test for different emergency scenarios


test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen", {
  expect_within_range(
    suppressWarnings({
      ind_8(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        id_variants = id_variante,
        emergency_name = "terremoto aquila"
      )
    }),
    min = 0, max = 1
  )
})


test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen AND it is consistent with a different scenario", {
  expect_within_range(
    suppressWarnings({
      ind_8(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        id_variants = id_variante,
        emergency_name = "terremoto aquila"
      )
    }),
    min = 0, max = 1
  )
})



test_that("check if the indicator table, in its column `emergency_name` and `emergency_id` is coherent with the change in emergency scenario", {
  expect_equal(
    ind_8(
      data = mock_data_core,
      publication_date = data_pubblicazione,
      stat_unit = cf_amministrazione_appaltante,
      id_variants = id_variante,
      emergency_name = "terremoto ischia"
    ) %>% distinct(emergency_name, emergency_id) %>% flatten(),
    list(
      emergency_id = 3,
      emergency_name = "Terremoto Ischia"
    )
  )
})
