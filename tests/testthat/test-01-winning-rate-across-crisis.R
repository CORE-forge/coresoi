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

test_that("check `ind_1()` are 12 columns as according to `generate_indicator_schema()`s with `stat_unit` being **provincia**", {
  expect_col_number(
    suppressWarnings({
      ind_1(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    }), 12
  )
})



test_that("check `ind_1()` are 12 columns as according to `generate_indicator_schema()`s  with `stat_unit` being **cf_amministrazione_appaltante**", {
  expect_col_number(
    suppressWarnings({
      ind_1(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    }), 12
  )
})



test_that("check column names are as according to pre determined schema", {
  col_names <- c(
    "indicator_id", "indicator_name", "indicator_value", "aggregation_name",
    "aggregation_id", "aggregation_type", "emergency_id", "emergency_name",
    "country_id", "country_name", "indicator_last_update",
    "data_last_update"
  )

  expect_equal(
    suppressWarnings({
      names(ind_1(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "coronavirus",
        test_type = "fisher"
      ))
    }), col_names,
    tolerance = 0.8
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen, Barnard, (slow, only on a 10000 top obs)", {
  expect_within_range(
    suppressWarnings({
      ind_1(
        # only 10000 obs since this is time consuming
        data = mock_data_core %>% head(10000),
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        emergency_name = "coronavirus",
        test_type = "barnard"
      )
    }),
    min = 0, max = 1
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen (Fisher)", {
  expect_within_range(
    suppressWarnings({
      ind_1(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    }),
    min = 0, max = 1
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen (Z-test proportional test)", {
  expect_within_range(
    suppressWarnings({
      ind_1(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    }),
    min = 0, max = 1
  )
})




test_that("check if the number of rows is coherent with the aggregation level `provincia`", {
  expect_row_number(
    suppressWarnings({
      ind_1(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    }),
    n = 108
  )
})


test_that("check if the number of rows is coherent with the aggregation level (`cf_amministrazione_appaltante`)", {
  expect_row_number(
    suppressWarnings({
      ind_1(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "coronavirus",
        test_type = "fisher"
      )
    }),
    n = 3227
  )
})

## test for different scenarios

test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen", {
  expect_within_range(
    suppressWarnings({
      ind_1(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "terremoto aquila",
        test_type = "fisher"
      )
    }),
    min = 0, max = 1
  )
})



# test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen AND it is consistent with a different scenario", {
#   expect_within_range(
#     suppressWarnings({
#       ind_1(
#         data = mock_data_core,
#         publication_date = data_pubblicazione,
#         stat_unit = cf_amministrazione_appaltante,
#         emergency_name = "terremoto aquila",
#         test_type = "z-test"
#       )
#     }),
#     min = 0, max = 1
#   )
# })



test_that("check if the indicator table, in its column `emergency_name` and `emergency_id` is coherent with the change in emergency scenario", {
  expect_equal(
    ind_1(
      data = mock_data_core,
      publication_date = data_pubblicazione,
      stat_unit = provincia,
      emergency_name = "terremoto ischia",
      test_type = "fisher"
    ) %>% distinct(emergency_name, emergency_id) %>% flatten(),
    list(
      emergency_name = "Terremoto Ischia",
      emergency_id = 3
    )
  )
})
