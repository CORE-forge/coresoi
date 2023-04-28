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

test_that("check `ind_6()` are 12 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressMessages({
      ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "coronavirus",
        award_col = id_aggiudicazione,
        stat_unit = provincia,
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
    suppressMessages({
      names(ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "coronavirus",
        award_col = id_aggiudicazione,
        stat_unit = provincia,
        test_type = "fisher"
      ))
    }), col_names
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen", {
  expect_within_range(
    suppressMessages({
      ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "coronavirus",
        award_col = id_aggiudicazione,
        stat_unit = provincia,
        test_type = "fisher"
      )
    }),
    min = 0, max = 1
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen, this time is 'z-test'", {
  expect_within_range(
    suppressMessages({
      ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "coronavirus",
        award_col = id_aggiudicazione,
        stat_unit = provincia,
        test_type = "z-test"
      )
    }),
    min = 0, max = 1
  )
})




test_that("check if the number of rows is coherent with the aggregation level (`provincia`)", {
  expect_row_number(
    suppressMessages({
      ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "coronavirus",
        award_col = id_aggiudicazione,
        stat_unit = provincia,
        test_type = "fisher"
      )
    }),
    n = 108
  )
})

test_that("check if the number of rows is coherent with the aggregation level (`cf_amministrazione_appaltante`)", {
  expect_row_number(
    suppressMessages({
      ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "coronavirus",
        award_col = id_aggiudicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "fisher"
      )
    }),
    n = 731
  )
})


## test for different scenarios
test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen i.e. 'terremoto aquila'", {
  expect_within_range(
    suppressMessages(
      ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "terremoto aquila",
        award_col = id_aggiudicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "fisher"
      )
    ),
    min = 0, max = 1
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen AND it is consistent with a different scenario", {
  expect_within_range(
    suppressMessages(
      ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "terremoto aquila",
        award_col = id_aggiudicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "fisher"
      )
    ),
    min = 0, max = 1
  )
})



test_that("check if the indicator table, in its column `emergency_name` and `emergency_id` is coherent with the change in emergency scenario, a new one i.e. 'terremoto ischia'", {
  expect_equal(
    suppressMessages(
      ind_6(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        emergency_name = "terremoto Ischia",
        award_col = id_aggiudicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "fisher"
      ) %>% distinct(emergency_name, emergency_id) %>% flatten()
    ),
    list(
      emergency_name = "Terremoto Ischia",
      emergency_id = 3
    )
  )
})
