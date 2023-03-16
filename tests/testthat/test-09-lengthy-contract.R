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


expect_some_variability <- function(object, exp_value_counts) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$value_counts <- unique(act$val$indicator_value)

  if (length(act$val$indicator_value) > exp_value_counts) {
    succeed()
    return(invisible(act$val))
  }

  message <- sprintf("`indicator_value` has not that many values %i", act$value_counts)
  fail(message)
}



test_that("check `ind_9()` are 12 columns as according to `generate_indicator_schema()`s with `stat_unit` being **cf_amministrazione_appaltante**", {
  expect_col_number(
      ind_9(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        eff_start = data_inizio_effettiva ,
        eff_end = data_effettiva_ultimazione,
        emergency_name = "coronavirus"
      ), 12
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
      names(ind_9(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        eff_start = data_inizio_effettiva ,
        eff_end = data_effettiva_ultimazione,
        emergency_name = "coronavirus"
      )), col_names,
    tolerance = 0.8
  )
})




test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen (Wilcoxon)", {
  expect_within_range(
      ind_9(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        eff_start = data_inizio_effettiva ,
        eff_end = data_effettiva_ultimazione,
        emergency_name = "coronavirus"
      ),
    min = 0, max = 1
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen (Wilcoxon)", {
  expect_some_variability(
      ind_9(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        eff_start = data_inizio_effettiva ,
        eff_end = data_effettiva_ultimazione,
        emergency_name = "coronavirus"
      ),
      exp_value_counts = 2
  )
})



## test resiliency oevr differnt scenario
test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen AND it is consistent with a different scenario", {
  expect_within_range(
      ind_9(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        eff_start = data_inizio_effettiva ,
        eff_end = data_effettiva_ultimazione,
        emergency_name = "terremoto aquila"
      ),
    min = 0, max = 1
  )
})



test_that("check if the indicator table, in its column `emergency_name` and `emergency_id` is coherent with the change in emergency scenario", {
  expect_equal(
    ind_9(
      data = mock_data_core,
      publication_date = data_pubblicazione,
      stat_unit = cf_amministrazione_appaltante,
      eff_start = data_inizio_effettiva ,
      eff_end = data_effettiva_ultimazione,
      emergency_name = "terremoto ischia"
    )%>% distinct(emergency_name, emergency_id) %>% flatten(),
    list(
      emergency_name = "Terremoto Ischia",
      emergency_id = 3
    )
  )
})

