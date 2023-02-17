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


test_that("check `ind_2()` are 12 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "ks",
        emergency_name = "coronavirus"
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
      names(ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "ks",
        emergency_name = "coronavirus"
      ))
    }), col_names,
    tolerance = 0.8
  )
})



test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen (Kolmogorv Smirnov)", {
  expect_within_range(
    suppressWarnings({
      ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "ks",
        emergency_name = "coronavirus"
      )
    }),
    min = 0, max = 1
  )
})




test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen (Wilcoxon-Mann-Whitney)", {
  expect_within_range(
    suppressWarnings({
      ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "wilcoxon",
        emergency_name = "coronavirus"
      )
    }),
    min = 0, max = 1
  )
})



test_that("check if the number of rows is coherent with the aggregation level (`provincia`)", {
  expect_row_number(
    suppressWarnings({
      ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        test_type = "ks",
        emergency_name = "coronavirus"
      )
    }),
    n = 101
  )
})


test_that("check if the number of rows is coherent with the aggregation level (`cf_amministrazione_appaltante`)", {
  expect_row_number(
    suppressWarnings({
      ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "ks",
        emergency_name = "coronavirus"
      )
    }),
    n = 624 # qui diverso perchè c'è filtro su cpv per 33, mi aspetto meno dati
  )
})

## test with different scenarios

test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen in a changed scenario i.e. Terremoto Aquila", {
  expect_within_range(
    suppressWarnings({
      ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = cf_amministrazione_appaltante,
        test_type = "ks",
        emergency_name = "terremoto aquila"
      )
    }),
    min = 0, max = 1
  )
})



test_that("check if the number of rows is coherent with the aggregation level (`provincia`) with a different emergency scenario", {
  expect_row_number(
    suppressWarnings({
      ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        test_type = "ks",
        emergency_name = "terremoto aquila"
      )
    }),
    n = 109 # qui diverso perchè c'è filtro su cpv per 33, mi aspetto meno dati
  )
})



test_that("check if `indicator_value` lays inbetween min/max values (different aggregation units) accroding to test chosen AND it is consistent with a different scenario", {
  expect_within_range(
    suppressWarnings({
      ind_2(
        data = mock_data_core,
        contract_value = importo_complessivo_gara,
        publication_date = data_pubblicazione,
        stat_unit = provincia,
        test_type = "ks",
        emergency_name = "terremoto aquila"
      )
    }),
    min = 0, max = 1
  )
})