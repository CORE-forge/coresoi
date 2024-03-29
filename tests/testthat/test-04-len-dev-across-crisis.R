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
  tidyr::unnest(aggiudicatari, keep_empty=TRUE)

test_that("check `ind_4()` by contr auth+wilcoxon are 11 columns as according to `generate_indicator_schema()`s", {
  expect_col_number(
    suppressWarnings({
      ind_4(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "coronavirus"
      )
    }), 11
  )
})


test_that("check column names are as according to pre determined schema (by company)", {
  col_names <- c(
    "indicator_id", "indicator_name", "indicator_value",
    "aggregation_id","aggregation_name",  "emergency_name", "emergency_id",
    "country_id", "country_name", "indicator_last_update",
    "data_last_update"
  )

  expect_equal(
    suppressWarnings({
      names(ind_4(
        data = mock_data_core_comp,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        emergency_name = "coronavirus",
        stat_unit = codice_fiscale
      ))
    }), col_names,
    tolerance = 0.2
  )
})


test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen, empirically verifies being in [-100,100]", {
  expect_within_range(
    suppressWarnings({
      ind_4(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "coronavirus"
      )
    }),
    min = 0, max = 1
  )
})


test_that("check if `indicator_value` by contr auth is variable (Kolmogorv Smirnov)", {
  expect_variability(
    suppressWarnings({
      ind_4(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "coronavirus",
        test_type = "ks"
      )
    })
  )
})


test_that("check if `indicator_value` by nuts3 is variable (Wilcoxon)", {
  expect_variability(
    suppressWarnings({
      ind_4(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        stat_unit = codice_nuts3_2021,
        emergency_name = "coronavirus"
      )
    })
  )
})



## test for different emergency scenarios
test_that("check column names are as according to pre determined schema (by company)", {
  col_names <- c(
    "indicator_id", "indicator_name", "indicator_value",
    "aggregation_id","aggregation_name",  "emergency_name", "emergency_id",
    "country_id", "country_name", "indicator_last_update",
    "data_last_update"
  )

  expect_equal(
    suppressWarnings({
      names(ind_4(
        data = mock_data_core_comp,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        emergency_name = "terremoto ischia",
        stat_unit = codice_fiscale
      ))
    }), col_names,
    tolerance = 0.2
  )
})


test_that("check if `indicator_value` lays inbetween min/max values accroding to test chosen, empirically verifies being in [-100,100]", {
  expect_within_range(
    suppressWarnings({
      ind_4(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "terremoto centro italia"
      )
    }),
    min = 0, max = 1
  )
})


test_that("check if `indicator_value` by contr auth is variable (Kolmogorv Smirnov)", {
  expect_variability(
    suppressWarnings({
      ind_4(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        stat_unit = cf_amministrazione_appaltante,
        emergency_name = "terremoto aquila",
        test_type = "ks"
      )
    })
  )
})


test_that("check if `indicator_value` by nuts3 is variable (Wilcoxon)", {
  expect_variability(
    suppressWarnings({
      ind_4(
        data = mock_data_core,
        publication_date = data_pubblicazione,
        exp_end = data_termine_contrattuale,
        eff_end = data_effettiva_ultimazione,
        eff_start = data_inizio_effettiva,
        stat_unit = codice_nuts3_2021,
        emergency_name = "terremoto aquila"
      )
    })
  )
})
