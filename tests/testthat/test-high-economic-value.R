expect_length <- function(object, n) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$n <- length(act$val)
  if (act$n == n) {
    succeed()
    return(invisible(act$val))
  }

  message <- sprintf("%s has length %i, not length %i.", act$lab, act$n, n)
  fail(message)
}


## test suite
## - testa le colonne (per differente scenario)
## - testa per compliance a schema
## - testa le righe
## - testa cje il valore dell'indicatore stia tra tot e tot
## - testa che
test_that("check columns schema for `ind_2()`", {
  expect_length(
    ind_2(
      data = mock_data_core,
      cpv = cod_cpv,
      contract_value = importo_complessivo_gara,
      publication_date = data_pubblicazione,
      stat_unit = provincia,
      outbreak_starting_date = lubridate::ymd("2017-06-30")
    ), 12)
})
