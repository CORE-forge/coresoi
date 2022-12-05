#' Gets Emergency id from outbreak date
#' @keywords internal
#' @export
get_emergency_id_from_date <- function(outbreak_starting_date) {

  # TODO distinguish emergecy id from date by state (Italy the
  # pandemic is started in february. China in decemebr 2019):
  # depends also from the TZ

  if (outbreak_starting_date %within% lubridate::interval(lubridate::ymd("2020-01-31"), lubridate::ymd("2022-03-23"))) {
    return(1)
  } else if (outbreak_starting_date %within% lubridate::interval(lubridate::ymd("2022-2-24"), lubridate::today())) {
    return(2)
  } else {
    return(3)
  }
}


# TODO: check this resource to dynamically get emergency scenarios https://emergenze.protezionecivile.gov.it/it/
# TODO: Italy or in general? those are italian contract so we should take care of only Italy!

#' Desume Emergency name from its date
#' @keywords internal
#' @export
get_emergency_name_from_date <- function(outbreak_starting_date) {
  if (lubridate::as_date(outbreak_starting_date) %within% lubridate::interval(start = lubridate::ymd("2020-01-31"), end = lubridate::ymd("2022-03-31"))) {
    return("Coronavirus")
  } else if (lubridate::as_date(outbreak_starting_date) %within% lubridate::interval(start = lubridate::ymd("2022-02-24"), end = lubridate::today())) {
    return("Ucraine - Russia war")
  } else if (lubridate::as_date(outbreak_starting_date) %within% lubridate::interval(start = lubridate::ymd("2009-04-06"), end = lubridate::ymd("2012-08-31"))) {
    return("L'Aquila Earthquake")
  } else {
    return("Other")
  }
}

# TODO function to grab latest data update
# TODO: need to think a way to open this thing to other countries besides Italy


from_aggregation_type_to_istat_code <- function(variables) {

}


from_aggregation_type_to_eu_nc <- function(variables) {

}


#' @title Generate commmon schema for indicator
#' @keywords internal
#' @export
generate_indicator_schema <- function(.data, indicator_id, indicator_name, aggregation_type, outbreak_starting_date, ...) {
  common_schema <- .data %>%
    dplyr::transmute(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      ..., ## indicator_value e aggregation_name
      aggregation_id = "ISTAT1", # istat id from function
      aggregation_type = aggregation_type, #  define also nuts within funs
      emergency_id = get_emergency_id_from_date(outbreak_starting_date),
      emergency_name = get_emergency_name_from_date(outbreak_starting_date),
      country_id = "1",
      country_name = "Italy",
      indicator_last_update = lubridate::now(),
      data_last_update = lubridate::now()
    )
  return(common_schema)
}
