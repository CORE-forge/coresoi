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


#' @title get the emergency date from input string
#' @description gets the outbreak starting date from emergency name input string
#' @param emergency_name  emergency name character string
#' @return a lubridate class dmy date.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  emergency_dates("Coronavirus")
#'  emergency_dates("Codronavilus")
#'  }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#' @rdname emergency_dates
#' @export
#' @importFrom lubridate ymd dmy
emergency_dates <- function(emergency_name) {
  # create a named list of emergency names and their corresponding dates
  # TODO add emergenze umanitarie
  emergency_list <- list(
    "Coronavirus" = lubridate::ymd("2020-01-31"),
    "Terremoto Aquila" = lubridate::ymd("2017-06-30"),
    "Terremoto Ischia" = lubridate::dmy("21/08/2017"),
    "Terremoto Centro Italia 2016-2017" = lubridate::dmy("24/08/2016"),
    "Terremoto Emilia-Romagna e Lombardia 2012" = lubridate::dmy("20/05/2012"),
    "Etna - Eruzione 2008-2009" = lubridate::dmy("13/05/2008"),
    "Etna - Eruzione 2006-2007" = lubridate::dmy("14/07/2006"),
    "Etna - Eruzione 2002-2003" = lubridate::dmy("28/10/2002"),
    "Stromboli - Eruzione 2007" = lubridate::dmy("24/08/2016"),
    "Terremoto Emilia-Romagna e Lombardia 2012" = lubridate::dmy("20/05/2012"),
    "Neve al centro-sud 2012"  = lubridate::dmy("31/01/2012"),
    "Alluvione di Messina" = lubridate::dmy("01/10/2009"),
    "Rimozione delle ecoballe nel Golfo di Follonica 2020" = lubridate::dmy("22/07/2020"),
    "Incendi stagione estiva 2017" = lubridate::dmy("01/07/2017"),
    "Naufragio della Concordia" = lubridate::dmy("13/01/2012"),
    "Incendi stagione estiva 2017" = lubridate::dmy("01/07/2017")
  )

  # use agrep to find approximate matches for the input emergency name
  emergency_match <- agrep(emergency_name, names(emergency_list), max.distance = 0.3)

  # check if any approximate matches were found
  if (length(emergency_match) > 0) {
    # if matches were found, return the date corresponding to the first match
    return(emergency_list[emergency_match[1]])
  } else {
    # if no matches were found, return an error message
    return("Error: Emergency not found in list.")
  }
}




# TODO function to grab latest data update

from_aggregation_type_to_istat_code <- function(variables) {

}


from_aggregation_type_to_eu_nc <- function(variables) {

}


#' generate indicator schema
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
