#' Download and extract zip from target url
#' @description  Download and extract zip from target url
#' @keywords internal
#' @export
download_and_unzip <- function(zip_url) {
  temp <- tempdir()
  dir <- here("data-raw")
  zip_file <- basename(zip_url)
  zip_combine <- paste(temp, zip_file, sep = "/")
  logger::log_info("Start downloading 🗂 zip file...  ")
  download.file(zip_url, destfile = zip_combine)
  logger::log_info("Downloading completed ✅ ")
  logger::log_info("Start unzippin 📦 ...  ")
  unzip(zip_combine, exdir = dir, overwrite = TRUE)
  logger::log_info("Unzippin completed ✅ ")
  unlink(temp)
}


#' Gets Emergency id from outbreak date
#' @keywords internal
#' @export
get_emergency_id_from_date <- function(outbreak_starting_date) {

  # TODO distinguish emergecy id from date by state (Italy the
  # pandemic is started in february. China in decemebr 2019):
  # depends also from the TZ

  if (outbreak_starting_date %within% lubridate::interval(lubridate::ymd("2020-02-21"), lubridate::ymd("2022-03-23"))) {
    return(1)
  } else if (outbreak_starting_date %within% lubridate::interval(lubridate::ymd("2022-2-24"), lubridate::today())) {
    return(2)
  } else {
    return(3)
  }
}

#' Desume Emergency name from its date
#' @keywords internal
#' @export
get_emergency_name_from_date <- function(outbreak_starting_date) {
  if (lubridate::as_date(outbreak_starting_date) %within% lubridate::interval(start = lubridate::ymd("2020-02-21"), end = lubridate::ymd("2022-03-23"))) {
    return("Coronavirus")
  } else if (lubridate::as_date(outbreak_starting_date) %within% lubridate::interval(start = lubridate::ymd("2022-2-24"), end = lubridate::today())) {
    return("Ucraine - Russia war")
  } else {
    return("other")
  }
}

# TODO function to grab latest data update
# TODO: need to think a way to open this thing to other countries besides Italy

#' @title Generate commmon schema for each indicator
#' @description provides a common schema to distribute indicators according to https://github.com/CORE-forge/core-dashboard/issues/2
#' @param .data dataset
#' @param indicator_id this is defined within each indicator function (`ind_2` with 2 and `ind_3` and 3)
#' @param indicator_name this is defined within each indicator function (`ind_2` with "High Economic Value" and `ind_3` and  One-shot opportunistic companies)
#' @param outbreak_starting_date the date in which the emergency erupts (this defined by )
#' @param ... indicator_value, aggregation_name
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{now}}
#'  \code{\link[dplyr]{transmute}}
#' @rdname generate_indicator_schema
#' @export
#' @importFrom lubridate now
#' @importFrom dplyr transmute
generate_indicator_schema <- function(.data, indicator_id, indicator_name, outbreak_starting_date, ...) {
  common_schema <- .data %>%
    dplyr::transmute(
      indicator_id = indicator_id,
      idicator_name = indicator_name,
      ..., ## indicator_value e aggregation_name
      emergency_id = get_emergency_id_from_date(outbreak_starting_date),
      emergency_name = get_emergency_name_from_date(outbreak_starting_date),
      aggregation_id = "ISTAT1", # istat id from function
      aggregation_type = "nuts_2", # nuts from function
      country_id = "1",
      country_name = "Italy",
      indicator_last_update = lubridate::now(),
      data_last_update = lubridate::now()
    )
  return(common_schema)
}
