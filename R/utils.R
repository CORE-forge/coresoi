#' @title get the emergency date from input string
#' @description gets the outbreak starting date from emergency name input string
#' @param emergency_name  emergency name character string
#' @return a lubridate class dmy date.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   emergency_dates("Coronavirus")
#'   emergency_dates("Codronavilus")
#' }
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
    "Neve al centro-sud 2012" = lubridate::dmy("31/01/2012"),
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
    # if matches were found, return the date adn id corresponding to the first match
    emergency <- list()
    emergency$em_name <- names(emergency_list[emergency_match[1]])
    emergency$em_date <- emergency_list[emergency_match[1]][[1]]
    emergency$em_id <- emergency_match[1]

    return(emergency)
  } else {
    # if no matches were found, return an error message
    return("Error: Emergency not found in list.")
  }
}


#' @title Get the associated CPV code from emergency name
#' @description Not all the emergency are going to impact each contract. Some of them like coronavirus are going to impact more CPV 33 "Medical equipments, pharmaceuticals and personal care products" insted of say "Agricultural, forestry, horticultural, aquacultural and apicultural services"
#' @param emergency_name PARAM_DESCRIPTION
#' @return cpv_code
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_associated_cpv_from_emergency
#' @export
get_associated_cpv_from_emergency <- function(emergency_name) {

  if(is.na(emergency_name)){
    stop("any cpv can be found since no emergency name has been matched")
  }

  associated_cpv_list <- list(
    "Coronavirus" = c(18, 19, 22, 24, 30,32, 34, 38,39, 41, 44, 65, 85),
    "Terremoto Aquila" = c(45, 34, 35, 39, 43, 50,  60, 77, 85, 90),
    "Terremoto Ischia" = c(45, 34, 35, 39, 43, 60, 50, 77, 85, 90),
    "Terremoto Centro Italia 2016-2017" = c(45, 34, 35, 39, 43, 50, 60, 77, 85, 90),
    "Terremoto Emilia-Romagna e Lombardia 2012" = c(45, 34, 35, 39, 43, 50, 60, 77, 85, 90),
    "Etna - Eruzione 2008-2009" = c(03, 15, 18, 31, 33, 35, 39, 43, 45, 50, 71),
    "Etna - Eruzione 2006-2007" = c(03, 15, 18, 31, 33, 35, 39, 43, 45, 50, 71),
    "Etna - Eruzione 2002-2003" = c(03, 15, 18, 31, 33, 35, 39, 43, 45, 50, 71),
    "Stromboli - Eruzione 2007" = c(03, 15, 18, 31, 33, 35, 39, 43, 45, 50, 71),
    "Terremoto Emilia-Romagna e Lombardia 2012" = c(45, 34, 35, 39, 43, 60, 77, 85, 90),
    "Neve al centro-sud 2012" = c(18, 19, 22, 24, 30,32, 34, 38,39, 41, 44, 65, 85), # TODO see better cpv specs
    "Alluvione di Messina" = c(45, 34, 35, 39, 43, 60, 77, 85, 90),
    "Rimozione delle ecoballe nel Golfo di Follonica 2020" = c(18, 19, 22, 24, 30,32, 34, 38,39, 41, 44, 65, 85),# TODO see better cpv specs
    "Incendi stagione estiva 2017" = c(18, 19, 22, 24, 30,32, 34, 38,39, 41, 44, 65, 85),# TODO see better cpv specs
    "Naufragio della Concordia" = c(18, 19, 22, 24, 30,32, 34, 38,39, 41, 44, 65, 85),# TODO see better cpv specs
    "Incendi stagione estiva 2017" = c(18, 19, 22, 24, 30,32, 34, 38,39, 41, 44, 65, 85) # TODO see better cpv specs
  )

  cpv_match = associated_cpv_list[[emergency_name]]

  return(cpv_match)

}


# TODO function to grab latest data update

from_aggregation_type_to_istat_code <- function(variables) {

}


from_aggregation_type_to_eu_nc <- function(variables) {

}


#' generate indicator schema
#' @keywords internal
#' @export
generate_indicator_schema <- function(.data, indicator_id, indicator_name, aggregation_type, emergency, ...) {
  common_schema <- .data %>%
    dplyr::transmute(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      ..., ## indicator_value e aggregation_name
      aggregation_id = "ISTAT1", # istat id from function
      aggregation_type = aggregation_type, #  define also nuts within funs
      emergency_id = emergency$em_id,
      # emergency_type = emergency$em_type,
      emergency_name = emergency$em_name,
      country_id = "1",
      country_name = "Italy",
      indicator_last_update = lubridate::now(),
      data_last_update = lubridate::now()
    )
  return(common_schema)
}


#' grab cpv colname from dataframe class
#' @keywords internal
#' @export
grab_cpv <- function(data) {

  pattern <- "(?i)[Cc][Pp][Vv]|[Cc]ommon_[Pp]rocurement_[Vv]ocabulary" # the pattern to search for
  column_names <- colnames(data) # get the column names of the data frame
  cpv_col_match <- grep(pattern = pattern, x = column_names, value = TRUE)

  if (length(cpv_col_match > 2)) {
    level_2_pattern <- "[Cc]ode|[Cc]odice|[Ii]d|[Cc][Oo][Dd]|CODE" # the pattern to search
    cpv_col_match <- grep(level_2_pattern, cpv_col_match, value = TRUE)

  }
  return(cpv_col_match)
}

## binary operators
`%within%` <- lubridate::`%within%`
`%m+%` <- lubridate::`%m+%`
