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
#' @description Not all the emergency are going to impact each contract. Some of them like coronavirus are going to impact more CPV 33 "Medical equipments, pharmaceuticals and personal care products" insted of say "Agricultural, forestry, horticultural, aquacultural and apicultural services". Here we emplyed a set of domain experts which have worked on associating each emergency to a set of respective CPVs.
#' @param emergency_name PARAM_DESCRIPTION
#' @return cpv_code
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_associated_cpv_from_emergency
#' @export
get_associated_cpv_from_emergency <- function(emergency_name) {
  if (is.na(emergency_name)) {
    stop("any cpv can be found since no emergency name has been matched")
  }

  associated_cpv_list <- list(
    "Coronavirus" = c(33, 35, 18),
    "Terremoto Aquila" = c(45, 34, 35, 39, 43, 50, 60, 77, 85, 90),
    "Terremoto Ischia" = c(45, 34, 35, 39, 43, 60, 50, 77, 85, 90),
    "Terremoto Centro Italia 2016-2017" = c(33, 85, 55, 50),
    "Terremoto Emilia-Romagna e Lombardia 2012" = c(45, 34, 35, 39, 43, 50, 60, 77, 85, 90),
    "Etna - Eruzione 2008-2009" = c(03, 15, 18, 31, 33, 35, 39, 43, 45, 50, 71),
    "Etna - Eruzione 2006-2007" = c(03, 15, 18, 31, 33, 35, 39, 43, 45, 50, 71),
    "Etna - Eruzione 2002-2003" = c(03, 15, 18, 31, 33, 35, 39, 43, 45, 50, 71),
    "Stromboli - Eruzione 2007" = c(03, 15, 18, 31, 33, 35, 39, 43, 45, 50, 71),
    "Terremoto Emilia-Romagna e Lombardia 2012" = c(45, 34, 35, 39, 43, 60, 77, 85, 90),
    "Neve al centro-sud 2012" = c(18, 19, 22, 24, 30, 32, 34, 38, 39, 41, 44, 65, 85), # TODO see better cpv specs
    "Alluvione di Messina" = c(45, 34, 35, 39, 43, 60, 77, 85, 90),
    "Rimozione delle ecoballe nel Golfo di Follonica 2020" = c(18, 19, 22, 24, 30, 32, 34, 38, 39, 41, 44, 65, 85), # TODO see better cpv specs
    "Incendi stagione estiva 2017" = c(18, 19, 22, 24, 30, 32, 34, 38, 39, 41, 44, 65, 85), # TODO see better cpv specs
    "Naufragio della Concordia" = c(18, 19, 22, 24, 30, 32, 34, 38, 39, 41, 44, 65, 85), # TODO see better cpv specs
    "Incendi stagione estiva 2017" = c(33, 45, 30, 71)
  )

  cpv_match <- associated_cpv_list[[emergency_name]]

  return(cpv_match)
}


#' italian mapping from aggregation_id to aggregation_name
#' @keywords internal
italian_aggregation_mapping <- list(
  cf_amministrazione_appaltante = "denominazione_amministrazione_appaltante",
  codice_fiscale = "denominazione",
  codice_nuts2_2021 = "nome_regione",
  codice_nuts3_2021 = "nome_provincia",
  luogo_istat = "citta_nome"
)


#' simple wrapper to check if important columns are missing
#' @keywords internal
check_columns <- function(df, columns) {
  missing_cols <- setdiff(columns, names(df))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing columns: ", paste(missing_cols, collapse=", ")))
  }
}


#' mapping from country name to id (this is used for frontend purposes)
#' @keywords internal
#' @export
get_country_id_from_name <- function(country_name) {
  associated_id_list <- list(
    "Italy" = 1,
    "Spain" = 2,
    "Portugal" = 3,
    "Ireland" = 4
  )
  country_match <- agrep(country_name, names(associated_id_list), max.distance = 0.3)

  if (length(country_match) > 0) {
    country <- list()
    country$name <- names(associated_id_list[country_match[1]])
    country$id <- associated_id_list[country_match[1]][[1]]
    return(country)
  } else {
    # if no matches were found, return an error message
    return("Error: Country specified not found in list. Country partners avaialble are 'Italy','Spain', 'Portugal', 'Ireland'. If you are willing to partecipate and generate indicators for your country please write email at @maintaner <niccolo.salvini27@gmail.com> ")
  }
}




#' generate indicator schema
#' @keywords internal
#' @export
generate_indicator_schema <- function(.data, indicator_id, emergency, indicator_name, missing_cols = c("codice_regione", "provincia_codice", "citta_codice", "cf_amministrazione_appaltante", "codice_fiscale"),  country_name = "Italy", ...) {

  #check_columns(.data, missing_cols)

  common_schema <- .data %>%
    dplyr::transmute(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      ...,
      # aggregation_id = aggregation_id,
      emergency_name = emergency$em_name,
      emergency_id = emergency$em_id,
      country_id = get_country_id_from_name(country_name)$id,
      country_name = get_country_id_from_name(country_name)$name,
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
