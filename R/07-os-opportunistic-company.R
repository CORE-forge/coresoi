#' @title Compute One-shot opportunistic companies over the crisiss indicator
#' @description The indicator focuses on companies that after the emergency outbreak were awarded one or more public contracts having participated in the public procurement process without winning in the 5 years before the emergency outbreak
#'
#' ### Motivation:
#' The red flag considers at risk those companies that **show a "one- shot opportunistic behaviour"**, that is, companies that after the emergency outbreak were awarded one or more public contracts but _did not show any competitive power in the previous years_
#'
#' ### Scoring Rule
#' If a company wins one or more contracts after the emergency outbreak having participated in the public procurement process without winning in the 5 years before the emergency outbreak -> 1, otherwise 0
#'
#' ### Main target unit
#' This indicator targets **companies**
#' @param data mock_data_core exmaple data
#' @param publication_date The date when the tender was published
#' @param stat_unit The unique ID Code that identifies the awarded company (ex. VAT or Tax Number)
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_7(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}, \code{\link[lubridate]{interval}}, \code{\link[lubridate]{period}}
#'  \code{\link[tidyr]{nest}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_7
#' @export
#' @importFrom lubridate ymd interval years %within%
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate across starts_with if_else group_by
#' @importFrom forcats as_factor
ind_7 <- function(data,
                  publication_date,
                  emergency_name,
                  stat_unit) {
  indicator_id <- 7
  indicator_name <- "One-shot opportunistic companies over the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)

  data %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("data"), lubridate::ymd),
      prepost = dplyr::if_else({{ publication_date }} >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs, 1, 0)
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      flag_opportunist = dplyr::if_else(max(data_aggiudicazione_definitiva) %within% lubridate::interval(data_pubblicazione - lubridate::years(1), data_pubblicazione), 1, 0),
      flag_opportunist = dplyr::if_else(data_pubblicazione <= data_aggiudicazione_definitiva, true = 0, false = 1)
    ) %>%
    ungroup({{ stat_unit }}) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = flag_opportunist,
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    return()
}
