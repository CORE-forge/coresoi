#' Compute One-shot opportunistic companies over the crisiss indicator
#'
#' @description
#' The indicator focuses on companies that after the emergency outbreak were awarded one or more public contracts having participated in the public procurement process without winning in the 5 years before the emergency outbreak
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
#' @param stat_unit The unique ID Code that identifies the awarded company (ex. VAT or Tax Number)
#' @param final_award_date Date of award, as per the minutes
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @param cpvs a vector of cpv on which contracts are filtered
#' @param years_before int how many years we have to
#' @param ... other parameters for generate_indicator_schema as country_name
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_7(
#'     data = mock_data_core,
#'     final_award_date = data_aggiudicazione_definitiva,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus",
#'     years_before = 1
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}, \code{\link[lubridate]{interval}}, \code{\link[lubridate]{period}}
#'  \code{\link[tidyr]{nest}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}
#' @rdname ind_7
#' @export
#' @importFrom lubridate ymd interval years %within%
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate across starts_with if_else group_by
ind_7 <- function(data,
                  final_award_date,
                  emergency_name,
                  stat_unit,
                  years_before,
                  cpvs,
                  ...) {
  indicator_id <- 7
  indicator_name <- "One-shot opportunistic companies over the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  if (missing(cpvs)) {
    cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  }
  cpv_col <- grab_cpv(data = data)

  data %>%
    dplyr::filter(!is.na({{ stat_unit }})) %>%
    dplyr::filter(!is.na({{ final_award_date }})) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("data"), lubridate::ymd),
      # NOTE: prepost according to final_award_date
      prepost = dplyr::if_else({{ final_award_date }} >= emergency_scenario$em_date,
        true = "post",
        false = "pre"
      ),
      prepost = factor(prepost, levels = c("post", "pre")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs,
        true = 1,
        false = 0
      )
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    # rimosso filtro: se non ha un post non è a rischio, la condizione sotto assegnerà flag_oneshot = 0
    # dplyr::filter(any(prepost == "post")) %>%
    dplyr::summarise(
      ncontr = dplyr::n(),
      npre = sum(prepost == "pre"),
      npost = sum(prepost == "post"),
      flag_oneshot = dplyr::if_else(
        any(prepost == "post") & max({{ final_award_date }}[prepost == "pre"]) <
          emergency_scenario$em_date - lubridate::years(years_before),
        true = 1,
        false = 0
      ) %>% suppressWarnings()
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = flag_oneshot, # no test
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
