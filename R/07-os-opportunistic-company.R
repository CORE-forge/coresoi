#' Compute One-shot opportunistic companies over the crisis indicator
#'
#' @description
#' The indicator focuses on companies that after the emergency outbreak win one or more public contracts,
#' without winning in the years before the emergency outbreak (e.g., in the preceding five years), with reference
#' to contracts belonging to relevant economic market.
#'
#' ### Motivation
#' The red flag considers at risk those companies that show a "one-shot opportunistic behaviour", that is,
#' companies that after the emergency outbreak have been awarded one or more public contracts but have not shown
#' any competitive power in the previous year(s).
#'
#' ### Scoring rule
#' If a company wins one or more contracts after the emergency outbreak but has not won any contracts in the years
#' before the emergency outbreak, the indicator will be equal to 1; otherwise, it will be equal to 0.
#'
#' ### Main target unit
#' This indicator targets **companies**.
#'
#' @param data a dataframe containing the data to use for computing the indicator.
#' @param stat_unit name of the variable in `data` containing the target unit ID (in this case, company).
#' @param final_award_date name of the variable in `data` containing award notice date for each contract.
#' @param emergency_name string specifying the name of the emergency to consider. Examples could include "Coronavirus" or
#' "Terremoto Centro Italia 2016-2017".
#' @param years_before how many years before the contract date we need to look for the presence of awards to a specific company.
#' @param ...  other parameters to pass to [generate_indicator_schema()], such as `country_name` (default: Italy).
#' @return indicator schema as from [generate_indicator_schema()].
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   mock_data_core <- mock_data_core |>
#'     tidyr::unnest(aggiudicatari, keep_empty = TRUE)
#'   ind_7(
#'     data = mock_data_core,
#'     final_award_date = data_aggiudicazione_definitiva,
#'     stat_unit = codice_fiscale,
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
                  ...) {
  indicator_id <- 7
  indicator_name <- "One-shot opportunistic companies over the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)
  aggregation_name <- italian_aggregation_mapping[[rlang::ensym(stat_unit)]]

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
      aggregation_name = dplyr::first(!!rlang::sym(aggregation_name)),
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
      aggregation_id = {{ stat_unit }},
      aggregation_name = aggregation_name,
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
