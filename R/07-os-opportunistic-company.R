#' @title Compute One-shot opportunistic companies over the crisiss indicator
#' @description The indicator reveals whether the company that won the public tender or contract has a previous history of at least one year since the opening of the tender.
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
#'     award_date = data_aggiudicazione_definitiva,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}, \code{\link[lubridate]{character(0)}}, \code{\link[lubridate]{interval}}, \code{\link[lubridate]{period}}
#'  \code{\link[tidyr]{nest}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_7
#' @export
#' @importFrom lubridate ymd interval years %within%
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate across starts_with if_else group_by
ind_7 <- function(data,
                  award_date,
                  emergency_name,
                  stat_unit,
                  years_before = 1) {
  indicator_id <- 7
  indicator_name <- "One-shot opportunistic companies over the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)

  data %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("data"), lubridate::ymd),
      prepost = dplyr::if_else({{ award_date }} >= emergency_scenario$em_date,
                               true = "post",
                               false = "pre"),
      prepost = factor(prepost, levels=c("post", "pre")), #NOTE: here prepost refers to the award date!
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs, 1, 0),
    ) %>%
    dplyr::filter(flagdivision == 1 & !is.na({{ award_date }}) & !is.na({{ stat_unit }})) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      ncontr = n(),
      flag_oneshot = dplyr::if_else(any(prepost == "post") &
                                 max({{ award_date }}[prepost == "pre"]) < emergency_scenario$em_date -
                                   lubridate::years(years_before),
                               true = 1,
                               false = 0)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = flag_oneshot,
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    return()
}
