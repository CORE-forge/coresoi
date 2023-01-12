#' @title Compute One-shot opportunistic companies indicator
#' @description The indicator reveals whether the company that won the public tender or contract has a previous history of at least one year since the opening of the tender.
#' @param data test bndcp data
#' @param publication_date The date when the tender was published
#' @param stat_unit The unique ID Code that identifies the awarded company (ex. VAT or Tax Number)
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_3(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
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
#' @rdname ind_3
#' @export
#' @importFrom lubridate ymd interval years %within%
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate across starts_with if_else group_by
#' @importFrom forcats as_factor
ind_3 <- function(data,
                  publication_date,
                  emergency_name,
                  stat_unit) {
  indicator_id <- 3
  indicator_name <- "One-shot opportunistic companies"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)

  data %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("data"), lubridate::ymd),
      prepost = dplyr::if_else({{ publication_date }} >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost)
    ) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      flag_opportunist = dplyr::if_else(max(data_aggiudicazione_definitiva) %within% lubridate::interval(data_pubblicazione - lubridate::years(1), data_pubblicazione), 1, 0),
      flag_opportunist = dplyr::if_else(data_pubblicazione <= data_aggiudicazione_definitiva, true = 0, false = 1)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      flag_opportunist,
      {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = flag_opportunist,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
