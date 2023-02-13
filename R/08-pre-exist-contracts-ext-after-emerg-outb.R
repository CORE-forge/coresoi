#' @title Compute Pre-existing contracts modified after the crisis indicator
#' @description The contract changed
#' @param data bndcp data
#' @param publication_date The date when the tender was published
#' @param stat_unit tatistical unit of measurement, aggregation variable, the indicator target. In this case the identifier of agency or winners.
#' @param variant_date Date of contract variation
#' @param months_win time window for variation to be happening
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
#'     variant_date = data_approvazione_variante,
#'     months_win = 6,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{n_distinct}}, \code{\link[dplyr]{rename}}
#'  \code{\link[lubridate]{ymd}}, \code{\link[lubridate]{interval}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_8
#' @export
#' @importFrom dplyr mutate if_else group_by summarise n n_distinct rename
#' @importFrom lubridate ymd interval
#' @importFrom forcats as_factor
ind_8 <- function(data,
                  publication_date,
                  stat_unit,
                  variant_date,
                  emergency_name,
                  months_win = 6) {
  indicator_id <- 8
  indicator_name <- "Pre-existing contracts modified after the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(
        lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post",
        false = "pre"
      ),
      prepost = forcats::as_factor(prepost),
      flag_modif = dplyr::if_else(
        prepost == "pre" & lubridate::ymd({{ variant_date }}) %within%
          lubridate::interval(
            emergency_scenario$em_date,
            emergency_scenario$em_date %m+% months(months_win)
          ),
        true = 1,
        false = 0
      ),
    ) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      n = dplyr::n(),
      npre = sum(prepost == "pre"),
      ncig = dplyr::n_distinct(cig),
      #    ncig_pre = data.table::uniqueN(cig[prepost == "pre"]),
      nmod = sum(flag_modif == 1),
      #    ncig_mod = data.table::uniqueN(cig[flag_modif == 1]),
      # prop_mod = nmod/npre,
      rf_value = 1 * (nmod > 0)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      rf_value,
      {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = rf_value,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
