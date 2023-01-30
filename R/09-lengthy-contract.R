#' @title Compute Lengthy contracts indicator
#' @description Divergence in duration between expected and effective times of execution of the contract
#' @param data data to be passed, expects tibble
#' @param exp_end Expected end of the contract i.e. contract completion date
#' @param exp_start Expected initial execution of the contract
#' @param eff_end Effective end of the execution of the contract
#' @param eff_start Effective contract signature
#' @param stat_unit the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @param publication_date The date when the tender was published
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_9(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     exp_start = data_esecutivita_contratto,
#'     exp_end = data_termine_contrattuale,
#'     eff_end = data_effettiva_ultimazione,
#'     eff_start = data_stipula_contratto,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_9
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n
#' @importFrom forcats as_factor
ind_9 <- function(data,
                  exp_start,
                  exp_end,
                  eff_start,
                  eff_end,
                  stat_unit,
                  emergency_name,
                  publication_date) {
  indicator_id <- 9
  indicator_name <- "Lengthy contracts"
  aggregation_type <- rlang::quo_expr(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)


  data %>%
    dplyr::filter(
      !is.na({{ exp_end }}) &
        !is.na({{ eff_end }}) &
        !is.na({{ exp_start }}) &
        !is.na({{ eff_start }})
    ) %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      dplyr::across(dplyr::contains("data"), lubridate::ymd),
      ratio = as.numeric({{ eff_end }} - {{ eff_start }}) / as.numeric({{ exp_end }} - {{ exp_start }})
    ) %>%
    dplyr::group_by({{ stat_unit }}, prepost) %>%
    dplyr::summarise(
      prepost_count = dplyr::n(),
      ind_9_mean = mean(ratio, na.rm = TRUE),
      ind_9_median = median(ratio, na.rm = TRUE),
      ind_9_mean = round(ind_9_mean, 3)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ stat_unit }},
      ind_9_mean,
      aggregation_type = rlang::as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = ind_9_mean,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
