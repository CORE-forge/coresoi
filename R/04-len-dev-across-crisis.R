#' Compute Contract Lenght deviation across the crisis indicator
#'
#' @description
#'  The indicator reveals whether there has been a deviation of the contract actual execution duration from its stated/expected duration
#'
#' ### Motivation:
#' The red flag considers at risk companies whose contracts undergo a significant increase of their length deviation ratio - i.e., ratio between contract actual execution duration and expected duration - across the crisis
#'
#' ### Scoring Rule
#' If Test-statistic significant-> 1, otherwise -> 0
#'
#' ### Main target unit
#' This indicator targets **companies** and **contracting authorities**
#' @param data data to be passed, expects tibble
#' @param exp_end Expected end of the contract i.e. contract completion date
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
#'   ind_4(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
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
#' @rdname ind_4
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n
ind_4 <- function(data,
                  exp_end,
                  eff_start,
                  eff_end,
                  stat_unit,
                  emergency_name,
                  publication_date) {
  indicator_id <- 4
  indicator_name <- "Length deviation across the crisis"
  aggregation_type <- rlang::quo_expr(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)


  data %>%
    dplyr::filter(
      !is.na({{ exp_end }}) &
        !is.na({{ eff_end }}) &
        !is.na({{ eff_start }})
    ) %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = factor(prepost, levels = c("pre", "post")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs, 1, 0),
      dplyr::across(dplyr::contains("data"), lubridate::ymd),
      ratio = as.numeric({{ eff_end }} - {{ eff_start }}) / as.numeric({{ exp_end }} - {{ eff_start }})
    ) %>%
    dplyr::filter(ratio != Inf, flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::filter(all(c("pre", "post") %in% prepost)) %>%
    dplyr::summarise(
      prepost,
      prepost_count = dplyr::n(),
      ratio_mean = mean(ratio, na.rm = TRUE),
      ratio_median = median(ratio, na.rm = TRUE),
      ratio_mean = round(ratio_mean, 3)
    ) %>%
    mutate(
      ind_4 = compute_kolmogorov_smirnoff(var = ratio_mean, group = prepost, data = .)[1]
    ) %>%
    ungroup({{ stat_unit }}) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = ind_4,
      aggregation_name = {{ stat_unit }},
      aggregation_type = rlang::as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    return()
}
