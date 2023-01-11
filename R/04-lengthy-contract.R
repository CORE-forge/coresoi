#' @title Compute divergence in durations between expected and effective end of the execution of the contract
#' @description Divergence in duration between expected and effective times of execution of the contract
#' @param data data to be passed, expects tibble
#' @param exp_end Expected end of the contract
#' @param eff_end Effective end of the execution of the contract
#' @param exp_start Starting date of execution
#' @param stat_unit the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by
#' @param outbreak_starting_date The date when the emergency officially started, Default: lubridate::ymd("2017-06-30")
#' @param publication_date The date when the tender was published
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_4(
#'     data = mock_data_core, publication_date = data_pubblicazione,
#'     exp_end = importo_aggiudicazione, eff_end = importo_lotto,
#'     cf_amministrazione_appaltante,
#'     outbreak_starting_date = lubridate::ymd("2017-06-30")
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_4
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n
#' @importFrom forcats as_factor
ind_4 <- function(data,
                  exp_start,
                   exp_end,
                   eff_end,
                   stat_unit,
                   outbreak_starting_date = lubridate::ymd("2017-06-30"),
                   publication_date) {
  indicator_id <- 4
  indicator_name <- "Distance between expected duration and effective Duration"
  aggregation_type <- rlang::quo_expr(enquo(stat_unit))

  data %>%
    dplyr::filter(
      !is.na({{ exp_end }}) &
        !is.na({{ eff_end }}) &
        !is.na({{ exp_start }})
      ) %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= outbreak_starting_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      ratio =
        as.numeric({{ eff_end }} - {{ exp_start }}) /
        as.numeric({{ exp_end }} - {{ exp_start }})
    ) %>%
    dplyr::group_by({{ stat_unit }}, prepost) %>%
    dplyr::summarise(
      prepost_count = dplyr::n(),
      ind_4_mean = mean(ratio, na.rm = TRUE),
      ind_4_median = median(ratio, na.rm = TRUE),
      ind_4_mean = round(ind_4_mean, 3)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ stat_unit }},
      ind_4_mean,
      aggregation_type = rlang::as_string(aggregation_type),
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = ind_4_mean,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
