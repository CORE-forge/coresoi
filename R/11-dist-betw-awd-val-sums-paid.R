#' @title Compute Distance between award value and sums paid indicator
#' @description The difference between the sum foreseen in the contract and the actual payment by the C.A. (ita SA stazione Appaltante)
#' @param data data to be passed, expects tibble
#' @param award_value The date when the tender was awarded
#' @param sums_paid The amount paid by the C.A.
#' @param stat_unit the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @param publication_date The date when the tender was published
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_11(
#'     data = mock_data_core, publication_date = data_pubblicazione,
#'     award_value = importo_aggiudicazione, sums_paid = importo_lotto,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_11
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n
#' @importFrom forcats as_factor
ind_11 <- function(data,
                   award_value,
                   sums_paid,
                   stat_unit,
                   emergency_name,
                   publication_date) {
  indicator_id <- 11
  indicator_name <- "Distance between award value and sums paid"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)


  # TODO
  # - data filtering for NA or 0s
  # - coltype checks
  # - colnames existence
  # - compute indicator for a single cf
  # - might want to use group_by(across(variables))

  data %>%
    dplyr::filter(!is.na({{ award_value }}) & !is.na({{ sums_paid }}) & {{ award_value }} > 0 &
      {{ sums_paid }} > 0) %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      ratio = {{ sums_paid }} / {{ award_value }}
    ) %>%
    dplyr::group_by({{ stat_unit }}, prepost) %>%
    dplyr::summarise(
      prepost_count = dplyr::n(),
      ind_11_mean = mean(ratio, na.rm = TRUE),
      ind_11_median = median(ratio, na.rm = TRUE),
      ind_11_mean = round(ind_11_mean, 3)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ stat_unit }},
      ind_11_mean,
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = ind_11_mean,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
