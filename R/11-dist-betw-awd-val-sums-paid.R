#' @title Compute Distance between award value and sums paid indicator
#' @description The difference between the sum foreseen in the contract and the actual payment by the C.A. (ita SA stazione Appaltante)
#' @param data data to be passed, expects tibble
#' @param award_value The date when the tender was awarded
#' @param sums_paid The amount paid by the C.A.
#' @param stat_unit the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by
#' @param outbreak_starting_date The date when the emergency officially started, Default: lubridate::ymd("2017-06-30")
#' @param publication_date The date when the tender was published
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_11(
#'     data = test_data_bndcp_core, publication_date = data_pubblicazione,
#'     award_value = importo_aggiudicazione, sums_paid = imp_finale,
#'     cf_amministrazione_appaltante
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
ind_11 <- function(data, award_value, sums_paid, stat_unit,
                   outbreak_starting_date = lubridate::ymd("2017-06-30"),
                   publication_date) {
  indicator_id <- 11
  indicator_name <- "Distance between award value and sums paid"

  # TODO
  # - data filtering for NA or 0s
  # - coltype checks
  # - colnames existence
  # - error customisation
  # - compute indicator for a single cf
  # - might want to use group_by(across(variables))

  data %>%
    dplyr::filter(!is.na({{ award_value }}) & !is.na({{ sums_paid }}) & {{ award_value }} > 0 &
      {{ sums_paid }} > 0) %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= outbreak_starting_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      ratio = {{ sums_paid }} / {{ award_value }}
    ) %>%
    dplyr::group_by({{ stat_unit }}, prepost) %>%
    dplyr::summarise(
      prepost_count = dplyr::n(),
      ind_11_mean = mean(ratio, na.rm = TRUE),
      ind_11_median = median(ratio, na.rm = TRUE)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ stat_unit }},
      ind_11_mean,
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = ind_11_mean,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
