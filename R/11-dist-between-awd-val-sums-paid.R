#' @title ind_11
#' @description The difference between the sum foreseen in the contract and the actual payment by the C.A.
#' @param data dataset to be passed, expects tibble
#' @param award_value The date when the tender was awarded
#' @param sums_paid The amount paid by the C.A.
#' @param group the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by
#' @param outbreak_starting_date The date when the emergency officially started, Default: lubridate::ymd("2017-06-30")
#' @param publication_date The date when the tender was published
#' @return a tibble [n x 5] containing cf_amministrazione_appaltante
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_11(
#'     .data = bncp_data, publication_date = data_pubblicazione,
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
ind_11 <- function(data, award_value, sums_paid, group,
                   outbreak_starting_date = lubridate::ymd("2017-06-30"),
                   publication_date) {

  # TODO
  # - data filtering for NA or 0s
  # - coltype checks
  # - colnames existence
  # - error customisation
  # - compute indicator for a single cf
  # - might want to use group_by(across(variables))

  df <- data %>%
    dplyr::filter(!is.na({{ award_value }}) & !is.na({{ sums_paid }}) & {{ award_value }} > 0 &
      {{ sums_paid }} > 0) %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= outbreak_starting_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      ratio = {{ sums_paid }} / {{ award_value }}
    ) %>%
    dplyr::group_by({{ group }}, prepost) %>%
    dplyr::summarise(
      prepost_count = dplyr::n(),
      ind_11_mean = mean(ratio, na.rm = TRUE),
      ind_11_median = median(ratio, na.rm = TRUE)
    )

  return(df)
}
