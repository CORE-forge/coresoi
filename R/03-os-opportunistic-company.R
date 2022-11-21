#' @title Compute One-shot opportunistic companies indicator
#' @description The indicator reveals whether the company that won the public tender or contract has a previous history of at least one year since the opening of the tender.
#' @param data test bndcp data
#' @param publication_date The date when the tender was published
#' @param stat_unit The unique ID Code that identifies the awarded company (ex. VAT or Tax Number)
#' @param outbreak_starting_date the date of the emergency outbreak, Default: lubridate::ymd("2017-06-30")
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("test_data_bndcp_core")
#'   ind_3(test_data_bndcp_core, publication_date = data_pubblicazione, stat_unit = codice_fiscale)
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
ind_3 <- function(data, publication_date, outbreak_starting_date = lubridate::ymd("2017-06-30"), stat_unit) {
  indicator_id <- 3
  indicator_name <- "One-shot opportunistic companies"

  data %>%
    tidyr::unnest(aggiudicatari) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("data"), lubridate::ymd),
      prepost = dplyr::if_else({{ publication_date }} >= lubridate::ymd("2017-06-30"), true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost)
    ) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::mutate(
      flag_opportunist = dplyr::if_else(max(data_aggiudicazione_definitiva) %within% lubridate::interval(data_pubblicazione - lubridate::years(1), data_pubblicazione), 1, 0),
      flag_opportunist = dplyr::if_else(data_pubblicazione <= data_aggiudicazione_definitiva, true = 0, false = 1)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ stat_unit }},
      flag_opportunist,
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = flag_opportunist,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
