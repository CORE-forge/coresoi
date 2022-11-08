#' @title compute One-shot opportunistic companies
#' @description The indicator reveals whether the company that won the public tender or contract has a previous history of at least one year since the opening of the tender.
#' @param data test bndcp data
#' @param publication_date The date when the tender was published
#' @param awd_company The unique ID Code that identifies the awarded company (ex. VAT or Tax Number)
#' @param outbreak_starting_date the date of the emergency outbreak, Default: lubridate::ymd("2017-06-30")
#' @return tibble n x 7
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}, \code{\link[lubridate]{character(0)}}, \code{\link[lubridate]{interval}}, \code{\link[lubridate]{period}}
#'  \code{\link[tidyr]{nest}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_3
#' @export
#' @importFrom lubridate ymd interval years
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate across starts_with if_else group_by
#' @importFrom forcats as_factor
ind_3 <- function(data, publication_date, awd_company, outbreak_starting_date = lubridate::ymd("2017-06-30")) {
  data %>%
    tidyr::unnest(aggiudicatari) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("data"), lubridate::ymd),
      prepost = dplyr::if_else({{ publication_date }} >= lubridate::ymd("2017-06-30"), true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost)
    ) %>%
    dplyr::group_by(codice_fiscale) %>%
    dplyr::mutate(
      flag_opportunist = dplyr::if_else(max(data_aggiudicazione_definitiva) %within% lubridate::interval(data_pubblicazione - lubridate::years(1), data_pubblicazione), 1, 0),
      flag_opportunist = dplyr::if_else(data_pubblicazione <= data_aggiudicazione_definitiva, true = 0, false = 1)
    ) %>%
    ungroup() %>%
    return()
}
