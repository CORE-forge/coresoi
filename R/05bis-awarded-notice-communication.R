#' @title Compute Awarded notice communication default indicator
#' @description The indicator reveals the fraction of contracts without any award notice communication.
#' @param data bndcp data
#' @param publication_date The date when the tender was published
#' @param cpv Common Procurement Vocabulary.The main vocabulary is based on a tree structure made up with codes of up to 9 digits (an 8 digit code plus a check digit). This combination of digits is associated with a wording that describes the type of supplies, works or services defining the subject of the contract.
#' @param outbreak_starting_date the date of the emergency outbreak, Default: lubridate::ymd("2017-06-30")
#' @param cpv_division initial two digits from cpv, Default: '33'
#' @param stat_unit statistical unit of measurement and stat_uniting
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_5_bis(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     cpv = cod_cpv,
#'     outbreak_starting_date = lubridate::ymd("2017-06-30"),
#'     cpv_division = "33",
#'     stat_unit = provincia
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{stat_unit_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname ind_5
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise distinct pull rename left_join arrange
#' @importFrom forcats as_factor
#' @importFrom stringr str_sub
ind_5_bis <- function(data,
                      publication_date,
                      cpv,
                      outbreak_starting_date = lubridate::ymd("2017-06-30"),
                      cpv_division = "33",
                      stat_unit) {
  indicator_id <- 5.1
  indicator_name <- "Awarded notice communication default"
  aggregation_type <- quo_expr(enquo(stat_unit))

  prel <- data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= outbreak_starting_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(stringr::str_sub({{ cpv }}, start = 1, end = 2) == cpv_division, 1, 0)
    ) %>%
    dplyr::group_by({{ stat_unit }}, prepost) %>%
    dplyr::summarise(prop_no_communic = 1 - mean(flagdivision))

  gr <- data %>%
    dplyr::distinct({{ stat_unit }}) %>%
    dplyr::pull()

  expand.grid(gr, prepost = c("pre", "post")) %>%
    dplyr::rename({{ stat_unit }} := Var1) %>%
    dplyr::left_join(prel) %>%
    dplyr::arrange({{ stat_unit }}) %>%
    dplyr::as_tibble() %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      prop_no_communic,
      {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = prop_no_communic,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
