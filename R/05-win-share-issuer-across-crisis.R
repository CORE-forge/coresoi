#' @title Compute Winner's share of issuer's contract across the crisis indicator
#' @description Compute ratio of Gini of winning companies for agency, before and after a crisis
#' @param data data to be passed, expects tibble
#' @param publication_date The date when the tender was published
#' @param stat_unit Column of entities who decide who win the public procurement
#' @param winners Column of winning companies
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_5(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     winners = denominazione,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_5
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n count
#' @importFrom forcats as_factor
#' @importFrom DescTools Gini
#' @importFrom tydir pivot_wider
ind_5 <- function(data,
                  stat_unit,
                  publication_date,
                  winners,
                  emergency_name
) {
  indicator_id <- 5
  indicator_name <- "Winner's share of issuer's contract across the crisis"
  aggregation_type <- rlang::quo_expr(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
                               true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
    ) %>%
    dplyr::count( {{ stat_unit }} , {{ winners }} , prepost) %>%
    dplyr::group_by({{ stat_unit }}, prepost) %>%
    dplyr::summarise(
      gini = DescTools::Gini(n)
    ) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = prepost, values_from = gini) %>%
    dplyr::mutate(
      ratio = post / pre
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ stat_unit }},
      ratio,
      aggregation_type = rlang::as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = ratio,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
