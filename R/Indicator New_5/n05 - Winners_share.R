#' @title Compute ratio of Gini of winning companies for agency, before and after a crisis
#' @description Compute ratio of Gini of winning companies for agency, before and after a crisis
#' @param data data to be passed, expects tibble
#' @param publication_date The date when the tender was published
#' @param agency Column of entities who decide who win the public procurement
#' @param winners Column of winning companies
#' @param outbreak_starting_date The date when the emergency officially started, Default: lubridate::ymd("2017-06-30")
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_5(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     agency = cf_amministrazione_appaltante,
#'     winners = denominazione,
#'     outbreak_starting_date = lubridate::ymd("2017-06-30")
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[rlang]{quo_expr}}, \code{\link[rlang]{as_string}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{rename}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[DescTools]{GiniSimpson}}
#'  \code{\link[tidyr]{pivot_wider}}
#' @rdname ind_5
#' @export
#' @importFrom lubridate ymd
#' @importFrom rlang quo_expr as_string
#' @importFrom dplyr mutate if_else count group_by summarise rename
#' @importFrom forcats as_factor
#' @importFrom DescTools Gini
#' @importFrom tidyr pivot_wider
ind_5 <- function(data,
                  agency,
                  publication_date,
                  winners,
                  outbreak_starting_date = lubridate::ymd("2017-06-30")
                  ) {
  indicator_id <- 5
  indicator_name <- "Gini of winners"
  aggregation_type <- rlang::quo_expr(enquo(stat_unit))

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= {{ outbreak_starting_date }},
                               true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
    ) %>%
    dplyr::group_by({{ agency }} , prepost) %>%
    dplyr::summarise(
      Norm_GS =
        DescTools::GiniSimpson({{ winners }} %>% as_factor()) / ((n() -1)/ n())
    ) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = prepost, values_from = Norm_GS) %>%
    dplyr::mutate(
      ratio = post / pre
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ agency }},
      ratio,
      aggregation_type = rlang::as_string(aggregation_type),
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = ind_5,
      aggregation_name = {{ agency }}
    ) %>%
    return()
}
