#' @title Compute Pre-existing contracts modified after the crisis indicator
#' @description The indicator reveals the fraction of contracts with at least one modification communication.
#' @param data mock bndcp data
#' @param publication_date PARAM_DESCRIPTION
#' @param stat_unit statistical unit of measurement, aggregation variable, the indicator target
#' @param id_variants the variants key id value
#' @param outbreak_starting_date the date of the emergency outbreak (the official one according to emergency declaration), , Default: lubridate::ymd("2017-06-30")
#' @return indicator schema as from `generate_indicator_schema()` rows determined by aggregation level and `indicator_value` based on statistical test performed in `ind_10`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_8(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = provincia,
#'     id_variants = id_variante,
#'     outbreak_starting_date = lubridate::ymd("2017-06-30")
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{rename}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stats]{prop.test}}
#' @rdname ind_8
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise n filter rowwise select contains rename
#' @importFrom forcats as_factor
#' @importFrom stats prop.test
ind_8 <- function(data,
                  publication_date,
                  stat_unit,
                  id_variants,
                  outbreak_starting_date = lubridate::ymd("2017-06-30")) {
  indicator_id <- 8
  indicator_name <- "Pre-existing contracts modified after the crisis"
  aggregation_type <- quo_expr(enquo(stat_unit))

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= outbreak_starting_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      flag_variant = dplyr::if_else(!is.na({{ id_variants }}), true = 1, false = 0)
    ) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_11 = sum(flag_variant == 0 & prepost == "pre"),
      n_12 = sum(flag_variant == 1 & prepost == "pre"),
      n_21 = sum(flag_variant == 0 & prepost == "post"),
      n_22 = sum(flag_variant == 1 & prepost == "post"),
      m_1 = n_11 + n_12,
      m_2 = n_21 + n_22,
      p_1 = n_12 / m_1,
      p_2 = n_22 / m_2,
      diff_p2_p1 = p_2 - p_1
    ) %>%
    dplyr::filter(!is.na(p_1) & !is.na(p_2)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      prop_test = stats::prop.test(
        x = c(n_22, n_12),
        n = c(m_2, m_1),
        correct = FALSE,
        alternative = "greater"
      )$p.value %>% suppressWarnings(),
      correct_prop_test = stats::prop.test(
        x = c(n_22, n_12),
        n = c(m_2, m_1),
        correct = TRUE,
        alternative = "greater"
      )$p.value %>% suppressWarnings(),
      correct_prop_test = round(correct_prop_test, 3)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      correct_prop_test,
      {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = correct_prop_test,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
