#' @title Compute Contracts with modifications
#' @description The indicator reveals the fraction of contracts with at least one modification communication.
#' @param data mock bndcp data
#' @param publication_date PARAM_DESCRIPTION
#' @param stat_unit statistical unit of measurement, aggregation variable, the indicator target
#' @param id_variants the variants key id value
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema()` rows determined by aggregation level and `indicator_value` based on statistical test performed in `ind_10`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_10(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = provincia,
#'     id_variants = id_variante,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{rename}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stats]{prop.test}}
#' @rdname ind_10
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise n filter rowwise select contains rename
#' @importFrom forcats as_factor
#' @importFrom stats prop.test
ind_10 <- function(data,
                   publication_date,
                   stat_unit,
                   id_variants,
                   emergency_name) {
  indicator_id <- 10
  indicator_name <- "Contracts with modifications"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
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
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = correct_prop_test,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
