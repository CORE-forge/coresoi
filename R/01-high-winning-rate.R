#' compute fisher test in dplyr pipeline
#' @description  compute fisher test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_fisher <- function(a, b, c, d) {
  data <- matrix(c(a, b, c, d), ncol = 2)
  c(
    p_value = round(fisher.test(data)$p.value, 3),
    fisher_estimate = round(fisher.test(data)$estimate, 3)
  )
}


#' @title Compute indicator High Winning Rate
#' @description it computes the indicator n° 1
#' @param data bndcp data
#' @param publication_date The date when the tender was published
#' @param cpv Common Procurement Vocabulary. The main vocabulary is based on a tree structure made up with codes of up to 9 digits (an 8 digit code plus a check digit). This combination of digits is associated with a wording that describes the type of supplies, works or services defining the subject of the contract
#' @param outbreak_starting_date the date of the emergency outbreak, Default: lubridate::ymd("2017-06-30")
#' @param divison first two digits in cpv code identifying the division, for more info check https://simap.ted.europa.eu/it/cpv
#' @param stat_unit statistical unit of measurement, aggregation variable, the indicator target
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("test_data_bndcp_core")
#'   ind_1(data = test_data_bndcp_core, publication_date = data_pubblicazione, cpv = cod_cpv, stat_unit = nome_provincia2)
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[tidyr]{nest}}
#' @rdname ind_1
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise n filter rowwise select contains
#' @importFrom forcats as_factor
#' @importFrom stringr str_sub
#' @importFrom tidyr unnest
ind_1 <- function(data,
                  publication_date,
                  cpv,
                  outbreak_starting_date = lubridate::ymd("2017-06-30"),
                  stat_unit) {

  # TODO: how do you make abstract aggiudicatatari, should I explicit that?
  # TODO: can make summarisation slimmer by defining a further function
  # TODO: desume emergency from date (f within, then ...)

  indicator_id <- 1
  indicator_name <- "High winning rate"


  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= outbreak_starting_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(stringr::str_sub({{ cpv }}, start = 1, end = 2) == "33", 1, 0)
    ) %>%
    tidyr::unnest(aggiudicatari, keep_empty = FALSE) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_11 = sum(flagdivision == 0 & prepost == "pre"),
      n_12 = sum(flagdivision == 1 & prepost == "pre"),
      n_21 = sum(flagdivision == 0 & prepost == "post"),
      n_22 = sum(flagdivision == 1 & prepost == "post"),
      m_1 = n_11 + n_12,
      m_2 = n_21 + n_22,
      p_1 = n_12 / m_1,
      p_2 = n_22 / m_2,
      diff_p2_p1 = p_2 - p_1
    ) %>%
    dplyr::filter(!is.na(p_1) & !is.na(p_2)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # prop_test: Simple asymptotic method (no correction)
      prop_test = stats::prop.test(
        x = c(n_22, n_12),
        n = c(m_2, m_1),
        correct = FALSE,
        alternative = "greater"
      )$p.value %>% suppressWarnings(),
      # correct_prop_test: Simple asymptotic method (with correction)
      correct_prop_test = stats::prop.test(
        x = c(n_22, n_12),
        n = c(m_2, m_1),
        correct = TRUE,
        alternative = "greater"
      )$p.value %>% suppressWarnings(),
      fisher_test = compute_fisher(n_11, n_12, n_21, n_22)[[1]],
      fisher_estimate = compute_fisher(n_11, n_12, n_21, n_22)[[2]]
    ) %>%
    dplyr::select({{ stat_unit }}, dplyr::contains("prop"), dplyr::contains("fisher")) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      fisher_test,
      {{ stat_unit }},
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = fisher_test,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
