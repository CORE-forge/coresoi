#' compute Fisher-exact test https://en.wikipedia.org/wiki/Fisher%27s_exact_test
#' @description  compute fisher test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_fisher <- function(a, b, c, d) {
  compute_fisher <- function(a, b, c, d) {
    if (any(is.na(list(a, b, c, d)))) {
      stop("All inputs must be non-missing")
    }

    data <- matrix(c(a, b, c, d), ncol = 2)
    c(
      p_value = round(fisher.test(data, alternative = "greater")$p.value, 3),
      estimate = round(fisher.test(data, alternative = "greater")$estimate, 3)
    )
  }
}

#' compute Barnard test https://en.wikipedia.org/wiki/Barnard%27s_test
#' @description  compute Barnard test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_barnard <- function(a, b, c, d, method = "boschloo") {
  if (any(is.na(list(a, b, c, d)))) {
    stop("All inputs must be non-missing")
  }

  data <- matrix(c(a, b, c, d), ncol = 2)
  c(
    p_value = suppressWarnings({
      round(DescTools::BarnardTest(data, alternative = "greater", method = method)$p.value, 3)
    }),
    estimate = suppressWarnings({
      round(DescTools::BarnardTest(data, alternative = "greater", method = method)$estimate, 3)
    })
  )
}

#' compute Z-test proportional
#' @description  compute Z-test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_prop_test <- function(a, b, c, d, correct = FALSE) {
  if (any(is.na(list(a, b, c, d)))) {
    stop("All inputs must be non-missing")
  }

  m_1 <- a + b
  m_2 <- c + d
  p_1 <- b / m_1
  p_2 <- d / m_2
  diff_p2_p1 <- p_2 - p_1

  c(
    p_value = stats::prop.test(
      x = c(d, b),
      n = c(m_2, m_1),
      correct = correct,
      alternative = "greater"
    )$p.value %>% suppressWarnings(),
    estimate = stats::prop.test(
      x = c(d, b),
      n = c(m_2, m_1),
      correct = correct,
      alternative = "greater"
    )$estimate %>% suppressWarnings()
  )
}


#' @title Compute Winning rate across the crisis indicator
#' @description companies that after the Covid19 outbreak were awarded public contracts much more frequently than before the Covid-19.
#' @param data bndcp data
#' @param publication_date The date when the tender was published
#' @param cpv Common Procurement Vocabulary. The main vocabulary is based on a tree structure made up with codes of up to 9 digits (an 8 digit code plus a check digit). This combination of digits is associated with a wording that describes the type of supplies, works or services defining the subject of the contract
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @param divison first two digits in cpv code identifying the division, for more info check https://simap.ted.europa.eu/it/cpv
#' @param test_type character vector identifying the type of test you want to, alternatives are c("barnard", "fisher", "z-test")
#' @param stat_unit statistical unit of measurement, aggregation variable, the indicator target
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("test_data_bndcp_core")
#'   ind_1(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     cpv = cod_cpv,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus",
#'     test_type = "fisher"
#'   )
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
                  emergency_name,
                  stat_unit,
                  test_type) {
  indicator_id <- 1
  indicator_name <- "Winning rate across the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)


  test <- function(a, b, c, d, test_type) {
    switch(test_type,
      "barnard" = {
        compute_barnard(a, b, c, d)
      },
      "fisher" = {
        compute_fisher(a, b, c, d)
      },
      "z-test" = {
        compute_prop_test(a, b, c, d)
      },
      stop(paste0("No handler for ", test_type))
    )
  }

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(stringr::str_sub({{ cpv }}, start = 1, end = 2) == "33", 1, 0)
    ) %>%
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
      ## apply test
      test = test(n_11, n_12, n_21, n_22, test_type)[1],
    ) %>%
    dplyr::select({{ stat_unit }}, dplyr::contains("test")) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      test,
      {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = test,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
