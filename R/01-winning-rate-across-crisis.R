#' compute Fisher-exact test https://en.wikipedia.org/wiki/Fisher%27s_exact_test
#' @description  compute fisher test pvalue and estimate in piped expression
#' @keywords internal
#' @export
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

#' compute Barnard test https://en.wikipedia.org/wiki/Barnard%27s_test
#' @description  compute Barnard test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_barnard <- function(a, b, c, d, method = "boschloo") {
  if (any(is.na(list(a, b, c, d)))) {
    stop("All inputs must be non-missing")
  }
  # only pre
  if ((a + b) > 0 & (c + d) == 0) {
    1
  }
  # only post
  else if ((a + b) == 0 & (c + d) > 0) {
    0
  } else {
    data <- matrix(c(d, b, c, a), ncol = 2)
    out_barn <- DescTools::BarnardTest(data, alternative = "greater", method = "boschloo") %>%
      suppressWarnings()
    c(
      p_value = round(out_barn$p.value, 5),
      estimate = round(out_barn$estimate, 3)
    )
  }
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


#' Compute Winning rate across the crisis indicator
#'
#' @description
#' The indicator focuses on companies that after the emergency outbreak were awarded public contracts much more frequently than before the emergency.
#'
#' ### Motivation:
#' The red flag considers at risk companies that **exceptionally increase** their competitive power over the emergency outbreak, in terms of proportion of awarded contracts on the relevant economic market.
#'
#' ### Scoring Rule
#' If Test-statistic significant-> 1, otherwise -> 0
#'
#' ### Main target unit
#' This indicator targets **Companies**
#' @param data bndcp data
#' @param publication_date date of publication of the notice
#' @param emergency_name emergency name character string for which you wish to calculate the indicator for, e.g. "Coronavirus" "Terremoto Aquila"
#' @param test_type  character vector string to identifying the test type you want to apply, available alternatives are c("barnard", "fisher", "z-test")
#' @param stat_unit statistical unit of measurement, aggregation variable, the indicator target
#' @param cpvs a vector of cpv on which contracts are filtered
#' @return indicator schema as from [generate_indicator_schema()]
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_1(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus",
#'     test_type = "fisher"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{rowwise}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[tidyr]{nest}}
#' @rdname ind_1
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise n filter rowwise select contains
#' @importFrom stringr str_sub
#' @importFrom tidyr unnest
ind_1 <- function(data,
                  publication_date,
                  emergency_name,
                  stat_unit,
                  test_type,
                  cpvs,
                  ...) {
  indicator_id <- 1
  indicator_name <- "Winning rate across the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))


  emergency_scenario <- emergency_dates(emergency_name)
  if(missing(cpvs)){
    cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  }
  cpv_col <- grab_cpv(data = data)

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
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post",
        false = "pre"
      ),
      prepost = factor(prepost, levels = c("post", "pre")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs,
        true = 1,
        false = 0
      )
    ) %>%
    dplyr::filter(!is.na({{ stat_unit }})) %>%
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
    # dplyr::filter(!is.na(p_1) & !is.na(p_2)) %>%
    dplyr::filter(n_12 > 0 | n_22 > 0) %>% # at least one contract in selected CPVs
    dplyr::rowwise() %>%
    dplyr::mutate(
      ## apply test
      tab = paste(n_11, n_12, n_21, n_22, sep = "-"),
      test = test(n_11, n_12, n_21, n_22, test_type)[1],
      # new companies --> at risk
      test = dplyr::if_else(m_1 == 0 & n_22 > 0,
        true = 0,
        false = test
      )
    ) %>%
    dplyr::select({{ stat_unit }}, dplyr::contains("test")) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = 1 - test, # 1 - pvalue
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
