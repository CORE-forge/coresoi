#' compute Wilcoxon-Mann-Whitney test in dplyr https://it.wikipedia.org/wiki/Test_di_Wilcoxon-Mann-Whitney
#' @description  compute Wilcoxon-Mann-Whitney test pvalue
#' @keywords internal
#' @export
compute_wilcox <- function(data, var, group, exact = TRUE, alternative = "greater") {
  test_res <- data %>%
    wilcox.test(var ~ group, data = ., exact = exact, alternative = alternative)
  c(
    p_value = round(test_res$p.value, 3),
    estimate = round(test_res$statistic, 3)
  )
}

#' compute Kolmogorov Smirnov test in dplyr https://it.wikipedia.org/wiki/Test_di_Kolmogorov-Smirnov
#' @description  compute Kolmogorov Smirnov test pvalue
#' @keywords internal
#' @export
compute_kolmogorov_smirnoff <- function(data, var, group, alternative = "less") {
  test_res <- suppressWarnings({
    data %>%
      ks.test(var ~ group, data = ., alternative = alternative)
  })

  c(
    p_value = round(test_res$p.value, 3),
    estimate = round(test_res$statistic, 3)
  )
}



#' @title Compute Awarded economic value across the crisis indicator
#' @description companies that after the emergency outbreak were awarded public contracts much higher in economic value than before the emergency scenario
#' @param data test bndcp data
#' @param contract_value the value of the contract
#' @param stat_unit statistical unit of measurement, aggregation variable, the indicator target
#' @param test_type character vector identifying the type of test you want to execute, alternatives are c("ks", "wilcoxon")
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema()` rows determined by aggregation level and `indicator_value` based on statistical test performed in `ind_2`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_2(
#'     data = mock_data_core,
#'     contract_value = importo_complessivo_gara,
#'     publication_date = data_pubblicazione,
#'     stat_unit = provincia,
#'     test_type = "ks",
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{summarise}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[tidyr]{nest}}, \code{\link[tidyr]{drop_na}}
#' @rdname ind_2
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by filter ungroup summarise
#' @importFrom stringr str_sub
#' @importFrom tidyr unnest drop_na
ind_2 <- function(data,
                  contract_value,
                  publication_date,
                  emergency_name,
                  stat_unit,
                  test_type) {
  indicator_id <- 2
  indicator_name <- "Awarded economic value across the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)


  test <- function(data, var, group, test_type) {
    switch(test_type,
      "ks" = {
        compute_kolmogorov_smirnoff(data, var, group)
      },
      "wilcoxon" = {
        compute_wilcox(data, var, group)
      },
      stop(paste0("No handler for ", test_type))
    )
  }

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = factor(prepost, levels=c("post", "pre")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs, 1, 0)
    ) %>%
    tidyr::drop_na({{ contract_value }}) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::filter(all(c("pre", "post") %in% prepost)) %>%
    dplyr::ungroup(prepost) %>%
    dplyr::summarise(
      count = n(),
      median_pre = median( {{ contract_value }}[prepost == "pre"]),
      median_post = median( {{ contract_value }}[prepost == "post"]),
      test = test(var = {{ contract_value }}, group = prepost, data = ., test_type)[1],
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = test,
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    return()
}
