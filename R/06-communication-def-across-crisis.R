#' @title Compute Communication default across the crisis indicator
#' @description The indicator reveals the fraction of contracts without any award notice communication.
#' @param data mock_data_core example bdncp data
#' @param publication_date The date when the tender was published
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @param award_col column indentifying id for that contract award
#' @param stat_unit statistical unit of measurement and stat_uniting
#' @param test_type type of the test we would like to apply
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_6(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     emergency_name = "coronavirus",
#'     award_col = id_aggiudicazione,
#'     stat_unit = provincia,
#'     test_type = "fisher"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{stat_unit_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname ind_6
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise distinct pull rename left_join arrange
#' @importFrom forcats as_factor
#' @importFrom stringr str_sub
ind_6 <- function(data,
                  publication_date,
                  emergency_name,
                  award_col,
                  stat_unit,
                  test_type) {
  indicator_id <- 6
  indicator_name <- "Communication default across the crisis"
  aggregation_type <- quo_expr(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)

  test <- function(a, b, c, d, test_type) {
    switch(test_type,
      "barnard" = {
        compute_barnard(d, b, c, a)
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
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs, 1, 0),
      flag_missing = dplyr::if_else(is.na({{ award_col }}), 1, 0)
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_11 = sum(flag_missing == 0 & prepost == "pre"),
      n_12 = sum(flag_missing == 1 & prepost == "pre"),
      n_21 = sum(flag_missing == 0 & prepost == "post"),
      n_22 = sum(flag_missing == 1 & prepost == "post"),
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
