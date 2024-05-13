#'  Compute Award communication default across the crisis indicator
#'
#' @description
#' The indicator reveals whether - and to what extent - the duty of contracting authorities to communicate the public procurement procedure activation to the Anticorruption Authority ended with a technical failure.
#'
#' ### Motivation:
#' The red flag considers at risk contracting authorities **who fail to accomplish their duty** to communicate the public procurement procedure activation to the Anticorruption Authority
#'
#' ### Scoring Rule
#' The output will give $1 - pvalue$, which will then be dichotomised to 1 if statistical test is significant, 0 otherwise.
#'
#' ### Main target unit
#' This indicator targets **contracting authorities**
#' @param data This argument should be a data frame or tibble containing the data you want to use to calculate the indicator.
#' @param publication_date This argument corresponds to the name of the column in data containing the publication date for each notice or report.
#' @param emergency_name This argument should be a character string specifying the name of the emergency or event you are analyzing. Examples could include "Coronavirus" or "Terremoto Aquila".
#' @param award_col This argument corresponds to the name of the column in data containing the unique identification number for each contract award. This column should contain numeric or character values.
#' @param stat_unit This argument should be a character string specifying the statistical unit of measurement or aggregation variable for the indicator. In this indicator contracting authorities are the targets.
#' @param test_type This argument should be a character vector specifying the type of hypothesis test belonging to category 1 i.e. see statistical_tests.R)to apply to the data. Available options are "barnard", "fisher", or "z-test".
#' @param emergency_name This argument should be a character string specifying the name of the emergency or event you are analyzing. Examples could include "Coronavirus" or "Terremoto Aquila".
#' @param cpvs character vector of macro-cpv on which data is filtered out. A panel of experts have already chosen which cpvs are most affected by which emergency for you.
#' @param ...  other parameters to pass to `generate_indicator_schema` as `country_name` if that not Italy, which is default behavior.
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
#'     stat_unit = cf_amministrazione_appaltante,
#'     test_type = "fisher"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname ind_6
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise distinct pull rename left_join arrange
#' @importFrom stringr str_sub
ind_6 <- function(data,
                  publication_date,
                  emergency_name,
                  award_col,
                  stat_unit,
                  test_type,
                  cpvs,
                  ...) {
  indicator_id <- 6
  indicator_name <- "Communication default across the crisis"
  emergency_scenario <- emergency_dates(emergency_name)
  aggregation_name <- italian_aggregation_mapping[[rlang::ensym(stat_unit)]]

  if (missing(cpvs)) {
    cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  }
  cpv_col <- grab_cpv(data = data)

  if (missing(test_type)) {
    test_type <- "fisher"
  }

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post",
        false = "pre"
      ),
      prepost = factor(prepost, levels = c("pre", "post")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs,
        true = 1,
        false = 0
      ),
      flag_missing = dplyr::if_else(is.na({{ award_col }}),
        true = 1,
        false = 0
      )
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      aggregation_name = dplyr::first(!!rlang::sym(aggregation_name)),
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
    # remove unit with missing information in pre and post
    dplyr::filter(!is.na(p_1) & !is.na(p_2)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ## apply test
      test = test_set_1(n_11, n_12, n_21, n_22, test_type)[1],
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = 1 - test, # 1 - pvalue
      aggregation_id = {{ stat_unit }},
      aggregation_name = aggregation_name,
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
