#' Compute Winning rate across the crisis indicator
#'
#' @description
#' The indicator focuses on companies that after the emergency outbreak were awarded public contracts much more frequently than before the emergency.
#'
#' ### Motivation:
#' The red flag considers at risk companies that **exceptionally increase** their competitive power over the emergency outbreak, in terms of proportion of awarded contracts on the relevant economic market.
#'
#' ### Scoring Rule:
#' The output will give $1 - pvalue$, which will then be dichotomised to 1 if statistical test is significant, 0 otherwise.
#'
#' ### Main target unit:
#' This indicator targets **Companies**
#'
#' @param data This argument should be a data frame or tibble containing the data you want to use to calculate the indicator.
#' @param publication_date This argument corresponds to the name of the column in data containing the publication date for each notice or report.
#' @param emergency_name This argument should be a character string specifying the name of the emergency or event you are analyzing. Examples could include "Coronavirus" or "Terremoto Aquila".
#' @param test_type  This argument should be a character vector specifying the type of hypothesis test (belonging to category 1 i.e. see statistical_tests.R) to apply to the data. Available options are "barnard", "fisher", or "z-test".
#' @param stat_unit This argument should be a character string specifying the statistical unit of measurement or aggregation variable for the indicator. In this indicator companies are the targets
#' @param cpvs  character vector of macro-cpv on which data is filtered out. A panel of experts have already chosen which cpvs are most affected by which emergency for you.
#' @param ... other parameters to pass to `generate_indicator_schema` as `country_name` if that not Italy, which is default behavior.
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
  # check_columns(.data, missing_cols)

  indicator_id <- 1
  indicator_name <- "Winning rate across the crisis"
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
      prepost = factor(prepost, levels = c("post", "pre")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs,
        true = 1,
        false = 0
      )
    ) %>%
    dplyr::filter(!is.na({{ stat_unit }})) %>%
    dplyr::group_by({{ stat_unit }}) %>% # .data[[aggregation_name]]
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
      diff_p2_p1 = p_2 - p_1,
      aggregation_name = dplyr::first(!!rlang::sym(aggregation_name))
    ) %>%
    # dplyr::filter(!is.na(p_1) & !is.na(p_2)) %>%
    dplyr::filter(n_12 > 0 | n_22 > 0) %>% # at least one contract in selected CPVs
    dplyr::rowwise() %>%
    dplyr::mutate(
      ## apply test
      tab = paste(n_11, n_12, n_21, n_22, sep = "-"),
      test = test_set_1(n_11, n_12, n_21, n_22, test_type)[1],
      # new companies --> at risk
      test = dplyr::if_else(m_1 == 0 & n_22 > 0,
        true = 0,
        false = test
      )
    ) %>%
    ungroup() %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = 1 - test,
      aggregation_name = aggregation_name,
      aggregation_id = {{ stat_unit }},
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
