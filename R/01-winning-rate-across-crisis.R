#' Compute Winning rate across the crisis indicator
#'
#' @description
#' The indicator focuses on companies that after the emergency outbreak win public contracts in the relevant economic market much more frequently than before the emergency.
#'
#' ### Motivation
#' The red flag considers at risk companies that exceptionally increase their competitive power over the emergency outbreak, in terms of proportion of awarded contracts on the relevant economic market(s).
#'
#' ### Scoring Rule
#' The computation procedure returns _1 - p-value_ (so that high values of the indicator correspond to high levels of corruption risk). When computing the composite, it will be dichotomised to 1 if statistical test is significant, and 0 otherwise (see [normalise()]).
#'
#' ### Main target unit
#' This indicator targets **Companies**
#'
#' @param data a dataframe containing the data to use for computing the indicator.
#' @param publication_date name of the variable in `data` containing the publication date of each contract.
#' @param emergency_name string specifying the name of the emergency to consider. Examples could include "Coronavirus" or "Terremoto Centro Italia 2016-2017".
#' @param test_type string specifying the statistical test to use for computing the indicator. Available options are "barnard", "fisher", or "z-test".
#' @param stat_unit name of the variable in `data` containing the target unit ID (in this case, the company).
#' @param cpvs character vector of CPV divisions (first two digits of CPV code) on which `data` are filtered out. Note: a panel of experts have already chosen which CPV divisions are most affected by which emergency.
#' @param ... other parameters to pass to [generate_indicator_schema()], such as `country_name` (default: Italy).
#' @return indicator schema as from [generate_indicator_schema()]
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   mock_data_core <- mock_data_core |>
#'     tidyr::unnest(aggiudicatari, keep_empty = TRUE)
#'   ind_1(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = codice_fiscale,
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
