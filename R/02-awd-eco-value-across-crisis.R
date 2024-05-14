#' Compute Awarded economic value across the crisis indicator
#'
#' @description
#' The indicator focuses on companies that after the emergency outbreak have been awarded public contracts in the relevant economic market with higher economic value than before the emergency occurred.
#'
#' ### Motivation
#' The red flag considers at risk those companies that exceptionally increase their competitive power over the outbreak, in terms of economic value of their awarded contracts on the relevant economic market.
#'
#' ### Scoring Rule
#' The computation procedure returns _1 - p-value_ (so that high values of the indicator correspond to high levels of corruption risk). When computing the composite, it will be dichotomised to 1 if statistical test is significant, and 0 otherwise (see [normalise()]).
#'
#' ### Main target unit
#' This indicator targets **Companies**
#' @param data a dataframe containing the data to use for computing the indicator.
#' @param contract_value name of the variable in `data` containing the economic amount of each contract.
#' @param stat_unit name of the variable in `data` containing the target unit ID (in this case, the company).
#' @param publication_date name of the variable in `data` containing the publication date of each contract.
#' @param test_type string specifying the statistical test to use for computing the indicator. Available options are "wilcoxon" and "ks" (Kolmogorov-Smirnov test).
#' @param emergency_name string specifying the name of the emergency to consider. Examples could include "Coronavirus" or "Terremoto Centro Italia 2016-2017".
#' @param cpvs character vector of CPV divisions (first two digits of CPV code) on which `data` are filtered out. Note: a panel of experts have already chosen which CPV divisions are most affected by which emergency.
#' @param ...  other parameters to pass to [generate_indicator_schema()], such as `country_name` (default: Italy).
#' @return indicator schema as from [generate_indicator_schema()]
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   mock_data_core <- mock_data_core |>
#'     tidyr::unnest(aggiudicatari, keep_empty = TRUE)
#'   ind_2(
#'     data = mock_data_core,
#'     contract_value = importo_lotto,
#'     publication_date = data_pubblicazione,
#'     stat_unit = codice_fiscale,
#'     test_type = "wilcoxon",
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{summarise}}
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
                  test_type,
                  cpvs,
                  ...) {
  indicator_id <- 2
  indicator_name <- "Awarded economic value across the crisis"
  emergency_scenario <- emergency_dates(emergency_name)
  aggregation_name <- italian_aggregation_mapping[[rlang::ensym(stat_unit)]]

  if (missing(cpvs)) {
    cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  }

  cpv_col <- grab_cpv(data = data)

  if (missing(test_type)) {
    test_type <- "wilcoxon"
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
    tidyr::drop_na({{ contract_value }}) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    # dplyr::filter(all(c("pre", "post") %in% prepost)) %>%
    # dplyr::ungroup(prepost) %>%
    dplyr::summarise(
      aggregation_name = dplyr::first(!!rlang::sym(aggregation_name)),
      npre = sum(prepost == "pre"),
      npost = sum(prepost == "post"),
      mean_pre = mean({{ contract_value }}[prepost == "pre"]),
      mean_post = mean({{ contract_value }}[prepost == "post"]),
      median_pre = median({{ contract_value }}[prepost == "pre"]),
      median_post = median({{ contract_value }}[prepost == "post"]),
      test = dplyr::case_when(
        npre > 0 & npost == 0 ~ 1, # not at risk, pvalue=1
        npre == 0 & npost > 0 ~ 0, # at risk, pvalue=0
        # npre > 0 & npost > 0 ~ test(var = {{ contract_value }}, group = prepost, data = ., test_type)[1],
        TRUE ~ test_set_2(var = {{ contract_value }}, group = prepost, data = ., test_type)[1]
      )
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
