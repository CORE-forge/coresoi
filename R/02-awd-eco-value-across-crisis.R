#' Compute Awarded economic value across the crisis indicator
#'
#' @description
#' The indicator focuses on companies that after the emergency outbreak were awarded public contracts much higher in economic value than before the emergency
#'
#' ### Motivation:
#' The red flag considers at risk those companies that **exceptionally increase** their _competitive power_ over the outbreak, in terms of economic value of their awarded contracts on the relevant economic market.
#'
#' ### Scoring Rule
#' The output will give $1 - pvalue$, which will then be dichotomised to 1 if statistical test is significant, 0 otherwise.
#'
#' ### Main target unit
#' This indicator targets **Companies**
#' @param data This argument should be a data frame or tibble containing the data you want to use to calculate the indicator.
#' @param contract_value This argument corresponds to the name of the column in data containing the overall amount of the tender for each contract. The values in this column should be numeric.
#' @param stat_unit This argument should be a character string specifying the statistical unit of measurement or aggregation variable for the indicator. In this indicator companies are the target.
#' @param publication_date This argument corresponds to the name of the column in data containing the publication date for each notice or report.
#' @param test_type This argument should be a character vector specifying the type of hypothesis test (belonging to category 2 i.e. see statistical_tests.R) to apply to the data. Available options are "wilcoxon" and "ks".
#' @param emergency_name This argument should be a character string specifying the name of the emergency or event you are analyzing. Examples could include "Coronavirus" or "Terremoto Aquila".
#' @param cpvs character vector of macro-cpv on which data is filtered out. A panel of experts have already chosen which cpvs are most affected by which emergency for you.
#' @param ...  other parameters to pass to `generate_indicator_schema` as `country_name` if that not Italy, which is default behavior.
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
      emergency = emergency_scenario
    ) %>%
    return()
}
