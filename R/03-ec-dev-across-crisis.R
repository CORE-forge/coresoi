#' Compute Economic deviation across the crisis indicator
#'
#' @description
#'  The indicator reveals whether there has been an increase in the deviation of the contract actual execution economic value from its initial awarded value,
#'  with reference to contracts belonging to relevant economic market.
#'
#' ### Motivation
#' The red flag considers at risk companies/contracting authorities whose contracts undergo a significant increase of their _economic deviation ratio_ - i.e.,
#' ratio between actual amount paid and awarded economic value - across the crisis.
#'
#' ### Scoring Rule
#' The computation procedure returns _1 - p-value_ (so that high values of the indicator correspond to high levels of corruption risk). When computing the composite,
#' it will be dichotomised to 1 if statistical test is significant, and 0 otherwise (see [normalise()]).
#'
#' ### Main target unit
#' This indicator targets **Companies** and **Contracting authorities**
#' @param data a dataframe containing the data to use for computing the indicator.
#' @param award_value name of the variable in `data` containing the award value of each contract.
#' @param sums_paid name of the variable in `data` containing the amount paid by the contracting authority for each contract.
#' @param stat_unit name of the variable in `data` containing the target unit ID (in this case, company or contracting authority).
#' @param emergency_name string specifying the name of the emergency to consider. Examples could include "Coronavirus" or "Terremoto Centro Italia 2016-2017".
#' @param publication_date name of the variable in `data` containing the publication date of each contract.
#' @param test_type string specifying the statistical test to use for computing the indicator. Available options are "wilcoxon" and "ks" (Kolmogorov-Smirnov test).
#' @param cpvs character vector of CPV divisions (first two digits of CPV code) on which `data` are filtered out. Note: a panel of experts have already chosen which CPV
#' divisions are most affected by which emergency.
#' @param ... other parameters to pass to [generate_indicator_schema()], such as `country_name` (default: Italy).
#' @return indicator schema as from [generate_indicator_schema()]
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_3(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     award_value = importo_lotto,
#'     sums_paid = importo_finale,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus",
#'     test_type = "wilcoxon"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#' @rdname ind_3
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n
ind_3 <- function(data,
                  award_value,
                  sums_paid,
                  stat_unit,
                  emergency_name,
                  publication_date,
                  test_type,
                  cpvs,
                  ...) {
  indicator_id <- 3
  indicator_name <- "Economic deviation across the crisis"
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
    dplyr::filter(!is.na({{ award_value }}) &
      !is.na({{ sums_paid }}) &
      {{ award_value }} > 0 &
      {{ sums_paid }} > 0) %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post",
        false = "pre"
      ),
      prepost = factor(prepost, levels = c("post", "pre")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs,
        true = 1,
        false = 0
      ),
      ratio = {{ sums_paid }} / {{ award_value }}
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::filter(all(c("pre", "post") %in% prepost)) %>%
    dplyr::ungroup(prepost) %>%
    dplyr::summarise(
      aggregation_name = dplyr::first(!!rlang::sym(aggregation_name)),
      npre = sum(prepost == "pre"),
      npost = sum(prepost == "post"),
      mean_pre = mean(ratio[prepost == "pre"]),
      mean_post = mean(ratio[prepost == "post"]),
      median_pre = median(ratio[prepost == "pre"]),
      median_post = median(ratio[prepost == "post"]),
      ind_3 = test_set_2(var = ratio, group = prepost, data = ., test_type)[1]
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = 1 - ind_3, # 1 - pvalue
      aggregation_id = {{ stat_unit }},
      aggregation_name = aggregation_name,
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
