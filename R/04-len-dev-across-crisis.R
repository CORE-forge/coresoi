#' Compute Contract length deviation across the crisis indicator
#'
#' @description
#' The indicator reveals whether there has been an increase in the deviation of the contract actual execution duration from its
#' stated/expected duration.
#'
#' ### Motivation
#' The red flag considers at risk companies/contracting authorities whose contracts undergo a significant increase of their
#' _length deviation ratio_ - i.e., ratio between contract actual execution duration and expected duration - across the crisis.
#'
#' ### Scoring rule
#' The computation procedure returns _1 - p-value_ of the involved test (so that high values of the indicator correspond
#' to high levels of corruption risk). When computing the composite, it will be dichotomised to 1 if statistical test is
#' significant, and 0 otherwise (see [normalise()]).
#'
#' ### Main target unit
#' This indicator targets **companies** and **contracting authorities**.
#'
#' @param data a dataframe containing the data to use for computing the indicator.
#' @param exp_end name of the variable in `data` containing the expected end date of each contract, i.e., contract completion date.
#' @param eff_end name of the variable in `data` containing the contract execution effective end date.
#' @param eff_start name of the variable in `data` containing the effective start date of each contract.
#' @param stat_unit name of the variable in `data` containing the target unit ID (in this case, company or contracting authority).
#' @param emergency_name string specifying the name of the emergency to consider. Examples could include "Coronavirus" or
#' "Terremoto Centro Italia 2016-2017".
#' @param publication_date name of the variable in `data` containing the publication date of each contract.
#' @param test_type string specifying the statistical test to use for computing the indicator. Available options are "wilcoxon"
#' and "ks" (Kolmogorov-Smirnov test).
#' @param cpvs character vector of CPV divisions (first two digits of CPV code) on which `data` are filtered out.
#' Note: a panel of experts have already chosen which CPV divisions are most affected by which emergency.
#' @param ...  other parameters to pass to [generate_indicator_schema()], such as `country_name` (default: Italy).
#' @return indicator schema as from [generate_indicator_schema()].
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_4(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     exp_end = data_termine_contrattuale,
#'     eff_end = data_effettiva_ultimazione,
#'     eff_start = data_inizio_effettiva,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#' @rdname ind_4
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n
ind_4 <- function(data,
                  exp_end,
                  eff_start,
                  eff_end,
                  stat_unit,
                  emergency_name,
                  publication_date,
                  test_type,
                  cpvs,
                  ...) {
  indicator_id <- 4
  indicator_name <- "Length deviation across the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
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
    dplyr::filter(
      !is.na({{ exp_end }}) &
        !is.na({{ eff_end }}) &
        !is.na({{ eff_start }})
    ) %>%
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
      dplyr::across(dplyr::contains("data"), lubridate::ymd),
      ratio = as.numeric({{ eff_end }} - {{ eff_start }} + 1) /
        as.numeric({{ exp_end }} - {{ eff_start }} + 1)
    ) %>%
    dplyr::filter(ratio != Inf, flagdivision == 1) %>%
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
      ind_4 = test_set_2(var = ratio, group = prepost, data = ., test_type)[1]
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = 1 - ind_4, # 1 - pvalue
      aggregation_id = {{ stat_unit }},
      aggregation_name = aggregation_name,
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
