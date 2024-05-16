#' Compute Lenghty contracts indicator
#'
#' @description
#' The indicator reveals whether the (average) contract duration of a contracting authority/company is significantly longer
#' than the overall (average) contract duration, with reference to post-emergency contracts belonging to relevant economic market.
#'
#' ### Motivation
#' The red flag considers at risk contracting authorities/companies with post-emergency contracts whose excessive duration
#' is not justified by the nature of the crisis, that is, contracts longer than the overall average duration of contracts won
#' after the emergency outbreak.
#'
#' ### Scoring rule
#' The computation procedure returns _1 - p-value_ of the involved test - Wilcoxon test in this case - (so that high values of
#' the indicator correspond to high levels of corruption risk). When computing the composite,
#' it will be dichotomised to 1 if statistical test is significant, and 0 otherwise (see [normalise()]).
#'
#' ### Main target unit
#' This indicator targets **companies** and **contracting authorities**.
#'
#' @param data a dataframe containing the data to use for computing the indicator.
#' @param publication_date name of the variable in `data` containing the publication date of each contract.
#' @param stat_unit name of the variable in `data` containing the target unit ID (in this case, company or contracting authority).
#' @param eff_start name of the variable in `data` containing the effective start date of each contract.
#' @param eff_end name of the variable in `data` containing the contract execution effective end date.
#' @param emergency_name string specifying the name of the emergency to consider. Examples could include "Coronavirus" or
#' "Terremoto Centro Italia 2016-2017".
#' @param ...  other parameters to pass to [generate_indicator_schema()], such as `country_name` (default: Italy).
#' @return indicator schema as from [generate_indicator_schema()].
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_9(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     eff_start = data_inizio_effettiva,
#'     eff_end = data_effettiva_ultimazione,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{rename}}
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[rlang]{as_string}}
#' @rdname ind_9
#' @export
#' @importFrom dplyr mutate if_else filter pull group_by summarise n rename
#' @importFrom lubridate ymd
#' @importFrom forcats as_factor
#' @importFrom stringr str_sub
#' @importFrom rlang as_string
ind_9 <- function(data,
                  publication_date,
                  stat_unit,
                  eff_start,
                  eff_end,
                  emergency_name,
                  ...) {
  indicator_id <- 9
  indicator_name <- "Lengthy contracts"
  aggregation_type <- rlang::quo_squash(rlang::enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)
  aggregation_name <- italian_aggregation_mapping[[rlang::ensym(stat_unit)]]

  data_out <- data %>%
    dplyr::filter(!is.na({{ stat_unit }})) %>%
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
      contract_duration = dplyr::if_else(
        {{ eff_end }} < {{ eff_start }},
        true = NA_real_,
        false = 1 + as.numeric(lubridate::ymd({{ eff_end }})) - as.numeric(lubridate::ymd({{ eff_start }}))
      )
    )

  grand_mean <- data_out %>%
    dplyr::filter(prepost == "post") %>%
    dplyr::pull(contract_duration) %>%
    mean(na.rm = TRUE)

  data_out %>%
    dplyr::filter(prepost == "post" & !is.na(contract_duration)) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      aggregation_name = dplyr::first(!!rlang::sym(aggregation_name)),
      n = dplyr::n(),
      mean = mean(contract_duration, na.rm = TRUE),
      median = median(contract_duration, na.rm = TRUE),
      wilctest = stats::wilcox.test(contract_duration,
        mu = grand_mean,
        alternative = "greater"
      )$p.value %>% suppressWarnings()
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = 1 - wilctest, # 1 - pvalue
      aggregation_id = {{ stat_unit }},
      aggregation_name = aggregation_name,
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
