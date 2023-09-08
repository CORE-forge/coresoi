#' Compute Lenghty Contracts indicator
#'
#' @description
#' The indicator reveals whether the duration of a contract is significantly longer than the average length of awarded contracts after the emergency outbreak
#'
#' ### Motivation:
#' The red flag considers at risk contracts won after the emergency outbreak whose duration is not justified by the nature of the crisis, that is, contracts longer than the average duration of contracts won after the emergency outbreak
#'
#' ### Scoring Rule
#' The output will give $1 - pvalue$, which will then be dichotomised to 1 if statistical test is significant, 0 otherwise.
#'
#' ### Main target unit
#' This indicator targets **companies** and **contracting authorities**
#' @param data This argument should be a data frame or tibble containing the data you want to use to calculate the indicator.
#' @param publication_date This argument corresponds to the name of the column in data containing the publication date for each notice or report.
#' @param stat_unit This argument should be a character string specifying the statistical unit of measurement or aggregation variable for the indicator. In this indicator both companies and contracting authorities are the targets.
#' @param eff_start This argument corresponds to the name of the column in data containing the effective start date for each contract.
#' @param eff_end This argument corresponds to the name of the column in data containing the effective end date for each contract.
#' @param emergency_name This argument should be a character string specifying the name of the emergency or event you are analyzing. Examples could include "Coronavirus" or "Terremoto Aquila".
#' @return indicator schema as from `generate_indicator_schema`
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
        false = 1 + as.numeric( lubridate::ymd({{ eff_end }})) - as.numeric( lubridate::ymd({{ eff_start }}))
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
