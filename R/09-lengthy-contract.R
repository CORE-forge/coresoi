#' compute t test
#' @description  compute  test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_ttest <- function(mean_to_compare, ground_mean) {
  data %>%
    stats::t.test(x=mean_to_compare, mu=ground_mean, alternative="greater")$p.value
}

#' @title compute Lenghty Contracts indicator
#' @description The indicator reveals whether the duration of the contract is significantly longer than the average length of awarded contracts after the emergency.
#' @param data bndcp data
#' @param publication_date The date when the tender was published
#' @param stat_unit the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by
#' @param cpv Common Procurement Vocabulary. The main vocabulary is based on a tree structure made up with codes of up to 9 digits (an 8 digit code plus a check digit). This combination of digits is associated with a wording that describes the type of supplies, works or services defining the subject of the contract
#' @param eff_start Effective start of the execution of the contract
#' @param eff_end Effective end of the execution of the contract
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_9(
#'     data = mock_data_core, publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     cpv = cod_cpv,
#'     eff_start = ...,
#'     eff_end = ...,
#'     emergency_name = ...
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
                  cpv,
                  eff_start,
                  eff_end,
                  emergency_name) {
  indicator_id <- 9
  indicator_name <- "Lengthy contracts"
  aggregation_type <- rlang::quo_squash(rlang::enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(
        lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post",false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(
        stringr::str_sub({{ cpv }}, start = 1, end = 2) == "33",
        true = 1, false = 0),
      contract_duration = dplyr::if_else(
        {{ eff_end }} < {{ eff_start }},
        true = NA_real_, false =
          1 + as.numeric({{ eff_end }} - {{ eff_start }})
      )
    ) -> data_out

  grand_mean <- data_out %>%
    dplyr::filter(prepost == "post") %>%
    dplyr::pull(contract_duration) %>%
    mean(na.rm = TRUE)

  data_out %>%
    dplyr::filter(prepost == "post" & !is.na(contract_duration)) %>%
    # dplyr::filter(flagdivision == 1) %>%  #commented for the moment (very few data)
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      ntot = dplyr::n(),
      nnomiss = sum(!is.na(contract_duration)),
      mean = mean(contract_duration, na.rm = TRUE),
      ttest  = t.test(x = contract_duration, mu =grand_mean)$p.value) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      ttest,
      {{ stat_unit }},
      aggregation_type = rlang::as_string(aggregation_type),
      emergency = emergency_name
    ) %>%
    dplyr::rename(
      indicator_value = ttest,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}


## source("R/utils.R")
indicator_id <- 9
indicator_name <- "Lengthy contracts"
emergency_scenario <- emergency_dates("coronavirus")

mock_data_core %>%
  dplyr::mutate(
    prepost = dplyr::if_else(
      lubridate::ymd(data_pubblicazione) >= emergency_scenario$em_date,
      true = "post",false = "pre"),
    prepost = forcats::as_factor(prepost),
    flagdivision = dplyr::if_else(
      stringr::str_sub(cod_cpv, start = 1, end = 2) == "33",
      true = 1, false = 0),
    contract_duration = dplyr::if_else(
      data_effettiva_ultimazione < data_inizio_effettiva,
      true = NA_real_, false =
        1 + as.numeric(data_effettiva_ultimazione - data_inizio_effettiva)
    )
  ) -> data_out

grand_mean <- data_out %>%
  dplyr::filter(prepost == "post") %>%
  dplyr::pull(contract_duration) %>%
  mean(na.rm = TRUE)

wowo =data_out %>%
  dplyr::filter(prepost == "post" & !is.na(contract_duration)) %>%
  # dplyr::filter(flagdivision == 1) %>%  #commented for the moment (very few data)
  dplyr::group_by(cf_amministrazione_appaltante) %>%
  dplyr::mutate(
    ntot = dplyr::n(),
    mean = mean(contract_duration, na.rm = TRUE),
    values = list(contract_duration),
    nnomiss = sum(!is.na(contract_duration)),
    ttest  = t.test(x = contract_duration[nnomiss!=1], mu =grand_mean)$p.value)



  generate_indicator_schema(
    indicator_id = indicator_id,
    indicator_name = indicator_name,
    ttest,
    {{ stat_unit }},
    aggregation_type = rlang::as_string(aggregation_type),
    emergency = emergency_name
  ) %>%
  dplyr::rename(
    indicator_value = ttest,
    aggregation_name = {{ stat_unit }}
  ) %>%
  return()
