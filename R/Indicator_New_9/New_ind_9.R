compute_ttest <- function(data, mu, alternative) {
  stats::t.test(x=data, mu=mu, alternative="greater")$p.value
}

#' @title Lenghty Contracts
#' @description Something
#' @param data data to be passed, expects tibble
#' @param publication_date The date when the tender was published
#' @param stat_unit the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by
#' @param cpv something
#' @param eff_start Effective start of the execution of the contract
#' @param eff_end Effective end of the execution of the contract
#' @param emergency_name Emergency
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_9(
#'     data = mock_data_core, publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     cpv = cod_cpv, eff_start = ..., eff_end = ...,
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
  aggregation_type <- quo_squash(enquo({{ stat_unit }}))
  emergency_scenario <- emergency_dates({{ emergency_name }})

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(
        lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post",false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(
        stringr::str_sub({{ cpv }}, start = 1, end = 2) == emergency_scenario$cpv,
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
      mean = mean(contract_duration, na.rm = TRUE), #to be compared with grand_mean
      # median = median(contract_duration, na.rm = TRUE),
      # test not to be performed if nnomiss == 1: not working, why?
      ttest = dplyr::if_else(nnomiss == 1,
                             true = NA_real_,
                             false = compute_ttest(
                               data=contract_duration, mu=grand_mean))
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ stat_unit }},
      output = ttest,
      aggregation_type = rlang::as_string(aggregation_type),
      emergency = emergency_name
    ) %>%
    dplyr::rename(
      indicator_value = output,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}

sinew::makeOxygen(ind_9)
