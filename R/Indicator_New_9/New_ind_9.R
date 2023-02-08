#' @title Lenghty Contracts
#' @description Something
#' @param data data to be passed, expects tibble
#' @param publication_date The date when the tender was published
#' @param stat_unit the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by
#' @param cpv something
#' @param cpv_focus something
#' @param eff_start Effective start of the execution of the contract
#' @param eff_end Effective end of the execution of the contract
#' @param emergency_name Emergency
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_4(
#'     data = mock_data_core, publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     cpv = ..., cpv_focus = "33",
#'     eff_start = ..., eff_end = ...,
#'     emergency_name = ...
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_4
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n
#' @importFrom forcats as_factor
ind_9 <- function(data,
                  publication_date,
                  stat_unit,
                  cpv,
                  cpv_focus = "33",
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
        stringr::str_sub({{ cpv }}, start = 1, end = 2) == cpv_focus,
        true = 1, false = 0),
      contract_duration = dplyr::if_else(
        {{ eff_end }} < {{ eff_start }},
        true = NA_real_, false =
          1 + as.numeric({{ eff_end }} - {{ eff_start }})
        )
      ) -> data_out

  grand_mean <- data_out %>%
    dplyer::filter(prepost == "post") %>%
    dplyer::pull(contract_duration) %>%
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
                             false = compute_ttest(data=contract_duration, mu=grand_mean))
      # the ideal would be using 'comp_grandmean' (see below), but it's computer intensive
    ) -> out
  # return()
  return(list(data=data_out, out=out))

  ### C'è un problema con questo indicatore: io non so cosa andare a mettere come output dello schema

  compute_ttest <- function(data, mu, alternative) {
    stats::t.test(x=data, mu=mu, alternative="greater")$p.value
  }

  comp_grandmean <- function(y, x) {
    # y: continuous variable (contract duration)
    # x: factor (companies/contracting authorities)
    a <- stats::aov(y~x)
    out <- multcomp::glht(a, linfct = multcomp::mcp(x = "GrandMean"), alternative="greater")
    summ_out <- multcomp::summary(out) #computer intensive
    return(summ_out)
  }

 %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      {{ stat_unit }},
      ind_9_mean,
      aggregation_type = rlang::as_string(aggregation_type),
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = ind_9_mean,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
