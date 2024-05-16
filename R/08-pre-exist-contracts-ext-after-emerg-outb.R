#' Compute Pre-existing contracts modified after the crisis indicator
#'
#' @description
#' The indicator reveals whether a contracting authority/company has at least one pre-existing contract with respect
#' to the emergency outbreak (i.e., with a call before the beginning of the emergency), which has been modified after
#' the emergency outbreak, with reference to relevant economic market.
#'
#' ### Motivation
#' The red flag considers at risk the contracting authority/company with existing contracts before the emergency outbreak
#' and subsequently modified through variants after the emergency began; this assessment excludes modifications made immediately
#' thereafter, using a parametric window (e.g., within the following six months).
#'
#' ### Scoring rule
#' For a given target unit, if a contract existing before the outbreak is modified through variants after the emergency outbreak
#' (given the parametric window, e.g., six months), the indicator will be equal to 1; otherwise, it will be equal to 0.
#'
#' ### Main target unit
#' This indicator targets **companies** and **contracting authorities**.
#'
#' @param data a dataframe containing the data to use for computing the indicator.
#' @param publication_date name of the variable in `data` containing the publication date of each contract.
#' @param stat_unit name of the variable in `data` containing the target unit ID (in this case, company or contracting authority).
#' @param variant_date name of the variable in `data` containing the date of occurrence for each contract variant.
#' @param months_win parametric time window (in months) from the emergency outbreak. Only contract modifications made after
#' this window are considered.
#' @param emergency_name string specifying the name of the emergency to consider. Examples could include "Coronavirus" or
#' "Terremoto Centro Italia 2016-2017".
#' @param ...  other parameters to pass to [generate_indicator_schema()], such as `country_name` (default: Italy).
#' @return indicator schema as from [generate_indicator_schema()].
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   mock_data_core <- mock_data_core |>
#'     tidyr::unnest(varianti, keep_empty = TRUE)
#'   ind_8(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     variant_date = data_approvazione_variante,
#'     months_win = 6,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{n_distinct}}, \code{\link[dplyr]{rename}}
#'  \code{\link[lubridate]{ymd}}, \code{\link[lubridate]{interval}}
#' @rdname ind_8
#' @export
#' @importFrom dplyr mutate if_else group_by summarise n n_distinct rename
#' @importFrom lubridate ymd interval
ind_8 <- function(data,
                  publication_date,
                  stat_unit,
                  variant_date,
                  emergency_name,
                  months_win = 6,
                  ...) {
  indicator_id <- 8
  indicator_name <- "Pre-existing contracts modified after the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)
  aggregation_name <- italian_aggregation_mapping[[rlang::ensym(stat_unit)]]

  data %>%
    filter(!is.na({{ stat_unit }})) %>%
    dplyr::mutate(
      prepost = dplyr::if_else(
        lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post",
        false = "pre"
      ),
      prepost = factor(prepost, levels = c("post", "pre")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs,
        true = 1,
        false = 0
      )
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::mutate(
      flag_modif = dplyr::if_else(
        prepost == "pre" &
          lubridate::ymd({{ variant_date }}) > emergency_scenario$em_date %m+% months(months_win),
        true = 1,
        false = 0
      ),
      # contract without variants --> 0
      flag_modif = dplyr::if_else(
        is.na({{ variant_date }}),
        true = 0,
        false = flag_modif
      )
    ) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      # n = dplyr::n(),
      # npre = sum(prepost == "pre"),
      # ncig = dplyr::n_distinct(cig),
      #    ncig_pre = data.table::uniqueN(cig[prepost == "pre"]),
      nmod = sum(flag_modif == 1),
      #    ncig_mod = data.table::uniqueN(cig[flag_modif == 1]),
      # prop_mod = nmod/npre,
      rf_value = 1 * (nmod > 0),
      aggregation_name = dplyr::first(!!rlang::sym(aggregation_name))
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = rf_value, # no test
      aggregation_id = {{ stat_unit }},
      aggregation_name = aggregation_name,
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
