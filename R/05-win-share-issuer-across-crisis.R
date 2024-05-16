#' Compute Excess of concentration of the winners’ distribution indicator
#'
#' @description The indicator compares the concentration degree of the winners’ distribution of a contracting authority based
#' on the contracts issued after the emergency with respect to what occurred before, with reference to the contracts belonging
#' to the pertinent market.
#'
#' ### Motivation
#' The red flag considers at risk contracting authorities showing an increase in the concentration of the winners’ distribution
#' across the crisis, thus highlighting a high degree of awarding their contracts to an increasingly small number of firms.
#'
#' ### Scoring rule
#' For each contracting authority, a comparison of concentration index (e.g., inverse of Gini-normalised for heterogeneity) -
#' discretised into five categories - before and after the outbreak is performed.
#' If concentration category across the emergency increases, this indicator will be equal to 1. Otherwise, it will be equal to 0.
#'
#' ### Main target unit
#' This indicator targets **contracting authorities**
#'
#' @param data a dataframe containing the data to use for computing the indicator.
#' @param publication_date name of the variable in `data` containing the publication date of each contract.
#' @param stat_unit name of the variable in `data` containing the target unit ID (in this case, contracting authority).
#' @param winners name of the variable in `data` containing the winning company ID of each contract.
#' @param emergency_name string specifying the name of the emergency to consider. Examples could include "Coronavirus" or
#' "Terremoto Centro Italia 2016-2017".
#' @param cpvs character vector of CPV divisions (first two digits of CPV code) on which `data` are filtered out.
#' Note: a panel of experts have already chosen which CPV divisions are most affected by which emergency.
#' @param ...  other parameters to pass to [generate_indicator_schema()], such as `country_name` (default: Italy).
#' @return indicator schema as from [generate_indicator_schema()].
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   mock_data_core <- mock_data_core |>
#'     tidyr::unnest(aggiudicatari, keep_empty = TRUE)
#'   ind_5(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     winners = codice_fiscale,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#' @rdname ind_5
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n count
#' @importFrom DescTools Gini
#' @importFrom tidyr pivot_wider
ind_5 <- function(data,
                  stat_unit,
                  publication_date,
                  winners,
                  emergency_name,
                  cpvs,
                  ...) {
  indicator_id <- 5
  indicator_name <- "Winner's share of issuer's contract across the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  aggregation_name <- italian_aggregation_mapping[[rlang::ensym(stat_unit)]]

  if (missing(cpvs)) {
    cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  }
  cpv_col <- grab_cpv(data = data)


  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post", false = "pre"
      ),
      prepost = factor(prepost, levels = c("pre", "post")),
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs, 1, 0)
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::filter(!is.na({{ winners }})) %>%
    # dplyr::count({{ stat_unit }}, {{ winners }}, prepost) %>%
    dplyr::group_by({{ stat_unit }}, prepost) %>%
    dplyr::summarise(
      norm_gs = 1 -
        DescTools::GiniSimpson({{ winners }} %>% as.factor()) /
          ((dplyr::n_distinct({{ winners }}) - 1) / dplyr::n_distinct({{ winners }})),
      norm_gs = round(norm_gs, 10),
      aggregation_name = dplyr::first(!!rlang::sym(aggregation_name))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = prepost, values_from = norm_gs) %>%
    dplyr::filter(!is.na(pre) & !is.na(post)) %>%
    dplyr::mutate(
      prepost00 = dplyr::if_else(pre == 0 & post == 0,
        true = 1,
        false = 0
      ),
      diff = post - pre,
      pre_cat = cut(pre,
        # breaks = quantile(pre[prepost00 == 0], c(0, 1/5, 2/5, 3/5, 4/5, 1)),
        breaks = quantile(pre[prepost00 == 0], c(0, 1 / 5, 2 / 5, 3 / 5, 4 / 5, 1)) +
          c(0, seq_along(1:4) * .Machine$double.eps, 0), # for possible overlapping quantiles
        labels = 1:5,
        include.lowest = TRUE
      ),
      post_cat = cut(post,
        # breaks = quantile(post[prepost00 == 0], c(0, 1/5, 2/5, 3/5, 4/5, 1)),
        breaks = quantile(post[prepost00 == 0], c(0, 1 / 5, 2 / 5, 3 / 5, 4 / 5, 1)) +
          c(0, seq_along(1:4) * .Machine$double.eps, 0), # for possible overlapping quantiles
        labels = 1:5,
        include.lowest = TRUE
      ),
      diff_cat = as.numeric(post_cat) - as.numeric(pre_cat),
      ind5 = dplyr::case_when(
        diff_cat > 0 ~ 1,
        diff_cat == 0 & post_cat %in% c(4, 5) & diff > 0 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = ind5, # no test
      aggregation_id = {{ stat_unit }},
      aggregation_name = aggregation_name,
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
