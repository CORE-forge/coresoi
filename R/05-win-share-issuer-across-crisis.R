#' Compute Winner's share of issuer's contract across the crisis indicator
#'
#' @description The indicator reveals, for each issuer, the share of contracts awarded to the same company out of the total number of contracts issued
#'
#' ### Motivation:
#' The red flag considers at risk companies that **exceptionally increase** _their competitive power_ over the emergency outbreak, as a consequence of a high proportion of contracts awarded by the same contracting authority
#'
#' ### Scoring Rule
#' If Test-statistic significant-> 1, otherwise -> 0
#'
#' ### Main target unit
#' This indicator targets **contracting authorities**
#' @param data This argument should be a data frame or tibble containing the data you want to use to calculate the indicator.
#' @param publication_date This argument corresponds to the name of the column in data containing the publication date for each notice or report.
#' @param stat_unit This argument should be a character string specifying the statistical unit of measurement or aggregation variable for the indicator. In this indicator both companies and contracting authorities are the targets.
#' @param winners  This argument corresponds to the name of the column in data containing the winning companies for each contract. This column should contain character or factor values.
#' @param emergency_name This argument should be a character string specifying the name of the emergency or event you are analyzing. Examples could include "Coronavirus" or "Terremoto Aquila".
#' @param cpvs character vector of macro-cpv on which data is filtered out. A panel of experts have already chosen which cpvs are most affected by which emergency for you.
#' @param ... other parameters to pass to `generate_indicator_schema` as `country_name` if that not Italy, which is default behavior.
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
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
      emergency = emergency_scenario
    ) %>%
    return()
}
