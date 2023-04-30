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
#' This indicator targets **companies** and **contracting authorities**
#' @param data data to be passed, expects tibble
#' @param publication_date The date when the tender was published
#' @param stat_unit Column of entities who decide who win the public procurement
#' @param winners Column of winning companies
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema`
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_5(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     stat_unit = cf_amministrazione_appaltante,
#'     winners = denominazione,
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
                  ...) {
  indicator_id <- 5
  indicator_name <- "Winner's share of issuer's contract across the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
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
      norm_gs = round(norm_gs, 10)
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
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario,
      ...
    ) %>%
    return()
}
