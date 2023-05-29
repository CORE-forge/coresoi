#' Compute Economic deviation across the crisis indicator
#'
#' @description
#'  The indicator reveals whether there has been a deviation of the contract actual execution economic value from its initial awarded value.
#'
#' ### Motivation:
#' The red flag considers at risk companies whose contracts undergo a **significant increase** of their _economic deviation ratio_ - i.e., ratio between awarded economic value and actual amount paid - across the crisis.
#'
#' ### Scoring Rule
#'  The output will give $1 - pvalue$, which will then be dichotomised to 1 if statistical test is significant, 0 otherwise.
#'
#' ### Main target unit
#' This indicator targets **companies** and **contracting authorities**
#' @param data This argument should be a data frame or tibble containing the data you want to use to calculate the indicator.
#' @param award_value This argument corresponds to the name of the column in data containing award value of the tender.
#' @param sums_paid This argument corresponds to the name of the column in data containing the amount paid by the contracting authority for each contract. The values in this column should be numeric.
#' @param stat_unit This argument should be a character string specifying the statistical unit of measurement or aggregation variable for the indicator. In this indicator both companies and contracting authorities are the targets.
#' @param emergency_name This argument should be a character string specifying the name of the emergency or event you are analyzing. Examples could include "Coronavirus" or "Terremoto Aquila".
#' @param publication_date This argument corresponds to the name of the column in data containing the publication date for each notice or report.
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ind_3(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     award_value = importo_aggiudicazione,
#'     sums_paid = importo_lotto,
#'     stat_unit = cf_amministrazione_appaltante,
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#' @rdname ind_3
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter mutate if_else group_by summarise n
ind_3 <- function(data,
                  award_value,
                  sums_paid,
                  stat_unit,
                  emergency_name,
                  publication_date) {
  indicator_id <- 3
  indicator_name <- "Economic deviation across the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)

  data %>%
    dplyr::filter(!is.na({{ award_value }}) &
      !is.na({{ sums_paid }}) &
      {{ award_value }} > 0 &
      {{ sums_paid }} > 0) %>%
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
      ratio = {{ sums_paid }} / {{ award_value }}
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::filter(all(c("pre", "post") %in% prepost)) %>%
    dplyr::ungroup(prepost) %>%
    dplyr::summarise(
      npre = sum(prepost == "pre"),
      npost = sum(prepost == "post"),
      mean_pre = mean(ratio[prepost == "pre"]),
      mean_post = mean(ratio[prepost == "post"]),
      median_pre = median(ratio[prepost == "pre"]),
      median_post = median(ratio[prepost == "post"]),
      ind_3 = compute_kolmogorov_smirnoff(var = ratio, group = prepost, data = .)[1]
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = 1 - ind_3, # 1 - pvalue
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    return()
}
