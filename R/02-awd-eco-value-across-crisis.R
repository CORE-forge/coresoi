#' compute Wilcox test in dplyr
#' @description  compute Wilcox test pvalue
#' @keywords internal
#' @export
compute_wilcox_test <- function(data, var, group, exact = TRUE, alternative = "greater") {
  data %>%
    wilcox.test(var ~ group, data = ., exact = exact, alternative = alternative) %>%
    return()
}



#' @title Compute Awarded economic value across the crisis indicator
#' @description companies that after the emergency outbreak were awarded public contracts much higher in economic value than before the emergency scenario
#' @param data test bndcp data
#' @param cpv Common Procurement Vocabulary. The main vocabulary is based on a tree structure made up with codes of up to 9 digits (an 8 digit code plus a check digit). This combination of digits is associated with a wording that describes the type of supplies, works or services defining the subject of the contract
#' @param contract_value the value of the contract
#' @param stat_unit statistical unit of measurement, aggregation variable, the indicator target
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema()` rows determined by aggregation level and `indicator_value` based on statistical test performed in `ind_2`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_2(
#'     data = mock_data_core,
#'     cpv = cod_cpv,
#'     contract_value = importo_complessivo_gara,
#'     publication_date = data_pubblicazione,
#'     stat_unit = provincia
#'     emergency_name = "coronavirus"
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{summarise}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[tidyr]{nest}}, \code{\link[tidyr]{drop_na}}
#' @rdname ind_2
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by filter ungroup summarise
#' @importFrom forcats as_factor
#' @importFrom stringr str_sub
#' @importFrom tidyr unnest drop_na
ind_2 <- function(data,
                  cpv,
                  contract_value,
                  publication_date,
                  emergency_name,
                  stat_unit) {
  indicator_id <- 2
  indicator_name <- "Awarded economic value across the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario = emergency_dates(emergency_name)

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(stringr::str_sub({{ cpv }}, start = 1, end = 2) == "33", 1, 0)
    ) %>%
    tidyr::drop_na({{ contract_value }}) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::filter(all(c("pre", "post") %in% prepost)) %>%
    dplyr::ungroup(prepost) %>%
    dplyr::summarise(
      count = n(),
      median = median({{ contract_value }}, na.rm = TRUE),
      iqr = IQR({{ contract_value }}, na.rm = TRUE),
      wilcox_test = compute_wilcox_test(var = {{ contract_value }}, group = prepost, data = .)$p.value,
      wilcox_test = round(wilcox_test, 3)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      wilcox_test,
      {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = wilcox_test,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
