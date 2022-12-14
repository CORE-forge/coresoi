#' compute Wilcox test in dplyr
#' @description  compute Wilcox test pvalue
#' @keywords internal
#' @export
compute_wilcox_test <- function(data, var, group, exact = TRUE, alternative = "greater") {
  data %>%
    wilcox.test(var ~ group, data = ., exact = exact, alternative = alternative) %>%
    return()
}



#' @title Compute High Economic Value indicator
#' @description High Economic Value for statistical unit
#' @param data test bndcp data
#' @param cpv Common Procurement Vocabulary. The main vocabulary is based on a tree structure made up with codes of up to 9 digits (an 8 digit code plus a check digit). This combination of digits is associated with a wording that describes the type of supplies, works or services defining the subject of the contract
#' @param contract_value the value of the contract
#' @param stat_unit statistical unit of measurement, aggregation variable, the indicator target
#' @param outbreak_starting_date the date of the emergency outbreak, Default: lubridate::ymd("2017-06-30")
#' @return indicator schema as from `generate_indicator_schema()` rows determined by aggregation level and `indicator_value` based on statistical test performed in `ind_2`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_2(
#'     data = mock_data_core,
#'     cpv = cod_cpv,
#'     contract_value = importo_complessivo_gara,
#'     publication_date = data_pubblicazione
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
                  outbreak_starting_date = lubridate::ymd("2017-06-30"),
                  stat_unit) {
  indicator_id <- 2
  indicator_name <- "High Economic Value"
  aggregation_type <- quo_expr(enquo(stat_unit))

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= outbreak_starting_date, true = "post", false = "pre"),
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
      outbreak_starting_date = outbreak_starting_date
    ) %>%
    dplyr::rename(
      indicator_value = wilcox_test,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
