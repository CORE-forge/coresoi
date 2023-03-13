#' @title Compute Pre-existing contracts modified after the crisis indicator
#' @description The contract changed
#' @param data bndcp data
#' @param publication_date The date when the tender was published
#' @param stat_unit tatistical unit of measurement, aggregation variable, the indicator target. In this case the identifier of agency or winners.
#' @param variant_date Date of contract variation
#' @param months_win time window for variation to be happening
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
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
#'  \code{\link[forcats]{as_factor}}
#' @rdname ind_8
#' @export
#' @importFrom dplyr mutate if_else group_by summarise n n_distinct rename
#' @importFrom lubridate ymd interval
#' @importFrom forcats as_factor
ind_8 <- function(data,
                  publication_date,
                  stat_unit,
                  variant_date,
                  emergency_name,
                  months_win = 6) {
  indicator_id <- 8
  indicator_name <- "Pre-existing contracts modified after the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)

  data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(
        lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
        true = "post",
        false = "pre"
      ),
      prepost = factor(prepost, levels=c("post", "pre")),
      flag_division = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs, 1, 0),
      flag_modif = dplyr::if_else(
        prepost == "pre" &
          lubridate::ymd({{ variant_date }}) > emergency_scenario$em_date %m+% months(months_win),
        true = 1,
        false = 0
      ),
    ) %>%
    dplyr::filter(flag_division == 1) %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      n = dplyr::n(),
      npre = sum(prepost == "pre"),
      ncig = dplyr::n_distinct(cig),
      nmod = sum(flag_modif == 1),
      rf_value = 1 * (nmod > 0)
    ) %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = rf_value,
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    return()
}
