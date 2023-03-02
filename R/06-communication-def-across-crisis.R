#' @title Compute Communication default across the crisis indicator
#' @description The indicator reveals whether - and to what extent - the duty of contracting authorities to communicate the public procurement procedure activation to the Anticorruption Authority ended with a technical failure
#'
#' ### Motivation:
#' The red flag considers at risk contracting authorities **who fail to accomplish their duty** to communicate the public procurement procedure activation to the Anticorruption Authority
#'
#' ### Scoring Rule
#' If Test-statistic significant-> 1, otherwise -> 0
#'
#' ### Main target unit
#' This indicator targets **contracting authorities**
#' @param data mock_data_core example bdncp data
#' @param publication_date The date when the tender was published
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @param award_col column indentifying id for that contract award
#' @param stat_unit statistical unit of measurement and stat_uniting
#' @return indicator schema as from `generate_indicator_schema`
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data("mock_data_core")
#'   ind_6(
#'     data = mock_data_core,
#'     publication_date = data_pubblicazione,
#'     emergency_name = "coronavirus",
#'     award_col = id_aggiudicazione,
#'     stat_unit = provincia
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{stat_unit_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname ind_6
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise distinct pull rename left_join arrange
#' @importFrom forcats as_factor
#' @importFrom stringr str_sub
ind_6 <- function(data,
                  publication_date,
                  emergency_name,
                  award_col,
                  stat_unit) {
  indicator_id <- 6
  indicator_name <- "Communication default across the crisis"
  aggregation_type <- quo_expr(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  cpvs <- get_associated_cpv_from_emergency(emergency_scenario$em_name)
  cpv_col <- grab_cpv(data = data)

  prel <- data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      ## leave business logic
      flagdivision = dplyr::if_else(stringr::str_sub(.data[[cpv_col]], start = 1, end = 2) %in% cpvs, 1, 0),
      flag_missing = dplyr::if_else(is.na({{ award_col }}), 1, 0)
    ) %>%
    dplyr::filter(flagdivision == 1) %>%
    dplyr::group_by({{ stat_unit }}, prepost) %>%
    dplyr::summarise(
      prop_no_communic = mean(flag_missing)
    )

  gr <- data %>%
    dplyr::distinct({{ stat_unit }}) %>%
    dplyr::pull()

  expand.grid(gr, prepost = c("pre", "post")) %>%
    dplyr::rename({{ stat_unit }} := Var1) %>%
    dplyr::left_join(prel) %>%
    dplyr::arrange({{ stat_unit }}) %>%
    dplyr::as_tibble() %>%
    generate_indicator_schema(
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      indicator_value = prop_no_communic,
      aggregation_name = {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    return()
}
