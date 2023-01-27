#' @title Compute Communication default across the crisis indicator
#' @description The indicator reveals the fraction of contracts without any award notice communication.
#' @param data mock_data_core example bdncp data
#' @param publication_date The date when the tender was published
#' @param cpv Common Procurement Vocabulary.The main vocabulary is based on a tree structure made up with codes of up to 9 digits (an 8 digit code plus a check digit). This combination of digits is associated with a wording that describes the type of supplies, works or services defining the subject of the contract.
#' @param emergency_name emergency name character string for which you want to evaluate the indicator, e.g. "Coronavirus" "Terremoto Aquila"
#' @param cpv_division initial two digits from cpv, Default: '33'
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
#'     cpv = cod_cpv,
#'     emergency_name = "coronavirus",
#'     cpv_division = "33",
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
                  cpv,
                  emergency_name,
                  cpv_division = "33",
                  award_col,
                  stat_unit) {
  indicator_id <- 6
  indicator_name <- "Communication default across the crisis"
  aggregation_type <- quo_expr(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)

  prel <- data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      ## leave business logic
      flagdivision = dplyr::if_else(stringr::str_sub({{ cpv }}, start = 1, end = 2) == cpv_division, 1, 0),
      flag_missing = dplyr::if_else(is.na({{ award_col }}), 1, 0)
    ) %>%
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
      prop_no_communic,
      {{ stat_unit }},
      aggregation_type = as_string(aggregation_type),
      emergency = emergency_scenario
    ) %>%
    dplyr::rename(
      indicator_value = prop_no_communic,
      aggregation_name = {{ stat_unit }}
    ) %>%
    return()
}
