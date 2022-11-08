#' @title Awarded notice communication default
#' @description The indicator reveals the fraction of contracts without award notice communication.
#' @param data bndcp data
#' @param publ_date The date when the tender was published
#' @param flag_agg PARAM_DESCRIPTION
#' @param cpv Common Procurement Vocabulary.The main vocabulary is based on a tree structure made up with codes of up to 9 digits (an 8 digit code plus a check digit). This combination of digits is associated with a wording that describes the type of supplies, works or services defining the subject of the contract.
#' @param outbreak_starting_date the date of the emergency outbreak, Default: lubridate::ymd("2017-06-30")
#' @param division initial two digits from cpv, Default: '33'
#' @param group statistical unit of measurement and grouping
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[lubridate]{ymd}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[forcats]{as_factor}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname ind_5
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate if_else group_by summarise distinct pull rename left_join arrange
#' @importFrom forcats as_factor
#' @importFrom stringr str_sub
ind_5_bis <- function(data,
                      publ_date,
                      flag_agg,
                      cpv,
                      outbreak_starting_date = lubridate::ymd("2017-06-30"),
                      division = "33",
                      group) {
  prel <- data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= outbreak_starting_date, true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(stringr::str_sub({{ cpv }}, start = 1, end = 2) == "33", 1, 0)
    ) %>%
    dplyr::group_by({{ group }}, prepost) %>%
    dplyr::summarise(prop_no_communic = 1 - mean({{ flagdivision }}))

  gr <- data %>%
    dplyr::distinct({{ group }}) %>%
    dplyr::pull()

  expand.grid(gr, prepost = c("pre", "post")) %>%
    dplyr::rename({{ group }} := Var1) %>%
    dplyr::left_join(prel) %>%
    dplyr::arrange({{ group }}) %>%
    return()
}
