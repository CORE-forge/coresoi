ind_8 <- function(data,
                  publication_date,
                  stat_unit,
                  variant_date,
                  emergency_name,
                  months_win = 6) {

  # out8 <- ind_8(
  #   data=mock_data_core,
  #   publication_date=data_aggiudicazione_definitiva,
  #   stat_unit=cf_amministrazione_appaltante,
  #   variant_date=data_approvazione_variante,
  #   emergency_name="coronavirus",
  # )

  indicator_id <- 8
  indicator_name <- "Pre-existing contracts modified after the crisis"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)

  # select useful variables and remove duplicates
  # @giulio: how to generalise this?
  data <- data %>%
    filter(!is.na(data_aggiudicazione_definitiva)) %>%
    select(cig, cf_amministrazione_appaltante, data_pubblicazione,
           data_aggiudicazione_definitiva, id_variante, data_approvazione_variante,
           codice_fiscale) %>%
    distinct()

  if (aggregation_type == "cf_amministrazione_appaltante") {
    data <- data %>%
      dplyr::select(-codice_fiscale) %>%
      dplyr::distinct()
  }


  data_out <- data %>%
    dplyr::mutate(
      prepost = dplyr::if_else(lubridate::ymd({{ publication_date }}) >= emergency_scenario$em_date,
                               true = "post", false = "pre"),
      prepost = forcats::as_factor(prepost),
      flag_modif = dplyr::if_else(prepost == "pre" &
                                    lubridate::ymd({{ variant_date }}) %within%
                                    lubridate::interval(emergency_scenario$em_date,
                                                        emergency_scenario$em_date %m+%
                                                          months(months_win)),
                                  true = 1, false = 0),
    )

  # out <- data_out %>%
  data_out %>%
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      n = dplyr::n(),
      npre = sum(prepost == "pre"),
      ncig = dplyr::n_distinct(cig),
      ncig_pre = data.table::uniqueN(cig[prepost == "pre"]),
      nmod = sum(flag_modif == 1),
      ncig_mod = data.table::uniqueN(cig[flag_modif == 1]),
      # prop_mod = nmod/npre,
      rf_value = 1*(nmod>0)
    ) %>%
    return()
  # return(list(data=data_out, out=out))
}
