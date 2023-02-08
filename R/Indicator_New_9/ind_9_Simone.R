ind_9 <- function(data,
                  publication_date,
                  stat_unit,
                  cpv,
                  cpv_focus = "33",
                  eff_start,
                  eff_end,
                  emergency_name) {
  

  # out9 <- ind_9(
  #   data=mock_data_core,
  #   publication_date=data_pubblicazione,
  #   stat_unit=cf_amministrazione_appaltante,
  #   cpv=cod_cpv,
  #   eff_start=data_inizio_effettiva,
  #   eff_end=data_effettiva_ultimazione,
  #   emergency_name="coronavirus"
  # )
  
  indicator_id <- 9
  indicator_name <- "Lengthy contracts"
  aggregation_type <- quo_squash(enquo(stat_unit))
  emergency_scenario <- emergency_dates(emergency_name)
  
  # select useful variables and remove duplicates
  # @giulio: how to generalise this?
  data <- data %>% 
    filter(!is.na(data_aggiudicazione_definitiva)) %>% 
    select(cig, cf_amministrazione_appaltante, data_pubblicazione, cod_cpv,
           data_inizio_effettiva, data_effettiva_ultimazione,
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
                               true = "post", 
                               false = "pre"),
      prepost = forcats::as_factor(prepost),
      flagdivision = dplyr::if_else(stringr::str_sub({{ cpv }}, start = 1, end = 2) == cpv_focus, 
                                    true = 1, 
                                    false = 0),
      contract_duration = dplyr::if_else({{ eff_end }} < {{ eff_start }}, 
                                         true = NA_real_, 
                                         false = 1 + as.numeric({{ eff_end }} - {{ eff_start }}))
    ) 
  
  grand_mean <- mean(data_out$contract_duration[data_out$prepost == "post"], na.rm = TRUE)

  data_out %>%
    dplyr::filter(prepost == "post" & !is.na(contract_duration)) %>%
    # dplyr::filter(flagdivision == 1) %>%  #commented for the moment (very few data)
    dplyr::group_by({{ stat_unit }}) %>%
    dplyr::summarise(
      ntot = dplyr::n(),
      nnomiss = sum(!is.na(contract_duration)),
      mean = mean(contract_duration, na.rm = TRUE), #to be compared with grand_mean
      # median = median(contract_duration, na.rm = TRUE),
      # test not to be performed if nnomiss == 1: not working, why?
      ttest = dplyr::if_else(nnomiss == 1,
                      true = NA_real_,
                      false = compute_ttest(data=contract_duration, mu=grand_mean))
      # the ideal would be using 'comp_grandmean' (see below), but it's computer intensive
    ) -> out
    # return()
  return(list(data=data_out, out=out))
}

compute_ttest <- function(data, mu, alternative) {
  stats::t.test(x=data, mu=mu, alternative="greater")$p.value
}

comp_grandmean <- function(y, x) {
  # y: continuous variable (contract duration)
  # x: factor (companies/contracting authorities)
  a <- stats::aov(y~x)
  out <- multcomp::glht(a, linfct = multcomp::mcp(x = "GrandMean"), alternative="greater")
  summ_out <- multcomp::summary(out) #computer intensive
  return(summ_out)
}
