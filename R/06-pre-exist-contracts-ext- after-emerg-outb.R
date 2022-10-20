

ind_6 <- function(data, outbreak_date = ymd(as_date("2022-02-21"))) {
  data <- data %>%
    select(cig, data_agiundicazione_definitiva, data_inizio_effettiva, data_termine_contrattuale, data_effettiva_ultimazione, nvarianti2)
  return(data)
}

# statistical unit:; contract CIG

# vars
## - cig
## - cpv
## - award date (data_agiundicazione_definitiva)
## - end date
## - modficiation date
## - moification type
## . outbreakl date (assuming covid: 21 FEBBRAIO 2020 )


## steps
## - those who have end cotnract date  after emergency out break
## - those who have end beofre outbreaks with modification after outbreaks


# vars to include
# -

## caveats:
## magari puoi guardare gli aggiundicati



# outbreak_date = ymd(as_date("2022-02-21"))
# bncp_data %>%
#   select(cig, data_aggiudicazione_definitiva, data_inizio_effettiva, data_termine_contrattuale, data_effettiva_ultimazione, nvarianti2) %>%
#   drop_na() %>%
#   mutate(
#     outbreak_date = outbreak_date,
#     flag_extension_date = case_when(
#       # data_termine_contrattuale < outbreak_date &data_effettiva_ultimazione> outbreak_date &nvarianti2 != 0 ~ "extended_after",
#       data_termine_contrattuale < outbreak_date & data_effettiva_ultimazione> outbreak_date ~ "extended_after",
#       TRUE ~ "not extd"
#     )
#   ) %>%
#   count(flag_extension_date)
#
# ## quando non chiedi modifiche
# # A tibble: 2 × 2
# # flag_extension_date     n
# # <chr>               <int>
# #   1 extended_after          3
# # 2 not extd            23598
#
# bncp_data %>%
#   select(cig, data_aggiudicazione_definitiva, data_inizio_effettiva, data_termine_contrattuale, data_effettiva_ultimazione, nvarianti2) %>%
#   drop_na() %>%
#   mutate(
#     outbreak_date = outbreak_date,
#     flag_extension_date = case_when(
#       data_termine_contrattuale < outbreak_date &data_effettiva_ultimazione> outbreak_date &nvarianti2 != 0 ~ "extended_after",
#       TRUE ~ "not extd"
#     )
#   ) %>%
#   count(flag_extension_date)
#
# ## quando invece chiedi modifiche diverse da 0
# # A tibble: 1 × 2
# # flag_extension_date     n
# # <chr>               <int>
# #   1 not extd            23601
#
