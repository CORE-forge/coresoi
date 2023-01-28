library(tidyr)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
load("R/Indicator New_5/datiBDNCP_2017_completi_infoterr_final3.RData") #dat9
# integrato con info territoriale presa da ipa_enti

# per ogni CIG, prende il cf dell'aggiudicatario/i e il flag aggiudicatario (ID aggiudicazione)
aggiudicatari <- read_delim("R/Indicator New_5/aggiudicatari_csv_0.zip", delim=";")
# solo cig_id_agg che abbiamo in dat9
aggiudicatari2 <- inner_join(aggiudicatari %>% select(cig, id_aggiudicazione, codice_fiscale),
                             dat9 %>% select(cig, id_aggiudicazione))
length(unique(aggiudicatari2$cig))
tab_aggiudicatari <- aggiudicatari2 %>%
  group_by(cig, id_aggiudicazione) %>%
  summarise(n())

aggiudicatari_dat9 <- left_join(aggiudicatari2,
                                dat9 %>% select(cig, id_aggiudicazione, sa))
tab_sa_cf <- aggiudicatari_dat9 %>%
  group_by(sa, codice_fiscale) %>%
  summarise(nproc=n())

tot_agg <- tab_sa_cf %>%
  group_by(sa) %>%
  summarise(tot_agg=sum(nproc))

tot_agg2 <- aggiudicatari_dat9 %>%
  group_by(sa) %>%
  summarise(ncig=n_distinct(cig))

tab_sa_cf2 <- left_join(tab_sa_cf, tot_agg)
tab_sa_cf2 <- left_join(tab_sa_cf2, tot_agg2)
tab_sa_cf2 <- tab_sa_cf2 %>%
  mutate(share=nproc/tot_agg)

tabella_gini_rov <- tab_sa_cf2 %>%
  group_by(sa) %>%
  summarise(totaggiudicatari=sum(nproc), #m
            ncig=mean(ncig),
            num_aggiudicatari_diversi=n(), #K
            eter=1-sum(share^2),
            eter_gini=eter*(num_aggiudicatari_diversi/(num_aggiudicatari_diversi-1)),
            eter_gini_rev=round(1-eter_gini,5))
