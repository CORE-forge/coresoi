Dictonary Cookbook
================
Giulio G. Cantone

The following code will build a *mock* dictionary for the categories of
contracts in the mock dataset.

- Select only relevant columns

``` r
coresoi::mock_data_core %>%
  select(ID = cig,
         contract_object = oggetto_lotto,
         contract_class = descrizione_cpv,
         Territory = provincia,
         Contracting_agent = denominazione_amministrazione_appaltante) -> micromock_data
```

- Macro grouped cpv

``` r
coresoi::mock_data_core %>%
  transmute(
    ID = cig,
    contract_object = oggetto_lotto,
    macroclass =
      cod_cpv %>%
      as.character() %>%
      str_extract("^.{2}"),
    Territory = provincia,
    Contracting_agent = denominazione_amministrazione_appaltante
    ) -> macromock_data
```

- Tidy the the `contract_object`

``` r
macromock_data %>%
  tidytext::unnest_tokens(word,contract_object) %>%
  anti_join(get_stopwords(language = "it")) %>%
  mutate(word = word %>% wordStem("italian")) -> tidy_macromock
```

    ## Joining, by = "word"

- Check statistics per contract class

``` r
tidy_macromock %>% count(macroclass, word) %>%
  bind_tf_idf(word, macroclass, n) %>%
  mutate(across(where(is.numeric),round,3)) %>%
  arrange(macroclass,-tf_idf) -> macromock_tfidf
```

- Summary stats

``` r
tidy_mock %>% group_by(macroclass) %>%
  summarise(
    n = n()
  ) %>%
  left_join(
    macromock_tfidf %>%
  group_by(macroclass) %>%
  summarise(gini = DescTools::Gini(n),
            atkinson = ineq::Atkinson(n))
  ) %>%
  mutate(across(where(is.numeric),round,3)) %>%
  pander::pander(split.table = Inf)
```

Quitting from lines 73-86 (Mock_dictionary.Rmd) Errore in group_by(.,
macroclass) : oggetto ‘tidy_mock’ non trovato Chiamate: <Anonymous> …
<Anonymous> -\> mutate -\> left_join -\> summarise -\> group_by

------------------------------------------------------------------------

- ONLY FOR MICROMOCK

``` r
micro %>% group_by(contract_class,word) %>%
  count() %>%
  arrange(contract_class,-n) %>% View()
```

------------------------------------------------------------------------

Two problems:

1.  Contract class is too broad

2.  After the ***n*** of contract classes is fixed, tokens should be
    weighted through their tf/idf.
