
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coresoi <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/gh/CORE-forge/coresoi/branch/main/graph/badge.svg?token=DGJ8P5BZNH)](https://codecov.io/gh/CORE-forge/coresoi)
<!-- badges: end -->

The goal of `coresoi` is to offer a *sandbox* in which researchers and
anti-corruption analysts may play and interact with the set of
indicators we designed, offering also mock data extracted from
[dati.anticorruzione](https://dati.anticorruzione.it/index.html#/home).

## Installation

You can install the development version of coresoi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CORE-forge/coresoi")
```

## Example: Compute indicator High Winning Rate

There might be the case in which you have to compute the indicator n° 1,
which mainly accounts for companies that after the Emergency outbreak
(say Covid-19) were awarded public contracts much more frequently than
before the Emergency. Indicator 11 computes a [Fish exact’s
test](https://en.wikipedia.org/wiki/Fisher%27s_exact_test) in proportion
from *pre* and *post* emergency, poiting out if there is any statistical
significance between the two group proportions. The resulting indicator
follows a schema generated through `coresoi::generate_indicator_schema`.

``` r
library(coresoi)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

## basic example code with ind_1() i.e. High Winning Rate
ind_1_res <- ind_1(
  data = mock_data_core, 
  publication_date = data_pubblicazione, 
  cpv = cod_cpv, 
  stat_unit = provincia)
#> Warning: `quo_expr()` is deprecated as of rlang 0.2.0.
#> Please use `quo_squash()` instead.
#> This warning is displayed once per session.
ind_1_res
#> # A tibble: 109 × 12
#> # Rowwise: 
#>    indicator_id indica…¹ indic…² aggre…³ aggre…⁴ aggre…⁵ emerg…⁶ emerg…⁷ count…⁸
#>           <dbl> <chr>      <dbl> <fct>   <chr>   <chr>     <dbl> <chr>   <chr>  
#>  1            1 High wi…   0.001 AGRIGE… ISTAT1  provin…       3 Other   1      
#>  2            1 High wi…   0.205 ALESSA… ISTAT1  provin…       3 Other   1      
#>  3            1 High wi…   0.004 ANCONA  ISTAT1  provin…       3 Other   1      
#>  4            1 High wi…   0.004 AREZZO  ISTAT1  provin…       3 Other   1      
#>  5            1 High wi…   1     ASCOLI… ISTAT1  provin…       3 Other   1      
#>  6            1 High wi…   0.659 ASTI    ISTAT1  provin…       3 Other   1      
#>  7            1 High wi…   1     AVELLI… ISTAT1  provin…       3 Other   1      
#>  8            1 High wi…   0.533 BARI    ISTAT1  provin…       3 Other   1      
#>  9            1 High wi…   0.003 BARLET… ISTAT1  provin…       3 Other   1      
#> 10            1 High wi…   0.436 BELLUNO ISTAT1  provin…       3 Other   1      
#> # … with 99 more rows, 3 more variables: country_name <chr>,
#> #   indicator_last_update <dttm>, data_last_update <dttm>, and abbreviated
#> #   variable names ¹​indicator_name, ²​indicator_value, ³​aggregation_name,
#> #   ⁴​aggregation_id, ⁵​aggregation_type, ⁶​emergency_id, ⁷​emergency_name,
#> #   ⁸​country_id
```

Let’s now visualize results for top 10 provinces given High Winning Rate
indicator estimate.

``` r
library(ggplot2)
library(forcats)
library(tidyr)
drop_na(ind_1_res) %>% 
  ggplot(aes(y = fct_reorder(aggregation_name, indicator_value), x = indicator_value)) +
  geom_col() +
  scale_y_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(
    y = "",
    x = "Fisher' Exact test pvalue (indicator 1)"
  )
```

<img src="man/figures/README-cars-1.png" width="100%" />

## CORE ecosystem

`coresoi` is part of of the project CO.R.E.- Corruption risk indicators
in emergency, financed by the EU Commission, as part of the Internal
Police Security Fund (ISF-P) program, The project presented by the
Department of Political Sciences as leader with the coordination of
Prof. Gnaldi (PI) was funded for a total of 514 thousand euros. The
working group is made up of colleagues Prof. Enrico Carloni, Prof. Marco
Mazzoni, Prof. Benedetto Ponti, Prof. Maria Giovanna Ranalli. The
international network involves Universitat Obierta Catalunya (Spain),
Dublin City University (Ireland), Oficina Antifrau de Catalunya (Spain),
Infonodes (Italy), Transparency International (Portugal), Villa Montesca
Foundation (Italy). The C.R.E. focuses on assessing the risk of
corruption in public procurement from a preventive perspective. In view
of achieving this goal, central to the European agenda, CO.R.E. it
therefore intends to develop and validate a procedure for constructing a
synthetic indicator of the risk of corruption in an emergency period
that can be usefully used by national anti-corruption agencies, the
media and citizens for accountability purposes.


<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>


## Code of Conduct

Please note that the core-soi project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
