
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coresoi <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/core-soi)](https://CRAN.R-project.org/package=core-soi)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/CORE-forge/core-soi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/CORE-forge/core-soi?branch=main)
<!-- badges: end -->

The goal of `coresoi` is to provide a *sandbox* environment for
researchers and anti-corruption analysts to interact with the indicators
we’ve designed. We also offer mock data extracted from
[dati.anticorruzione](https://dati.anticorruzione.it/index.html#/home)
to aid in their analysis. Our hope is that this platform will facilitate
more effective efforts against corruption and promote greater
transparency in government.

## Installation

You can install the development version of coresoi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CORE-forge/coresoi")
```

## Example: Compute Winning rate across the crisis indicator

In certain cases, it may be necessary to calculate Indicator 1, which
primarily focuses on companies that were awarded public contracts at a
significantly higher rate after the outbreak of the Emergency (in this
case, the Coronavirus) than before. Indicator 1 uses a [Fisher’s
exact](https://en.wikipedia.org/wiki/Fisher%27s_exact_test) (other test
choices include
[Barnard](https://en.wikipedia.org/wiki/Barnard%27s_test) and
[z-test](https://en.wikipedia.org/wiki/Z-test)) test to compare the
proportions of pre- and post-Emergency contracts and determine if there
is any statistically significant difference between the two groups.

The resulting indicator follows a schema (which is convenient to ship to
frontends) that is generated using the
`coresoi::generate_indicator_schema` function.

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
  stat_unit = cf_amministrazione_appaltante,
  emergency_name = "coronavirus",
  test_type = "fisher")
ind_1_res
#> # A tibble: 624 × 12
#> # Rowwise: 
#>    indicator_id indica…¹ indic…² aggre…³ aggre…⁴ aggre…⁵ emerg…⁶ emerg…⁷ count…⁸
#>           <dbl> <chr>      <dbl> <chr>   <chr>   <chr>     <int> <chr>   <chr>  
#>  1            1 Winning…       1 000585… ISTAT1  cf_amm…       1 Corona… 1      
#>  2            1 Winning…       1 000647… ISTAT1  cf_amm…       1 Corona… 1      
#>  3            1 Winning…       1 000675… ISTAT1  cf_amm…       1 Corona… 1      
#>  4            1 Winning…       1 000759… ISTAT1  cf_amm…       1 Corona… 1      
#>  5            1 Winning…       1 000802… ISTAT1  cf_amm…       1 Corona… 1      
#>  6            1 Winning…       1 000808… ISTAT1  cf_amm…       1 Corona… 1      
#>  7            1 Winning…       1 000982… ISTAT1  cf_amm…       1 Corona… 1      
#>  8            1 Winning…       1 001043… ISTAT1  cf_amm…       1 Corona… 1      
#>  9            1 Winning…       1 001086… ISTAT1  cf_amm…       1 Corona… 1      
#> 10            1 Winning…       1 001107… ISTAT1  cf_amm…       1 Corona… 1      
#> # … with 614 more rows, 3 more variables: country_name <chr>,
#> #   indicator_last_update <dttm>, data_last_update <dttm>, and abbreviated
#> #   variable names ¹​indicator_name, ²​indicator_value, ³​aggregation_name,
#> #   ⁴​aggregation_id, ⁵​aggregation_type, ⁶​emergency_id, ⁷​emergency_name,
#> #   ⁸​country_id
```

## CORE ecosystem 🌏

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

## 📝 TODOs

-   setup Google Analytics
-   build a template for package
-   fix behavior `ind_3`, `ind_5` and `ind_7`

## Code of Conduct

Please note that the core-soi project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
