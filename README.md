
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coresoi <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/gh/CORE-forge/coresoi/branch/main/graph/badge.svg?token=DGJ8P5BZNH)](https://codecov.io/gh/CORE-forge/coresoi)
[![R-CMD-check](https://github.com/CORE-forge/core-soi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CORE-forge/core-soi/actions/workflows/R-CMD-check.yaml)
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

## CORE ecosystem 🌏

`coresoi` is part of the project **CO.R.E.** - Corruption Risk
indicators in Emergency, financed by the EU Commission, as part of the
Internal Police Security Fund (ISF-P) program. The project presented by
the Department of Political Sciences of the University of Perugia
(Italy) as leader with the coordination of Prof. Gnaldi (PI) was funded
for a total of 514 thousand euros. The international network involves
Universitat Obierta Catalunya ( **Spain** ), Dublin City University (
**Ireland**), Oficina Antifrau de Catalunya ( **Spain**), Infonodes (
**Italy**), Transparency International ( **Portugal**), Villa Montesca
Foundation ( **Italy**). CO.R.E. focuses on assessing the risk of
corruption in public procurement in emergency settings from a preventive
point of view. In view of achieving this goal, central to the European
agenda, CO.R.E. intends to develop and validate a replicable procedure
for the construction of a synthetic (or composite) indicator (CI) of the
risk of corruption in public procurement in various emergency scenarios,
which can be usefully employed by national anti-corruption agencies, the
media and the citizens for accountability purposes.

The development of a synthetic measure of corruption risk involves
several stages:

1.  selection of the data
2.  computation of elementary indicators (i.e. red flags) of corruption
    risk;
3.  choice of normalization, weighting and aggregation schemes;
4.  multivariate analysis for the study of the data relational
    structure;
5.  sensitivity analysis of the resulting synthetic indicator to check
    its robustness.

For each of these steps, `coresoi` provides a support to any interested
user through analytical codes, users’ guides and practical examples.

## Code of Conduct

Please note that the core-soi project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
