
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CompInt

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/11annah/CompInt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/11annah/CompInt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of CompInt is to (eventually) allow users to compute all
quantities from [“A formal framework for generalized reporting methods
in parametric settings”](https://arxiv.org/abs/2211.02621) for a variety
of model objects. Currently, this package is still in mid-development.

## Installation

You can install the development version of CompInt from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("11annah/CompInt")

# Alternatively, via devtools:
# install.packages("devtools")
devtools::install_github("11annah/CompInt")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CompInt)

data(Affairs, package = "AER")
poisson.model <- glm(affairs ~ ., family = "poisson", data = Affairs)

draws_gME_religiousness <- get_gME(poisson.model, integration = assumption3(all_empirical()), reg_of_interest = "religiousness", seed = 100)
mean(draws_gME_religiousness)

draws_IE_children <- get_IE(poisson.model, integration = assumption3(all_empirical()), reg_of_interest = "children", seed = 100)
rowMeans(draws_IE_children)
```
