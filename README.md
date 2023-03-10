
<!-- README.md is generated from README.Rmd. Please edit that file -->

# New column-pair mapping functions

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/pairmaps)](https://CRAN.R-project.org/package=pairmaps)

<!-- badges: end -->

The pairmaps package provides `as_colpair_mapper()`, which modifies a
function so that it is applied to each pair of columns in a data frame.
Every function created this way is a wrapper around
`corrr::colpair_map()`.

You can avoid pairmaps as a dependency by copying and pasting the
functions returned by `as_colpair_mapper()` into your source code.

pairmaps is an unofficial add-on to
[corrr](https://corrr.tidymodels.org/). Its functions don’t have any
dependencies beyond those of corrr (and corrr itself).

## Installation

You can install the development version of pairmaps from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("lhdjung/pairmaps")
```

## Get started

Go to the [*Using
pairmaps*](https://lhdjung.github.io/pairmaps/articles/using-pairmaps.html)
vignette.

## Acknowledgements

pairmaps was inspired by
[factory](https://github.com/jonthegeek/factory).
