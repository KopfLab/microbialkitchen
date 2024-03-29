
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Microbial Kitchen

<!-- badges: start -->

[![R-CMD-check](https://github.com/KopfLab/microbialkitchen/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KopfLab/microbialkitchen/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://microbialkitchen.kopflab.org/reference/)
[![codecov](https://codecov.io/github/KopfLab/microbialkitchen/branch/master/graphs/badge.svg)](https://codecov.io/github/Kopflab/microbialkitchen)
<!-- badges: end -->

## About

The **[microbialkitchen](https://microbialkitchen.kopflab.org/)**
package is a collection of tools to simplify working with the chemical
composition and speciation of defined culture media for microbial
physiology and environmental microbiology research. It includes a wide
range of general purpose functionality for chemical applications
including built-in, data-frame-compatible [chemical
quantities](https://microbialkitchen.kopflab.org/articles/quantities.html)
(volume, mass, molarity, temperature, pressure, etc.) that automatically
keep track of their units and metric scaling, as well as more
specialized tools for the assembly and comparison of culturing media
recipes, pH buffering strategies and aqueous speciation. All basic data
types and operations are fully implemented, documented and ready to use.
However, since the package is still in active development and some
syntax and function names may change.

## Installation

You can install
**[microbialkitchen](https://microbialkitchen.kopflab.org/)** from
github with the devtools package.

``` r
# install.packages("devtools") 
devtools::install_github("KopfLab/microbialkitchen")
```

## Functionality

- [function reference](https://microbialkitchen.kopflab.org/reference/)
  for details on all functions
- [quantities
  vignette](https://microbialkitchen.kopflab.org/articles/quantities.html)
  on built-in chemical quantities
- [operations
  vignette](https://microbialkitchen.kopflab.org/articles/operations.html)
  on working with quantities
- carbonate chemistry vignettes for working with carbonate buffered
  media systems:
  - [equations
    vignette](https://microbialkitchen.kopflab.org/articles/carbonate_chemistry_equations.html)
  - [examples
    vignette](https://microbialkitchen.kopflab.org/articles/carbonate_chemistry_examples.html)
