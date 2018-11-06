
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mediatools

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mediatools)](https://cran.r-project.org/package=mediatools)
[![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.2.0.9999-orange.svg?style=flat-square)](/commits)
[![Last-Update](https://img.shields.io/badge/updated-2018--11--05-yellowgreen.svg)](/commits)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://kopflab.github.io/mediatools/reference/)
[![Build
Status](https://travis-ci.org/KopfLab/mediatools.svg?branch=master)](https://travis-ci.org/KopfLab/mediatools)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/KopfLab/mediatools?branch=master&svg=true)](https://ci.appveyor.com/project/KopfLab/mediatools)

## About

The [mediatools](https://kopflab.github.io/mediatools/) package is a
collection of tools to simplify working with the chemical composition
and speciation of defined culture media for microbial physiology and
environmental microbiology research. It includes a wide range of general
purpose functionality for chemical applications including built-in,
data-frame-compatible [chemical
quantities](https://kopflab.github.io/mediatools/articles/quantities.html)
(volume, mass, molarity, temperature, pressure, etc.) that automatically
keep track of their units and metric scaling, as well as more
specialized tools for the assembly and comparison of culturing media
recipes, pH buffering strategies and aqueous speciation. All basic data
types and operations are fully implemented, documented and ready to use.
However, since the package is still in active development and some
syntax and function names may change.

## Installation

You can install [mediatools](https://kopflab.github.io/mediatools/) from
github with the devtools package.

``` r
# install.packages("devtools") 
devtools::install_github("KopfLab/mediatools")
```

## Functionality

  - [function
    reference](https://kopflab.github.io/mediatools/reference/)
  - [quantities
    vignette](https://kopflab.github.io/mediatools/articles/quantities.html)
    on built-in chemical quantities
  - [operations
    vignette](https://kopflab.github.io/mediatools/articles/operations.html)
    on working with quantities
