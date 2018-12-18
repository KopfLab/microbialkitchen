
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mediachemtools

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mediachemtools)](https://cran.r-project.org/package=mediachemtools)
[![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.3.2.9999-orange.svg?style=flat-square)](/commits)
[![Last-Update](https://img.shields.io/badge/updated-2018--12--16-yellowgreen.svg)](/commits)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://kopflab.github.io/mediachemtools/reference/)
[![codecov](https://codecov.io/github/KopfLab/mediachemtools/branch/master/graphs/badge.svg)](https://codecov.io/github/Kopflab/mediachemtools)
[![Build
Status](https://travis-ci.org/KopfLab/mediachemtools.svg?branch=master)](https://travis-ci.org/KopfLab/mediachemtools)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/KopfLab/mediachemtools?branch=master&svg=true)](https://ci.appveyor.com/project/KopfLab/mediachemtools)
[![Binder](https://img.shields.io/badge/launch-Jupyter-blue.svg)](https://mybinder.org/v2/gh/KopfLab/mediachemtools/binder?urlpath=lab)
[![Binder](https://img.shields.io/badge/launch-RStudio-blue.svg)](https://mybinder.org/v2/gh/KopfLab/mediachemtools/binder?urlpath=rstudio)

## About

The [mediachemtools](https://kopflab.github.io/mediachemtools/) package is a
collection of tools to simplify working with the chemical composition
and speciation of defined culture media for microbial physiology and
environmental microbiology research. It includes a wide range of general
purpose functionality for chemical applications including built-in,
data-frame-compatible [chemical
quantities](https://kopflab.github.io/mediachemtools/articles/quantities.html)
(volume, mass, molarity, temperature, pressure, etc.) that automatically
keep track of their units and metric scaling, as well as more
specialized tools for the assembly and comparison of culturing media
recipes, pH buffering strategies and aqueous speciation. All basic data
types and operations are fully implemented, documented and ready to use.
However, since the package is still in active development and some
syntax and function names may change.

## Installation

You can install [mediachemtools](https://kopflab.github.io/mediachemtools/) from
github with the devtools package.

``` r
# install.packages("devtools") 
devtools::install_github("KopfLab/mediachemtools")
```

## Functionality

  - [function
    reference](https://kopflab.github.io/mediachemtools/reference/) for
    details on all functions
  - [quantities
    vignette](https://kopflab.github.io/mediachemtools/articles/quantities.html)
    on built-in chemical quantities
  - [operations
    vignette](https://kopflab.github.io/mediachemtools/articles/operations.html)
    on working with quantities
  - carbonate chemistry vignettes for working with carbonate buffered
    media systems:
      - [equations
        vignette](https://kopflab.github.io/mediachemtools/articles/carbonate_chemistry_equations.html)
      - [examples
        vignette](https://kopflab.github.io/mediachemtools/articles/carbonate_chemistry_examples.html)

Explore all functionality and vignettes of the **mediachemtools** package
interactively on
[binder](https://mybinder.org/):

[![Binder](https://img.shields.io/badge/launch-Jupyter-blue.svg)](https://mybinder.org/v2/gh/KopfLab/mediachemtools/binder?urlpath=lab)
[![Binder](https://img.shields.io/badge/launch-RStudio-blue.svg)](https://mybinder.org/v2/gh/KopfLab/mediachemtools/binder?urlpath=rstudio)
