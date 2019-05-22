
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamma <img width=120px src="man/figures/logo.png" align="right" />

[![Build
Status](https://travis-ci.org/crp2a/gamma.svg?branch=master)](https://travis-ci.org/crp2a/gamma)
[![codecov](https://codecov.io/gh/crp2a/gamma/branch/master/graph/badge.svg)](https://codecov.io/gh/crp2a/gamma)
[![GitHub
Release](https://img.shields.io/github/release/crp2a/gamma.svg)](https://github.com/crp2a/gamma/releases)
[![CRAN
Version](http://www.r-pkg.org/badges/version/gamma)](https://cran.r-project.org/package=gamma)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/gamma)](https://cran.r-project.org/package=gamma)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2652393.svg)](https://doi.org/10.5281/zenodo.2652393)

## Overview

`gamma` is intended to process in-situ gamma-ray spectrometry
measurements for luminescence dating. This package allows to import,
inspect and (automatically) correct the energy scale of the spectrum. It
provides methods for estimating the gamma dose rate by the use of a
calibration curve. This package only supports Canberra CNF and TKA
files.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("crp2a/gamma")
```

## Usage

``` r
# See the package manual
utils::vignette("gamma", package = "gamma")
```

``` r
# A minimal example
# You may want to give extra attention to the energy calibration step

## Set the expected chanel/energy peaks for the energy scale calibration
calib_lines <- list(
  Pb212 = c(chanel = 86, energy = 238),
  K40 = c(chanel = 496, energy = 1461),
  Tl208 = c(chanel = 876, energy = 2615)
)

## Load the calibration curve for the dose rate estimation
## As this curve is instrument specific, you will have to build your own
## See help(fit)
data(BDX1, package = "gamma")

## Find the full path to the spectrum file
spectrum <- system.file("extdata/test_CNF.cnf", package = "gamma")

## Estimate the gamma dose rate
spectrum %>%
  gamma::read(skip = TRUE) %>%
  gamma::calibrate(lines = calib_lines) %>%
  gamma::predict(BDX1, ., simplify = TRUE)
#>            value    error
#> test_CNF 3964.39 129.4871
```

## Contributing

Please note that the `gamma` project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

## Acknowledgements

This work received a state financial support managed by the Agence
Nationale de la Recherche (France) throught the program *Investissements
d’avenir* (ref. [ANR-10-LABX-52](https://lascarbx.labex.u-bordeaux.fr)).
