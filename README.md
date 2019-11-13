
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamma <img width=120px src="man/figures/logo.png" align="right" />

[![Appveyor build
status](https://ci.appveyor.com/api/projects/status/h7jjppg72oiq9pnf/branch/master?svg=true)](https://ci.appveyor.com/project/nfrerebeau/gamma/branch/master)
[![Travis build
Status](https://travis-ci.org/crp2a/gamma.svg?branch=master)](https://travis-ci.org/crp2a/gamma)
[![codecov](https://codecov.io/gh/crp2a/gamma/branch/master/graph/badge.svg)](https://codecov.io/gh/crp2a/gamma)

<!-- CRAN badges
[![CRAN Version](http://www.r-pkg.org/badges/version/gamma)](https://cran.r-project.org/package=gamma)
[![CRAN checks](https://cranchecks.info/badges/worst/gamma)](https://cran.r-project.org/web/checks/check_results_gamma.html)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/gamma)](https://cran.r-project.org/package=gamma)
-->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2652393.svg)](https://doi.org/10.5281/zenodo.2652393)

## Overview

**gamma** is intended to process in-situ gamma-ray spectrometry
measurements for luminescence dating. This package allows to import,
inspect and (automatically) correct the energy scale of the spectrum. It
provides methods for estimating the gamma dose rate by the use of a
calibration curve. This package only supports Canberra CNF and TKA
files.

## Installation

Install the development version from GitHub with:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("crp2a/gamma")
```

## Usage

A [**Shiny**](https://shiny.rstudio.com) application provides an
exhanced graphical user interface:

``` r
## Run the app
launch_app()
```

![](man/figures/README-shiny-1.png)

Or, if you need a more reproducible workflow:

``` r
## A minimal example
## You may want to give extra attention to the energy calibration step
library(gamma)
library(magrittr)

## Find the full path to the spectrum file
spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")

## Import the spectrum
spectrum <- read(spc_file)

## Set the expected chanel/energy peaks for the energy scale calibration
## Spectrum pre-processing and peak detection
peaks <- spectrum %>%
  slice_signal() %>%
  stabilize_signal(transformation = sqrt) %>%
  smooth_signal(method = "savitzky", m = 21) %>%
  remove_baseline() %>%
  find_peaks()

## Set the energy values (in keV)
set_energy(peaks) <- c(238, NA, NA, NA, 1461, NA, NA, 2615)
peaks
#> 8 peaks were detected:
#>   chanel energy
#> 1     86    238
#> 2    208     NA
#> 3    314     NA
#> 4    384     NA
#> 5    496   1461
#> 6    596     NA
#> 7    722     NA
#> 8    879   2615

## Inspect peaks
plot(spectrum, peaks)
```

<img src="man/figures/README-usage-1.png" style="display: block; margin: auto;" />

``` r

## Calibrate the energy scale
calib <- calibrate_energy(spectrum, peaks)

## Inspect results
plot(calib, xaxis = "energy", yaxis = "rate")
```

<img src="man/figures/README-usage-2.png" style="display: block; margin: auto;" />

``` r

## Load the calibration curve for the dose rate estimation
## As this curve is instrument specific, you will have to build your own
## See help(fit_dose)
data(BDX100, package = "gamma")

## Estimate the gamma dose rate
(doses <- predict_dose(BDX100, calib, simplify = TRUE))
#>              name live_time signal_value signal_error dose_value
#> test_CNF test_CNF   3385.54     126234.7     9.605737   3981.133
#>          dose_error
#> test_CNF   75.23958
```

## Contributing

Please note that the **gamma** project is released with a [Contributor
Code of
Conduct](https://github.com/crp2a/gamma/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Acknowledgements

This work received a state financial support managed by the Agence
Nationale de la Recherche (France) throught the program *Investissements
d’avenir* (ref. [ANR-10-LABX-52](https://lascarbx.labex.u-bordeaux.fr)).
