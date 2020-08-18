
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamma <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R build
status](https://github.com/crp2a/gamma/workflows/R-CMD-check/badge.svg)](https://github.com/crp2a/gamma/actions)
[![codecov](https://codecov.io/gh/crp2a/gamma/branch/master/graph/badge.svg)](https://codecov.io/gh/crp2a/gamma)

<!--[![CRAN Version](http://www.r-pkg.org/badges/version/gamma)](https://cran.r-project.org/package=gamma)
[![CRAN checks](https://cranchecks.info/badges/worst/gamma)](https://cran.r-project.org/web/checks/check_results_gamma.html)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/gamma)](https://cran.r-project.org/package=gamma)-->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2652393.svg)](https://doi.org/10.5281/zenodo.2652393)
<!-- badges: end -->

## Overview

**gamma** is intended to process in-situ gamma-ray spectrometry
measurements for luminescence dating. This package allows to import,
inspect and (automatically) correct the energy scale of the spectrum. It
provides methods for estimating the gamma dose rate by the use of a
calibration curve. This package only supports Canberra CNF and TKA
files.

The [**gammaShiny**](https://github.com/crp2a/gammaShiny) package
provides an exhanced graphical user interface for the main applications
of **gamma**.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("crp2a/gamma")
```

## Usage

``` r
## A minimal example
library(gamma)
library(magrittr)

## Find the full path to the spectrum file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
## Import the spectrum
spectrum <- read(spc_file)

## Set the expected channel/energy peaks for the energy scale calibration
## Spectrum pre-processing and peak detection
peaks <- spectrum %>%
  signal_slice() %>%
  signal_stabilize(f = sqrt) %>%
  signal_smooth(method = "savitzky", m = 21) %>%
  signal_correct(method = "SNIP", n = 100) %>%
  peaks_find()

## Set the energy values (in keV)
set_energy(peaks) <- c(238, NA, NA, NA, 1461, NA, NA, 2615)

## Calibrate the energy scale
calib <- energy_calibrate(spectrum, peaks)

## Inspect peaks
plot(calib, peaks)
```

<img src="man/figures/README-usage-1.png" style="display: block; margin: auto;" />

``` r
## Estimate the gamma dose rate of a set of spectra
## You may want to give extra attention to the energy calibration step
spc_file <- system.file("extdata/BDX_LaBr_1/test", package = "gamma")
spectra <- read(spc_file)

## Load the calibration curve for the dose rate estimation
## As this curve is instrument specific, you will have to build your own
## See help(fit_dose)
data("BDX_LaBr_1", package = "gamma")
plot(BDX_LaBr_1)
```

<img src="man/figures/README-calib-1.png" style="display: block; margin: auto;" />

``` r

## Estimate the gamma dose rate
(doses <- dose_predict(BDX_LaBr_1, spectra))
#>            names   dose_Ni  error_Ni dose_NiEi error_NiEi
#> 1 20110523204008  252.2866 12.385847  220.6747   4.393262
#> 2 20110523210008  257.3316 10.527417  219.1303   4.357479
#> 3 20110527205316  247.5890  8.296925  203.0288   4.033624
#> 4 20130809172451  892.1003 22.333994  849.8817  16.868056
#> 5 20130813181639 1065.0899 26.314061 1024.9325  20.341807
#> 6 20160717175757  565.6418 19.579056  496.7518   9.869339
#> 7 20160717181052  480.1928 15.805970  421.8396   8.379351
#> 8 20160717182601  497.7403 15.232673  436.2712   8.663842
```

## Contributing

Please note that the **gamma** project is released with a [Contributor
Code of
Conduct](https://github.com/crp2a/gamma/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Acknowledgements

This work received a state financial support managed by the Agence
Nationale de la Recherche (France) throught the program *Investissements
d’avenir* (ref. [10-LABX-0052](https://lascarbx.labex.u-bordeaux.fr) and
[11-IDEX-0001](https://amidex.univ-amu.fr)).
