
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamma <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/crp2a/gamma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/crp2a/gamma/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/crp2a/gamma/branch/master/graph/badge.svg)](https://app.codecov.io/gh/crp2a/gamma)

<a href="https://crp2a.r-universe.dev" class="pkgdown-devel"><img
src="https://crp2a.r-universe.dev/badges/gamma" alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=gamma"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/gamma"
alt="CRAN Version" /></a>
<a href="https://cran.r-project.org/package=gamma"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/gamma" alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2652393.svg)](https://doi.org/10.5281/zenodo.2652393)
[![SWH](https://archive.softwareheritage.org/badge/swh:1:dir:459ecf47c4c0bb768732bd56c5c245ddab0d33f9/)](https://archive.softwareheritage.org/swh:1:dir:459ecf47c4c0bb768732bd56c5c245ddab0d33f9;origin=https://github.com/crp2a/gamma;visit=swh:1:snp:10e6be6e5cbe735b58c45abbcbabf20b93019e9c;anchor=swh:1:rev:1b3baf8821267ed656d780ae154d347769141d0c/)

<!-- badges: end -->

## Overview

**gamma** is intended to process in-situ gamma-ray spectrometry
measurements for luminescence dating. This package allows to import,
inspect and (automatically) correct the energy scale of the spectrum. It
provides methods for estimating the gamma dose rate by the use of a
calibration curve. This package only supports Canberra CNF and TKA
files.

The [**gammaShiny**](https://github.com/crp2a/gammaShiny) package
provides an enhanced graphical user interface for the main applications
of **gamma**.

    To cite gamma in publications use:

      Lebrun B, Frerebeau N, Paradol G, Guérin G, Mercier N, Tribolo C,
      Lahaye C, Rizza M (2020). "gamma: An R Package for Dose Rate
      Estimation from In-Situ Gamma-Ray Spectrometry Measurements."
      _Ancient TL_, *38*(2), 1-5.

      Frerebeau N, Lebrun B, Paradol G (2024). _gamma: Dose Rate Estimation
      from in-Situ Gamma-Ray Spectrometry_. Université Bordeaux Montaigne,
      Pessac, France. doi:10.5281/zenodo.2652393
      <https://doi.org/10.5281/zenodo.2652393>, R package version 1.0.5.

## Installation

You can install the released version of **gamma** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gamma")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("crp2a/gamma")
```

## Usage

``` r
## A minimal example
library(gamma)

## Find the full path to the spectrum file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
## Import the spectrum
spectrum <- read(spc_file)

## Set the expected channel/energy peaks for the energy scale calibration
## Spectrum pre-processing and peak detection
peaks <- spectrum |>
  signal_slice() |>
  signal_stabilize(f = sqrt) |>
  signal_smooth(method = "savitzky", m = 21) |>
  signal_correct(method = "SNIP", n = 100) |>
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
#> 1 20110523204008  252.2866  378.6325  220.6747   331.0412
#> 2 20110523210008  257.3316  386.1409  219.1303   328.7243
#> 3 20110527205316  247.5890  371.4762  203.0288   304.5699
#> 4 20130809172451  892.1003 1338.3368  849.8817  1274.9341
#> 5 20130813181639 1065.0899 1597.8515 1024.9325  1537.5334
#> 6 20160717175757  565.6418  848.6886  496.7518   745.1931
#> 7 20160717181052  480.1928  720.4625  421.8396   632.8149
#> 8 20160717182601  497.7403  746.7658  436.2712   654.4642
```

## Contributing

Please note that the **gamma** project is released with a [Contributor
Code of
Conduct](https://github.com/crp2a/gamma/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Acknowledgements

This work received a state financial support managed by the Agence
Nationale de la Recherche (France) through the program *Investissements
d’avenir* (ref. [10-LABX-0052](https://lascarbx.labex.u-bordeaux.fr) and
[11-IDEX-0001](https://www.univ-amu.fr/amidex)).
