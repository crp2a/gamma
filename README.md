
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamma <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/crp2a/gamma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/crp2a/gamma/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/crp2a/gamma/branch/master/graph/badge.svg)](https://app.codecov.io/gh/crp2a/gamma)

<a href="https://crp2a.r-universe.dev" class="pkgdown-devel"><img
src="https://crp2a.r-universe.dev/badges/gamma" alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=gamma"
class="pkgdown-release"><img
src="https://www.r-pkg.org/badges/version/gamma"
alt="CRAN Version" /></a>
<a href="https://cran.r-project.org/web/checks/check_results_gamma.html"
class="pkgdown-release"><img
src="https://badges.cranchecks.info/worst/gamma.svg"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=gamma"
class="pkgdown-release"><img
src="https://cranlogs.r-pkg.org/badges/gamma"
alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2652393.svg)](https://doi.org/10.5281/zenodo.2652393)
[![CRAN
DOI](https://img.shields.io/badge/DOI-10.32614/CRAN.package.gamma-1f57b6?style=flat&link=https://doi.org/10.32614/CRAN.package.gamma)](https://doi.org/10.32614/CRAN.package.gamma)

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

      Frerebeau N, Lebrun B, Paradol G, Kreutzer S (2024). _gamma: Dose
      Rate Estimation from in-Situ Gamma-Ray Spectrometry_. Université
      Bordeaux Montaigne, Pessac, France. doi:10.32614/CRAN.package.gamma
      <https://doi.org/10.32614/CRAN.package.gamma>, R package version
      1.0.5.9000.

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
```

| name           | signal_Ni | signal_err_Ni |   dose_Ni | dose_err_Ni | signal_NiEi | signal_err_NiEi | dose_NiEi | dose_err_NiEi | dose_final | dose_err_final |
|:---------------|----------:|--------------:|----------:|------------:|------------:|----------------:|----------:|--------------:|-----------:|---------------:|
| 20110523204008 |  8.604666 |     0.2691316 |  252.2866 |   10.372500 |    7626.493 |        8.747617 |  220.6747 |      5.495079 |   236.4806 |      11.366900 |
| 20110523210008 |  8.775092 |     0.2166075 |  257.3316 |    9.354274 |    7577.302 |        7.021326 |  219.1303 |      5.454610 |   238.2309 |      10.495729 |
| 20110527205316 |  8.445976 |     0.1546491 |  247.5890 |    8.012680 |    7064.449 |        5.009961 |  203.0288 |      5.052359 |   225.3089 |       9.198048 |
| 20130809172451 | 30.218479 |     0.2549754 |  892.1003 |   24.967288 |   27667.473 |        8.089989 |  849.8817 |     21.142124 |   870.9910 |      32.614162 |
| 20130813181639 | 36.062314 |     0.2853286 | 1065.0899 |   29.644785 |   33243.050 |        9.028097 | 1024.9325 |     25.496544 |  1045.0112 |      39.010062 |
| 20160717175757 | 19.190250 |     0.3635398 |  565.6418 |   18.510895 |   16419.873 |       11.495584 |  496.7518 |     12.361511 |   531.1968 |      21.838611 |
| 20160717181052 | 16.303659 |     0.2837191 |  480.1928 |   15.297869 |   14033.831 |        9.033011 |  421.8396 |     10.496701 |   451.0162 |      18.231817 |
| 20160717182601 | 16.896441 |     0.2557337 |  497.7403 |   15.269850 |   14493.495 |        8.131459 |  436.2712 |     10.854933 |   467.0058 |      18.446627 |

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
