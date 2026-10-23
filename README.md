
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamma <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/crp2a/gamma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/crp2a/gamma/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/crp2a/gamma/graph/badge.svg)](https://app.codecov.io/gh/crp2a/gamma)

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

[![SWH](https://archive.softwareheritage.org/badge/swh:1:dir:459ecf47c4c0bb768732bd56c5c245ddab0d33f9/)](https://archive.softwareheritage.org/swh:1:dir:459ecf47c4c0bb768732bd56c5c245ddab0d33f9;origin=https://github.com/crp2a/gamma;visit=swh:1:snp:10e6be6e5cbe735b58c45abbcbabf20b93019e9c;anchor=swh:1:rev:1b3baf8821267ed656d780ae154d347769141d0c/)
<!-- badges: end -->

## Overview

**gamma** is intended to process in-situ gamma-ray spectrometry
measurements for luminescence dating. This package allows to import,
inspect and (automatically) correct the energy scale of the spectrum. It
provides methods for estimating the gamma dose rate by the use of a
calibration curve. This package only supports Canberra CNF and TKA and
Kromek SPE files.

The [**gammaShiny**](https://github.com/crp2a/gammaShiny) package
provides an enhanced graphical user interface for the main applications
of **gamma**.

    To cite gamma in publications use:

      Lebrun B, Frerebeau N, Paradol G, Guérin G, Mercier N, Tribolo C,
      Lahaye C, Rizza M (2020). "gamma: An R Package for Dose Rate
      Estimation from In-Situ Gamma-Ray Spectrometry Measurements."
      _Ancient TL_, *38*(2), 1-5. doi:10.26034/la.atl.2020.544
      <https://doi.org/10.26034/la.atl.2020.544>.

      Frerebeau N, Lebrun B, Paradol G, Kreutzer S (2026). _gamma: Dose
      Rate Estimation from in-Situ Gamma-Ray Spectrometry_. Université
      Bordeaux Montaigne, Pessac, France. doi:10.5281/zenodo.2652393
      <https://doi.org/10.5281/zenodo.2652393>. R package version 1.2.0.

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

<img src="man/figures/README-usage-1.png" alt="" style="display: block; margin: auto;" />

``` r
## Load the calibration curve for the dose rate estimation
## As this curve is instrument specific, you will have to build your own
data("BDX_LaBr_1", package = "gamma")
plot(BDX_LaBr_1)
```

<img src="man/figures/README-calib-1.png" alt="" style="display: block; margin: auto;" />

``` r
## Estimate the gamma dose rate
(doses <- dose_predict(BDX_LaBr_1, calib))
```

| name | signal_Ni | signal_err_Ni | dose_Ni | dose_err_Ni | signal_NiEi | signal_err_NiEi | dose_NiEi | dose_err_NiEi | dose_final | dose_err_final |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| LaBr | 141.014 | 0.2253903 | 4171.876 | 111.5254 | 126234.7 | 6.893242 | 3944.501 | 98.11905 | 4058.189 | 148.1876 |

## Contributing

Please note that the **gamma** project is released with a [Contributor
Code of
Conduct](https://github.com/crp2a/gamma/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Acknowledgements

This work received a state financial support managed by the Agence
Nationale de la Recherche (France) through the program *Investissements
d’avenir* (ref. 10-LABX-0052 and 11-IDEX-0001).
