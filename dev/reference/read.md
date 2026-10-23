# Data Input

Reads a gamma ray spectrum file.

## Usage

``` r
read(file, ...)

# S4 method for class 'character'
read(file, extensions = c("cnf", "tka", "spe"), ...)
```

## Arguments

- file:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the path of files to be imported.

- ...:

  Extra parameters to be passed to
  [`rxylib::read_xyData()`](https://r-lum.github.io/rxylib/reference/read_xyData.html).

- extensions:

  A [`character`](https://rdrr.io/r/base/character.html) vector
  specifying the possible file extensions. It must be one or more of
  "`cnf`", "`tka`", "`spe`".

## Value

A
[GammaSpectra](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md)
object if more than one spectrum are imported at once, else a
[GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
object.

## Note

Supports Canberra CNF and TKA and Kromek SPE files.

## See also

[`rxylib::read_xyData()`](https://r-lum.github.io/rxylib/reference/read_xyData.html)

Other IO:
[`summarise()`](https://crp2a.github.io/gamma/dev/reference/summarise.md)

## Author

N. Frerebeau

## Examples

``` r
## Import a Canberra CNF file
cnf_file <- system.file("extdata/LaBr.CNF", package = "gamma")
(cnf_spc <- read(cnf_file))
#> Gamma spectrum:
#> *  name: LaBr
#> *  date: 2019-02-07 11:48:18
#> *  live_time: 3385.54
#> *  real_time: 3403.67
#> *  channels: 1024
#> *  energy_min: -7
#> *  energy_max: 3124.91

## Import a TKA file
tka_file <- system.file("extdata/LaBr.TKA", package = "gamma")
(tka_spc <- read(tka_file))
#> Gamma spectrum:
#> *  name: LaBr
#> *  date: 2026-10-23 13:26:20.598214
#> *  live_time: 3385.54
#> *  real_time: 3403.67
#> *  channels: 1024
#> *  energy_min: NA
#> *  energy_max: NA

## Import all files in a given directory
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
(spc <- read(spc_dir))
#> A collection of 7 gamma spectra: BRIQUE, C341, C347, GOU, LMP, MAZ, PEP
```
