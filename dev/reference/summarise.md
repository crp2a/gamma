# Summarize

Summarize

## Usage

``` r
summarise(object, ...)

# S4 method for class 'GammaSpectrum'
summarise(object)

# S4 method for class 'GammaSpectra'
summarise(object)

# S4 method for class 'DoseRateModel'
summarise(object)

# S4 method for class 'CalibrationCurve'
summarise(object)
```

## Arguments

- object:

  A
  [GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
  or
  [GammaSpectra](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md)
  object.

- ...:

  Currently not used.

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## See also

Other IO:
[`read()`](https://crp2a.github.io/gamma/dev/reference/read.md)

## Author

N. Frerebeau

## Examples

``` r
## Import a Canberra CNF file
cnf_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(cnf_file)
summarise(spc)
#>   name                date live_time real_time channels energy_min energy_max
#> 1 LaBr 2019-02-07 11:48:18   3385.54   3403.67     1024         -7    3124.91

## Import all CNF files in a given directory
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
spc <- read(spc_dir)
summarise(spc)
#>          name                date live_time real_time channels energy_min
#> BRIQUE BRIQUE 2009-08-28 11:43:20   8473.45   8501.68     1024       3.83
#> C341     C341 2009-09-07 15:10:52    976.08    978.36     1024       8.99
#> C347     C347 2009-09-07 14:56:02    703.92    705.96     1024       9.84
#> GOU       GOU 2009-09-07 18:34:19   1008.58   1011.56     1024       7.91
#> LMP       LMP 2009-09-07 16:59:25    908.20    910.17     1024      10.93
#> MAZ       MAZ 2009-09-07 17:47:29    965.86    968.34     1024       9.28
#> PEP       PEP 2009-09-07 16:31:17    507.07    508.96     1024       7.35
#>        energy_max
#> BRIQUE    3243.21
#> C341      3278.18
#> C347      3291.25
#> GOU       3244.38
#> LMP       3273.94
#> MAZ       3248.53
#> PEP       3252.67
```
