# Coerce

Coerce

## Usage

``` r
as_spectra(x)

as_peaks(x)

# S4 method for class 'GammaSpectrum'
as.matrix(x, ...)

# S4 method for class 'PeakPosition'
as.matrix(x, ...)

# S4 method for class 'GammaSpectrum'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S4 method for class 'PeakPosition'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S4 method for class 'PeakPosition'
as.list(x, ...)

# S4 method for class 'GammaSpectrum'
as_spectra(x)

# S4 method for class 'list'
as_spectra(x)

# S4 method for class 'list'
as_peaks(x)
```

## Arguments

- x:

  An object to be coerced.

- ...:

  Currently not used.

- row.names, optional:

  Currently not used.

## Value

A coerced object.

## See also

Other class:
[`Baseline-class`](https://crp2a.github.io/gamma/reference/Baseline-class.md),
[`CalibrationCurve-class`](https://crp2a.github.io/gamma/reference/CalibrationCurve-class.md),
[`GammaSpectra-class`](https://crp2a.github.io/gamma/reference/GammaSpectra-class.md),
[`GammaSpectrum-class`](https://crp2a.github.io/gamma/reference/GammaSpectrum-class.md),
[`PeakPosition-class`](https://crp2a.github.io/gamma/reference/PeakPosition-class.md)

## Author

N. Frerebeau

## Examples

``` r
## Import a Canberra CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Coerce
mtx <- as.matrix(spc)
df <- as.data.frame(spc)
head(df)
#>   channel    energy count rate
#> 1       1 -7.004032     0    0
#> 2       2 -4.007936     0    0
#> 3       3 -1.011712     0    0
#> 4       4  1.984640     0    0
#> 5       5  4.981120     0    0
#> 6       6  7.977728     0    0
```
