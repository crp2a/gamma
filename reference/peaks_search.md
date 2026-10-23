# Search Peaks

Search the maxima in sequential data around a given value.

## Usage

``` r
peaks_search(object, index, ...)

# S4 method for class 'GammaSpectrum,integer'
peaks_search(object, index, span = 10, tolerance = 0.025)

# S4 method for class 'GammaSpectrum,numeric'
peaks_search(object, index, span = 10, tolerance = 0.025)
```

## Arguments

- object:

  A
  [GammaSpectrum](https://crp2a.github.io/gamma/reference/GammaSpectrum-class.md)
  object.

- index:

  A vector giving the expected peak position. If `index` is a
  [`numeric`](https://rdrr.io/r/base/numeric.html) vector, peaks are
  searched by energy (`index` is assumed to be expressed in keV). If
  `index` is an [`integer`](https://rdrr.io/r/base/integer.html) vector,
  peaks are searched by channel.

- ...:

  Currently not used.

- span:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) value giving the
  half window size for searching. If `index` is a
  [`numeric`](https://rdrr.io/r/base/numeric.html) vector, `span` is
  expressed in keV. If `index` is an
  [`integer`](https://rdrr.io/r/base/integer.html) vector, `span` is
  expressed in channel.

- tolerance:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) value giving the
  threshold above which a warning/error is raised.

## Value

A
[PeakPosition](https://crp2a.github.io/gamma/reference/PeakPosition-class.md)
object.

## See also

Other signal processing:
[`baseline`](https://crp2a.github.io/gamma/reference/baseline.md),
[`peaks_find()`](https://crp2a.github.io/gamma/reference/peaks_find.md),
[`signal_integrate()`](https://crp2a.github.io/gamma/reference/integrate.md),
[`signal_slice()`](https://crp2a.github.io/gamma/reference/signal_slice.md),
[`signal_split()`](https://crp2a.github.io/gamma/reference/signal_split.md),
[`signal_stabilize()`](https://crp2a.github.io/gamma/reference/stabilize.md),
[`smooth`](https://crp2a.github.io/gamma/reference/smooth.md)

## Author

N. Frerebeau

## Examples

``` r
## Import a LaBr spectrum
LaBr_file <- system.file("extdata/LaBr.TKA", package = "gamma")
LaBr_spc <- read(LaBr_file)

## Find peaks by channel
(LaBr_pks <- peaks_find(LaBr_spc)) # Ugly
#> 2 peaks were detected.
plot(LaBr_spc, LaBr_pks)


## Search peaks by channel
(LaBr_pks <- peaks_search(LaBr_spc, index = c(86L, 207L, 496L), span = 7))
#> 3 peaks were detected.
plot(LaBr_spc, LaBr_pks, split = TRUE)
#> Ignoring unknown labels:
#> • colour : "Name"


## Import a BEGe spectrum
BEGe_file <- system.file("extdata/BEGe.CNF", package = "gamma")
BEGe_spc <- read(BEGe_file)

## Search peaks by energy
(BEGe_pks <- peaks_search(BEGe_spc, index = c(47, 63, 911, 1460)))
#> 4 peaks were detected.
plot(BEGe_spc, BEGe_pks, split = TRUE)
#> Ignoring unknown labels:
#> • colour : "Name"
```
