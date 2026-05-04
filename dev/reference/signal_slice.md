# Choose channels by Position

Choose channels by position.

## Usage

``` r
signal_slice(object, ...)

# S4 method for class 'GammaSpectrum'
signal_slice(object, ...)

# S4 method for class 'GammaSpectra'
signal_slice(object, ...)
```

## Arguments

- object:

  A
  [GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
  or
  [GammaSpectra](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md)
  object.

- ...:

  [`integer`](https://rdrr.io/r/base/integer.html) values giving the
  channels of the spectrum to be kept/dropped (see below). Numeric
  values are coerced to integer as by
  [`as.integer()`](https://rdrr.io/r/base/integer.html) (and hence
  truncated towards zero).

## Value

A
[GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
or
[GammaSpectra](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md)
object.

## Details

Either positive values to keep, or negative values to drop, should be
provided. The values provided must be either all positive or all
negative.

If no value is provided, an attempt is made to define the number of
channels to skip at the beginning of the spectrum. This drops all
channels before the highest count maximum. This is intended to deal with
the artefact produced by the rapid growth of random background noise
towards low energies.

## See also

Other signal processing:
[`baseline`](https://crp2a.github.io/gamma/dev/reference/baseline.md),
[`peaks_find()`](https://crp2a.github.io/gamma/dev/reference/peaks_find.md),
[`peaks_search()`](https://crp2a.github.io/gamma/dev/reference/peaks_search.md),
[`signal_integrate()`](https://crp2a.github.io/gamma/dev/reference/integrate.md),
[`signal_split()`](https://crp2a.github.io/gamma/dev/reference/signal_split.md),
[`signal_stabilize()`](https://crp2a.github.io/gamma/dev/reference/stabilize.md),
[`smooth`](https://crp2a.github.io/gamma/dev/reference/smooth.md)

## Author

N. Frerebeau

## Examples

``` r
## Import CNF files
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Plot spectrum
plot(spc)


## Slice
sliced <- signal_slice(spc)
plot(sliced)


sliced <- signal_slice(spc, -c(1:35))
plot(sliced)


sliced <- signal_slice(sliced, 450:550)
plot(sliced)


## Split
g <- rep(c("A", "B", "C"), c(250, 500, 274))
splited <- signal_split(spc, g)
plot(splited, facet = TRUE)
#> Ignoring unknown labels:
#> • colour : "Name"
```
