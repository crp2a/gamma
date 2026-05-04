# Split

Split

## Usage

``` r
signal_split(object, ...)

# S4 method for class 'GammaSpectrum'
signal_split(object, groups)
```

## Arguments

- object:

  A
  [GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
  object.

- ...:

  Currently not used.

- groups:

  A a [`factor`](https://rdrr.io/r/base/factor.html) in the sense that
  `as.factor(groups)` defines the grouping (see
  [`split`](https://rdrr.io/r/base/split.html)).

## Value

A
[GammaSpectra](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md)
object.

## See also

Other signal processing:
[`baseline`](https://crp2a.github.io/gamma/dev/reference/baseline.md),
[`peaks_find()`](https://crp2a.github.io/gamma/dev/reference/peaks_find.md),
[`peaks_search()`](https://crp2a.github.io/gamma/dev/reference/peaks_search.md),
[`signal_integrate()`](https://crp2a.github.io/gamma/dev/reference/integrate.md),
[`signal_slice()`](https://crp2a.github.io/gamma/dev/reference/signal_slice.md),
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
