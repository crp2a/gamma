# Find Peaks

Finds local maxima in sequential data.

## Usage

``` r
peaks_find(object, ...)

# S4 method for class 'GammaSpectrum'
peaks_find(object, method = c("MAD"), SNR = 2, span = NULL, ...)
```

## Arguments

- object:

  A
  [GammaSpectrum](https://crp2a.github.io/gamma/reference/GammaSpectrum-class.md)
  object.

- ...:

  Extra parameters to be passed to internal methods.

- method:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the method to be used for background noise estimation (see
  below).

- SNR:

  An [`integer`](https://rdrr.io/r/base/integer.html) giving the
  signal-to-noise-ratio for peak detection (see below).

- span:

  An [`integer`](https://rdrr.io/r/base/integer.html) giving the half
  window size (in number of channels). If `NULL`, 5\\ window size.

## Value

A
[PeakPosition](https://crp2a.github.io/gamma/reference/PeakPosition-class.md)
object.

## Details

A local maximum has to be the highest one in the given window and has to
be higher than \\SNR \times noise\\ to be recognized as peak.

The following methods are available for noise estimation:

- `MAD`:

  Median Absolute Deviation.

## See also

Other signal processing:
[`baseline`](https://crp2a.github.io/gamma/reference/baseline.md),
[`peaks_search()`](https://crp2a.github.io/gamma/reference/peaks_search.md),
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
