# Transform Intensities

Transform Intensities

## Usage

``` r
signal_stabilize(object, ...)

# S4 method for class 'GammaSpectrum'
signal_stabilize(object, f, ...)

# S4 method for class 'GammaSpectra'
signal_stabilize(object, f, ...)
```

## Arguments

- object:

  A
  [GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
  object.

- ...:

  Extra arguments to be passed to `f`.

- f:

  A [`function`](https://rdrr.io/r/base/function.html) that takes a
  numeric vector as argument and returns a numeric vector.

## Value

A
[GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
or
[GammaSpectra](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md)
object with transformed intensities.

## Details

The stabilization step aims at improving the identification of peaks
with a low signal-to-noise ratio. This particularly targets higher
energy peaks.

## See also

Other signal processing:
[`baseline`](https://crp2a.github.io/gamma/dev/reference/baseline.md),
[`peaks_find()`](https://crp2a.github.io/gamma/dev/reference/peaks_find.md),
[`peaks_search()`](https://crp2a.github.io/gamma/dev/reference/peaks_search.md),
[`signal_integrate()`](https://crp2a.github.io/gamma/dev/reference/integrate.md),
[`signal_slice()`](https://crp2a.github.io/gamma/dev/reference/signal_slice.md),
[`signal_split()`](https://crp2a.github.io/gamma/dev/reference/signal_split.md),
[`smooth`](https://crp2a.github.io/gamma/dev/reference/smooth.md)

## Author

N. Frerebeau
