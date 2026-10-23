# Plot

Plot

## Usage

``` r
# S4 method for class 'GammaSpectrum,missing'
plot(x, xaxis = c("channel", "energy"), yaxis = c("count", "rate"), ...)

# S4 method for class 'GammaSpectrum,Baseline'
plot(x, y, xaxis = c("channel", "energy"), yaxis = c("count", "rate"), ...)

# S4 method for class 'GammaSpectra,missing'
plot(
  x,
  xaxis = c("channel", "energy"),
  yaxis = c("count", "rate"),
  select = NULL,
  facet = FALSE,
  nrow = c("fixed", "auto")
)

# S4 method for class 'GammaSpectrum,PeakPosition'
plot(x, y, split = FALSE, span = 25)

# S4 method for class 'CalibrationCurve,missing'
plot(
  x,
  error_ellipse = TRUE,
  error_bar = FALSE,
  energy = FALSE,
  level = 0.95,
  n = 50,
  ...
)
```

## Arguments

- x, y:

  Objects to be plotted.

- xaxis, yaxis:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the data to be plotted along each axis. It must be one of
  "`energy`" or "`channel`" (`x` axis) and "`counts`" or "`rate`" (`y`
  axis). Any unambiguous substring can be given.

- ...:

  Currently not used.

- select:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) or
  [`character`](https://rdrr.io/r/base/character.html) vector giving the
  selection of the spectrum that are drawn.

- facet:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should a
  matrix of panels defined by spectrum be drawn?

- nrow:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the number of rows. It must be one of "`fixed`" or
  "`auto`". Any unambiguous substring can be given. Only used if `facet`
  is `TRUE`.

- split:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should.

- span:

  An [`integer`](https://rdrr.io/r/base/integer.html) giving the half
  window size (in number of channels). Only used if `split` is `TRUE`.

- error_ellipse:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should
  error ellipses be plotted?

- error_bar:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should
  error bars be plotted?

- energy:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar plotting the
  count threshold value or the energy threshold value

- level:

  length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the the probability cutoff for the error ellipses.

- n:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the resolution of the error ellipses.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`IsoplotR::ellipse()`](https://rdrr.io/pkg/IsoplotR/man/ellipse.html),
[`IsoplotR::isochron()`](https://rdrr.io/pkg/IsoplotR/man/isochron.html)

## Author

N. Frerebeau

## Examples

``` r
# Import CNF files
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
spc <- read(spc_dir)

# Plot all spectra
plot(spc, yaxis = "rate", facet = FALSE) +
  ggplot2::theme_bw()


# Plot the spectrum named 'BRIQUE'
plot(spc, xaxis = "energy", yaxis = "count", select = "BRIQUE") +
  ggplot2::theme_bw()


# Plot the first three spectra
plot(spc, xaxis = "channel", yaxis = "rate", select = 1:3, facet = TRUE) +
  ggplot2::theme_bw()
#> Ignoring unknown labels:
#> • colour : "Name"
```
