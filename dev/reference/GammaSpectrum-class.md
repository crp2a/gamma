# An S4 Class to Represent a Gamma Spectrum

Represents a single spectrum of a gamma ray spectrometry measurement.

## Slots

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the imported file.

- `name`:

  A [`character`](https://rdrr.io/r/base/character.html) string the
  measurement reference.

- `date`:

  A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) element
  giving the measurement date and time.

- `instrument`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the instrument name.

- `file_format`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the format of the imported file.

- `live_time`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) value.

- `real_time`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) value.

- `channel`:

  A [`integer`](https://rdrr.io/r/base/integer.html) vector giving the
  channel number. Numeric values are coerced to integer as by
  [`as.integer()`](https://rdrr.io/r/base/integer.html) (and hence
  truncated towards zero).

- `energy`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  gamma ray's energy (in keV).

- `count`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  counts number for each channel. Numeric values are coerced to integer
  as by [`as.integer()`](https://rdrr.io/r/base/integer.html) (and hence
  truncated towards zero).

- `rate`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector the count
  rate (\\1/s\\) for each channel.

- `calibration`:

  A [linear model](https://rdrr.io/r/stats/lm.html) used for energy
  scale calibration (see
  [`energy_calibrate()`](https://crp2a.github.io/gamma/dev/reference/energy.md)).

## Note

This class retains copy construction.

## Access

In the code snippets below, `x` is a `GammaSpectrum` object.

- `length(x)`:

  Get number of channel in `x`.

- `get_hash(x)`:

  Get the MD5 hash of the raw data file.

- `get_names(x)`, `set_names(x) <- value`:

  Retrieves or sets the name of `x` according to `value`.

- `get_channels(x)`:

  Get the number of channels in `x`.

- `get_counts(x)`:

  Get the counts of `x`.

- `get_energy(x)`:

  Get the energy range of `x`.

- `get_rates(x)`:

  Get the count rates of `x`.

## Coerce

In the code snippets below, `x` is a `GammaSpectrum` object.

- `as.matrix(x)`:

  Coerces `x` to a [`matrix`](https://rdrr.io/r/base/matrix.html).

- `as.data.frame(x)`:

  Coerces `x` to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Subset

In the code snippets below, `x` is a `GammaSpectrum` object.

- `x[[i]]`:

  Extracts information from a slot selected by subscript `i`. `i` is a
  `character` vector of length one and will be matched to the name of
  the slots.

## See also

Other class:
[`Baseline-class`](https://crp2a.github.io/gamma/dev/reference/Baseline-class.md),
[`CalibrationCurve-class`](https://crp2a.github.io/gamma/dev/reference/CalibrationCurve-class.md),
[`GammaSpectra-class`](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md),
[`PeakPosition-class`](https://crp2a.github.io/gamma/dev/reference/PeakPosition-class.md),
[`coerce`](https://crp2a.github.io/gamma/dev/reference/coerce.md)

## Author

N. Frerebeau

## Examples

``` r
## Import a Canberra CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
(spc <- read(spc_file))
#> Gamma spectrum:
#> *  name: LaBr
#> *  date: 2019-02-07 11:48:18
#> *  live_time: 3385.54
#> *  real_time: 3403.67
#> *  channels: 1024
#> *  energy_min: -7
#> *  energy_max: 3124.91

## Access
get_hash(spc)
#> [1] "e0a2c67173cf7f407db7148ae0058bbf"
get_names(spc)
#> [1] "LaBr"
get_livetime(spc)
#> [1] 3385.54
get_realtime(spc)
#> [1] 3403.67

length(spc)
#> [1] 1024
range_energy(spc)
#> [1]   -7.004032 3124.914528

## Subset
spc[["date"]]
#> [1] "2019-02-07 11:48:18 UTC"
spc[["instrument"]]
#> [1] "InSpector 1000"
spc[["file_format"]]
#> [1] "CNF"
```
