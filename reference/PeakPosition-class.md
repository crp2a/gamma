# An S4 Class to Represent a Set of Peaks

An S4 Class to Represent a Set of Peaks

## Slots

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the imported spectrum file.

- `noise_method`:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the method used for peak detection.

- `noise_threshold`:

  A length one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the noise threshold.

- `window`:

  A length one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the half-window size.

- `channel`:

  A [`integer`](https://rdrr.io/r/base/integer.html) vector giving the
  channel number. Numeric values are coerced to integer as by
  [`as.integer()`](https://rdrr.io/r/base/integer.html) (and hence
  truncated towards zero).

- `energy_observed`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  observed gamma ray energy (in keV).

- `energy_expected`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  expected gamma ray energy (in keV).

## Note

This class retains copy construction.

## Access

In the code snippets below, `x` is a `PeakPosition` object.

- `get_hash(x)`:

  Get the MD5 hash of the raw data file.

- `get_channels(x)`:

  Get the channels of `x`.

- `get_energy(x)`, `set_energy(x) <- value`:

  Retrieves or sets the energy scale of `x` according to `value`.

## Coerce

In the code snippets below, `x` is a `PeakPosition` object.

- `as.matrix(x)`:

  Coerces `x` to a [`matrix`](https://rdrr.io/r/base/matrix.html).

- `as.data.frame(x)`:

  Coerces `x` to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Subset

In the code snippets below, `x` is a `PeakPosition` object.

- `x[[i]]`:

  Extracts information from a slot selected by subscript `i`. `i` is a
  `character` vector of length one and will be matched to the name of
  the slots.

## See also

Other class:
[`Baseline-class`](https://crp2a.github.io/gamma/reference/Baseline-class.md),
[`CalibrationCurve-class`](https://crp2a.github.io/gamma/reference/CalibrationCurve-class.md),
[`GammaSpectra-class`](https://crp2a.github.io/gamma/reference/GammaSpectra-class.md),
[`GammaSpectrum-class`](https://crp2a.github.io/gamma/reference/GammaSpectrum-class.md),
[`coerce`](https://crp2a.github.io/gamma/reference/coerce.md)

## Author

N. Frerebeau
