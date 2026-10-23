# An S4 class to Represent a Dose Rate Calibration Curve

An S4 class to Represent a Dose Rate Calibration Curve

## Slots

- `Ni`:

  A DoseRateModel object.

- `NiEi`:

  A DoseRateModel object.

- `details`:

  A [`list`](https://rdrr.io/r/base/list.html) of length-one vector
  giving the curve metadata.

- `slope`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

- `intercept`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

- `covariance`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

- `MSWD`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

- `df`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

- `p_value`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

- `data`:

  A [`data.frame`](https://rdrr.io/r/base/data.frame.html).

- `range`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

- `background`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector.

## Subset

In the code snippets below, `x` is a `CalibrationCurve` object.

- `x[[i]]`:

  Extracts information from a slot selected by subscript `i`. `i` is a
  `character` vector of length one.

## See also

Other class:
[`Baseline-class`](https://crp2a.github.io/gamma/reference/Baseline-class.md),
[`GammaSpectra-class`](https://crp2a.github.io/gamma/reference/GammaSpectra-class.md),
[`GammaSpectrum-class`](https://crp2a.github.io/gamma/reference/GammaSpectrum-class.md),
[`PeakPosition-class`](https://crp2a.github.io/gamma/reference/PeakPosition-class.md),
[`coerce`](https://crp2a.github.io/gamma/reference/coerce.md)

## Author

N. Frerebeau
