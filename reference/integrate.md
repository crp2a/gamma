# Signal Integration

Integration of the spectrum including uncertainty calculation.

## Usage

``` r
signal_integrate(object, background, ...)

# S4 method for class 'GammaSpectrum,missing'
signal_integrate(object, range = NULL, energy = FALSE)

# S4 method for class 'GammaSpectrum,GammaSpectrum'
signal_integrate(object, background, range = NULL, energy = FALSE)

# S4 method for class 'GammaSpectrum,numeric'
signal_integrate(object, background, range = NULL, energy = FALSE)

# S4 method for class 'GammaSpectra,missing'
signal_integrate(object, range = NULL, energy = FALSE, simplify = TRUE)

# S4 method for class 'GammaSpectra,GammaSpectrum'
signal_integrate(
  object,
  background,
  range = NULL,
  energy = FALSE,
  simplify = TRUE
)

# S4 method for class 'GammaSpectra,numeric'
signal_integrate(
  object,
  background,
  range = NULL,
  energy = FALSE,
  simplify = TRUE
)
```

## Arguments

- object:

  A
  [GammaSpectrum](https://crp2a.github.io/gamma/reference/GammaSpectrum-class.md)
  or
  [GammaSpectra](https://crp2a.github.io/gamma/reference/GammaSpectra-class.md)
  object.

- background:

  A
  [GammaSpectrum](https://crp2a.github.io/gamma/reference/GammaSpectrum-class.md)
  object.

- ...:

  Currently not used.

- range:

  A length-two [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the energy range to integrate within (in keV).

- energy:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: use the
  energy or count threshold for the signal integration

- simplify:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should the
  result be simplified to a
  [`matrix`](https://rdrr.io/r/base/matrix.html)? The default value,
  `FALSE`, returns a [`list`](https://rdrr.io/r/base/list.html).

## Value

If `simplify` is `FALSE` (the default) returns a
[`list`](https://rdrr.io/r/base/list.html) of numeric vectors (the
signal value and its error), else returns a
[`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

The function supports two integration techniques (see Guérin & Mercier
2011), the (1) count threshold integration and the (2) energy
integration method:

The count integration technique (`energy = FALSE`) integrates all counts
in given `range`:

\$\$ A = \frac{\Sigma\_{i}^{N}S_i}{t\_{live}} \$\$

Contrary, the energy integration techniques is the integrated
cross-product of counts and corresponding energy per channel:

\$\$ A = \frac{\Sigma\_{i}^{N}S_i \times E_i}{t\_{live}} \$\$

\\A\\ is the area, \\S_i\\ is the signal in the \\i^{th}\\ channel,
\\N\\ the number of channels, \\E_i\\ the energy of the corresponding
channel in keV. \\t\_{live}\\ is the live time of the measurement in
*s*.

For calculating the uncertainties, Poisson statistics are assumed and
hence the errors is calculated as:

\$\$ \sigma_A = \frac{\sqrt{A}}{t\_{live}} \$\$

## Note

The integration assumes that each spectrum has an energy scale.

## References

Guérin, G. & Mercier, M. (2011). Determining Gamma Dose Rates by Field
Gamma Spectroscopy in Sedimentary Media: Results of Monte Carlo
Simulations. *Radiation Measurements*, 46(2), p. 190-195.
[doi:10.1016/j.radmeas.2010.10.003](https://doi.org/10.1016/j.radmeas.2010.10.003)
.

Mercier, N. & Falguères, C. (2007). Field Gamma Dose-Rate Measurement
with a NaI(Tl) Detector: Re-Evaluation of the "Threshold" Technique.
*Ancient TL*, 25(1), p. 1-4.

## See also

Other signal processing:
[`baseline`](https://crp2a.github.io/gamma/reference/baseline.md),
[`peaks_find()`](https://crp2a.github.io/gamma/reference/peaks_find.md),
[`peaks_search()`](https://crp2a.github.io/gamma/reference/peaks_search.md),
[`signal_slice()`](https://crp2a.github.io/gamma/reference/signal_slice.md),
[`signal_split()`](https://crp2a.github.io/gamma/reference/signal_split.md),
[`signal_stabilize()`](https://crp2a.github.io/gamma/reference/stabilize.md),
[`smooth`](https://crp2a.github.io/gamma/reference/smooth.md)

## Author

N. Frerebeau
