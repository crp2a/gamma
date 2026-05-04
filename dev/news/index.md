# Changelog

## gamma 1.1.0.9000

### Additions

- Add user friendly coercion methods:
  [`as_peaks()`](https://crp2a.github.io/gamma/dev/reference/coerce.md)
  and
  [`as_spectra()`](https://crp2a.github.io/gamma/dev/reference/coerce.md).

### Datasets

- Add reference datasets for calibration sites around Heidelberg and the
  CZT detector calibration files as detailed in Kreutzer et al. (2025,
  Geochronology) (PR [\#54](https://github.com/crp2a/gamma/issues/54) by
  [@RLumSK](https://github.com/RLumSK))

## gamma 1.1.0

CRAN release: 2024-09-23

### Bugfixes

- Fix an error in the uncertainty calculation of
  [`dose_predict()`](https://crp2a.github.io/gamma/dev/reference/doserate.md).
  The returned error was too large and did not make much sense due to an
  internal calculation error. Along with the fix, the manual was updated
  to detail the uncertainty calculation (PR
  [\#42](https://github.com/crp2a/gamma/issues/42) by
  [@RLumSK](https://github.com/RLumSK)).
- Fix a graphical issue where the peaks were peaks were set with
  `set_energy()` but did not show correctly when plotted using the
  standard plot method, e.g., `plot(cal, pks)` would show only observed
  but not expected energy lines in the secondary x-axis. Now the
  expected energy lines (if set) are show
  ([\#29](https://github.com/crp2a/gamma/issues/29), PR
  [\#32](https://github.com/crp2a/gamma/issues/32) by
  [@RLumSK](https://github.com/RLumSK)).
- Fix the uncertainty calculation for the integrated signal. The formula
  had an factor of 2 in `var(2 * x)` where `x` is the integrated signal.
  Now the formula considers plain Poisson statistics. Along with this
  change, the calculation is now detailed in the manual (PR
  [\#46](https://github.com/crp2a/gamma/issues/46) by
  [@RLumSK](https://github.com/RLumSK)).

### Additions

- Add support for Kromek SPE files to
  [`read()`](https://crp2a.github.io/gamma/dev/reference/read.md)
  ([\#28](https://github.com/crp2a/gamma/issues/28) by
  [@RLumSK](https://github.com/RLumSK)).
- Add support for `GammaSpectra-class` objects for
  [`energy_calibrate()`](https://crp2a.github.io/gamma/dev/reference/energy.md)
  ([\#22](https://github.com/crp2a/gamma/issues/22), PR
  [\#31](https://github.com/crp2a/gamma/issues/31) by
  [@RLumSK](https://github.com/RLumSK)).
- Add coercion method for `PeakPosition-class` to `list` (exported as
  [`as.list()`](https://rdrr.io/r/base/list.html)) and from `list` to
  `PeakPosition-class`. This enables better plotting functionality if
  the peak positions for where provided manually as `list` and not via,
  e.g., `peak_find()` (PR
  [\#37](https://github.com/crp2a/gamma/issues/37) by
  [@RLumSK](https://github.com/RLumSK)).
- Add additional columns to the output object of
  [`dose_predict()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  and calculate a “final” dose based on the mean of the findings from
  the count and the energy threshold (PR
  [\#43](https://github.com/crp2a/gamma/issues/43) by
  [@RLumSK](https://github.com/RLumSK)).
- Add new methods for signatures `lm`, `CalibrationCurve-class`, and
  `GammaSpectrum-class`
  to[`energy_calibrate()`](https://crp2a.github.io/gamma/dev/reference/energy.md)
  for `GammaSpectrum-class` and `GammaSpectra-class()` objects for the
  argument `lines`. In simple words, instead of providing data for an
  energy/channel calibration such calibration can be copied over from
  another already calibrated spectrum (PR
  [\#49](https://github.com/crp2a/gamma/issues/49),
  [\#52](https://github.com/crp2a/gamma/issues/52) by
  [@RLumSK](https://github.com/RLumSK)).
- Add support for the energy calibration model to
  [`dose_fit()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  and
  [`dose_predict()`](https://crp2a.github.io/gamma/dev/reference/doserate.md).
  What does it mean? (1) If an energy calibration was performed on the
  spectra used for the dose rate model fitting, the model information is
  forwarded to the info slot of the model. (2) The function
  [`dose_fit()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  can read this information and double-check whether the user tries to
  predict the dose with calibrated or uncalibrated data. If the
  calibration has data but the spectrum does not, the function tries to
  use the available calibration. Given that the energy calibration often
  does not change considerably, this should dramatically simplify the
  workflow once the equipment was calibrated (PR
  [\#49](https://github.com/crp2a/gamma/issues/49) by
  [@RLumSK](https://github.com/RLumSK)).
- Add new argument `use_MC` to
  [`dose_predict()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  method. The default is `FALSE` to maintain compatibility with old code
  and output exceptions. If set to `TRUE` the uncertainty on the gamma
  dose rate uses a Monte Carlo simulation approach for a more realistic
  error estimation (PR [\#46](https://github.com/crp2a/gamma/issues/46)
  by RLumSK).
- Add new function parameter `water_content` to
  [`dose_predict()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  to allow for an estimate of the dry gamma dose rate using the
  correction factor by Aitken (1985). The default is `NULL`, in this
  case nothing is corrected (PR
  [\#48](https://github.com/crp2a/gamma/issues/48) by
  [@RLumSK](https://github.com/RLumSK)).
- Add two more generics: `set_energy_calibration()` and
  [`get_energy_calibration()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  and corresponding methods for `GammaSpectrum` and `GammaSpectra`
  objects. They build on
  [`energy_calibrate()`](https://crp2a.github.io/gamma/dev/reference/energy.md)
  but enable a more comprehensible scripting.
- Extend
  [`dose_predict()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  to work with a `numeric` input for `background` as claimed in the
  documentary. This value can also be set to `c(0,0)` if no background
  subtraction is wanted (PR
  [\#38](https://github.com/crp2a/gamma/issues/38) by
  [@RLumSK](https://github.com/RLumSK)).

### Changes

- Update vignette about the dose rate calibration curve determination to
  make it more intelligible
  ([\#30](https://github.com/crp2a/gamma/issues/30) by
  [@RLumSK](https://github.com/RLumSK)).
- Update manual for the output object of
  [`dose_predict()`](https://crp2a.github.io/gamma/dev/reference/doserate.md),
  which had some loopholes (PR
  [\#43](https://github.com/crp2a/gamma/issues/43) by
  [@RLumSK](https://github.com/RLumSK)).

### Datasets

- Add conversion factor reference to `clermont` dataset for better
  transparency.
- Add new dataset called `clermont_2024` based on the original
  `clermont` dataset but with dose rate conversion factors and gamma
  dose rate calculated for different conversion factor datasets (PR
  [\#40](https://github.com/crp2a/gamma/issues/40) by
  [@RLumSK](https://github.com/RLumSK)).

## gamma 1.0.5

CRAN release: 2024-04-08

### Internals

- Fix unicode character in plot axis labels.

## gamma 1.0.4

CRAN release: 2023-06-23

### Bugfixes & changes

- Fix the metadata when reading a CNF file: do not use a fixed index to
  isolate particular metadata (the number of metadata may vary depending
  on the user’s acquisition).
- Fix `set_energy<-` so that argument `value` appears in the method at
  the end of the argument list.

## gamma 1.0.3

CRAN release: 2022-09-15

### Bugfixes & changes

- Remove the `alpha` argument in
  [`dose_fit()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  to follow changes in **IsoplotR**.

## gamma 1.0.2

CRAN release: 2022-08-23

### Internals

- Use **testthat** 3rd edition
  ([\#20](https://github.com/crp2a/gamma/issues/20)).
- Change package maintainer
  ([\#24](https://github.com/crp2a/gamma/issues/24)).
- The package was removed from CRAN due to an internal call to a defunct
  function
  [`default.stringsAsFactors()`](https://rdrr.io/r/base/base-defunct.html);
  fixed ([\#23](https://github.com/crp2a/gamma/issues/23),
  [@RLumSK](https://github.com/RLumSK)).

## gamma 1.0.1

CRAN release: 2021-01-13

### Internals

- Skip test if suggested packages are not installed
  ([\#19](https://github.com/crp2a/gamma/issues/19)).
- Use `\doi` instead of `\href` in documentation.

## gamma 1.0.0

CRAN release: 2020-09-18

- First CRAN release.

## gamma 0.2.0

- Beta release.

## gamma 0.1.1

- Alpha release.

## gamma 0.1.0

- Alpha release.
