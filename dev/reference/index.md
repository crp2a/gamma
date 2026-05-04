# Package index

## Data Import

- [`read()`](https://crp2a.github.io/gamma/dev/reference/read.md) : Data
  Input
- [`summarise()`](https://crp2a.github.io/gamma/dev/reference/summarise.md)
  : Summarize

## Plot

- [`plot(`*`<GammaSpectrum>`*`,`*`<missing>`*`)`](https://crp2a.github.io/gamma/dev/reference/plot.md)
  [`plot(`*`<GammaSpectrum>`*`,`*`<Baseline>`*`)`](https://crp2a.github.io/gamma/dev/reference/plot.md)
  [`plot(`*`<GammaSpectra>`*`,`*`<missing>`*`)`](https://crp2a.github.io/gamma/dev/reference/plot.md)
  [`plot(`*`<GammaSpectrum>`*`,`*`<PeakPosition>`*`)`](https://crp2a.github.io/gamma/dev/reference/plot.md)
  [`plot(`*`<CalibrationCurve>`*`,`*`<missing>`*`)`](https://crp2a.github.io/gamma/dev/reference/plot.md)
  : Plot

## Signal Processing

- [`signal_baseline()`](https://crp2a.github.io/gamma/dev/reference/baseline.md)
  [`signal_correct()`](https://crp2a.github.io/gamma/dev/reference/baseline.md)
  [`baseline_snip()`](https://crp2a.github.io/gamma/dev/reference/baseline.md)
  [`baseline_rubberband()`](https://crp2a.github.io/gamma/dev/reference/baseline.md)
  [`baseline_linear()`](https://crp2a.github.io/gamma/dev/reference/baseline.md)
  : Baseline Estimation and Removal
- [`signal_integrate()`](https://crp2a.github.io/gamma/dev/reference/integrate.md)
  : Signal Integration
- [`peaks_find()`](https://crp2a.github.io/gamma/dev/reference/peaks_find.md)
  : Find Peaks
- [`peaks_search()`](https://crp2a.github.io/gamma/dev/reference/peaks_search.md)
  : Search Peaks
- [`signal_slice()`](https://crp2a.github.io/gamma/dev/reference/signal_slice.md)
  : Choose channels by Position
- [`signal_split()`](https://crp2a.github.io/gamma/dev/reference/signal_split.md)
  : Split
- [`signal_smooth()`](https://crp2a.github.io/gamma/dev/reference/smooth.md)
  [`smooth_rectangular()`](https://crp2a.github.io/gamma/dev/reference/smooth.md)
  [`smooth_triangular()`](https://crp2a.github.io/gamma/dev/reference/smooth.md)
  [`smooth_savitzky()`](https://crp2a.github.io/gamma/dev/reference/smooth.md)
  : Smooth
- [`signal_stabilize()`](https://crp2a.github.io/gamma/dev/reference/stabilize.md)
  : Transform Intensities

## Energy Scale Calibration

- [`energy_calibrate()`](https://crp2a.github.io/gamma/dev/reference/energy.md)
  [`has_energy()`](https://crp2a.github.io/gamma/dev/reference/energy.md)
  [`has_calibration()`](https://crp2a.github.io/gamma/dev/reference/energy.md)
  : Energy Scale Calibration

## Dose Rate Estimation

- [`dose_fit()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  [`dose_predict()`](https://crp2a.github.io/gamma/dev/reference/doserate.md)
  : Dose Rate Estimation

## Classes

- [`Baseline-class`](https://crp2a.github.io/gamma/dev/reference/Baseline-class.md)
  [`.Baseline`](https://crp2a.github.io/gamma/dev/reference/Baseline-class.md)
  [`BaseLine-class`](https://crp2a.github.io/gamma/dev/reference/Baseline-class.md)
  : An S4 Class to Represent a Spectrum Baseline
- [`CalibrationCurve-class`](https://crp2a.github.io/gamma/dev/reference/CalibrationCurve-class.md)
  [`DoseRateModel-class`](https://crp2a.github.io/gamma/dev/reference/CalibrationCurve-class.md)
  [`.DoseRateModel`](https://crp2a.github.io/gamma/dev/reference/CalibrationCurve-class.md)
  [`.CalibrationCurve`](https://crp2a.github.io/gamma/dev/reference/CalibrationCurve-class.md)
  : An S4 class to Represent a Dose Rate Calibration Curve
- [`GammaSpectra-class`](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md)
  [`.GammaSpectra`](https://crp2a.github.io/gamma/dev/reference/GammaSpectra-class.md)
  : An S4 Class to Represent a Collection of Gamma Sectra
- [`GammaSpectrum-class`](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
  [`.GammaSpectrum`](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
  : An S4 Class to Represent a Gamma Spectrum
- [`PeakPosition-class`](https://crp2a.github.io/gamma/dev/reference/PeakPosition-class.md)
  [`.PeakPosition`](https://crp2a.github.io/gamma/dev/reference/PeakPosition-class.md)
  : An S4 Class to Represent a Set of Peaks
- [`as_spectra()`](https://crp2a.github.io/gamma/dev/reference/coerce.md)
  [`as_peaks()`](https://crp2a.github.io/gamma/dev/reference/coerce.md)
  [`as.matrix(`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/coerce.md)
  [`as.matrix(`*`<PeakPosition>`*`)`](https://crp2a.github.io/gamma/dev/reference/coerce.md)
  [`as.data.frame(`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/coerce.md)
  [`as.data.frame(`*`<PeakPosition>`*`)`](https://crp2a.github.io/gamma/dev/reference/coerce.md)
  [`as.list(`*`<PeakPosition>`*`)`](https://crp2a.github.io/gamma/dev/reference/coerce.md)
  : Coerce

## Getters and Setters

- [`get_hash()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_names()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`` `set_names<-`() ``](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_energy_calibration()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`` `set_energy_calibration<-`() ``](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_livetime()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_realtime()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_channels()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_counts()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_rates()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_energy()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`` `set_energy<-`() ``](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_method()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`` `set_method<-`() ``](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`get_residuals()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`range_channels()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`range_energy()`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  [`length(`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/mutator.md)
  : Get or Set Parts of an Object
- [`` `[[`( ``*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/subset.md)
  [`` `[`( ``*`<GammaSpectra>`*`)`](https://crp2a.github.io/gamma/dev/reference/subset.md)
  [`` `[[`( ``*`<DoseRateModel>`*`)`](https://crp2a.github.io/gamma/dev/reference/subset.md)
  [`` `[[`( ``*`<CalibrationCurve>`*`)`](https://crp2a.github.io/gamma/dev/reference/subset.md)
  [`` `[[`( ``*`<PeakPosition>`*`)`](https://crp2a.github.io/gamma/dev/reference/subset.md)
  : Extract or Replace Parts of an Object

## Common Operations

- [`Arith(`*`<GammaSpectrum>`*`,`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Arith(`*`<GammaSpectrum>`*`,`*`<numeric>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Compare(`*`<GammaSpectrum>`*`,`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Compare(`*`<GammaSpectrum>`*`,`*`<numeric>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Logic(`*`<GammaSpectrum>`*`,`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Logic(`*`<GammaSpectrum>`*`,`*`<numeric>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Logic(`*`<GammaSpectrum>`*`,`*`<logical>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Math(`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Math2(`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  [`Summary(`*`<GammaSpectrum>`*`)`](https://crp2a.github.io/gamma/dev/reference/operator.md)
  : Common Operations on GammaSpectrum Objects

## Data

- [`AIX_NaI_1`](https://crp2a.github.io/gamma/dev/reference/AIX_NaI_1.md)
  : CEREGE Calibration Curve (NaI)
- [`BDX_LaBr_1`](https://crp2a.github.io/gamma/dev/reference/BDX_LaBr_1.md)
  : CRP2A Calibration Curve (LaBr)
- [`HD_CZT_1`](https://crp2a.github.io/gamma/dev/reference/HD_CZT_1.md)
  : heiLUM Calibration Curve (CZT)
- [`clermont`](https://crp2a.github.io/gamma/dev/reference/clermont.md)
  : Clermont Reference Data
- [`clermont_2024`](https://crp2a.github.io/gamma/dev/reference/clermont_2024.md)
  : Clermont Reference Data 2024
- [`heidelberg_2024`](https://crp2a.github.io/gamma/dev/reference/heidelberg_2024.md)
  : Heidelberg Reference Data 2024
