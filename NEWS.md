




<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

<!-- # gamma 0.1.1 (2019-06-06) -->

# gamma 0.1.1 (release date: 2019-06-06)

## New classes and methods

  - The functions `getDoseRate()` and `getDoseRate<-` allow to extract
    and replace the gamma dose rate in `GammaSpectrum` objects.
  - The function `smooth()` allows to smooth a spectrum.
  - `integrateSignal()` gained several methods.
  - `fit()` gained a method for the `GammaSpectra,GammaSpectrum`
    signature.

## Bugfixes & changes

  - The `BDX1` curve was slightly corrected (see `help(BDX1)` for
    details).
  - The `DoseRate` class was removed.
  - The slot `peak` of the `PeakPosition` class is now of class
    `matrix`.
  - The slot `count` of the `GammaSpectrum` class is now of class
    `integer`.

## Internals

  - Use `vapply()` instead of `sapply()`.
  - Use `seq_len()` instead of `1:...`.
  - The `fitPeak()` method no longer stops as soon as an error is
    raised: problematic cases are skipped and errors are returned as
    warnings.

## Enhancements

  - The `AIX1` dataset contains the CEREGE calibration curve for dose
    rate estimation.
  - The `GammaSpectrum` class gained a new slot to store the gamma dose
    rate.

# gamma 0.1.0 (release date: 2019-04-26)

  - First release
