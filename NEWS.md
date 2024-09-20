# gamma 1.0.5.9000
## Bugfixes
* Fix an error in the uncertainty calculation of `dose_predict()`. The returned error was too large and did not make 
much sense due to an internal calculation error. Along with the fix, the manual was updated to detail the 
uncertainty calculation. (PR #42 by @RLumSK)
* Fix a graphical issue where the peaks were peaks were set with `set_energy()` but did not show correctly when plotted using the standard plot method, e.g., `plot(cal, pks)` would show only observed but not expected energy lines in the secondary x-axis. Now the expected energy lines (if set) are show. (#29, PR #32 by @RLumSK).
* Fix the uncertainty calculation for the integrated signal. The formula had an factor of 2 in `var(2 * x)` where `x` is the integrated signal. Now the formula considers plain Poisson statistics. Along with this change, the calculation is now detailed in the manual (PR #46 by @RLumSK).

## Additions
* Add support for Kromek SPE files to `read()`(#28 by @RLumSK).
* Add support for `GammaSpectra-class` objects for `energy_calibrate()`(issue: #22, PR #31 by @RLumSK).
* Add coercion method for `PeakPosition-class` to `list` (exported as `as.list()`) and from `list` to `PeakPosition-class`. This enables better plotting functionality if the peak positions for where provided manually as `list` and not via, e.g., `peak_find()` (PR #37 by @RLumSK).
* Add additional columns to the output object of `dose_predict()` and calculate a "final" dose based on the mean of the findings from the count and the energy threshold (PR #43 by @RLumSK) 
* Add new methods for signatures `lm`, `CalibrationCurve-class`, and `GammaSpectrum-class` to`energy_calibrate()` for `GammaSpectrum-class` and `GammaSpectra-class()` objects for the argument `lines`. In simple words, instead of providing data for an energy/channel calibration such calibration can be copied over from another already calibrated spectrum (PR #49, #XX by @RLumSK).  
* Add support for the energy calibration model to `dose_fit()` and `dose_predict()`. What does it mean? (1) If an energy calibration was performed on the spectra used for the dose rate model fitting, the model information is forwarded to the info slot of the model. (2) The function `dose_fit()` can read this information and double-check whether the user tries to predict the dose with calibrated or uncalibrated data. If the calibration has data but the spectrum does not, the function tries to use the available calibration. Given that the energy calibration often does not change considerably, this should dramatically simplify the 
workflow once the equipment was calibrated (PR #49 by @RLumSK).
* Add new argument `use_MC` to `dose_predict()` method. The default is `FALSE` to maintain compatibility with old code and output exceptions. If set to `TRUE` the uncertainty on the gamma dose rate uses a Monte Carlo simulation approach for a more realistic error estimation (PR #46 by RLumSK)
* Add new function parameter `water_content` to `dose_predict()` to allow for an estimate of the dry gamma dose rate using the correction factor by Aitken (1985). The default is `NULL`, in this case nothing is corrected (PR #48 by @RLumSK)
* Extend `dose_predict()` to work with a `numeric` input for `background` as claimed in the documentary. This value can also be set to `c(0,0)` if no background
subtraction is wanted (PR #38 by @RLumSK)

## Changes
* Update vignette about the dose rate calibration curve determination to make it more intelligible (#30 by @RLumSK). 
* Update manual for the output object of `dose_predict()`, which had some loopholes (PR #43 by @RLumSK) 

## Datasets
* Add conversion factor reference to `clermont` dataset for better transparency.
* Add new dataset called `clermont_2024` based on the original `clermont` dataset but with dose rate conversion factors and gamma dose rate calculated for different conversion factor datasets (PR #40 by @RLumSK)

# gamma 1.0.5

## Internals
* Fix unicode character in plot axis labels.

# gamma 1.0.4
## Bugfixes & changes
* Fix the metadata when reading a CNF file: do not use a fixed index to isolate particular metadata (the number of metadata may vary depending on the user's acquisition).
* Fix `set_energy<-` so that argument `value` appears in the method at the end of the argument list.

# gamma 1.0.3
## Bugfixes & changes
* Remove the `alpha` argument in `dose_fit()` to follow changes in **IsoplotR**.

# gamma 1.0.2
## Internals
* Use **testthat** 3rd edition (#20).
* Change package maintainer (#24).
* The package was removed from CRAN due to an internal call to a defunct function `default.stringsAsFactors()`; fixed (#23, @RLumSK)

# gamma 1.0.1

## Internals
* Skip test if suggested packages are not installed (#19).
* Use `\doi` instead of `\href` in documentation.

# gamma 1.0.0

* First CRAN release.

# gamma 0.2.0

* Beta release.

# gamma 0.1.1

* Alpha release.

# gamma 0.1.0

* Alpha release.
