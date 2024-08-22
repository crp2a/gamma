# gamma 1.0.5.9000

# gamma 1.0.5
## Bugfixes & changes
* Add support for Kromek SPE files to `read()`(#28 by @RLumSK)
* Add support for `GammaSpectra-class` objects for `energy_calibrate()`(issue: #22, PR #31 by @RLumSK)
* Update vignette about the dose rate calibration curve determination to make it more 
intelligible (#30 by @RLumSK). 

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
