## Import CNF files for calibration
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
spectra <- read(spc_dir)

## Set dose rate values and errors for each spectrum
data("clermont")
set_dose(spectra) <- clermont[, c("gamma_dose", "gamma_error")]

## Build the calibration curve
calib_curve <- fit_dose(
  spectra,
  Ni_noise = c(22.61, 0.05),
  Ni_range = c(300, 2800),
  NiEi_noise = c(25279.63, 1.66),
  NiEi_range = c(165, 2800)
)

## Check the linear model
get_model(calib_curve, "Ni")
get_model(calib_curve, "NiEi")

## Plot the curve
plot(calib_curve, threshold = "Ni")

## Estimate gamma dose rates
predict_dose(calib_curve, spectra, threshold = "Ni")
predict_dose(calib_curve, spectra, threshold = "NiEi")
