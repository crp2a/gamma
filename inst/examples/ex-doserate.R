# Import CNF files for calibration
spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
spectra <- read(spc_dir)

# Set dose rate values and errors for each spectrum
data("clermont")
set_dose(spectra) <- clermont[, c("gamma", "gamma_error")]

# Build the calibration curve
calib_curve <- fit_dose(
  spectra,
  noise = c(25279.63171, 1.66235),
  range = c(165, 2800)
)

# Check the linear model
summary(calib_curve[["model"]])

# Plot the curve
plot(calib_curve) +
  ggplot2::labs(x = "Signal", y = "Dose rate [µGy/y]")

# Estimate gamma dose rates
(dose_rate <- predict_dose(calib_curve, spectra))
