# Import CNF files for calibration
spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
spc_calib <- read(spc_dir, skip = TRUE)

# Set dose rate values and errors for each spectrum
setDoseRate(spc_calib) <- list(
  BRIQUE = c(1986, 36),
  C341 = c(850, 21),
  C347 = c(1424, 24),
  GOU = c(1575, 17),
  LMP = c(642, 18),
  MAZ = c(1141, 12),
  PEP = c(2538, 112)
)

# Build the calibration curve
calib_curve <- fit(
  spc_calib,
  noise = c(value = 1190, error = 1),
  range = c(200, 2800)
)

# Check the linear model
summary(calib_curve[["model"]])

# Plot the curve
plot(calib_curve) +
  ggplot2::labs(x = "Signal", y = "Dose rate [µGy/y]")

# Estimate gamma dose rates
(dose_rate <- predict(calib_curve, spc_calib))

# Plot the estimated values
plot(calib_curve, dose_rate) +
  ggplot2::labs(x = "Signal", y = "Dose rate [µGy/y]")
