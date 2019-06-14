# Import CNF files for calibration
spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
spc_calib <- read(spc_dir, skip = TRUE)

# Set dose rate values and errors for each spectrum
data("clermont")
setDoseRate(spc_calib) <- clermont[, c("gamma", "gamma_error")]

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
