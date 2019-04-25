# Import CNF files for calibration
spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
spc <- read(spc_dir, skip = TRUE)
## Select 'BRIQUE', 'C341', 'C347', 'GOU', 'LMP', 'MAZ' and 'PEP' to build the curve
spc_calib <- spc[c("BRIQUE", "C341", "C347", "GOU", "LMP", "MAZ", "PEP")]

# Set dose rate values and errors for each spectrum
known_doses <- list(
  reference = c("BRIQUE", "C341", "C347", "GOU", "LMP", "MAZ", "PEP"),
  dose_value = c(1986, 850, 1424, 1575, 642, 1141, 2538),
  dose_error = c(36, 21, 24, 17, 18, 12, 112)
)
## Coerce to a 'DoseRate' object
known_doses <- methods::as(known_doses, "DoseRate")

# Build the calibration curve
calib_curve <- fit(
  spc_calib,
  doses = known_doses,
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
