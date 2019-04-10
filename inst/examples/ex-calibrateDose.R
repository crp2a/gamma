# Import CNF files for calibration
dir <- system.file("extdata/cerege/", package = "gamma")
calib_spc <- read(dir)[-5]

# Calibrate energy scale
calib_lines <- list(
  Pb = c(86, 238),
  K = c(493, 1461),
  Cs = c(876, 2614.5)
)

calib_scale <- calibrateEnergy(calib_spc, lines = calib_lines)

# Build calibration curve
known_dose <- list(
  BRIQUE = c(1984.64, 34.08),
  C347 = c(1421.38, 25.25),
  C341 = c(849.01, 21.32),
  PEP = c(2535.53, 112.17),
  GOU = c(1573.41, 17.43)
)

calib_curve <- calibrateDose(
  calib_spc,
  dose = known_dose,
  noise = list(value = 1190, error = 1)
)

# Plot curve
plot(calib_curve) +
  ggplot2::labs(x = "Dose rate [µGy/y]", y = "Integrated signal") +
  ggplot2::theme_bw()

# Import CNF file
dir <- system.file("extdata/", package = "gamma")
gamma_spc <- read(dir)

# Estimate gamma dose rate
dose_rate <- estimateDoseRate(gamma_spc, calib_curve)
