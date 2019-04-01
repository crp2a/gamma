# Import CNF files for calibration
dir <- system.file("extdata/calib/", package = "gamma")
calib_spc <- read(dir)

# Build calibration curve
calib_curve <- calibrate(
  calib_spc,
  dose = list(BRIQUE = c(1984.64, 34.08), C347 = c(1421.38, 25.25),
              C341 = c(849.01, 21.32), PEP = c(2535.53, 112.17),
              GOU = c(1573.41, 17.43))
)

# Plot curve
plot(calib_curve)

# Import CNF file
dir <- system.file("extdata/", package = "gamma")
gamma_spc <- read(dir)

adjust(gamma_spc, calib_curve)
