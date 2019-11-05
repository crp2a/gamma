context("Calibrate energy scale")

test_that("Calibrate GammaSpectrum", {
  spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
  spectrum <- read(spc_file)

  lines <- list(
    Pb = c(chanel = 76, energy = 238),
    K = c(chanel = 459, energy = 1461),
    Cs = c(chanel = 816, energy = 2614.5)
  )

  calib <- calibrate_energy(spectrum, lines = lines)
  expect_s4_class(calib, "GammaSpectrum")

  expect_equal(calib[["hash"]], spectrum[["hash"]])
  expect_equal(calib[["reference"]], spectrum[["reference"]])
  expect_s3_class(calib[["date"]], "POSIXct")
  expect_equal(calib[["instrument"]], spectrum[["instrument"]])
  expect_equal(calib[["file_format"]], spectrum[["file_format"]])
  expect_equal(calib[["live_time"]], spectrum[["live_time"]])
  expect_equal(calib[["real_time"]], spectrum[["real_time"]])
  expect_equal(calib[["chanel"]], spectrum[["chanel"]])
  expect_length(calib[["energy"]], 1024)
  expect_true(all(calib[["energy"]] != spectrum[["energy"]]))
  expect_equal(calib[["counts"]], spectrum[["counts"]])
  expect_equal(calib[["rate"]], spectrum[["rate"]])
  expect_s3_class(calib[["calibration"]], "lm")

  expect_error(calibrate_energy(spectrum, lines = lines[1]),
               "You have to provide at least 3 lines for calibration, not 1.")
  lines <- list(
    Pb = c(76, 238),
    K = c(chanel = 459, energy = 1461),
    Cs = c(chanel = 816, energy = 2614.5)
  )
  expect_error(calibrate_energy(spectrum, lines = lines),
               "`lines` is a list but does not have components 'chanel' and 'energy'.")
})

test_that("Calibrate GammaSpectra", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)

  lines <- list(
    Pb = c(chanel = 76, energy = 238),
    K = c(chanel = 459, energy = 1461),
    Cs = c(chanel = 816, energy = 2614.5)
  )

  calib <- calibrate_energy(spectra, lines = lines)
  expect_s4_class(calib, "GammaSpectra")
})
