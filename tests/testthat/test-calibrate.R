context("Calibrate energy scale")

test_that("Calibrate a GammaSpectrum object with a list", {
  spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
  spectrum <- read(spc_file)

  lines <- list(
    chanel = c(76, 459, 816),
    energy = c(238, 1461, 2614.5)
  )

  calib <- calibrate_energy(spectrum, lines = lines)
  expect_s4_class(calib, "GammaSpectrum")

  expect_equal(calib[["hash"]], spectrum[["hash"]])
  expect_equal(calib[["name"]], spectrum[["name"]])
  expect_s3_class(calib[["date"]], "POSIXct")
  expect_equal(calib[["instrument"]], spectrum[["instrument"]])
  expect_equal(calib[["file_format"]], spectrum[["file_format"]])
  expect_equal(calib[["live_time"]], spectrum[["live_time"]])
  expect_equal(calib[["real_time"]], spectrum[["real_time"]])
  expect_equal(calib[["chanel"]], spectrum[["chanel"]])
  expect_length(calib[["energy"]], 1024)
  expect_true(all(calib[["energy"]] != spectrum[["energy"]]))
  expect_equal(calib[["count"]], spectrum[["count"]])
  expect_equal(calib[["rate"]], spectrum[["rate"]])
  expect_s3_class(calib[["calibration"]], "lm")

  lines <- list(
    X = c(76, 459, 816),
    Y = c(238, 1461, 2614.5)
  )
  expect_error(calibrate_energy(spectrum, lines = lines),
               "does not have components 'chanel' and 'energy'")

  lines <- list(
    chanel = c(76, 816),
    energy = c(238, 2614.5)
  )
  expect_error(calibrate_energy(spectrum, lines = lines),
               "You have to provide at least 3 lines for calibration, not 2.")
})
test_that("Calibrate a GammaSpectrum object with a PeakPosition object", {
  spc_file <- system.file("extdata/test_TKA.tka", package = "gamma")
  spectrum <- read(spc_file)

  peaks <- .PeakPosition(
    hash = spectrum@hash,
    chanel = c(76L, 459L, 816L),
    energy = c(NA_real_, NA_real_, NA_real_)
  )

  expect_error(calibrate_energy(spectrum, lines = peaks),
               "You have to provide at least 3 lines for calibration, not 0.")

  set_energy(peaks) <- c(238, 1461, 2614.5)
  calib <- calibrate_energy(spectrum, lines = peaks)

  expect_s4_class(calib, "GammaSpectrum")
  expect_length(spectrum@energy, 0)
  expect_length(calib@energy, 1024)
})
test_that("the energy scale of a GammaSpectrum is set", {
  cnf_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
  cnf_spc <- read(cnf_file)
  expect_true(is_calibrated(cnf_spc))

  tka_file <- system.file("extdata/test_TKA.tka", package = "gamma")
  tka_spc <- read(tka_file)
  expect_false(is_calibrated(tka_spc))

  set_file <- system.file("extdata/", package = "gamma")
  set_spc <- read(set_file)
  expect_equivalent(is_calibrated(set_spc), c(TRUE, FALSE))
})

