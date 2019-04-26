context("Calibrate energy scale")

test_that("Calibrate GammaSpectrum", {
  spc_file <- system.file("extdata/test.cnf", package = "gamma")
  spectrum <- read(spc_file)

  lines <- list(
    Pb = c(chanel = 76, energy = 238),
    K = c(chanel = 459, energy = 1461),
    Cs = c(chanel = 816, energy = 2614.5)
  )

  calib <- calibrate(spectrum, lines = lines)
  expect_s4_class(calib, "GammaSpectrum")

  expect_equal(calib[["hash"]], spectrum[["hash"]])
  expect_equal(calib[["reference"]], spectrum[["reference"]])
  expect_is(calib[["date"]], "POSIXct")
  expect_equal(calib[["instrument"]], spectrum[["instrument"]])
  expect_equal(calib[["file_format"]], spectrum[["file_format"]])
  expect_equal(calib[["live_time"]], spectrum[["live_time"]])
  expect_equal(calib[["real_time"]], spectrum[["real_time"]])
  expect_equal(calib[["chanel"]], spectrum[["chanel"]])
  expect_length(calib[["energy"]], 1024)
  expect_true(all(calib[["energy"]] != spectrum[["energy"]]))
  expect_equal(calib[["counts"]], spectrum[["counts"]])
  expect_equal(calib[["rate"]], spectrum[["rate"]])
  expect_is(calib[["calibration"]], "lm")

  expect_error(calibrate(spectrum, lines = lines[1]),
               "provide at least two lines")
  lines <- list(
    Pb = c(76, 238),
    K = c(chanel = 459, energy = 1461),
    Cs = c(chanel = 816, energy = 2614.5)
  )
  expect_error(calibrate(spectrum, lines = lines),
               "a list of length-two numeric vectors with names")
})

test_that("Calibrate GammaSpectra", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)

  lines <- list(
    Pb = c(chanel = 76, energy = 238),
    K = c(chanel = 459, energy = 1461),
    Cs = c(chanel = 816, energy = 2614.5)
  )

  calib <- calibrate(spectra, lines = lines)
  expect_s4_class(calib, "GammaSpectra")
})

test_that("Calibrate GammaSpectra", {
  # Make a fake spectrum with no baseline
  cts <- dnorm(1:1024, mean = 86, sd = 5) +
    dnorm(1:1024, mean = 493, sd = 7) +
    dnorm(1:1024, mean = 876, sd = 10)
  # Add some noise
  set.seed(12345)
  spc <- new("GammaSpectrum",
             chanel = 1:1024,
             counts = cts * 10^5 + sample(1:10, 1024, TRUE))
  # Fit peaks
  fit <- fitPeaks(spc, peaks = c(86, 493, 876))

  calib <- calibrate(fit, lines = c(76, 459, 816))
  expect_s4_class(calib, "GammaSpectrum")
  expect_length(calib[["chanel"]], 1024)
  expect_length(calib[["energy"]], 1024)
  expect_length(calib[["counts"]], 1024)
  expect_length(calib[["rate"]], 0)
  expect_is(calib[["calibration"]], "lm")

  expect_error(calibrate(fit, lines = c(76, 459)),
               "must be of length")
})
