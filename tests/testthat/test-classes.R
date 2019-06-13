context("Classes")

test_that("Initialize an empty GammaSpectrum instance", {
  expect_s4_class(new("GammaSpectrum"), "GammaSpectrum")

  spectrum <- new("GammaSpectrum")
  expect_output(show(spectrum), "An empty gamma spectrum")

  expect_type(spectrum[["hash"]], "character")
  expect_type(spectrum[["reference"]], "character")
  expect_s3_class(spectrum[["date"]], "POSIXct")
  expect_type(spectrum[["instrument"]], "character")
  expect_type(spectrum[["file_format"]], "character")
  expect_type(spectrum[["live_time"]], "double")
  expect_type(spectrum[["real_time"]], "double")
  expect_type(spectrum[["chanel"]], "integer")
  expect_type(spectrum[["energy"]], "double")
  expect_type(spectrum[["counts"]], "double")
  expect_type(spectrum[["rate"]], "double")
  expect_type(spectrum[["calibration"]], "NULL")
  expect_type(spectrum[["dose_rate"]], "double")

  expect_type(getDoseRate(spectrum), "double")
  expect_error(setDoseRate(spectrum) <- c(1, 2, 3),
               "`value` must be a length-two numeric vector.")

  expect_type(as(spectrum, "matrix"), "double")
  expect_s3_class(as(spectrum, "data.frame"), "data.frame")
  expect_s4_class(as(spectrum, "GammaSpectra"), "GammaSpectra")

  expect_equal(length(spectrum), 0)
})
test_that("Initialize an empty GammaSpectra instance", {
  expect_s4_class(new("GammaSpectra"), "GammaSpectra")
  expect_s4_class(new("GammaSpectra"), "list")

  spectra <- new("GammaSpectra")
  expect_output(show(spectra), "An empty set of gamma spectra")
  expect_equal(length(spectra), 0)
  expect_equal(length(names(spectra)), 0)
  expect_type(names(spectra), "character")

  expect_error(getDoseRate(spectra),
               "No dose rate available for these spectra.")
})
test_that("Initialize an empty BaseLine instance", {
  expect_s4_class(new("BaseLine"), "BaseLine")

  baseline <- new("BaseLine")
  expect_output(show(baseline), "An empty gamma spectrum")

  expect_type(baseline[["hash"]], "character")
  expect_type(baseline[["reference"]], "character")
  expect_s3_class(baseline[["date"]], "POSIXct")
  expect_type(baseline[["instrument"]], "character")
  expect_type(baseline[["file_format"]], "character")
  expect_type(baseline[["live_time"]], "double")
  expect_type(baseline[["real_time"]], "double")
  expect_type(baseline[["chanel"]], "integer")
  expect_type(baseline[["energy"]], "double")
  expect_type(baseline[["counts"]], "double")
  expect_type(baseline[["rate"]], "double")
  expect_type(baseline[["calibration"]], "NULL")
  expect_type(baseline[["dose_rate"]], "double")

  expect_type(as(baseline, "matrix"), "double")
  expect_s3_class(as(baseline, "data.frame"), "data.frame")
})
test_that("Initialize an empty CalibrationCurve instance", {
  expect_s4_class(new("CalibrationCurve"), "CalibrationCurve")

  calib <- new("CalibrationCurve")
  expect_output(show(calib), "no model")

  expect_type(calib[["details"]], "list")
  expect_s3_class(calib[["details"]]$date, "POSIXct")
  expect_type(calib[["model"]], "NULL")
  expect_type(calib[["noise"]], "double")
  expect_type(calib[["integration"]], "double")
  expect_s3_class(calib[["data"]], "data.frame")

  info_details <- list(
    instrument = LETTERS, laboratory = LETTERS,
    detector = LETTERS, authors = 1:3
  )
  expect_error(new("CalibrationCurve", details = info_details),
               "Slot `details` is a list, but some components are not of length 1.")

  expect_error(new("CalibrationCurve", noise = 1:3),
               "Slot `noise` must be a numeric vector of length 2, not 3.")
  expect_error(new("CalibrationCurve", integration = 1:3),
               "Slot `integration` must be a numeric vector of length 2, not 3.")
})
test_that("Initialize an empty PeakModel instance", {
  expect_s4_class(new("PeakModel"), "PeakModel")

  peak <- new("PeakModel")
  expect_output(show(peak), "No peaks parameters")

  expect_type(peak[["model"]], "list")
  expect_type(peak[["coefficients"]], "double")
  expect_s4_class(peak[["spectrum"]], "GammaSpectrum")
  expect_s4_class(peak[["baseline"]], "BaseLine")

  expect_error(new("PeakModel", model = list(1:3)),
               "must be of class")
  expect_error(new("PeakModel", coefficients = as.matrix(LETTERS)),
               "must be a numeric matrix")
  df <- data.frame(mean = 1:26, sd = 1:26, height = 1:26)
  expect_error(new("PeakModel", coefficients = as.matrix(df[, 1:2])),
               "must be a 3 columns matrix")
})
test_that("Initialize an empty PeakPosition instance", {
  expect_s4_class(new("PeakPosition"), "PeakPosition")

  peak <- .PeakPosition()
  expect_output(show(peak), "No peaks were detected")

  expect_type(peak[["noise_method"]], "character")
  expect_type(peak[["noise_threshold"]], "double")
  expect_type(peak[["window"]], "integer")
  expect_type(peak[["chanel"]], "integer")
  expect_type(peak[["energy"]], "double")
  expect_error(peak[["X"]])

  expect_error(.PeakPosition(noise_method = LETTERS),
               "must be a character vector of length 1, not 26")
  expect_error(.PeakPosition(noise_threshold = 1:26),
               "must be a numeric vector of length 1, not 26")
  expect_error(.PeakPosition(noise_threshold = -1),
               "must be a positive number")
  expect_error(.PeakPosition(window = 1:26),
               "must be an integer vector of length 1, not 26")
  expect_error(.PeakPosition(window = as.integer(-1)),
               "Slot `window` must be a strictly positive integer, not -1.")
  mtx <- cbind(chanel = 1:26, energy = 1:26, dose = 1:26, rate = 1:26)
  expect_error(.PeakPosition(chanel = 1:26),
               "Slots `chanel` and `energy` must have the same length.")
})
