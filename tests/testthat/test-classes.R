context("Classes")

test_that("Initialize an empty GammaSpectrum instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("GammaSpectrum"), "GammaSpectrum")
  expect_message(new("GammaSpectrum"), "instance initialized")

  options("verbose" = FALSE)
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
  options("verbose" = TRUE)
  expect_s4_class(new("GammaSpectra"), "GammaSpectra")
  expect_s4_class(new("GammaSpectra"), "list")
  expect_message(new("GammaSpectra"), "instance initialized")

  options("verbose" = FALSE)
  spectra <- new("GammaSpectra")
  expect_output(show(spectra), "An empty set of gamma spectra")
  expect_equal(length(spectra), 0)
  expect_equal(length(names(spectra)), 0)
  expect_type(names(spectra), "character")

  expect_error(getDoseRate(spectra),
               "No dose rate available for these spectra.")
})
test_that("Initialize an empty BaseLine instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("BaseLine"), "BaseLine")
  expect_message(new("BaseLine"), "instance initialized")

  options("verbose" = FALSE)
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
  options("verbose" = TRUE)
  expect_s4_class(new("CalibrationCurve"), "CalibrationCurve")
  expect_message(new("CalibrationCurve"), "instance initialized")

  options("verbose" = FALSE)
  calib <- new("CalibrationCurve")
  expect_output(show(calib), "no model")

  expect_type(calib[["details"]], "list")
  expect_s3_class(calib[["details"]]$date, "POSIXct")
  expect_type(calib[["model"]], "NULL")
  expect_type(calib[["noise"]], "double")
  expect_type(calib[["integration"]], "double")
  expect_s4_class(calib[["data"]], "data.frame")

  expect_error(new("CalibrationCurve", details = list(X = "X")),
               "`details` is a list, but does not have components")
  info_details <- list(
    instrument = LETTERS, laboratory = LETTERS,
    detector = LETTERS, authors = 1:3
  )
  expect_error(new("CalibrationCurve", details = info_details),
               "must be a length-one character vector")

  expect_error(new("CalibrationCurve", noise = 1:3),
               "must be a numeric vector of length two, not 3")
  expect_error(new("CalibrationCurve", integration = 1:3),
               "must be a numeric vector of length two, not 3")
})
test_that("Initialize an empty PeakModel instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("PeakModel"), "PeakModel")
  expect_message(new("PeakModel"), "instance initialized")

  options("verbose" = FALSE)
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
  options("verbose" = TRUE)
  expect_s4_class(new("PeakPosition"), "PeakPosition")
  expect_message(new("PeakPosition"), "instance initialized")

  options("verbose" = FALSE)
  peak <- new("PeakPosition")
  expect_output(show(peak), "No peaks were detected")

  expect_type(peak[["method"]], "character")
  expect_type(peak[["noise"]], "double")
  expect_type(peak[["window"]], "double")
  expect_type(peak[["peaks"]], "double")
  expect_s4_class(peak[["spectrum"]], "GammaSpectrum")
  expect_s4_class(peak[["baseline"]], "BaseLine")

  expect_error(new("PeakPosition", method = LETTERS),
               "must be a character vector of length one, not 26")
  expect_error(new("PeakPosition", noise = 1:26),
               "must be a numeric vector of length one, not 26")
  expect_error(new("PeakPosition", noise = -1),
               "must be a positive number")
  expect_error(new("PeakPosition", window = 1:26),
               "must be a numeric vector of length one, not 26")
  expect_error(new("PeakPosition", window = -1),
               "must be a positive number")
  mtx <- cbind(chanel = 1:26, energy = 1:26, dose = 1:26, rate = 1:26)
  expect_error(new("PeakPosition", peaks = mtx[, 1:3]),
               "must be a 4 columns matrix")
})
