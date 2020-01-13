context("Classes")

test_that("Initialize an empty GammaSpectrum instance", {
  expect_s4_class(new("GammaSpectrum"), "GammaSpectrum")

  spectrum <- new("GammaSpectrum")
  expect_output(show(spectrum), "An empty gamma spectrum")

  expect_type(spectrum[["hash"]], "character")
  expect_type(spectrum[["name"]], "character")
  expect_s3_class(spectrum[["date"]], "POSIXct")
  expect_type(spectrum[["instrument"]], "character")
  expect_type(spectrum[["file_format"]], "character")
  expect_type(spectrum[["live_time"]], "double")
  expect_type(spectrum[["real_time"]], "double")
  expect_type(spectrum[["chanel"]], "integer")
  expect_type(spectrum[["energy"]], "double")
  expect_type(spectrum[["count"]], "double")
  expect_type(spectrum[["rate"]], "double")
  expect_type(spectrum[["calibration"]], "list")
  expect_type(spectrum[["dose_rate"]], "double")

  expect_type(get_hash(spectrum), "character")

  set_names(spectrum) <- "X"
  expect_equal(get_names(spectrum), "X")

  set_dose(spectrum) <- c(1, 2)
  expect_equal(get_dose(spectrum), c(value = 1, error = 2))
  expect_error(set_dose(spectrum) <- c(1, 2, 3),
               "must be a length-two numeric vector.")

  expect_s3_class(as(spectrum, "data.frame"), "data.frame")
  expect_s4_class(as(spectrum, "GammaSpectra"), "GammaSpectra")

  expect_equal(get_chanels(spectrum), 0)
})
test_that("Initialize an empty GammaSpectra instance", {
  expect_s4_class(new("GammaSpectra"), "GammaSpectra")
  expect_s4_class(new("GammaSpectra"), "list")

  spectra <- new("GammaSpectra")
  expect_output(show(spectra), "An empty set of gamma spectra")
  expect_length(spectra, 0)
  expect_length(names(spectra), 0)
  expect_type(names(spectra), "character")

  expect_length(get_dose(spectra), 2)
})
test_that("Initialize an empty BaseLine instance", {
  expect_s4_class(new("BaseLine"), "BaseLine")

  baseline <- new("BaseLine")
  expect_output(show(baseline), "An empty gamma spectrum")

  expect_type(baseline[["hash"]], "character")
  expect_type(baseline[["name"]], "character")
  expect_s3_class(baseline[["date"]], "POSIXct")
  expect_type(baseline[["instrument"]], "character")
  expect_type(baseline[["file_format"]], "character")
  expect_type(baseline[["live_time"]], "double")
  expect_type(baseline[["real_time"]], "double")
  expect_type(baseline[["chanel"]], "integer")
  expect_type(baseline[["energy"]], "double")
  expect_type(baseline[["count"]], "double")
  expect_type(baseline[["rate"]], "double")
  expect_type(baseline[["calibration"]], "list")
  expect_type(baseline[["dose_rate"]], "double")

  expect_s3_class(as(baseline, "data.frame"), "data.frame")
})
test_that("Initialize an empty CalibrationCurve instance", {
  expect_s4_class(new("CalibrationCurve"), "CalibrationCurve")

  calib <- new("CalibrationCurve")
  # expect_output(show(calib), "no model")

  expect_type(calib[["details"]], "list")
  expect_s3_class(calib[["details"]]$date, "POSIXct")
  expect_s4_class(calib[["data"]], "data.frame")

  info_details <- list(
    instrument = LETTERS, laboratory = LETTERS,
    detector = LETTERS, authors = 1:3
  )
  # expect_error(new("CalibrationCurve", details = info_details),
  #              "Slot `details` is a list, but some components are not of length 1.")
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
  expect_error(.PeakPosition(chanel = 1:26),
               "Slots `chanel` and `energy` must have the same length.")

  pks <- .PeakPosition(chanel = 1:3, energy = c(NA_real_, NA_real_, NA_real_))
  expect_equal(get_chanels(pks), 1:3)
  expect_equal(get_energy(pks), c(NA_real_, NA_real_, NA_real_))
  expect_error(set_energy(pks) <- "X", "must be a numeric vector")
  set_energy(pks) <- 1:3
  expect_equal(get_energy(pks), 1:3)
})
