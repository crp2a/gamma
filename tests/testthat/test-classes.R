context("Classes")

test_that("Initialize a GammaSpectrum instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("GammaSpectrum"), "GammaSpectrum")
  expect_message(new("GammaSpectrum"), "instance initialized")

  options("verbose" = FALSE)
  file <- system.file("extdata/test.cnf", package = "gamma")
  spectrum <- read(file)

  expect_is(spectrum[["hash"]], "character")
  expect_is(spectrum[["reference"]], "character")
  expect_is(spectrum[["date"]], "POSIXct")
  expect_is(spectrum[["instrument"]], "character")
  expect_is(spectrum[["file_format"]], "character")
  expect_is(spectrum[["live_time"]], "numeric")
  expect_is(spectrum[["real_time"]], "numeric")
  expect_is(spectrum[["chanel"]], "integer")
  expect_is(spectrum[["energy"]], "numeric")
  expect_is(spectrum[["counts"]], "integer")
  expect_is(spectrum[["rate"]], "numeric")
  expect_is(spectrum[["calibration"]], "NULL")

  expect_is(as(spectrum, "matrix"), "matrix")
  expect_is(as(spectrum, "data.frame"), "data.frame")

  expect_equal(length(spectrum), 1024)
})
test_that("Initialize a GammaSpectra instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("GammaSpectra"), "GammaSpectra")
  expect_s4_class(new("GammaSpectra"), "list")
  expect_message(new("GammaSpectra"), "instance initialized")

  options("verbose" = FALSE)
  dir <- system.file("extdata/cerege/", package = "gamma")
  spectra <- read(dir)
  expect_equal(length(spectra), 6)
  expect_equal(length(names(spectra)), 6)
  expect_is(names(spectra), "character")

  expect_equal(length(spectra[]), 6) # All spectra
  expect_equal(length(spectra[NULL]), 6) # All spectra
  expect_equal(length(spectra[1]), 1) # The first spectrum
  expect_equal(length(spectra[-6]), 5) # Delete the sixth spectrum
  expect_equal(length(spectra[1:3]), 3) # The first three spectra
  expect_equal(length(spectra[c(1, 3)]), 2) # The first and third spectra
  expect_equal(length(spectra["BRIQUE"]), 1) # The spectrum named 'BRIQUE'
  expect_equal(length(spectra[c("BRIQUE", "C347")]), 2) # The spectra named 'BRIQUE' and 'C347'

  expect_is(spectra[1:3, "energy"], "list") # The slot 'energy' of the first three spectra

  expect_s4_class(spectra[[1]], "GammaSpectrum")
  expect_s4_class(spectra[["BRIQUE"]], "GammaSpectrum")
})
test_that("Initialize a BaseLine instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("BaseLine"), "BaseLine")
  expect_message(new("BaseLine"), "instance initialized")

  options("verbose" = FALSE)
  file <- system.file("extdata/test.cnf", package = "gamma")
  spectrum <- read(file)
  baseline <- estimateBaseline(spectrum)

  expect_equal(baseline[["hash"]], spectrum[["hash"]])
  expect_equal(baseline[["reference"]], spectrum[["reference"]])
  expect_is(baseline[["date"]], "POSIXct")
  expect_equal(baseline[["instrument"]], spectrum[["instrument"]])
  expect_equal(baseline[["file_format"]], spectrum[["file_format"]])
  expect_equal(baseline[["live_time"]], spectrum[["live_time"]])
  expect_equal(baseline[["real_time"]], spectrum[["real_time"]])
  expect_is(baseline[["chanel"]], "integer")
  expect_is(baseline[["energy"]], "numeric")
  expect_is(baseline[["counts"]], "integer")
  expect_is(baseline[["rate"]], "numeric")
  expect_equal(baseline[["calibration"]], spectrum[["calibration"]])

  expect_is(as(spectrum, "matrix"), "matrix")
  expect_is(as(spectrum, "data.frame"), "data.frame")
})
test_that("Initialize a CalibrationCurve instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("CalibrationCurve"), "CalibrationCurve")
  expect_message(new("CalibrationCurve"), "instance initialized")

  options("verbose" = FALSE)
  calib <- new("CalibrationCurve")
  expect_is(calib[["instrument"]], "character")
  expect_is(calib[["laboratory"]], "character")
  expect_is(calib[["date"]], "POSIXct")
  expect_is(calib[["model"]], "NULL")
  expect_is(calib[["noise"]], "numeric")
  expect_is(calib[["integration"]], "numeric")
  expect_is(calib[["data"]], "data.frame")

  expect_error(new("CalibrationCurve", instrument = LETTERS),
               "must be a character vector of length 1 not 26")
  expect_error(new("CalibrationCurve", laboratory = LETTERS),
               "must be a character vector of length 1 not 26")
  expect_error(new("CalibrationCurve", noise = 1:3),
               "must be a numeric vector of length 2 not 3")
  expect_error(new("CalibrationCurve", integration = 1:3),
               "must be a numeric vector of length 2 not 3")
  df <- data.frame(reference = LETTERS, dose = 1:26, dose_error = 1:26,
                   signal = 1:26, signal_error = 1:26)
  expect_error(new("CalibrationCurve", data = df[, 1:3]),
               "must be a 5 columns data frame")
})
test_that("Initialize a DoseRate instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("DoseRate"), "DoseRate")
  expect_message(new("DoseRate"))

  expect_error(new("DoseRate", reference = LETTERS),
               "slots must have the same length")
  expect_error(new("DoseRate", reference = NA_character_),
               "Missing values were detected")
  expect_error(new("DoseRate", dose_value = NA_real_),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", dose_value = NaN),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", dose_value = Inf),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", dose_error = NA_real_),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", dose_error = NaN),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", dose_error = Inf),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", signal_value = NA_real_),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", signal_value = NaN),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", signal_value = Inf),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", signal_error = NA_real_),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", signal_error = NaN),
               "Missing or infinite values were detected")
  expect_error(new("DoseRate", signal_error = Inf),
               "Missing or infinite values were detected")
})
test_that("Initialize a PeakModel instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("PeakModel"), "PeakModel")
  expect_message(new("PeakModel"), "instance initialized")

  options("verbose" = FALSE)
  peak <- new("PeakModel")
  expect_is(peak[["model"]], "list")
  expect_is(peak[["scale"]], "character")
  expect_is(peak[["peaks"]], "data.frame")
  expect_is(peak[["spectrum"]], "GammaSpectrum")
  expect_is(peak[["baseline"]], "BaseLine")

  expect_error(new("PeakModel", model = list(1:3)),
               "must be of class")
  expect_error(new("PeakModel", scale = LETTERS),
               "must be a character vector of length 1 not 26")
  df <- data.frame(chanel = 1:26, energy = 1:26, dose = 1:26, rate = 1:26)
  expect_error(new("PeakModel", peaks = df[, 1:3]),
               "must be a 4 columns data frame")
})
test_that("Initialize a PeakPosition instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("PeakPosition"), "PeakPosition")
  expect_message(new("PeakPosition"), "instance initialized")

  options("verbose" = FALSE)
  peak <- new("PeakPosition")
  expect_is(peak[["method"]], "character")
  expect_is(peak[["noise"]], "numeric")
  expect_is(peak[["window"]], "numeric")
  expect_is(peak[["peaks"]], "data.frame")
  expect_is(peak[["spectrum"]], "GammaSpectrum")
  expect_is(peak[["baseline"]], "BaseLine")

  expect_error(new("PeakPosition", method = LETTERS),
               "must be a character vector of length 1 not 26")
  expect_error(new("PeakPosition", noise = 1:26),
               "must be a numeric vector of length 1 not 26")
  expect_error(new("PeakPosition", noise = -1),
               "must be a strictly positive number")
  expect_error(new("PeakPosition", window = 1:26),
               "must be a numeric vector of length 1 not 26")
  expect_error(new("PeakPosition", window = -1),
               "must be a strictly positive number")
  df <- data.frame(chanel = 1:26, energy = 1:26, dose = 1:26, rate = 1:26)
  expect_error(new("PeakPosition", peaks = df[, 1:3]),
               "must be a 4 columns data frame")
})
