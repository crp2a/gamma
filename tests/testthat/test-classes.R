context("Classes")

test_that("Initialize an empty GammaSpectrum instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("GammaSpectrum"), "GammaSpectrum")
  expect_message(new("GammaSpectrum"), "instance initialized")

  options("verbose" = FALSE)
  spectrum <- new("GammaSpectrum")
  expect_output(show(spectrum), "An empty gamma spectrum")

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
  expect_is(as(spectrum, "GammaSpectra"), "GammaSpectra")

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
  expect_is(names(spectra), "character")
})
test_that("Initialize an empty BaseLine instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("BaseLine"), "BaseLine")
  expect_message(new("BaseLine"), "instance initialized")

  options("verbose" = FALSE)
  baseline <- new("BaseLine")
  expect_output(show(baseline), "An empty gamma spectrum")

  expect_is(baseline[["hash"]], "character")
  expect_is(baseline[["reference"]], "character")
  expect_is(baseline[["date"]], "POSIXct")
  expect_is(baseline[["instrument"]], "character")
  expect_is(baseline[["file_format"]], "character")
  expect_is(baseline[["live_time"]], "numeric")
  expect_is(baseline[["real_time"]], "numeric")
  expect_is(baseline[["chanel"]], "integer")
  expect_is(baseline[["energy"]], "numeric")
  expect_is(baseline[["counts"]], "integer")
  expect_is(baseline[["rate"]], "numeric")
  expect_is(baseline[["calibration"]], "NULL")

  expect_is(as(baseline, "matrix"), "matrix")
  expect_is(as(baseline, "data.frame"), "data.frame")
})
test_that("Initialize an empty CalibrationCurve instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("CalibrationCurve"), "CalibrationCurve")
  expect_message(new("CalibrationCurve"), "instance initialized")

  options("verbose" = FALSE)
  calib <- new("CalibrationCurve")
  expect_output(show(calib), "no model")

  expect_is(calib[["instrument"]], "character")
  expect_is(calib[["laboratory"]], "character")
  expect_is(calib[["date"]], "POSIXct")
  expect_is(calib[["model"]], "NULL")
  expect_is(calib[["noise"]], "numeric")
  expect_is(calib[["integration"]], "numeric")
  expect_is(calib[["data"]], "DoseRate")

  expect_error(new("CalibrationCurve", instrument = LETTERS),
               "must be a character vector of length 1 not 26")
  expect_error(new("CalibrationCurve", laboratory = LETTERS),
               "must be a character vector of length 1 not 26")
  expect_error(new("CalibrationCurve", noise = 1:3),
               "must be a numeric vector of length 2 not 3")
  expect_error(new("CalibrationCurve", integration = 1:3),
               "must be a numeric vector of length 2 not 3")
})
test_that("Initialize an empty DoseRate instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("DoseRate"), "DoseRate")
  expect_message(new("DoseRate"))

  options("verbose" = FALSE)
  dose <- new("DoseRate")
  expect_output(show(dose), "gamma dose rate estimate")

  expect_is(dose[["reference"]], "character")
  expect_is(dose[["dose_value"]], "numeric")
  expect_is(dose[["dose_error"]], "numeric")
  expect_is(dose[["signal_value"]], "numeric")
  expect_is(dose[["signal_error"]], "numeric")

  expect_is(as(dose, "data.frame"), "data.frame")

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

  dose <- list(
    reference = c("BRIQUE", "C341", "C347", "GOU", "LMP", "MAZ", "PEP"),
    dose_value = c(1986, 850, 1424, 1575, 642, 1141, 2538),
    dose_error = c(36, 21, 24, 17, 18, 12, 112)
  )

  dose_alt <- dose
  names(dose_alt) <- NULL
  expect_error(as(dose_alt, "DoseRate"), "elements of the list must be named")
  dose_alt <- dose
  dose_alt[[1]] <- c("BRIQUE", "C341", "C347")
  expect_error(as(dose_alt, "DoseRate"), "must have the same length")
  dose_alt <- dose
  names(dose_alt) <- c("A", "B", "C")
  expect_error(as(dose_alt, "DoseRate"), "must one or more of")
  dose_alt <- dose
  names(dose_alt)[1] <- c("A")
  expect_warning(as(dose_alt, "DoseRate"), "ignored")
})
test_that("Initialize an empty PeakModel instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("PeakModel"), "PeakModel")
  expect_message(new("PeakModel"), "instance initialized")

  options("verbose" = FALSE)
  peak <- new("PeakModel")
  expect_output(show(peak), "No peaks parameters")

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
test_that("Initialize an empty PeakPosition instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("PeakPosition"), "PeakPosition")
  expect_message(new("PeakPosition"), "instance initialized")

  options("verbose" = FALSE)
  peak <- new("PeakPosition")
  expect_output(show(peak), "No peaks were detected")

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
               "must be a positive number")
  expect_error(new("PeakPosition", window = 1:26),
               "must be a numeric vector of length 1 not 26")
  expect_error(new("PeakPosition", window = -1),
               "must be a positive number")
  df <- data.frame(chanel = 1:26, energy = 1:26, dose = 1:26, rate = 1:26)
  expect_error(new("PeakPosition", peaks = df[, 1:3]),
               "must be a 4 columns data frame")
})
