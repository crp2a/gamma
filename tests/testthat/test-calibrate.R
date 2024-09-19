test_that("Calibrate a GammaSpectrum object with a list", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- read(spc_file)

  lines <- list(
    channel = c(76, 459, 816),
    energy = c(238, 1461, 2614.5)
  )

  calib <- energy_calibrate(spectrum, lines = lines)
  expect_s4_class(calib, "GammaSpectrum")

  expect_equal(calib[["hash"]], spectrum[["hash"]])
  expect_equal(calib[["name"]], spectrum[["name"]])
  expect_s3_class(calib[["date"]], "POSIXct")
  expect_equal(calib[["instrument"]], spectrum[["instrument"]])
  expect_equal(calib[["file_format"]], spectrum[["file_format"]])
  expect_equal(calib[["live_time"]], spectrum[["live_time"]])
  expect_equal(calib[["real_time"]], spectrum[["real_time"]])
  expect_equal(calib[["channel"]], spectrum[["channel"]])
  expect_length(calib[["energy"]], 1024)
  expect_true(all(calib[["energy"]] != spectrum[["energy"]]))
  expect_equal(calib[["count"]], spectrum[["count"]])
  expect_equal(calib[["rate"]], spectrum[["rate"]])
  expect_s3_class(calib[["calibration"]], "lm")

  lines <- list(
    X = c(76, 459, 816),
    Y = c(238, 1461, 2614.5)
  )
  expect_error(energy_calibrate(spectrum, lines = lines),
               "does not have components")

  lines <- list(
    channel = c(76, 816),
    energy = c(238, 2614.5)
  )
  expect_error(energy_calibrate(spectrum, lines = lines),
               "You have to provide at least 3 lines for calibration, not 2.")
})
test_that("Calibrate a GammaSpectrum object with a PeakPosition object", {
  spc_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  spectrum <- read(spc_file)

  peaks <- .PeakPosition(
    hash = spectrum@hash,
    channel = c(76L, 459L, 816L),
    energy_expected = c(NA_real_, NA_real_, NA_real_)
  )

  expect_error(energy_calibrate(spectrum, lines = peaks),
               "You have to provide at least 3 lines for calibration, not 0.")

  set_energy(peaks) <- c(238, 1461, 2614.5)
  calib <- energy_calibrate(spectrum, lines = peaks)

  expect_s4_class(calib, "GammaSpectrum")
  expect_length(spectrum@energy, 0)
  expect_length(calib@energy, 1024)
})
test_that("Calibrate a GammaSpectra object with a list object", {
  spc_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  spectrum_1 <- spectrum_2 <- read(spc_file)
  spectra <- methods::as(list(spectrum_1, spectrum_2), "GammaSpectra")

  lines <- list(
    channel = c(76, 459, 816),
    energy = c(238, 1461, 2614.5)
  )

  calib <- energy_calibrate(spectra, lines = lines)

  expect_s4_class(calib, "GammaSpectra")

})

test_that("Calibrate a GammaSpectra object with a PeakPosition object", {
  spc_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  spectrum_1 <- spectrum_2 <- read(spc_file)
  spectra <- methods::as(list(spectrum_1, spectrum_2), "GammaSpectra")

  peaks <- .PeakPosition(
    hash = spectrum_1@hash,
    channel = c(76L, 459L, 816L),
    energy_expected = c(NA_real_, NA_real_, NA_real_)
  )

  set_energy(peaks) <- c(238, 1461, 2614.5)
  calib <- energy_calibrate(spectra, lines = peaks)

  expect_s4_class(calib, "GammaSpectra")

})

test_that("the energy scale of a GammaSpectrum is set", {
  cnf_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  cnf_spc <- read(cnf_file)
  expect_true(has_energy(cnf_spc))

  tka_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  tka_spc <- read(tka_file)
  expect_false(has_energy(tka_spc))

  set_file <- system.file("extdata/", package = "gamma")
  set_spc <- read(set_file)
  expect_equal(has_energy(set_spc), c(TRUE, TRUE, TRUE, FALSE), ignore_attr = TRUE)
})


test_that("Calibrate a GammaSpectrum and GammaSpectra object with a lm object", {
  spc_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  spectrum_1 <- spectrum_2 <- spectrum_3 <- spectrum_4 <- read(spc_file)
  spectra <- as(list(spectrum_1, spectrum_2), "GammaSpectra")
  spectra_2 <- as(list(spectrum_1, spectrum_2), "GammaSpectra")

  ## assign first manually
  peaks <- gamma:::.PeakPosition(
    hash = spectrum_1@hash,
    channel = c(76L, 459L, 816L),
    energy_expected = c(NA_real_, NA_real_, NA_real_)
  )

  set_energy(peaks) <- c(238, 1461, 2614.5)
  calib <- energy_calibrate(spectrum_1, lines = peaks)

  ## assign the calibration to spectrum_2
  spectrum_2 <- energy_calibrate(spectrum_2, lines = calib@calibration)

  ##check results
  expect_true(!is.null(spectrum_2@calibration))
  expect_s3_class(spectrum_2@calibration, "lm")
  expect_equal(object = sum(calib@energy), expected = sum(spectrum_2@energy))

  ## now the same test on spectra
  spectra <- energy_calibrate(spectra, lines = calib@calibration)
  expect_true(all(vapply(spectra, function(x) inherits(x@calibration, "lm"), logical(1))))
  expect_equal(object = sum(calib@energy), expected = sum(spectra[[1]]@energy))

  ## new provide just another spectrum
  spectrum_3 <- energy_calibrate(spectrum_3, lines = calib)
  expect_s3_class(spectrum_3@calibration, "lm")
  expect_equal(object = sum(calib@energy), expected = sum(spectrum_3@energy))

  ## new provide just another spectrum to spectra
  spectra_2 <- energy_calibrate(spectra_2, lines = calib)
  expect_true(all(vapply(spectra_2, function(x) inherits(x@calibration, "lm"), logical(1))))
  expect_equal(object = sum(calib@energy), expected = sum(spectra_2[[1]]@energy))

  ## call stop
  calib@calibration <- NULL
  expect_error(spectrum_4 <- energy_calibrate(spectrum_4, lines = calib),
               regexp = "The spectrum provided via 'lines' does not have any calibration!")

})
