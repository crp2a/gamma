context("Classes")

test_that("Initialize a GammaSpectrum instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("GammaSpectrum"), "GammaSpectrum")
  expect_message(new("GammaSpectrum"))

  options("verbose" = FALSE)
  file <- system.file("extdata/test1.cnf", package = "gamma")
  spectrum <- read(file)

  expect_is(spectrum[["reference"]], "character")
  expect_is(spectrum[["date"]], "character")
  expect_is(spectrum[["instrument"]], "character")
  expect_is(spectrum[["file_format"]], "character")
  expect_is(spectrum[["live_time"]], "numeric")
  expect_is(spectrum[["real_time"]], "numeric")
  expect_is(spectrum[["chanel"]], "integer")
  expect_is(spectrum[["energy"]], "numeric")
  expect_is(spectrum[["counts"]], "numeric")

  expect_is(as(spectrum, "matrix"), "matrix")
  expect_is(as(spectrum, "data.frame"), "data.frame")
})
test_that("Initialize a GammaSpectra instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("GammaSpectra"), "GammaSpectra")
  expect_s4_class(new("GammaSpectra"), "list")
  expect_message(new("GammaSpectra"))

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
test_that("Initialize a DoseRate instance", {
  options("verbose" = TRUE)
  expect_s4_class(new("DoseRate"), "DoseRate")
  expect_message(new("DoseRate"))

  expect_error(new("DoseRate", reference = LETTERS), "slots must have the same length")
  expect_error(new("DoseRate", reference = NA_character_), "missing values were detected")
  expect_error(new("DoseRate", reference = 1:2))
  expect_error(new("DoseRate", dose_value = NA_real_), "missing values were detected")
  expect_error(new("DoseRate", dose_value = NaN), "missing values were detected")
  expect_error(new("DoseRate", dose_value = Inf), "missing values were detected")
  expect_error(new("DoseRate", dose_value = LETTERS))
  expect_error(new("DoseRate", dose_error = NA_real_), "missing values were detected")
  expect_error(new("DoseRate", dose_error = NaN), "missing values were detected")
  expect_error(new("DoseRate", dose_error = Inf), "missing values were detected")
  expect_error(new("DoseRate", dose_error = LETTERS))
  expect_error(new("DoseRate", signal_value = NA_real_), "missing values were detected")
  expect_error(new("DoseRate", signal_value = NaN), "missing values were detected")
  expect_error(new("DoseRate", signal_value = Inf), "missing values were detected")
  expect_error(new("DoseRate", signal_value = LETTERS))
  expect_error(new("DoseRate", signal_error = NA_real_), "missing values were detected")
  expect_error(new("DoseRate", signal_error = NaN), "missing values were detected")
  expect_error(new("DoseRate", signal_error = Inf), "missing values were detected")
  expect_error(new("DoseRate", signal_error = LETTERS))
})
