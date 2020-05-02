context("Mutators")

test_that("GammaSpectrum", {
  file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- read(file)

  expect_equal(get_hash(spectrum), "e0a2c67173cf7f407db7148ae0058bbf")

  expect_equal(get_names(spectrum), "LaBr")
  set_names(spectrum) <- "X"
  expect_equal(get_names(spectrum), "X")
  expect_error(set_names(spectrum) <- 1,
               "a length-one character vector")
  expect_error(set_names(spectrum) <- c("A", "B"),
               "a length-one character vector")

  expect_equal(get_time(spectrum, "live"), 3385.54)
  expect_equal(get_time(spectrum, "real"), 3403.67)

  expect_length(get_chanels(spectrum), 1024)
  expect_equal(get_nchanels(spectrum), 1024)

  expect_length(get_counts(spectrum), 1024)

  expect_length(get_rates(spectrum), 1024)

  expect_length(get_energy(spectrum), 1024)
  expect_equal(range_energy(spectrum), c(-7.004032, 3124.914528))

  expect_equal(get_dose(spectrum), c(value = 0, error = 0))
  set_dose(spectrum) <- c(1, 2)
  expect_equal(get_dose(spectrum), c(value = 1, error = 2))
  expect_error(set_dose(spectrum) <- 1,
               "a length-two numeric vector")
  expect_error(set_dose(spectrum) <- "X",
               "a length-two numeric vector")
})
test_that("GammaSpectra", {
  dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spectra <- read(dir)

  expect_length(spectra, 7)
  expect_length(names(spectra), 7)

  expect_length(spectra[], 7)
  expect_length(spectra[NULL], 7)
  expect_length(spectra[1], 1)
  expect_length(spectra[-7], 6)
  expect_length(spectra[1:3], 3)
  expect_length(spectra[c(1, 3)], 2)
  expect_length(spectra["BRIQUE"], 1)
  expect_length(spectra[c("BRIQUE", "C347")], 2)
  expect_type(spectra[1:3, "energy"], "list")

  expect_s4_class(spectra[[1]], "GammaSpectrum")
  expect_s4_class(spectra[["BRIQUE"]], "GammaSpectrum")

  expect_true(all(nchar(get_hash(spectra)) == 32))
  expect_true(all(nchar(get_names(spectra)) > 0))
  set_names(spectra) <- LETTERS[1:7]
  expect_true(all(get_names(spectra) == LETTERS[1:7]))
  expect_true(all(get_names(spectra) == names(spectra)))
  expect_true(all(get_nchanels(spectra) == 1024))
  expect_equal(dim(range_energy(spectra)), c(7, 2))

  doses <- data.frame(value = 1:7, error = 1:7)
  expect_error(set_dose(spectra) <- doses, "do not match")
  expect_error(set_dose(spectra) <- doses[, 1, drop = FALSE],
               "must have at least 2 columns")
  expect_error(set_dose(spectra) <- doses[, 1, drop = TRUE],
               "must be a matrix or a data.frame")

  expect_equal(dim(summarise(spectra)), c(7, 7))
})
