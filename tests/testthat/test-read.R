context("Import files")

test_that("Import a gamma spectrum", {
  tka_file <- system.file("extdata/xxx.cnf", package = "gamma")
  expect_error(read(tka_file))

  tka_file <- system.file("extdata/test_LaBr.TKA", package = "gamma")
  tka_spectrum <- read(tka_file)
  expect_output(show(tka_spectrum), "Gamma spectrum")

  cnf_file <- system.file("extdata/test_LaBr.CNF", package = "gamma")
  cnf_spectrum <- read(cnf_file)
  expect_output(show(cnf_spectrum), "Gamma spectrum")
  expect_equal(get_nchanels(cnf_spectrum), 1024)
  expect_equal(round(range_energy(cnf_spectrum)), c(-7, 3125))

  expect_s3_class(methods::as(cnf_spectrum, "data.frame"), "data.frame")
  expect_type(methods::as(cnf_spectrum, "list"), "list")

  expect_equal(dim(summarise(cnf_spectrum)), c(1, 7))
})
test_that("Import a set of gamma spectra", {
  spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
  spectra <- read(spc_dir)

  expect_error(read(system.file("extdata/BDX100/", package = "gamma")))

  expect_output(show(spectra), "A collection")
  expect_length(spectra, 7)
  expect_length(names(spectra), 7)
  expect_type(names(spectra), "character")

  expect_length(spectra[], 7) # All spectra
  expect_length(spectra[NULL], 7) # All spectra
  expect_length(spectra[1], 1) # The first spectrum
  expect_length(spectra[-7], 6) # Delete the seventh spectrum
  expect_length(spectra[1:3], 3) # The first three spectra
  expect_length(spectra[c(1, 3)], 2) # The first and third spectra
  expect_length(spectra["BRIQUE"], 1) # The spectrum named 'BRIQUE'
  expect_length(spectra[c("BRIQUE", "C347")], 2) # The spectra named 'BRIQUE' and 'C347'

  expect_type(spectra[1:3, "energy"], "list") # The slot 'energy' of the first three spectra

  expect_s4_class(spectra[[1]], "GammaSpectrum")
  expect_s4_class(spectra[["BRIQUE"]], "GammaSpectrum")

  expect_type(methods::as(spectra, "list"), "list")
  expect_s3_class(methods::as(spectra, "data.frame"), "data.frame")

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
