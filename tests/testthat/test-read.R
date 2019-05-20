context("Import files")

test_that("Import a gamma spectrum", {
  tka_file <- system.file("extdata/xxx.cnf", package = "gamma")
  expect_error(read(tka_file))

  tka_file <- system.file("extdata/test_TKA.tka", package = "gamma")
  tka_spectrum <- read(tka_file)
  expect_output(show(tka_spectrum), "Gamma spectrum")

  cnf_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
  cnf_spectrum <- read(cnf_file)
  expect_output(show(cnf_spectrum), "Gamma spectrum")
})
test_that("Import a set of gamma spectra", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)

  expect_error(read(system.file("extdata/crp2a/", package = "gamma")))

  expect_output(show(spectra), "A collection")
  expect_length(spectra, 7)
  expect_length(names(spectra), 7)
  expect_is(names(spectra), "character")

  expect_length(spectra[], 7) # All spectra
  expect_length(spectra[NULL], 7) # All spectra
  expect_length(spectra[1], 1) # The first spectrum
  expect_length(spectra[-7], 6) # Delete the seventh spectrum
  expect_length(spectra[1:3], 3) # The first three spectra
  expect_length(spectra[c(1, 3)], 2) # The first and third spectra
  expect_length(spectra["BRIQUE"], 1) # The spectrum named 'BRIQUE'
  expect_length(spectra[c("BRIQUE", "C347")], 2) # The spectra named 'BRIQUE' and 'C347'

  expect_is(spectra[1:3, "energy"], "list") # The slot 'energy' of the first three spectra

  expect_s4_class(spectra[[1]], "GammaSpectrum")
  expect_s4_class(spectra[["BRIQUE"]], "GammaSpectrum")
})
