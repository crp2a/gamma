context("Import files")

test_that("Import a gamma spectrum", {
  tka_file <- system.file("extdata/xxx.cnf", package = "gamma")
  expect_error(read(tka_file))

  tka_file <- system.file("extdata/test.tka", package = "gamma")
  tka_spectrum <- read(tka_file)
  expect_output(show(tka_spectrum), "Gamma spectrum")

  cnf_file <- system.file("extdata/test.cnf", package = "gamma")
  cnf_spectrum <- read(cnf_file)
  expect_output(show(cnf_spectrum), "Gamma spectrum")
})
test_that("Import a set of gamma spectra", {
  spc_dir <- system.file("extdata/crp2a/", package = "gamma")
  spectra <- read(spc_dir)

  expect_output(show(spectra), "A collection")
  expect_equal(length(spectra), 8)
  expect_equal(length(names(spectra)), 8)
  expect_is(names(spectra), "character")

  expect_equal(length(spectra[]), 8) # All spectra
  expect_equal(length(spectra[NULL]), 8) # All spectra
  expect_equal(length(spectra[1]), 1) # The first spectrum
  expect_equal(length(spectra[-8]), 7) # Delete the heighth spectrum
  expect_equal(length(spectra[1:3]), 3) # The first three spectra
  expect_equal(length(spectra[c(1, 3)]), 2) # The first and third spectra
  expect_equal(length(spectra["BRIQUE"]), 1) # The spectrum named 'BRIQUE'
  expect_equal(length(spectra[c("BRIQUE", "C347")]), 2) # The spectra named 'BRIQUE' and 'C347'

  expect_is(spectra[1:3, "energy"], "list") # The slot 'energy' of the first three spectra

  expect_s4_class(spectra[[1]], "GammaSpectrum")
  expect_s4_class(spectra[["BRIQUE"]], "GammaSpectrum")
})
