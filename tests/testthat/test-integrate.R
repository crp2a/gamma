context("Integrate spectrum")

test_that("Integrate GammaSpectrum", {
  spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
  spectrum <- read(spc_file)

  int_NiEi <- integrateSignal(spectrum, NiEi = TRUE)
  expect_equal(int_NiEi, c(1.483392e+05, 9.361146e+00),
               tolerance = 1e-06, check.attributes = FALSE)
  expect_length(int_NiEi, 2)

  int_Ni <- integrateSignal(spectrum, noise = c(50, 10), NiEi = FALSE)
  expect_equal(int_Ni, c(233.98690, 10.00838),
               tolerance = 1e-07, check.attributes = FALSE)
  expect_length(int_Ni, 2)

  expect_error(integrateSignal(spectrum, range = 1),
               "must be a numeric vector of length two")
  expect_error(integrateSignal(spectrum, range = "X"),
               "must be a numeric vector of length two")
  expect_error(integrateSignal(spectrum, noise = 1),
               "must be a numeric vector of length two")
  expect_error(integrateSignal(spectrum, noise = "X"),
               "must be a numeric vector of length two")
})

test_that("Integrate GammaSpectra", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)

  int_mtx <- integrateSignal(spectra, simplify = TRUE)
  expect_is(int_mtx, "matrix")
  expect_equal(ncol(int_mtx), length(spectra))
  expect_equal(nrow(int_mtx), 2)

  int_list <- integrateSignal(spectra, simplify = FALSE)
  expect_is(int_list, "list")
  expect_length(int_list, length(spectra))
  expect_equal(lengths(int_list), rep(2, length(spectra)),
               check.attributes = FALSE)
})
