context("Integrate spectrum")

test_that("Integrate GammaSpectrum", {
  spc_cnf <- system.file("extdata/test_CNF.cnf", package = "gamma")
  cnf <- read(spc_cnf)
  cnf <- slice_signal(cnf)
  noise_file <- system.file("extdata/BDX100/background", package = "gamma")
  noise <- read(noise_file)
  noise <- slice_signal(noise)

  int1 <- integrate_signal(cnf, range = c(200, 2800), NiEi = TRUE)
  expect_equivalent(int1, c(1.483392e+05, 9.361146e+00),
                    tolerance = 1e-06)
  expect_length(int1, 2)

  int2 <- integrate_signal(cnf, range = c(200, 2800), noise = c(50, 10),
                           NiEi = FALSE)
  expect_equivalent(int2, c(233.98690, 10.00838), tolerance = 1e-07)
  expect_length(int2, 2)

  expect_error(integrate_signal(cnf, range = c(200)),
               "must be a numeric vector of length 2")
  expect_error(integrate_signal(cnf, range = c(200, 2800), noise = 1),
               "must be a numeric vector of length 2")

  spc_tka <- system.file("extdata/test_TKA.tka", package = "gamma")
  tka <- read(spc_tka)
  expect_error(integrate_signal(tka, range = c(200, 2800)),
               "You must calibrate the energy scale of your spectrum first.")
})

test_that("Integrate GammaSpectra", {
  spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
  spectra <- read(spc_dir)
  spectra <- slice_signal(spectra)
  noise_dir <- system.file("extdata/BDX100/background", package = "gamma")
  noise <- read(noise_dir)
  noise <- slice_signal(noise)

  int1 <- integrate_signal(spectra, range = c(200, 2800), noise = c(50, 10),
                           simplify = TRUE)
  expect_type(int1, "double")
  expect_equal(ncol(int1), 2)
  expect_equal(nrow(int1), length(spectra))

  int2 <- integrate_signal(spectra, range = c(200, 2800), simplify = FALSE)
  expect_type(int2, "list")
  expect_length(int2, length(spectra))
  expect_equivalent(lengths(int2), rep(2, length(spectra)))

  expect_type(integrate_signal(spectra, range = c(200, 2800), simplify = TRUE),
              "double")
})
