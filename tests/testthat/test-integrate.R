context("Integrate spectrum")

test_that("Integrate GammaSpectrum", {
  spc_cnf <- system.file("extdata/test_CNF.cnf", package = "gamma")
  cnf <- read(spc_cnf)
  noise_file <- system.file("extdata/crp2a/background", package = "gamma")
  noise <- read(noise_file)

  int3 <- integrateSignal(cnf, range = c(200, 2800), NiEi = TRUE)
  expect_equivalent(int3, c(1.483392e+05, 9.361146e+00),
                    tolerance = 1e-06)
  expect_length(int3, 2)

  int2 <- integrateSignal(cnf, range = c(200, 2800), noise = c(50, 10),
                          NiEi = FALSE)
  expect_equivalent(int2, c(233.98690, 10.00838), tolerance = 1e-07)
  expect_length(int2, 2)

  int3 <- integrateSignal(cnf, range = c(200, 2800), noise = noise,
                          NiEi = FALSE)
  expect_equivalent(int3, c(258.6836050, 0.4129534), tolerance = 1e-07)
  expect_length(int3, 2)

  expect_error(integrateSignal(cnf, range = c(200)),
               "must be a numeric vector of length two")
  expect_error(integrateSignal(cnf, range = c(200, 2800), noise = 1),
               "must be a numeric vector of length two")

  spc_tka <- system.file("extdata/test_TKA.tka", package = "gamma")
  tka <- read(spc_tka)
  expect_error(integrateSignal(tka, range = c(200, 2800)),
               "You must calibrate the energy scale of your spectrum first.")
})

test_that("Integrate GammaSpectra", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)
  noise_dir <- system.file("extdata/crp2a/background", package = "gamma")
  noise <- read(noise_dir)

  int1 <- integrateSignal(spectra, range = c(200, 2800), noise = c(50, 10),
                          simplify = TRUE)
  expect_type(int1, "double")
  expect_equal(ncol(int1), 2)
  expect_equal(nrow(int1), length(spectra))

  int2 <- integrateSignal(spectra, range = c(200, 2800), simplify = FALSE)
  expect_type(int2, "list")
  expect_length(int2, length(spectra))
  expect_equivalent(lengths(int2), rep(2, length(spectra)))

  int3 <- integrateSignal(spectra, range = c(200, 2800), noise = noise)
  expect_type(int3, "list")
  expect_length(int3, length(spectra))
  expect_equivalent(lengths(int3), rep(2, length(spectra)))

  expect_type(integrateSignal(spectra, range = c(200, 2800), simplify = TRUE),
              "double")
  expect_type(integrateSignal(spectra, range = c(200, 2800), noise = noise,
                              simplify = TRUE), "double")
})
