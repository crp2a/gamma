context("Dose rate")
data("clermont")

test_that("Get and set dose rate", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)

  dose_rate1 <- clermont[, c("gamma", "gamma_error")]

  setDoseRate(spectra[["BRIQUE"]]) <- c(1986, 36)
  setDoseRate(spectra[["C341"]]) <- c(850, 21)
  setDoseRate(spectra[["C347"]]) <- c(1424, 24)
  setDoseRate(spectra[["GOU"]]) <- c(1575, 17)
  setDoseRate(spectra[["LMP"]]) <- c(642, 18)
  setDoseRate(spectra[["MAZ"]]) <- c(1141, 12)
  setDoseRate(spectra[["PEP"]]) <- c(2538, 112)

  expect_output(show(spectra[["BRIQUE"]]), "Dose rate: 1986 \\+/- 36")

  expect_invisible(setDoseRate(spectra) <- dose_rate1)
})
test_that("Build calibration curve", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir, skip = TRUE)

  setDoseRate(spectra) <- clermont[, c("gamma", "gamma_error")]

  # Fit with intercept
  calib1 <- fit(
    spectra, noise = c(25312, 1.66), range = c(200, 2800),
    intercept = TRUE, weights = FALSE,
    details = NULL
  )
  expect_output(show(calib1), "Calibration curve")
  expect_length(stats::coef(calib1[["model"]]), 2)
  expect_equal(dim(calib1[["data"]]), c(7, 5))
  expect_s3_class(plot(calib1), "ggplot")
  # Fit with no intercept
  calib2 <- fit(
    spectra, noise = c(25312, 1.66), range = c(200, 2800),
    intercept = FALSE, weights = FALSE,
    details = NULL
  )
  expect_output(show(calib2), "Calibration curve")
  expect_length(stats::coef(calib2[["model"]]), 1)
  expect_equal(dim(calib2[["data"]]), c(7, 5))
  expect_s3_class(plot(calib2), "ggplot")
  # Fit with weights
  calib3 <- fit(
    spectra, noise = c(25312, 1.66), range = c(200, 2800),
    intercept = TRUE, weights = TRUE,
    details = NULL
  )
  expect_output(show(calib3), "Calibration curve")
  expect_length(stats::weights(calib3[["model"]]), 7)
  expect_equal(dim(calib3[["data"]]), c(7, 5))
  expect_s3_class(plot(calib3), "ggplot")

  expect_error(fit(spectra, noise = c(25312), range = c(200, 2800)),
               "`noise` must be a numeric vector of length two, not 1.")
  expect_error(fit(spectra, noise = c(25312, 1.66), range = c(200)),
               "`range` must be a numeric vector of length two, not 1.")

  spectra[["BRIQUE"]]@dose_rate <- numeric(0)
  expect_error(fit(spectra, noise = c(25312, 1.66), range = c(200, 2800)),
               "1 spectrum do not have a dose rate")
})
test_that("Estimate dose rate", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir, skip = TRUE)
  bdf_dir <- system.file("extdata/crp2a/background", package = "gamma")
  bdf <- read(bdf_dir, skip = TRUE)

  setDoseRate(spectra) <- clermont[, c("gamma", "gamma_error")]
  noise <- integrateSignal(bdf, range = c(200, 2800))
  calib1 <- fit(
    spectra, noise = noise, range = c(200, 2800),
    intercept = FALSE, weights = FALSE,
    details = NULL
  )
  calib2 <- fit(
    spectra, noise = bdf, range = c(200, 2800),
    intercept = FALSE, weights = FALSE,
    details = NULL
  )
  expect_equal(calib1@model, calib2@model)

  dose_rate <- predict(calib1, spectra, simplify = TRUE)
  expect_type(dose_rate, "double")
  expect_equal(dim(dose_rate), c(7, 2))

  expect_identical(predict(calib1, spectra), predict(calib1))
  expect_error(predict(calib1, 1:3), "`spectra` must be a")
  expect_type(predict(calib1, spectra[[1]], simplify = TRUE), "double")
})
