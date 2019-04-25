context("Dose rate")

test_that("Build calibration curve", {
  spc_dir <- system.file("extdata/crp2a/", package = "gamma")
  spectra <- read(spc_dir, skip = TRUE)
  spectra <- spectra[c("BRIQUE", "C341", "C347", "GOU", "LMP", "MAZ", "PEP")]

  dose <- list(
    reference = c("BRIQUE", "C341", "C347", "GOU", "LMP", "MAZ", "PEP"),
    dose_value = c(1986, 850, 1424, 1575, 642, 1141, 2538),
    dose_error = c(36, 21, 24, 17, 18, 12, 112)
  )

  calib <- fit(
    spectra, dose = as(dose, "DoseRate"), noise = c(25312, 1.66),
    range = c(200, 2800), intercept = TRUE, weights = FALSE,
    details = NULL
  )
  expect_output(show(calib), "Calibration curve")

  expect_is(calib[["model"]], "lm")
  expect_length(stats::coef(calib[["model"]]), 2)
  expect_equal(dim(as(calib[["data"]], "data.frame")), c(7, 5))
  expect_is(plot(calib), "ggplot")

  dose_rate <- predict(calib, spectra)
  expect_is(as(dose_rate, "data.frame"), "data.frame")
  expect_equal(dim(as(dose_rate, "data.frame")), c(7, 5))
  expect_is(plot(calib, dose_rate), "ggplot")
  expect_error(predict(calib, 1:3), "must be a")
  expect_identical(predict(calib, spectra), predict(calib))

  expect_error(fit(spectra, dose = as(dose, "DoseRate"), noise = c(25312)),
               "must be a numeric vector of length two")
})
