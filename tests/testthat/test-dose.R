context("Dose rate")

test_that("Build calibration curve", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir, skip = TRUE)

  setDoseRate(spectra) <- list(
    BRIQUE = c(1986, 36),
    C341 = c(850, 21),
    C347 = c(1424, 24),
    GOU = c(1575, 17),
    LMP = c(642, 18),
    MAZ = c(1141, 12),
    PEP = c(2538, 112)
  )

  calib <- fit(
    spectra, noise = c(25312, 1.66),
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
