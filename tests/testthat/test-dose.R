context("Dose rate")
data("clermont")

test_that("Get and set dose rate", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)

  dose_rate1 <- clermont[, c("gamma", "gamma_error")]

  set_dose(spectra[["BRIQUE"]]) <- c(1986, 36)
  set_dose(spectra[["C341"]]) <- c(850, 21)
  set_dose(spectra[["C347"]]) <- c(1424, 24)
  set_dose(spectra[["GOU"]]) <- c(1575, 17)
  set_dose(spectra[["LMP"]]) <- c(642, 18)
  set_dose(spectra[["MAZ"]]) <- c(1141, 12)
  set_dose(spectra[["PEP"]]) <- c(2538, 112)

  expect_output(show(spectra[["BRIQUE"]]), "Date: 2009-08-28 11:43:20")

  expect_invisible(set_dose(spectra) <- dose_rate1)
})
test_that("Build calibration curve", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)
  spectra <- slice_signal(spectra)

  set_dose(spectra) <- clermont[, c("gamma", "gamma_error")]

  # Fit with intercept
  calib1 <- fit_dose(
    spectra, noise = c(25312, 1.66), range = c(200, 2800),
    intercept = TRUE, weights = FALSE,
    details = NULL
  )
  expect_output(show(calib1), "Calibration curve")
  expect_length(stats::coef(calib1[["model"]]), 2)
  expect_equal(dim(calib1[["data"]]), c(7, 5))
  expect_s3_class(plot(calib1), "ggplot")
  # Fit with no intercept
  calib2 <- fit_dose(
    spectra, noise = c(25312, 1.66), range = c(200, 2800),
    intercept = FALSE, weights = FALSE,
    details = NULL
  )
  expect_output(show(calib2), "Calibration curve")
  expect_length(stats::coef(calib2[["model"]]), 1)
  expect_equal(dim(calib2[["data"]]), c(7, 5))
  expect_s3_class(plot(calib2), "ggplot")
  # Fit with weights
  calib3 <- fit_dose(
    spectra, noise = c(25312, 1.66), range = c(200, 2800),
    intercept = TRUE, weights = TRUE,
    details = NULL
  )
  expect_output(show(calib3), "Calibration curve")
  expect_length(stats::weights(calib3[["model"]]), 7)
  expect_equal(dim(calib3[["data"]]), c(7, 5))
  expect_s3_class(plot(calib3), "ggplot")

  expect_error(fit_dose(spectra, noise = c(25312), range = c(200, 2800)),
               "`noise` must be a numeric vector of length 2, not 1.")
  expect_error(fit_dose(spectra, noise = c(25312, 1.66), range = c(200)),
               "`range` must be a numeric vector of length 2, not 1.")

  spectra[["BRIQUE"]]@dose_rate <- numeric(2)
  expect_warning(fit_dose(spectra, noise = c(25312, 1.66), range = c(200, 2800)),
                 "1 spectrum have a dose rate of 0")
})
test_that("Estimate dose rate", {
  spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
  spectra <- read(spc_dir)
  spectra <- slice_signal(spectra)
  bdf_dir <- system.file("extdata/crp2a/background", package = "gamma")
  bdf <- read(bdf_dir)
  bdf <- slice_signal(bdf)

  set_dose(spectra) <- clermont[, c("gamma", "gamma_error")]
  noise <- integrate_signal(bdf, range = c(200, 2800))
  calib1 <- fit_dose(
    spectra, noise = noise, range = c(200, 2800),
    intercept = FALSE, weights = FALSE,
    details = NULL
  )
  calib2 <- fit_dose(
    spectra, noise = bdf, range = c(200, 2800),
    intercept = FALSE, weights = FALSE,
    details = NULL
  )
  expect_equal(calib1@model, calib2@model)

  dose_rate <- predict_dose(calib1, spectra, simplify = TRUE)
  expect_type(dose_rate, "list")
  expect_equal(dim(dose_rate), c(7, 5))

  expect_identical(predict_dose(calib1, spectra), predict_dose(calib1))
  expect_type(predict_dose(calib1, spectra[[1]], simplify = FALSE), "list")
  expect_error(do_predict_dose(calib1, 1:3), "must be a data frame")
})
