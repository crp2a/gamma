context("Dose rate")
data("clermont")

test_that("Get and set dose rate", {
  spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
  spectra <- read(spc_dir)

  dose_rate1 <- clermont[, c("gamma_dose", "gamma_error")]

  set_dose(spectra[["BRIQUE"]]) <- c(1986, 36)
  set_dose(spectra[["C341"]]) <- c(850, 21)
  set_dose(spectra[["C347"]]) <- c(1424, 24)
  set_dose(spectra[["GOU"]]) <- c(1575, 17)
  set_dose(spectra[["LMP"]]) <- c(642, 18)
  set_dose(spectra[["MAZ"]]) <- c(1141, 12)
  set_dose(spectra[["PEP"]]) <- c(2538, 112)

  expect_output(show(spectra[["BRIQUE"]]), "date: 2009-08-28 11:43:20")

  expect_invisible(set_dose(spectra) <- dose_rate1)
})
test_that("Build calibration curve", {
  spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
  spectra <- read(spc_dir)
  spectra <- slice_signal(spectra)

  set_dose(spectra) <- clermont[, c("gamma_dose", "gamma_error")]

  # Fit with intercept
  calib1 <- fit_dose(
    spectra,
    Ni_noise = c(22.61, 0.05), Ni_range = c(300, 2800),
    NiEi_noise = c(25312, 1.66), NiEi_range = c(165, 2800),
    details = NULL
  )
  expect_output(show(calib1), "Calibration curve")
  expect_length(stats::coef(calib1@Ni@model), 2)
  expect_equal(dim(calib1[["data"]]), c(7, 8))
  expect_s3_class(plot(calib1), "ggplot")

  expect_error(fit_dose(spectra,
                        Ni_noise = c(25312), Ni_range = c(300, 2800),
                        NiEi_noise = c(25312, 1.66), NiEi_range = c(165, 2800)),
               "must be numeric vectors of length 2")

  spectra[["BRIQUE"]]@dose_rate <- numeric(2)
  expect_warning(fit_dose(spectra,
                          Ni_noise = c(22.61, 0.05), Ni_range = c(300, 2800),
                          NiEi_noise = c(25312, 1.66), NiEi_range = c(165, 2800)),
                 "1 spectrum have a dose rate of 0")
})
test_that("Estimate dose rate", {
  spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
  spectra <- read(spc_dir)
  spectra <- slice_signal(spectra)
  bdf_dir <- system.file("extdata/BDX100/background", package = "gamma")
  bdf <- read(bdf_dir)
  bdf <- slice_signal(bdf)

  set_dose(spectra) <- clermont[, c("gamma_dose", "gamma_error")]
  # noise <- integrate_signal(bdf, range = c(200, 2800), threshold = "NiEi")
  calib1 <- fit_dose(
    spectra,
    Ni_noise = c(22.61, 0.05), Ni_range = c(300, 2800),
    NiEi_noise = c(25312, 1.66), NiEi_range = c(165, 2800),
    details = NULL
  )

  dose_rate <- suppressWarnings(predict_dose(calib1, spectra, simplify = TRUE))
  expect_type(dose_rate, "list")
  expect_equal(dim(dose_rate), c(7, 6))

  expect_type(predict_dose(calib1, spectra[[1]], simplify = FALSE), "list")
})
