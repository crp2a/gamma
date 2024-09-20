test_that("Build a calibration curve", {

  ## prepare this test
  data("clermont")
  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spc <- read(spc_dir)
  bkg_dir <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
  bkg <- read(bkg_dir)
  bkg_numeric <- c(0,0)
  doses <- as.matrix(clermont[, c("gamma_dose", "gamma_error")])

  ## check a few stops and warnings
  ## check for expected warning
  expect_warning(
    dose_fit(spc, bkg, doses,  range_Ni = c(300, 2800), range_NiEi = c(165, 2800)),
    regexp = "All spectra without energy calibration. You can proceed but it is not recommended!")

  ## now with suppressed warning to get NA
  t <- expect_s4_class(
    suppressWarnings(dose_fit(spc, bkg, doses,  range_Ni = c(300, 2800), range_NiEi = c(165, 2800))),
    class = "CalibrationCurve")
  expect_type(t@details$energy_calibration, "logical")

  ## trigger crash because we mixed calibrated with non calibrated
  spc_err <- spc
  spc_err$C341 <- energy_calibrate(spc_err$C341, lines = list(channel = c(1,2,3), energy = c(100,200,300)))
  expect_error(
    dose_fit(spc_err, bkg, doses,  range_Ni = c(300, 2800), range_NiEi = c(165, 2800)),
    regexp = "You must not mix spectra with and without energy/channel calibration!")

  ## now run the rest of the test but with calibrated data
  lines <- data.frame(
    channel = c(86, 496, 870),
    energy = c(238, 1461, 2615)
  )
  spc <- energy_calibrate(spc, lines)
  bkg <- energy_calibrate(bkg, lines)

  ## run with energy calibration
  calib <- expect_s4_class(
    object = dose_fit(spc, bkg, doses,  range_Ni = c(300, 2800), range_NiEi = c(165, 2800)),
    class = "CalibrationCurve")

  ## check whether we have a calibration
  expect_s3_class(calib@details$energy_calibration[[1]], class = "lm")

  ## change energy calibration for one
  lines <- data.frame(
    channel = c(95, 496, 870),
    energy = c(238, 1461, 2615)
  )
  spc_changed <- spc
  spc_changed$BRIQUE <- energy_calibrate(spc_changed$BRIQUE, lines)
  calibs <- expect_s4_class(
    object = dose_fit(spc_changed, bkg, doses,  range_Ni = c(300, 2800), range_NiEi = c(165, 2800)),
    class = "CalibrationCurve")

  ## check for length
  expect_length(calibs@details$energy_calibration, n = 7)

  ## regression test dose fit
  expect_equal(object = sum(calib@Ni@slope), 34, tolerance = 0.1)
  expect_equal(object = sum(calib@NiEi@slope), 0.030, tolerance = 0.1)

  calib_zero <- dose_fit(spc, bkg_numeric, doses,  range_Ni = c(300, 2800), range_NiEi = c(165, 2800))

  ## check the results for background zero; the background
  ## should be numeric of length 2 each and sum to zero
  expect_equal(
    object = sum(c(calib_zero@Ni@background, calib_zero@NiEi@background)),
    expected = 0)

  ## check for controlled stop
  expect_error(
    dose_fit(spc, bkg, doses, range_Ni = c(300), range_NiEi = c(165, 2800)),
    regexp = "must be of length 2"
  )

})
test_that("Estimate dose rates", {
  testthat::skip_on_cran()

  data("clermont")
  set.seed(1234)

  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spc <- read(spc_dir)
  bkg_dir <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
  bkg <- read(bkg_dir)
  bkg_numeric <- c(0,0)

  doses <- clermont[, c("gamma_dose", "gamma_error")]

  calib <- suppressWarnings(dose_fit(spc, bkg, doses,  range_Ni = c(300, 2800),
                    range_NiEi = c(165, 2800)))


  calib_zeroBG <- suppressWarnings(dose_fit(spc, background = bkg_numeric, doses,  range_Ni = c(300, 2800),
                    range_NiEi = c(165, 2800)))

  # Missing
  dose_rate1 <- expect_silent(dose_predict(calib))
  dose_rate1_MC <- expect_silent(dose_predict(calib, use_MC = TRUE))
  expect_type(dose_rate1, "list")
  expect_type(dose_rate1_MC, "list")
  expect_equal(dim(dose_rate1), c(7, 11))

  # GammaSpectrum
  dose_rate2 <- expect_silent(dose_predict(calib, spc[[1]]))
  dose_rate2_MC <- expect_silent(dose_predict(calib, spc[[1]], use_MC = TRUE))
  expect_type(dose_rate2, "list")
  expect_equal(dim(dose_rate2), c(1, 11))
  # GammaSpectra
  dose_rate3 <- expect_silent(dose_predict(calib, spc))
  dose_rate3_MC <- expect_silent(dose_predict(calib, spc, use_MC = TRUE))
  expect_type(dose_rate3, "list")
  expect_equal(dim(dose_rate3), c(7, 11))
  # Background is numeric
  dose_rate4 <- expect_silent(dose_predict(calib_zeroBG, spc))
  dose_rate4_MC <- expect_silent(dose_predict(calib_zeroBG, spc, use_MC = TRUE))
  expect_type(dose_rate4, "list")

  ## check water content attribute
  expect_type(dose_predict(calib, water_content = c(0.02,0.0001), use_MC = FALSE), "list")
  water_content <- matrix(c(0.05,0.20,0,0), ncol = 2)
  expect_warning(dose_predict(calib, water_content = water_content, use_MC = FALSE),
                 regexp = "Number of rows in matrix 'water_content' unequal to number of spectra. Values recycled!")
  water_content <- matrix(c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0,0,0,0,0,0,0), ncol = 2)
  expect_type(dose_predict(calib, water_content = water_content, use_MC = FALSE), "list")

  ## regression test
  expect_equal(sum(dose_rate2[,-1]), expected = 70105, tolerance = 1)
  expect_equal(sum(dose_rate2_MC[,-1]), expected = 70138, tolerance = 0.01)

  ## check energy calibration issues
  calib_no_energy <- calib
  calib_no_energy@details$energy_calibration <- list(NA)

  lines <- data.frame(
    channel = c(95, 496, 870),
    energy = c(238, 1461, 2615)
  )
  spc <- energy_calibrate(spc, lines)
  expect_error(dose_predict(calib_no_energy, spc[[1]]),
               regexp = "Your dose-rate calibration does not have an energy calibration, while your spectra have!")


  ## assign different calibrations
  lines <- data.frame(
    channel = c(115, 496, 870),
    energy = c(238, 1461, 2615)
  )
  diff_calib <- spc[[1]]
  diff_calib <- energy_calibrate(diff_calib, lines)
  spc_no_calib <- spc[[1]]
  spc_no_calib@calibration <- NULL
  calib@details$energy_calibration <- list(spc[[1]]@calibration,diff_calib@calibration)

  expect_error(
    dose_predict(calib, spc_no_calib),
    regexp = "No energy calibration found 'spectrum' and 'object' has more than one calibration!")

  ## now try an assignment of the energy calibration from the calibration dataset
  calib@details$energy_calibration[[2]] <- NULL
  expect_message(
    dose_predict(calib, spc_no_calib),
    regexp = "No energy calibration found for 'spectrum', apply calibration from dose-rate calibration model!")

  })
test_that("Get residuals", {
  data("BDX_LaBr_1")

  Ni <- get_residuals(BDX_LaBr_1[["Ni"]])
  expect_equal(dim(Ni), c(7, 4))
  expect_equal(colnames(Ni), c("names", "fitted", "residuals", "standardized"))

  NiEi <- get_residuals(BDX_LaBr_1[["NiEi"]])
  expect_equal(dim(NiEi), c(7, 4))
})
