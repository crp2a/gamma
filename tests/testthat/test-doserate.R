data("clermont")

test_that("Build a calibration curve", {
  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spc <- read(spc_dir)
  bkg_dir <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
  bkg <- read(bkg_dir)
  bkg_numeric <- c(0,0)

  doses <- as.matrix(clermont[, c("gamma_dose", "gamma_error")])

  calib <- dose_fit(spc, bkg, doses,  range_Ni = c(300, 2800),
                    range_NiEi = c(165, 2800))

  ## regression test dose fit
  expect_equal(object = sum(calib@Ni@slope), 29, tolerance = 0.1)
  expect_equal(object = sum(calib@NiEi@slope), 0.030, tolerance = 0.1)

  calib_zero <- dose_fit(spc, bkg_numeric, doses,  range_Ni = c(300, 2800),
                    range_NiEi = c(165, 2800))

  ## check the results for background zero; the background
  ## should be numeric of length 2 each and sum to zero
  expect_equal(
    object = sum(c(calib_zero@Ni@background, calib_zero@NiEi@background)),
    expected = 0)

  ## check for controlled stop
  expect_error(
    dose_fit(spc, bkg, doses, range_Ni = c(300), range_NiEi = c(165, 2800)),
    "must be of length 2"
  )

})
test_that("Estimate dose rates", {
  testthat::skip_on_cran()
  set.seed(1234)
  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spc <- read(spc_dir)
  bkg_dir <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
  bkg <- read(bkg_dir)
  bkg_numeric <- c(0,0)

  doses <- clermont[, c("gamma_dose", "gamma_error")]

  calib <- dose_fit(spc, bkg, doses,  range_Ni = c(300, 2800),
                    range_NiEi = c(165, 2800))
  calib_zeroBG <- dose_fit(spc, background = bkg_numeric, doses,  range_Ni = c(300, 2800),
                    range_NiEi = c(165, 2800))

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

  ## regression test
  expect_equal(sum(dose_rate2[,-1]), expected = 70105, tolerance = 1)
  expect_equal(sum(dose_rate2_MC[,-1]), expected = 70138, tolerance = 0.01)


  })
test_that("Get residuals", {
  data("BDX_LaBr_1")

  Ni <- get_residuals(BDX_LaBr_1[["Ni"]])
  expect_equal(dim(Ni), c(7, 4))
  expect_equal(colnames(Ni), c("names", "fitted", "residuals", "standardized"))

  NiEi <- get_residuals(BDX_LaBr_1[["NiEi"]])
  expect_equal(dim(NiEi), c(7, 4))
})
