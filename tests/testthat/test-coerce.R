context("Coerce")

test_that("Coerce a list to a GammaSpectra object", {
  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spc1 <- read(spc_dir)

  spc_list <- list(spc1[[1]], spc1[[2]], spc1[[3]], spc1[[4]],
                   spc1[[5]], spc1[[6]], spc1[[7]])

  spc2 <- methods::as(spc_list, "GammaSpectra")
  expect_equal(spc1, spc2)
})
