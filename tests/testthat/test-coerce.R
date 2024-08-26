# GammaSpectrum ================================================================
test_that("GammaSpectrum to matrix or data.frame", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- read(spc_file)

  mtx <- as.matrix(spectrum)
  expect_type(mtx, "double")
  expect_equal(dim(mtx), c(1024, 4))

  df <- as.data.frame(spectrum)
  expect_type(df, "list")
  expect_equal(dim(df), c(1024, 4))
})
# GammaSpectra =================================================================
test_that("GammaSpectra from list", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- list(read(spc_file))

  spectra <- methods::as(spectrum, "GammaSpectra")
  expect_length(spectra, 1)
  expect_equal(names(spectra), "LaBr")
})
# PeakPosition =================================================================
test_that("PeakPosition to and from", {
  LaBr_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  LaBr_spc <- read(LaBr_file)
  pks <- peaks_find(LaBr_spc)

  ## try coercion to data.frame and matrix
  expect_type(as.matrix(pks), "double")
  expect_type(as.data.frame(pks), "list")
  t <- expect_type(as.list(pks), "list")
  expect_length(t, 4)

  ## try the back conversion from list
  pks <- list(channel = c(10,20,30), energy = c(100,200,300))
  expect_s4_class(as(pks, "PeakPosition"), "PeakPosition")

  ## crash function for list to PeakPosition
  pks <- list(chan = c(10,20,30), en = c(100,200,300))
  expect_error(as(pks, "PeakPosition"), "Coercion failed because of list-element name mismatch")

  ## it should work, however, with a longer list
  pks <- list(channel = c(10,20,30), energy = c(100,200,300), test = "s")
  expect_s4_class(as(pks, "PeakPosition"), "PeakPosition")
})
