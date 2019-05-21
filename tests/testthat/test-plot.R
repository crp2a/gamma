context("Plot spectrum")

test_that("Plot GammaSpectrum", {
  spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
  spectrum <- read(spc_file)

  expect_s3_class(plot(spectrum), "ggplot")
})

test_that("Plot GammaSpectra", {
  spc_dir <- system.file("extdata/", package = "gamma")
  spectra <- read(spc_dir)

  for (i in c(TRUE, FALSE)) {
    expect_s3_class(plot(spectra, facet = i), "ggplot")
  }

  spc_file <- system.file("extdata/test_TKA.tka", package = "gamma")
  spectrum <- methods::as(read(spc_file), "GammaSpectra")
  expect_s3_class(plot(spectrum), "ggplot")
  expect_warning(plot(spectrum, xaxis = "energy"),
                 "The energy scale is missing for one or more spectra")
})
