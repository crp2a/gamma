context("Plot spectrum")

test_that("Plot GammaSpectrum", {
  spc_file <- system.file("extdata/test.cnf", package = "gamma")
  spectrum <- read(spc_file)

  expect_is(plot(spectrum), "ggplot")
})

test_that("Plot GammaSpectra", {
  spc_dir <- system.file("extdata/crp2a/", package = "gamma")
  spectra <- read(spc_dir)

  for (i in c(TRUE, FALSE)) {
    expect_is(plot(spectra, facet = i), "ggplot")
  }
})
