context("Plot spectrum")

test_that("plot a GammaSpectrum object", {
  spc_file_cnf <- system.file("extdata/test_CNF.cnf", package = "gamma")
  spectrum_cnf <- read(spc_file_cnf)

  gg_spectrum_cnf <- plot(spectrum_cnf)
  vdiffr::expect_doppelganger("spectrum_CNF", gg_spectrum_cnf)

  spc_file_tka <- system.file("extdata/test_TKA.tka", package = "gamma")
  spectrum_tka <- read(spc_file_tka)

  gg_spectrum_tka <- plot(spectrum_tka)
  vdiffr::expect_doppelganger("spectrum_TKA", gg_spectrum_tka)
})
test_that("plot GammaSpectrum and PeakPosition objects", {
  spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
  spectrum <- read(spc_file)
  peaks <- find_peaks(spectrum)

  gg_peaks <- plot(spectrum, peaks)
  vdiffr::expect_doppelganger("spectrum_peaks", gg_peaks)
})
test_that("plot a GammaSpectra", {
  spc_dir <- system.file("extdata/", package = "gamma")
  spectra <- read(spc_dir)

  for (i in c(TRUE, FALSE)) {
    gg_spectra <- plot(spectra, facet = i)
    vdiffr::expect_doppelganger(paste0("spectra_facet-", i), gg_spectra)
  }

  spc_file <- system.file("extdata/test_TKA.tka", package = "gamma")
  spectrum <- methods::as(read(spc_file), "GammaSpectra")
  expect_warning(plot(spectrum, xaxis = "energy"),
                 "The energy scale is missing for one or more spectra")
})
