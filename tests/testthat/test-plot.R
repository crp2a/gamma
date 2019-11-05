context("Plot spectrum")

test_that("plot a GammaSpectrum object", {
  spc_file_cnf <- system.file("extdata/test_CNF.cnf", package = "gamma")
  spectrum_cnf <- read(spc_file_cnf)

  gg_spectrum_chanel <- plot(spectrum_cnf, xaxis= "chanel")
  vdiffr::expect_doppelganger("spectrum_chanel", gg_spectrum_chanel)

  gg_spectrum_energy <- plot(spectrum_cnf, xaxis= "energy")
  vdiffr::expect_doppelganger("spectrum_energy", gg_spectrum_energy)

  gg_spectrum_count <- plot(spectrum_cnf, yaxis= "count")
  vdiffr::expect_doppelganger("spectrum_count", gg_spectrum_count)

  gg_spectrum_rate <- plot(spectrum_cnf, yaxis= "rate")
  vdiffr::expect_doppelganger("spectrum_rate", gg_spectrum_rate)
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
