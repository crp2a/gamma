context("Baseline")

spc_file <- system.file("extdata/test_CNF.cnf", package = "gamma")
spectrum <- read(spc_file)

spc_dir <- system.file("extdata/crp2a/calibration", package = "gamma")
spectra <- read(spc_dir)

test_that("Estimate baseline from GammaSpectrum", {
  baseline <- estimateBaseline(spectrum)

  expect_equal(baseline[["hash"]], spectrum[["hash"]])
  expect_equal(baseline[["reference"]], spectrum[["reference"]])
  expect_is(baseline[["date"]], "POSIXct")
  expect_equal(baseline[["instrument"]], spectrum[["instrument"]])
  expect_equal(baseline[["file_format"]], spectrum[["file_format"]])
  expect_equal(baseline[["live_time"]], spectrum[["live_time"]])
  expect_equal(baseline[["real_time"]], spectrum[["real_time"]])
  expect_is(baseline[["chanel"]], "integer")
  expect_is(baseline[["energy"]], "numeric")
  expect_is(baseline[["counts"]], "integer")
  expect_is(baseline[["rate"]], "numeric")
  expect_equal(baseline[["calibration"]], spectrum[["calibration"]])

  expect_is(plot(baseline), "ggplot")
  expect_is(plot(spectrum, baseline), "ggplot")
})
test_that("Estimate baseline from GammaSpectra", {
  baseline <- estimateBaseline(spectra)

  expect_s4_class(baseline, "GammaSpectra")
  expect_equal(length(baseline), length(spectra))
  expect_is(plot(baseline), "ggplot")
})
test_that("Remove baseline from GammaSpectrum", {
  baseline <- removeBaseline(spectrum)

  expect_equal(baseline[["hash"]], spectrum[["hash"]])
  expect_equal(baseline[["reference"]], spectrum[["reference"]])
  expect_is(baseline[["date"]], "POSIXct")
  expect_equal(baseline[["instrument"]], spectrum[["instrument"]])
  expect_equal(baseline[["file_format"]], spectrum[["file_format"]])
  expect_equal(baseline[["live_time"]], spectrum[["live_time"]])
  expect_equal(baseline[["real_time"]], spectrum[["real_time"]])
  expect_is(baseline[["chanel"]], "integer")
  expect_is(baseline[["energy"]], "numeric")
  expect_is(baseline[["counts"]], "integer")
  expect_is(baseline[["rate"]], "numeric")
  expect_equal(baseline[["calibration"]], spectrum[["calibration"]])

  expect_is(plot(baseline), "ggplot")
})
test_that("Remove baseline from GammaSpectra", {
  baseline <- removeBaseline(spectra)

  expect_s4_class(baseline, "GammaSpectra")
  expect_equal(length(baseline), length(spectra))
  expect_is(plot(baseline), "ggplot")
})
test_that("SNIP algorithm", {
  k <- 6
  baseline <- estimateBaseline(spectrum, LLS = TRUE, k = k)

  spc1 <- spectrum - baseline
  spc2 <- removeBaseline(spectrum, LLS = TRUE, k = k)

  expect_equal(spc1[["chanel"]], spc2[["chanel"]])
  expect_equal(spc1[["energy"]], spc2[["energy"]])
  expect_equal(spc1[["counts"]], spc2[["counts"]])
  expect_equal(spc1[["rate"]], spc2[["rate"]])

  spc3 <- spc1 + baseline
  expect_equal(spc3[["chanel"]], spectrum[["chanel"]])
  expect_equal(spc3[["energy"]], spectrum[["energy"]])
  expect_equal(spc3[["counts"]], spectrum[["counts"]])
  expect_equal(spc3[["rate"]], spectrum[["rate"]])

  expect_error(SNIP(LETTERS), "A numeric vector is expected.")
})
