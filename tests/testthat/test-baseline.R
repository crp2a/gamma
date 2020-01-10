context("Baseline")

spc_file <- system.file("extdata/test_LaBr.CNF", package = "gamma")
spectrum <- read(spc_file)

spc_dir <- system.file("extdata/BDX100/calibration", package = "gamma")
spectra <- read(spc_dir)

test_that("Estimate baseline from GammaSpectrum", {
  baseline <- estimate_baseline(spectrum)

  expect_equal(baseline[["hash"]], spectrum[["hash"]])
  expect_equal(baseline[["name"]], spectrum[["name"]])
  expect_s3_class(baseline[["date"]], "POSIXct")
  expect_equal(baseline[["instrument"]], spectrum[["instrument"]])
  expect_equal(baseline[["file_format"]], spectrum[["file_format"]])
  expect_equal(baseline[["live_time"]], spectrum[["live_time"]])
  expect_equal(baseline[["real_time"]], spectrum[["real_time"]])
  expect_type(baseline[["chanel"]], "integer")
  expect_type(baseline[["energy"]], "double")
  expect_type(baseline[["count"]], "double")
  expect_type(baseline[["rate"]], "double")
  expect_equal(baseline[["calibration"]], spectrum[["calibration"]])

  expect_s3_class(plot(baseline), "ggplot")
  expect_s3_class(plot(spectrum, baseline), "ggplot")
})
test_that("Estimate baseline from GammaSpectra", {
  baseline <- estimate_baseline(spectra)

  expect_s4_class(baseline, "GammaSpectra")
  expect_equal(length(baseline), length(spectra))
  expect_s3_class(plot(baseline), "ggplot")
})
test_that("Remove baseline from GammaSpectrum", {
  baseline <- remove_baseline(spectrum)

  expect_equal(baseline[["hash"]], spectrum[["hash"]])
  expect_equal(baseline[["name"]], spectrum[["name"]])
  expect_s3_class(baseline[["date"]], "POSIXct")
  expect_equal(baseline[["instrument"]], spectrum[["instrument"]])
  expect_equal(baseline[["file_format"]], spectrum[["file_format"]])
  expect_equal(baseline[["live_time"]], spectrum[["live_time"]])
  expect_equal(baseline[["real_time"]], spectrum[["real_time"]])
  expect_type(baseline[["chanel"]], "integer")
  expect_type(baseline[["energy"]], "double")
  expect_type(baseline[["count"]], "double")
  expect_type(baseline[["rate"]], "double")
  expect_equal(baseline[["calibration"]], spectrum[["calibration"]])

  expect_s3_class(plot(baseline), "ggplot")
})
test_that("Remove baseline from GammaSpectra", {
  baseline <- remove_baseline(spectra)

  expect_s4_class(baseline, "GammaSpectra")
  expect_equal(length(baseline), length(spectra))
  expect_s3_class(plot(baseline), "ggplot")
})
test_that("SNIP algorithm", {
  k <- 6
  baseline <- estimate_baseline(spectrum, LLS = TRUE, k = k)

  spc1 <- spectrum - baseline
  spc2 <- remove_baseline(spectrum, LLS = TRUE, k = k)

  expect_equal(spc1[["chanel"]], spc2[["chanel"]])
  expect_equal(spc1[["energy"]], spc2[["energy"]])
  expect_equal(spc1[["count"]], spc2[["count"]])
  expect_equal(spc1[["rate"]], spc2[["rate"]])

  spc3 <- spc1 + baseline
  expect_equal(spc3[["chanel"]], spectrum[["chanel"]])
  expect_equal(spc3[["energy"]], spectrum[["energy"]])
  expect_equal(spc3[["count"]], spectrum[["count"]])
  expect_equal(spc3[["rate"]], spectrum[["rate"]])

  expect_error(SNIP(LETTERS), "A numeric vector is expected.")
})
