context("Detect peaks")

# Make a fake spectrum with no baseline
cts <- dnorm(1:1024, mean = 86, sd = 5) +
  dnorm(1:1024, mean = 493, sd = 7) +
  dnorm(1:1024, mean = 876, sd = 10)
# Add some noise
set.seed(12345)
spc <- new("GammaSpectrum",
           chanel = 1:1024,
           counts = cts * 10^5 + sample(1:10, 1024, TRUE))

test_that("Find peaks", {
  peaks <- findPeaks(spc, SNR = 3, span = 50)
  expect_length(peaks@peaks, 12)

  peaks <- findPeaks(spc, SNR = 3, span = NULL)
  expect_output(show(peaks), "3 peaks were detected")

  expect_equal(nrow(peaks@peaks), 3)
  expect_equivalent(peaks@peaks[, "chanel"], c(86, 493, 876))
  expect_s3_class(plot(peaks), "ggplot")
  expect_s3_class(as(peaks, "data.frame"), "data.frame")
})
test_that("Fit peaks (autodetect)", {
  peaks <- findPeaks(spc, SNR = 3, span = 50)

  fit <- fitPeaks(peaks)
  expect_silent(fit)

  expect_equal(nrow(fit@coefficients), 3)
  expect_equivalent(fit@coefficients[, "mean"], c(86, 493, 876),
                    tolerance = 0.00001)
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(as(fit, "data.frame"), "data.frame")
})
test_that("Fit peaks", {
  fit <- fitPeaks(spc, peaks = c(86, 493, 876), bounds = c(0.1, 0.1, 0.1))
  expect_output(show(fit), "peaks were estimated")

  expect_equal(nrow(fit@coefficients), 3)
  expect_equivalent(fit@coefficients[, "mean"], c(86, 493, 876),
                    tolerance = 0.00001)
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(as(fit, "data.frame"), "data.frame")

  expect_error(fitPeaks(spc, peaks = c(86, 493, 876), scale = "chanel",
                        bounds = c(0.1, 0.1, 0.1, 0.1)))
  expect_error(fitPeaks(spc, peaks = c(86, 493, 876), scale = "chanel",
               bounds = 2))
})
test_that("Fit NLS", {
  df <- methods::as(spc, "data.frame")
  fit <- fitNLS(df, peaks = c(chanel = 86, counts = 5))
  expect_s3_class(fit, "nls")

  expect_null(fitNLS(df, peaks = c(chanel = 250, counts = 0)))
  expect_error(fitNLS(df, peaks = c(250, 0)),
               "`peaks` is a numeric vector, but does not have components")
  expect_error(fitNLS(df, peaks = c("a", "b")),
               "`peaks` must be a numeric vector.")
  expect_error(fitNLS(df, peaks = c(chanel = 86, counts = 5000),
                      bounds = c(1, 2)),
               "`bounds` must be of length one or 3, not 2.")
})
test_that("FWHM", {
  df <- methods::as(spc, "data.frame")[, c("chanel", "counts")]
  fwhm <- FWHM(df, center = 86)
  expect_true(fwhm == 10)
  expect_equal(fwhm / (2 * sqrt(2 * log(2))), 5, tolerance = 0.2)
  expect_identical(FWHM(df, center = 86), FWHM(as.list(df), center = 86))
  expect_error(FWHM(x = 1:5, y = 1:10, center = 86),
               "`x` and `y` lengths differ.")
})
