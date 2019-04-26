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

test_that("Find and fit peaks", {
  peaks <- findPeaks(spc, SNR = 3, span = 50)
  expect_output(show(peaks), "peaks were detected")

  expect_equal(nrow(peaks@peaks), 3)
  expect_equal(peaks@peaks[, "chanel"], c(86, 493, 876),
               check.attributes = FALSE)
  expect_is(plot(peaks), "ggplot")
  expect_is(as(peaks, "data.frame"), "data.frame")

  fit <- fitPeaks(peaks)
  expect_silent(fit)

  expect_equal(nrow(fit@coefficients), 3)
  expect_equal(fit@coefficients[, "mean"], c(86, 493, 876),
               tolerance = 0.00001, check.attributes = FALSE)
  expect_is(plot(fit), "ggplot")
  expect_is(as(fit, "data.frame"), "data.frame")
})

test_that("Fit peaks", {
  fit <- fitPeaks(spc, peaks = c(86, 493, 876), bounds = c(0.1, 0.1, 0.1))
  expect_output(show(fit), "peaks were estimated")

  expect_equal(nrow(fit@coefficients), 3)
  expect_equal(fit@coefficients[, "mean"], c(86, 493, 876),
               tolerance = 0.00001, check.attributes = FALSE)
  expect_is(plot(fit), "ggplot")
  expect_is(as(fit, "data.frame"), "data.frame")

  expect_error(fitPeaks(spc, peaks = c(86, 493, 876), scale = "chanel",
                        bounds = c(0.1, 0.1, 0.1, 0.1)))
  expect_error(fitPeaks(spc, peaks = c(86, 493, 876), scale = "chanel",
               bounds = 2))
})
