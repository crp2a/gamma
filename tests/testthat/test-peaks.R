context("Detect peaks")

# Make a fake spectrum with no baseline
cts <- dnorm(1:1024, mean = 86, sd = 5) +
  dnorm(1:1024, mean = 493, sd = 7) +
  dnorm(1:1024, mean = 876, sd = 10)
# Add some noise
set.seed(12345)
spc <- .GammaSpectrum(chanel = 1:1024,
                      count = cts * 10^5 + sample(1:10, 1024, TRUE))

test_that("Find peaks", {
  peaks <- find_peaks(spc, SNR = 3, span = 50)
  expect_length(peaks@chanel, 3)

  peaks <- find_peaks(spc, SNR = 3, span = NULL)
  expect_output(show(peaks), "3 peaks were detected")

  expect_equal(peaks@chanel, c(86, 493, 876))
  expect_s3_class(plot(spc, peaks), "ggplot")
  expect_s3_class(as(peaks, "data.frame"), "data.frame")
})
test_that("FWHM", {
  df <- methods::as(spc, "data.frame")[, c("chanel", "count")]
  fwhm <- FWHM(df, center = 86)
  expect_true(fwhm == 10)
  expect_equal(fwhm / (2 * sqrt(2 * log(2))), 5, tolerance = 0.2)
  expect_identical(FWHM(df, center = 86), FWHM(as.list(df), center = 86))
  expect_error(FWHM(x = 1:5, y = 1:10, center = 86),
               "`x` and `y` lengths differ.")
})
