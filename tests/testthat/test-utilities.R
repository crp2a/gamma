context("Utilities")

test_that("Find nearest value", {
  closest <- findClosest(seq(1, 10, 0.2), c(2.5, 4, 6, 8))
  expect_true(all(closest == c(8, 16, 26, 36)))

  expect_error(findClosest(LETTERS, c(2, 4, 6, 8)),
               "Numeric vectors are expected.")
  expect_error(findClosest(c(2, 4, 6, 8), LETTERS),
               "Numeric vectors are expected.")
})
test_that("Equality within a vector", {
  expect_true(isEqual(c(1, 1, 1)))
  expect_false(isEqual(c(1, 1, 1.1)))

  expect_error(isEqual(LETTERS), "A numeric vector is expected.")
})
test_that("Positive numbers", {
  expect_true(isPositive(c(1, 1, 1), strict = FALSE, na.rm = TRUE))
  expect_false(isPositive(c(-1, 1, 1), strict = FALSE, na.rm = TRUE))
  expect_true(isPositive(c(0, 1, 1), strict = FALSE, na.rm = TRUE))
  expect_false(isPositive(c(0, 1, 1), strict = TRUE, na.rm = TRUE))
  expect_true(is.na(isPositive(c(NA, 1, 1), strict = FALSE, na.rm = FALSE)))

  expect_error(isPositive(LETTERS), "A numeric vector is expected.")
})
