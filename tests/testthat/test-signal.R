context("Signal process")

test_that("Slice chanels", {
  spc_file <- system.file("extdata/test_LaBr.CNF", package = "gamma")
  spc_cnf <- read(spc_file)

  expect_equal(get_chanels(spc_cnf), 1024)
  spc1 <- slice_signal(spc_cnf, 1:10)
  expect_equal(get_chanels(spc1), 10)
  expect_equal(get_hash(spc_cnf), get_hash(spc1))
  spc2 <- slice_signal(spc_cnf, -1:-10)
  expect_equal(get_chanels(spc2), 1014)
  expect_equal(get_hash(spc_cnf), get_hash(spc2))

  expect_error(slice_signal(spc_cnf, 1, -2),
               "strictly positive of negative integers")

  spc_files <- system.file("extdata/", package = "gamma")
  spc_set <- read(spc_files)

  spc3 <- slice_signal(spc_set, 1:10)
  expect_true(all(get_chanels(spc3) == 10))
  expect_equal(get_hash(spc_set), get_hash(spc3))
})
test_that("Stabilize signal", {
  spc_file <- system.file("extdata/test_LaBr.CNF", package = "gamma")
  spc_cnf <- read(spc_file)

  spc1 <- stabilize_signal(spc_cnf, sqrt)
  expect_equal(get_hash(spc_cnf), get_hash(spc1))

  spc_files <- system.file("extdata/", package = "gamma")
  spc_set <- read(spc_files)

  spc2 <- stabilize_signal(spc_set, sqrt)
  expect_equal(get_hash(spc_set), get_hash(spc2))
})
test_that("Smooth signal", {
  smooth_methods <- c("unweighted", "weighted", "savitzky")

  spc_file <- system.file("extdata/test_LaBr.CNF", package = "gamma")
  spc_cnf <- read(spc_file)

  for (i in smooth_methods) {
    spc1 <- smooth_signal(spc_cnf, method = i)
    expect_equal(get_hash(spc_cnf), get_hash(spc1))
  }

  expect_error(smooth_signal(spc_cnf, m = 2), "must be an odd integer")

  spc_files <- system.file("extdata/", package = "gamma")
  spc_set <- read(spc_files)

  for (i in smooth_methods) {
    spc2 <- smooth_signal(spc_set, method = i)
    expect_equal(get_hash(spc_set), get_hash(spc2))
  }
})
