context("make_filename")

test_that("make_filename returns the expected value", {
  yr <- "2013"
  out <- "accident_2013.csv.bz2"
  expect_equal(make_filename(yr), out)
})
