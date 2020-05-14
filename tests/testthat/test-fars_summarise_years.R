context("fars_summarize_years")
require(dplyr)

test_that("fars_summarize_years returns correct number of rows", {
  expect_equal(nrow(fars_summarize_years(2013)), 12)
})
