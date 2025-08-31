context("Testing agrInt2alpha function works as advertised")

test_that("agrInt2alpha has correct length", {
  expect_length(agrInt2alpha(clin.limit=c(-15, 15), n=52, sigmae=46.09245), 1)
})

test_that("tolProb errors correctly", {
  expect_error(agrInt2alpha(clin.limit=3, n=4))
  expect_error(agrInt2alpha(clin.limit=c(-3, 3), n="a"))
  expect_error(agrInt2alpha(clin.limit=c(-3, 3), n=c(32, 34)))
})

test_that("agrInt2alpha returns the same value", {
  expect_equal(round(agrInt2alpha(clin.limit=c(-15, 15), n=52, sigmae=46.09245), 3), 0.124)
})
