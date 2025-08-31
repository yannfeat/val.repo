context("Testing tolProb function works as advertised")

test_that("tolProb has correct length", {
  expect_length(tolProb(10, 2), 1)
})

test_that("tolProb errors correctly", {
  expect_error(tolProb(n=3, k="a"))
  expect_error(tolProb(n=3, k=4))
  expect_error(tolProb(n=3, k=c(2, 3)))
  expect_error(tolProb(n=c(3, 4), 2))
})

test_that("tolProb returns the same value", {
  expect_equal(trunc(tolProb(n=52, k=5, alpha=0.125)*100)/100, 0.64)
  expect_equal(trunc(tolProb(n=52, k=2, alpha=0.05)*100)/100, 0.48)
  expect_equal(trunc(tolProb(n=52, k=0, alpha=0.05)*100)/100, 0.93)
  expect_equal(trunc(tolProb(n=32, k=0, alpha=0.05)*100)/100, 0.80) # assay bridging study
  expect_equal(trunc(tolProb(n=32, k=0, alpha=0.034)*100)/100, 0.66) # assay bridging study
})
