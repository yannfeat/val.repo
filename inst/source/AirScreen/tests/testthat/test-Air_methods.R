
library(testthat)
library(AirScreen)

test_that("rnormCS returns an n x p numeric matrix", {
  n <- 10
  p <- 5
  rho <- 0.3

  X <- rnormCS(n, p, rho)

  expect_true(is.matrix(X), info = "output should be a matrix")
  expect_equal(dim(X), c(n, p), info = "matrix dimensions should be n x p")
  expect_true(is.numeric(X), info = "matrix entries should be numeric")
})

test_that("rnormAR1 returns an n x p numeric matrix", {
  n <- 10
  p <- 5
  rho <- 0.3

  X <- rnormAR1(n, p, rho)

  expect_true(is.matrix(X), info = "output should be a matrix")
  expect_equal(dim(X), c(n, p), info = "matrix dimensions should be n x p")
  expect_true(is.numeric(X), info = "matrix entries should be numeric")
})

test_that("Air() returns a well-formed AirResult object", {
  set.seed(42)
  X <- matrix(rnorm(80), nrow = 10, ncol = 8)
  y <- rnorm(10)

  res <- Air(X, y,
             m                   = 3,
             screening_threshold = 5,
             penalty_type        = "both",
             penalty             = c(1, 10))

  # check S3 class
  expect_s3_class(res, "AirResult")

  # it should carry back the original dimensions and attr
  expect_equal(attr(res, "screening_threshold"), 5)

  # order should be a matrix with as many rows as features and
  # one column per penalty value
  expect_true(is.matrix(res$order_adaptive))
  expect_equal(ncol(res$order_fixed), 2)
  expect_equal(nrow(res$order_adaptive), ncol(X))
})
