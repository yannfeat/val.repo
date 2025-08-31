test_that("Adplot works", {
  set.seed(2025)
  X<-matrix(rnorm(100, mean = 2 , sd = 5))
  g=adplot(X, title = "Ad-plot", xlab = "x", lcol = "black", rcol = "grey60")
  expect_true(is.ggplot(g))
})
