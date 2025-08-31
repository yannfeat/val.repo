test_that("Udplot works without the estimated normal pdf curve", {
  set.seed(2025)
  X<-matrix(rnorm(100, mean = 2 , sd = 5))
  g1=udplot(X,npdf=FALSE,xlab="x",lcol="black",rcol="grey60",pdfcol="red")
  expect_true(is.ggplot(g1))
})

test_that("Udplot works with the estimated normal pdf curve", {
  set.seed(2025)
  X<-matrix(rnorm(100, mean = 2 , sd = 5))
  g2=udplot(X,npdf=TRUE,xlab="x",lcol="black",rcol="grey60",pdfcol="red")
  expect_true(is.ggplot(g2))
})

