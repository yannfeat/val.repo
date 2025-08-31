context("Testing aiAdj function works as advertised")

ans <- ai(x=IPIA$Tomography, y=IPIA$Urography)

#' aiAdj(object=ans, x=c(1, 2))
test_that("aiAdj has correct length", {
  expect_equal(NROW(aiAdj(object=ans, x=1)), 1)
  expect_equal(NROW(aiAdj(object=ans, x=c(1, 2))), 2)
})

test_that("aiAdj errors correctly", {
  expect_error(aiAdj(object=1:3, x=1))
  expect_error(aiAdj(object=ans, x="A"))
})


