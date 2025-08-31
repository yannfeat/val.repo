context("Testing ai function works as advertised")

a <- c(1, 2, 3, 4, 7)
b <- c(1, 3, 2, 5, 3)
test_that("ai returns correct classes",{
          expect_is(ai(a, b), "ai")
})


test_that("ai errors correctly", {
    expect_error(ai(3, "a"))
    expect_error(ai(3, 2))
    expect_error(ai(3, 2), lambda=-3)
    expect_error(ai(3, 2), alpha=4)
})


ans=ai(x=IPIA$Tomography, y=IPIA$Urography)
test_that("ai returns the same value", {
  expect_equal(round(ans$indexEst["Lin.CCC", "Est"], 3), 0.810)
  expect_equal(round(ans$indexEst["Lin.CCC", "95LL"], 3), 0.693)
  expect_equal(round(ans$indexEst["Lin.CCC", "95UL"], 3), 0.885)
  expect_equal(round(ans$intervalEst["Bland-Altman.LOA", "95LL"], 3), -17.752)
  expect_equal(round(ans$intervalEst["Bland-Altman.LOA", "95UL"], 3), 21.098)
  expect_equal(round(ans$intervalEst["Liao.AI", "95LL"], 3), -19.275)
  expect_equal(round(ans$intervalEst["Liao.AI", "95UL"], 3),  19.275)
  expect_equal(round(ans$tolProb, 3), 0.485)
})

ans=ai(x=IPIA$Urography, y=IPIA$Tomography, clin.limit=c(-15, 15))
test_that("ai returns the same value with clin.limit", {
  expect_equal(round(ans$alpha.cl, 3), 0.124)
  expect_equal(ans$k.cl, 5)
  expect_equal(round(ans$tolProb.cl, 2), 0.64)
})
