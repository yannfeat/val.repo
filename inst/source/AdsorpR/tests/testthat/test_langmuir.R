
test_that("Langmuir model returns expected structure", {
  Ce <- c(1, 2, 3, 4, 5)
  Qe <- c(0.8, 1.5, 2.1, 2.6, 2.9)
  result <- langmuir_model(Ce, Qe)
  expect_type(result, "list")
  expect_named(result, c("Langmuir Qmax (mg/g)", "Langmuir KL (L/mg)", "Model Summary", "Plot"))
})
