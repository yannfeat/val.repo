test_that("multiplication works", {
  model_fit(SCK_dataset[1:5, 1:2], 2, "SCK", 20, 50, 3.9E-9, "names") %>% expect_error()
})
