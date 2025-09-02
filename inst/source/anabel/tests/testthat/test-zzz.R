test_that("test zzzz", {
  check_dependencies() %>%
    is.null() %>%
    expect_true()
  check_dependencies(libs = c("Heatmap", "ggplot")) %>% expect_error()
  .onLoad() %>%
    is.null() %>%
    expect_true()
})
