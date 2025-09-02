######### Fitting quality ##########

test_that("remove_Neg_Kds", {
  remove_Neg_Kds() %>% expect_error()
  remove_Neg_Kds(kntks = NA) %>% expect_error()

  remove_Neg_Kds(kntks = data.frame(KD = -6, kdiss = -1, kass = -5)) %>%
    is.na() %>%
    sum() %>%
    expect_equal(3)
})

test_that("check_fitting_params", {
  check_fitting_params() %>% expect_error()
  check_fitting_params(kntks = NA, to_check = NA, lbs = NA, ubs = NA, isLog = NA) %>% expect_error()

  check_fitting_params(
    kntks = data.frame(one = 1, two = 3),
    to_check = "one", lbs = c("one" = -1),
    ubs = c("one" = 2), isLog = c("one" = F)
  ) %>%
    is.character() %>%
    expect_true()

  check_fitting_params(
    kntks = data.frame(one = NA, two = 3),
    to_check = "one", lbs = c("one" = -1),
    ubs = c("one" = 2), isLog = c("one" = T)
  ) %>%
    is.na() %>%
    expect_true()
})

test_that("save_tables", {
  outdir <- "./tests_rslts"
  save_tables() %>%
    expect_error()

  save_tables(
    list(
      kinetics = data.frame(one = 1:3, two = 1:3),
      fit_data = data.frame(one = 1:3, two = 1:3)
    ),
    Path = outdir, save_tables_as = "xlsx"
  ) %>%
    expect_null()

  save_tables(
    list(
      kinetics = data.frame(one = 1:3, two = 1:3),
      fit_data = data.frame(one = 1:3, two = 1:3)
    ),
    Path = outdir, save_tables_as = "csv"
  ) %>%
    expect_null()

  save_tables(
    list(
      kinetics = data.frame(one = 1:3, two = 1:3),
      fit_data = data.frame(one = 1:3, two = 1:3)
    ),
    Path = outdir, save_tables_as = "rds"
  ) %>%
    expect_null()

  save_tables(
    list(
      kinetics = data.frame(one = 1:3, two = 1:3),
      fit_data = data.frame(one = 1:3, two = 1:3)
    ),
    Path = outdir, save_tables_as = "txt"
  ) %>%
    expect_null()
})
