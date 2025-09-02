######## Unit check ########
# testing whether converting the value into Molar will be valid #

test_that("Unit check", {
  expect_identical(convert_toMolar(1, "Millimolar"), 0.001) # expect_equal has more numeric tolerance
  expect_identical(convert_toMolar(1, "Micromolar"), 1e-6)
  expect_identical(convert_toMolar(1, "MiM"), 1e-6)
  expect_identical(convert_toMolar(1, "Nanomolar"), 1e-9)
  expect_identical(convert_toMolar(1, "nM"), 1e-9)
  expect_identical(convert_toMolar(1, "Picomolar"), 1e-12)
  expect_identical(convert_toMolar(1, "PM"), 1e-12)
  expect_equal(convert_toMolar(1, "milLiMolar"), 0.001)

  convert_toMolar(1, "milLi") %>%
    expect_equal(., 0.001) %>%
    expect_error()
  expect_equal(is.na(convert_toMolar("one", "Picomolar")), TRUE) %>% suppressWarnings()
  expect_equal(is.na(convert_toMolar("", "Nanomolar")), TRUE) # %>% suppressWarnings()
  expect_equal(is.na(convert_toMolar(" ", "Micromolar")), TRUE) # %>% suppressWarnings()
  expect_equal(is.na(convert_toMolar("&", "Millimolar")), TRUE) %>% suppressWarnings()
  expect_equal(convert_toMolar(1, "MM"), 0.001)
})


####### Input file test #########
# testing the validity of the file's format
# no time column provided
# one row, i.e. very few data point
# only time provided
# only header
# complete empty
# null df

test_that("check_input_file", {
  data("SCK_dataset")
  # valid test
  check_input_file(SCK_dataset) %>%
    is.data.frame() %>%
    expect_true()

  # invalid tests
  check_input_file(SCK_dataset[, 2:5]) %>% expect_error()
  check_input_file(SCK_dataset[1, ]) %>% expect_error()
  check_input_file(SCK_dataset[, "Time"]) %>%
    expect_error() %>%
    suppressWarnings()
  check_input_file("test_emptybutHeaderTrue.xlsx") %>% expect_error()
  check_input_file("test_onlyTimeColumn.csv") %>% expect_error()
  check_input_file("test_empty.xlsx") %>%
    expect_error() %>%
    suppressWarnings()
  check_input_file(NULL) %>% expect_error()
})

####### Input time test #########

test_that("check_input_time", {
  data("SCK_dataset")

  # check sca & mck conditions
  check_input_time(c(1:10), tstart = NA, tass = 10, tdiss = 5, tend = NA, 1) %>%
    is.data.frame() %>%
    expect_false()
  check_input_time(c(1:5, 5, 6:9, 9, 9:12), tstart = NA, tass = 3, tdiss = 7, tend = NA, 1) %>%
    is.data.frame() %>%
    expect_false()
  check_input_time(c(1:5, 9, 8, 10:12), tstart = NA, tass = 3, tdiss = 7, tend = NA, 1) %>%
    is.data.frame() %>%
    expect_false()
  check_input_time(c(1:5, 8, "nine", 10:12), tstart = NA, tass = 3, tdiss = 7, tend = NA, 1) %>%
    is.data.frame() %>%
    expect_false() %>%
    suppressWarnings()
  check_input_time(c(1:10), tstart = NA, tass = NA, tdiss = 5, tend = NA, 1) %>%
    is.data.frame() %>%
    expect_false()
  check_input_time(c(1:10), tstart = "NA", tass = NA, tdiss = 5, tend = NA, 1) %>%
    is.data.frame() %>%
    expect_false() %>%
    suppressWarnings()

  # check sck, sck always gives df
  check_input_time(c(1:100), tstart = NA, tass = c(1, NA, 10), tdiss = c(10, 14, 50), tend = NA, 3) %>%
    is.data.frame() %>%
    expect_false()
  check_input_time(c(1:100), tstart = NA, tass = c(1, 5, 10), tdiss = c(10, 14, 50), tend = NA, 3) %>%
    is.data.frame() %>%
    expect_true()
  check_input_time(c(1:100), tstart = NA, tass = NA, tdiss = c(10, 14, 50), tend = NA, 3) %>%
    is.data.frame() %>%
    expect_false()
  check_input_time(c(1:100), tstart = NA, tass = c(1, 60, 60), tdiss = c(40, 70, 80), tend = NA, 3) %>%
    is.data.frame() %>%
    expect_true()
  x <- check_input_time(c(1:100), tstart = NA, tass = c(1, 60, 60), tdiss = c(40, 70, 8000), tend = 1000, 3)
  any(x$Status) %>% expect_true()
  expect_equal(sum(x$Status), 1)

  # working sca
  # check_input_time(c(1:10), tstart = NA, tass = 1, tdiss = 5, tend = NA, 1) %>%
  #   as.data.frame() %>%
  #   expect_true()
  # check_input_time(c(1:10), tstart = NA, tass = 1, tdiss = 5, tend = 100, 1) %>%
  #   as.data.frame() %>%
  #   expect_true()
})


test_that("map_names", {
  x <- check_input_ext(file = "./setup.R") %>%
    expect_error()

  # here the function "check_input_ext" is implicitly also checked
  data("SCK_dataset")
  map_names(bc_df = SCK_dataset, names_file = "./test_empty.xlsx") %>%
    is.null() %>%
    expect_true() %>%
    suppressWarnings()
  map_names(bc_df = SCK_dataset, names_file = "./test_emptybutHeaderTrue.xlsx") %>%
    is.character() %>%
    expect_true()

  # mismatch colnames and ids
  map_names(
    bc_df = data.frame(Time = 1:5, A = 1:5, B = 1:5),
    names_file = data.frame(ID = c(1, 2), Name = c("A", "B"))
  ) %>%
    is.data.frame() %>%
    expect_false()
  map_names(
    bc_df = data.frame(Time = 1:5, A = 1:5, B = 1:5, C = 1:5),
    names_file = data.frame(ID = c(1, 2), Name = c("A", "B"))
  ) %>%
    is.data.frame() %>%
    expect_false()
  # wrong colnames
  map_names(
    bc_df = data.frame(Time = 1:5, A = 1:5, B = 1:5, C = 1:5),
    names_file = data.frame(ID = c(1, 2), nam = c("A", "B"))
  ) %>%
    is.data.frame() %>%
    expect_false()
  map_names(
    bc_df = data.frame(Time = 1:5, A = 1:5, B = 1:5, C = 1:5),
    names_file = data.frame(id = c(1, 2), Name = c("A", "B"))
  ) %>%
    is.data.frame() %>%
    expect_false()
  map_names(
    bc_df = data.frame(Time = 1:5, A = 1:5, B = 1:5, C = 1:5),
    names_file = data.frame(ID = 1:3, Name = c("A", "B", "C"))
  ) %>%
    is.data.frame() %>%
    expect_false()

  # correct run
  map_names(
    bc_df = data.frame(Time = 1:5, A = c(1, 3, 4, 5, 2), B = 1:5, C = 1:5),
    names_file = data.frame(ID = c("B", "A", "C"), Name = c("new_B", "new_A", "new_C"))
  ) %>%
    is.data.frame() %>%
    expect_true()
})
