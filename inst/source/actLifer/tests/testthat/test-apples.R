library(dplyr)
library(tibble)

lifetable_ref <- tribble(
  ~age_group, ~deaths, ~population, ~CentralDeathRate, ~ConditionalProbDeath, ~ConditionalProbLife, ~NumberToSurvive, ~PropToSurvive, ~PersonYears, ~TotalYears, ~LifeExpectancy,
  "< 1 year", 23161, 3970145, 0.005833791965, 0.005816824892, 0.9941831751, 100000, 1, 99709.15876, 7588010.714, 75.88010714,
  "1 year", 1568, 3995008, 0.0003924898273, 0.0003924128183, 0.9996075872, 99418.31751, 0.9941831751, 99398.811, 7488301.555, 75.32114547,
  "2 years", 1046, 3992154, 0.0002620139403, 0.0002619796192, 0.9997380204, 99379.30449, 0.9937930449, 99366.28681, 7388902.744, 74.35051777
)

tbl_missing_col <- tribble(
  ~years, ~mortality, ~population,
  "< 1 year", 23161, 3970145,
  "1 year", 1568, 3995008,
  "2 years", 1046, 3992154
)

tbl_char <- tribble(
  ~age_group, ~deaths, ~population,
  "< 1 year", "23161", "3970145",
  "1 year", "1568", "3995008",
  "2 years", "1046", "3992154"
)

mortality_matrix <- matrix(c(1,2,3,4,5,6,7,8,9), ncol = 3, byrow = FALSE,
                 dimnames = list(c("one", "two", "three"),
                   c("age_group","deaths", "population")))

test_that("central_death_rate works - length", {
  mortality3 <- central_death_rate(mortality2, "age_group", "population", "deaths")
  expect_equal(length(mortality3$CentralDeathRate), length(mortality3$age_group))
})

test_that("conditional_death_prob works", {
  mortality_new <- conditional_death_prob(mortality2, "age_group", "population", "deaths")
  expect_equal(mortality_new$ConditionalProbDeath[1], 0.005816824892)
})

test_that("lifetable function works", {
  lifetable <- lifetable(mortality2, "age_group", "population", "deaths")
  expect_equal(head(lifetable, 3), lifetable_ref)
})

test_that("column type warning", {
  expect_warning(out<- lifetable(tbl_char, "age_group", "population", "deaths"))
  expect_equal(sapply(out, class), sapply(lifetable_ref, class))
})

test_that("columns exist error", {
  expect_error(lifetable(tbl_missing_col, "age_group", "population", "deaths"))
})

test_that("input not df or tbl error", {
  expect_error(lifetable(mortality_matrix, "age_group", "population", "deaths"))
})
