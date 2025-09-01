context("Test if url is correctly created")

test_that("Add path to base url", {
  result <- add_path("www.test.com/v1", "first")
  expected <- "www.test.com/v1/first"
  expect_equal(result, expected)
})

test_that("Add json extension to url", {
  result <- add_json_extension("www.test.com/first")
  expected <- "www.test.com/first.json"
  expect_equal(result, expected)
})

test_that("Request urls are created properly", {
  result <- create_request_url("www.test.com",
                               c("first", "second"))
  expected <- "www.test.com/first/second.json"
  expect_equal(result, expected)
})

test_that("Request urls are created properly (one element)", {
  result <- create_request_url("www.test.com",
                               "first")
  expected <- "www.test.com/first.json"
  expect_equal(result, expected)
})

test_that("Request urls are created properly without json", {
  result <- create_request_url("www.test.com",
                               c("first", "second"),
                               FALSE)
  expected <- "www.test.com/first/second"
  expect_equal(result, expected)
})
