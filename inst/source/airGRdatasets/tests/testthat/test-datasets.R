context("dataset format")

# --------

list_ds <- try(data(package = "airGRdatasets"), silent = TRUE)
list_ds <- list_ds$results[, "Item"]

# --------

test_that("Element names", {
  for (i_list_ds in list_ds) {
    i_ds <- get(i_list_ds)
    expect_equal(object = names(i_ds),
                 expected = c("Meta", "TS", "Hypso"))
    expect_equal(object = names(i_ds$Meta),
                 expected = c("Code", "Name", "Coor", "Area"))
    expect_equal(object = names(i_ds$TS),
                 expected = c("Date", "Ptot", "Temp", "Evap", "Qls", "Qmmd"))
  }
})

# --------

test_that("Missing data", {
  for (i_list_ds in list_ds) {
    i_ds <- get(i_list_ds)
    i_ds$TS$Qls  <- NULL
    i_ds$TS$Qmmd <- NULL
    expect_false(object = anyNA(i_ds, recursive = TRUE))
  }
})

# --------

rm(list_ds, i_list_ds, i_ds)
