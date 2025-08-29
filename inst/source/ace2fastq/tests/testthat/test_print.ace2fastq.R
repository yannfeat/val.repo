context("printing")
filename <- system.file("sampledat/1.seq.ace", package = "ace2fastq")
testdir <- tempdir()

test_that("printing for path works", {
  fileout <- ace_to_fastq(filename = filename, target_dir = testdir)
  res <- capture.output(print(fileout))
  
  expect_true(length(res) == 1)
  
})


test_that("printing for list works", {
  
  fileout <- ace_to_fastq(filename = filename, target_dir = "stdout")
  res <- capture.output(print(fileout))
  
  expect_true(length(res) == 3)
  
})

test_that("printing for list works", {
  filename <- system.file("sampledat/3.seq.ace", package = "ace2fastq")
  fileout <- ace_to_fastq(filename = filename, target_dir = "stdout")
  res <- capture.output(print(fileout))
  
  expect_true(length(res) == 5)
  
})