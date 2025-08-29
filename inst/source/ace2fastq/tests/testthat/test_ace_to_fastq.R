context("ACE to FASTQ conversion")
filename <- system.file("sampledat/1.seq.ace", package = "ace2fastq")
testdir <- tempdir()

test_that("conversion works", {
  target_name <- ace_to_fastq(filename = filename, target_dir = testdir)
  file.info(target_name$path)

  expect_true(file.exists(target_name$path))
  expect_true(file.info(target_name$path)$size > 2000)

  id_expected <- "@1.seq CO Contig1 1475 8 156 U"
  txt <- readLines(target_name$path)

  expect_true(id_expected == txt[1])
})

test_that("id parameter works", {
  target_name <- ace_to_fastq(
    filename = filename, target_dir = testdir,
    name2id = FALSE
  )
  id_expected <- "@CO Contig1 1475 8 156 U"
  txt <- readLines(target_name$path)

  expect_true(id_expected == txt[1])
})


test_that("error checks on parameters work", {
  expect_error(
    ace_to_fastq("x-32143")
  )
  expect_error(
    ace_to_fastq("x-32143.ace")
  )
  expect_error(
    ace_to_fastq(filename = filename, target_dir = "x")
  )
  expect_error(
    ace_to_fastq(filename = filename, target_dir = testdir, 
                 name2id = "x")
  )
})


test_that("output to stdout works", {
  txt <- ace_to_fastq(
    filename = filename, target_dir = 'stdout',
    name2id = FALSE
  )
  id_expected <- "@CO Contig1 1475 8 156 U"
  
  expect_true(id_expected == txt[[1]][1])
})
