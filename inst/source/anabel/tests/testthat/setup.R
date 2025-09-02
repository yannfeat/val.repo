# Run before any test
dir.create("./tests_rslts")
writeLines(text = "mytests", con = "./tests_rslts/fortests.txt")


# Run after all tests
withr::defer(unlink("./tests_rslts", recursive = TRUE), teardown_env())
