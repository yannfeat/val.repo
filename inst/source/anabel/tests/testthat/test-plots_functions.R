######### Main output plot ##########

test_that("pdf_doc_size", {
  pdf_doc_size(val = NA) %>%
    expect_error()
  pdf_doc_size(val = sample(c(3:5), 1)) %>%
    expect_equal(c(11, 5))
  pdf_doc_size(val = sample(c(6:10), 1)) %>%
    expect_equal(c(10, 5))
  pdf_doc_size(val = sample(c(11:19), 1)) %>%
    expect_equal(c(10, 11))

  pdf_doc_size(val = sample(c(21:49), 1)) %>%
    expect_equal(c(14, 14))

  pdf_doc_size(val = sample(c(50:69), 1)) %>%
    expect_equal(c(14, 21))

  pdf_doc_size(val = sample(c(70:99), 1)) %>%
    expect_equal(c(14, 40))

  pdf_doc_size(val = sample(c(100:199), 1)) %>%
    expect_equal(c(14, 49))
})

test_that("prePlot", {
  outdir <- "./tests_rslts/"

  prePlot() %>%
    expect_error()

  fit_df <- data.frame(
    Id = rep(1:2, each = 3), fit = NA,
    Name = rep(letters[1:2], each = 3),
    Time = rep(1:3, 2), Response = rep(11:13, 2)
  )
  fit_df$ID <- paste0(fit_df$Id, "_", fit_df$Name)
  prePlot(fit_df = fit_df, kntks_df = data.frame(), outdir = outdir) %>%
    expect_true()

  fit_df <- data.frame(
    Id = rep(1:200, each = 3), fit = NA,
    Name = rep(paste0("X", 1:200), each = 3),
    Time = rep(1:3, 200), Response = rep(11:13, 200)
  )
  fit_df$ID <- paste0(fit_df$Id, "_", fit_df$Name)
  prePlot(fit_df = fit_df, kntks_df = data.frame(), outdir = outdir, quiet = FALSE) %>%
    expect_equal(0) %>%
    suppressMessages()
})
