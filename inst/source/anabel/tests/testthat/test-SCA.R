###### check sca ######

test_that("run_sca", {
  outdir <- "./tests_rslts"
  sca_sim <- read.csv("./sca_dataset_anabel_self_generated.csv")

  # couple of erroneous runs
  run_sca(input = sca_sim, tstart = 0, tend = 100, tass = 10, tdiss = 5) %>% expect_error()
  run_sca(input = sca_sim, tstart = NA, tend = 100, tass = 10, tdiss = 5, conc = 1) %>% expect_error()
  run_sca(input = sca_sim, tstart = 0, tend = 100, tass = 10, tdiss = "5", conc = 1) %>% expect_error()
  run_sca(input = sca_sim, tstart = 0, tend = 100, tass = 10, tdiss = 5, conc = 10) %>% expect_error()
  run_sca(input = sca_sim, tstart = 0, tend = 100, tass = 10, tdiss = 5, conc = c(1:5)) %>%
    expect_error() %>%
    suppressWarnings()
  run_sca(input = sca_sim, tstart = 0, tend = 100, tass = 5, tdiss = 15, conc = "molar") %>%
    expect_error() %>%
    suppressWarnings()
  run_sca(input = sca_sim, tstart = 0, tend = 100, tass = c(1:5), tdiss = 5, conc = 1) %>%
    expect_error() %>%
    suppressWarnings()

  # the error msg tstart < tass < tdiss < tend because
  # 1) tend violates the given input [specified range 1:10]
  # 2) tend will be reset by anabel
  run_sca(input = sca_sim[1:10, 1:3], tstart = 0, tend = 100, tass = 5, tdiss = 50, conc = 1) %>%
    expect_error() %>%
    suppressWarnings()

  run_sca(
    input = sca_sim[, c("time", "A")], tstart = 0, tend = 1000, tass = 200, tdiss = 500, conc = 10 * 10^-9,
    samples_names_file = data.frame(ID = "B", Name = "Mein_einziges_Spot")
  ) %>%
    expect_error()

  # not all sensograms are fitted
  run_sca(
    input = sca_sim[, c("time", "A", "F")], tstart = 1.2, tend = 200, tass = 10, tdiss = 20, conc = 10 * 10^-9,
    generate_output = TRUE, outdir = outdir, generate_Plots = TRUE,
    quiet = FALSE
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressWarnings() %>%
    suppressMessages()

  run_sca(
    input = sca_sim[, c(1:3)], tstart = 0, tend = 800, tass = 200, tdiss = 500, conc = 10 * 10^-9,
    generate_output = TRUE, generate_Plots = TRUE, generate_Tables = TRUE, generate_Report = FALSE,
    outdir = outdir, quiet = FALSE,
    samples_names_file = data.frame(ID = LETTERS[1:2], Name = paste0("New_", LETTERS[1:2]))
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages()
})
