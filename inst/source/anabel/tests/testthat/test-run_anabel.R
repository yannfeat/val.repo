###### check anabel ######
# sanity checks include:
# no input
# missing time point
# invalid method name
# all model corrections are set to TRUE


test_that("run_anabel", {
  outdir <- "./tests_rslts"
  run_anabel() %>%
    expect_error()
  run_anabel(SCA_dataset, tass = NA, tdiss = 1, tstart = 1, tend = 1, conc = 1) %>%
    expect_error()

  run_anabel(SCA_dataset, tstart = 1, tass = 10, tdiss = 30, tend = 100, conc = NA) %>%
    expect_error()
  run_anabel(SCA_dataset, tstart = 1, tass = 10, tdiss = 30, tend = 100, conc = "") %>%
    expect_error()
  run_anabel(SCA_dataset,
    tstart = 1, tass = 10, tdiss = 30, tend = 100,
    conc = 10 * 10^-9, drift = TRUE, decay = TRUE
  ) %>%
    expect_error()
  run_anabel(SCA_dataset,
    tstart = 1, tass = 10, tdiss = 30, tend = 100,
    conc = 10 * 10^-9, generate_output = "all"
  ) %>%
    expect_error()
  run_anabel(SCA_dataset,
    tstart = 1, tass = 10, tdiss = 30, tend = 100,
    conc = 10 * 10^-9, generate_output = "all", outdir = " "
  ) %>%
    expect_error()
  run_anabel(SCA_dataset,
    tstart = 1, tass = 10, tdiss = 30, tend = 100,
    conc = 10 * 10^-9, method = "SCX"
  ) %>%
    expect_error()
  run_anabel(SCA_dataset,
    tstart = 1, tass = 10, tdiss = 30, tend = 100,
    conc = 10 * 10^-9, generate_output = "okay", outdir = outdir
  ) %>%
    expect_error()
  run_anabel(
    input = SCK_dataset, tstart = 1, tass = 10, tdiss = 50, tend = 100,
    conc = 10 * 10^-9, method = "SCK"
  ) %>%
    expect_error()
  run_anabel(
    input = MCK_dataset, tass = 10, tdiss = 50,
    conc = 10 * 10^-9, method = "MCK"
  ) %>%
    expect_error()

  run_anabel(SCA_dataset,
    tstart = 1, tass = 10, tdiss = 30, tend = 100, conc = 10^-9,
    outdir = outdir, generate_Tables = TRUE, save_tables_as = "json"
  ) %>%
    expect_error()

  run_anabel(SCA_dataset,
    tstart = 1, tass = 10, tdiss = 30, tend = 100, conc = 10^-9,
    outdir = outdir, generate_Plots = "customized"
  ) %>%
    expect_error()

  # one spot will pass the second will fail
  run_anabel(SCA_dataset[, 1:3],
    drift = TRUE,
    tstart = 1, tass = 10, tdiss = 30, tend = 100, conc = 10^-9,
    outdir = outdir, generate_output = "customized", debug_mode = TRUE
  ) %>%
    is.list() %>%
    expect_true()

  run_anabel(SCA_dataset, tstart = 1, tass = 10, tdiss = 30, tend = 100, conc = 10 * 10^-9, quiet = FALSE) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages()
})
