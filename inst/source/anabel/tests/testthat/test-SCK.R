###### check sck ######
# sanity checks involve:
# missing input
# missing conc
# missing/incorrect time
# corssing arcs
#

test_that("run_sck", {
  outdir <- "./tests_rslts"
  run_sck() %>% expect_error()
  run_sck(tstart = 1, tass = 10, tdiss = 50, tend = 100) %>% expect_error()
  run_sck(input = SCK_dataset, tstart = 1, tass = 10, tdiss = 50, tend = 100, conc = 10 * 10^-9) %>% expect_error()
  run_sck(input = SCK_dataset, tstart = c(1:3), tass = c(1:3), tdiss = c(1:3), tend = c(1:3), conc = c(1:4) * 10^-9) %>% expect_error()
  run_sck(input = SCK_dataset, samples_names_file = NA, tass = c(1:3), tdiss = c(1:3), conc = c(1:3) * 10^-9) %>% expect_error()
  run_sck(input = SCK_dataset, samples_names_file = data.frame(), tstart = 1, tass = c(1:3), tdiss = c(1:3), conc = c(1:3) * 10^-9) %>% expect_error()
  run_sck(
    input = SCK_dataset, tstart = 1, tass = c(200, 250), tdiss = c(500, 1500), tend = 1000, conc = c(10, 20) * 10^-9
  ) %>%
    expect_error() %>%
    suppressMessages()

  run_sck(input = SCK_dataset, tass = c(900, 1250), tdiss = c(200, 1500), conc = c(10, 20) * 10^-9) %>%
    expect_error() %>%
    suppressMessages()
  # incorrect ac
  run_sck(
    input = SCK_dataset, tstart = 1, tass = c(190, 1200), tdiss = c(410, 1430), tend = 2000,
    generate_output = TRUE, conc = c(15, "")
  ) %>%
    expect_error() %>%
    suppressMessages()

  # incorrect time point
  run_sck(
    input = SCK_dataset, tstart = 1, tass = c(NA, 1200), tdiss = c(410, 1430), tend = 2000, generate_output = T,
    conc = c(15, 30) * 10^-9, decay = TRUE
  ) %>% expect_error()

  # one is failing
  run_sck(
    input = SCK_dataset,
    tstart = 1, tass = c(50, 260, 500), tdiss = c(140, 380, 820), tend = 900,
    conc = c(5, 30, 40) * 10^-9, decay = TRUE
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages() %>%
    suppressWarnings()

  # bad fitting
  run_sck(
    input = SCK_dataset, tstart = 1, tass = c(50, 220, 390, 560, 730),
    tdiss = c(150, 320, 490, 660, 830), quiet = FALSE,
    conc = c(6.17e-10, 1.85e-09, 5.56e-09, 1.67e-08, 5.00e-08), save_tables_as = "rds",
    samples_names_file = data.frame(
      ID = c("Sample.A"),
      Name = paste0(LETTERS[1], "M")
    )
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages() %>%
    suppressWarnings()

  # failed fitting
  run_sck(
    input = SCK_dataset, tstart = 1, tass = c(30, 220, 390, 560, 730),
    tdiss = c(170, 320, 490, 660, 830), quiet = FALSE,
    generate_output = TRUE, outdir = outdir, generate_Plots = TRUE, generate_Tables = TRUE, generate_Report = FALSE,
    conc = c(6.17e-10, 1.85e-09, 5.56e-09, 1.67e-08, 50), save_tables_as = "csv"
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages() %>%
    suppressWarnings()

  # successful fitting
  run_sck(
    input = SCK_dataset, tstart = 1, tass = c(50, 220, 390, 560, 730),
    tdiss = c(150, 320, 490, 660, 830), quiet = FALSE,
    generate_output = TRUE, outdir = outdir, generate_Plots = TRUE, generate_Tables = TRUE, generate_Report = FALSE,
    conc = c(6.17e-10, 1.85e-09, 5.56e-09, 1.67e-08, 5.00e-08), save_tables_as = "csv"
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages() %>%
    suppressWarnings()
})
