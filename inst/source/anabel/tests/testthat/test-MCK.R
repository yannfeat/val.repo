###### test mck method ######
# checks involve:
# only input data, no conc ot ts
# data and ts but no conc
# data, wrong ts and no conc
# nr of conc is icorrect
# internal arc-cross
# wrong sample name

test_that("run_mck", {
  outdir <- "./tests_rslts"
  data("MCK_dataset")
  run_mck() %>% expect_error()
  run_mck(input = MCK_dataset) %>% expect_error()
  run_mck(input = MCK_dataset, tstart = 1, tass = 0, tdiss = 90, tend = 101) %>% expect_error()
  run_mck(input = MCK_dataset, tstart = 1, tass = c(1:6), tdiss = 90, tend = 101) %>% expect_error()
  run_mck(input = MCK_dataset, tstart = 1, tass = 0, tdiss = 90, tend = 101, conc = c(1:5)) %>% expect_error()
  run_mck(input = MCK_dataset, tstart = 1, tass = 50, tdiss = 30, tend = 101, conc = c(1:6)) %>% expect_error()
  run_mck(
    input = MCK_dataset, tstart = 1, tass = 10, tdiss = 40, tend = 150, conc = c(1:6),
    samples_names_file = data.frame(
      ID = c("A", "B", "C", "D", "E", "F", "X"),
      Name = c("A_nM", "B_nM", "C_nM", "D_nM", "E_nM", "F_nM", "X_nM")
    )
  ) %>% expect_error()

  run_mck(
    input = MCK_dataset, tass = 10, tdiss = 40, tend = 101, tstart = 1,
    conc = c(5, 5 * 3, "one", 5 * 3^3, 5 * 3^4, 5 * 3^5)
  ) %>%
    expect_error() %>%
    suppressWarnings()

  # working example
  # resetting generate_output happens in run_anabel, to produce output it must be set explicitly here
  # bad fitting
  run_mck(
    input = MCK_dataset, tass = 30, tdiss = 170, quiet = FALSE,
    conc = c(5.00e-08, 1.67e-08, 5.56e-09, 1.85e-09, 6.17e-10),
    samples_names_file = data.frame(
      ID = c(
        "Conc..50.nM.", "Conc..16.7.nM.", "Conc..5.56.nM.",
        "Conc..1.85.nM.", "Conc..6.17e.1.nM."
      ),
      Name = c("A_50nM", "A_16.7nM", "A_5.56nM", "A_1.85nM", "A_0.617nM")
    )
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages()


  # successful run
  run_mck(
    input = MCK_dataset, tass = 50, tdiss = 150, quiet = FALSE,
    conc = c(5.00e-08, 1.67e-08, 5.56e-09, 1.85e-09, 6.17e-10),
    samples_names_file = data.frame(
      ID = c(
        "Conc..50.nM.", "Conc..16.7.nM.", "Conc..5.56.nM.",
        "Conc..1.85.nM.", "Conc..6.17e.1.nM."
      ),
      Name = c("A_50nM", "A_16.7nM", "A_5.56nM", "A_1.85nM", "A_0.617nM")
    )
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages()

  # to fail the fit and generate empty kinetics table
  run_mck(
    input = MCK_dataset, tass = 50, tdiss = 250, conc = c(-1:3), quiet = FALSE,
    generate_output = TRUE, generate_Plots = TRUE, outdir = outdir, generate_Tables = TRUE
  ) %>%
    is.list() %>%
    expect_true() %>%
    suppressMessages()
})
