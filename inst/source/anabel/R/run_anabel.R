# Copyright (C) 2025  Stefan Kraemer
#   
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation as version 3 of the License
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.



#' Analysis for 1:1 Biomolecular Interactions
#' @description Analysis for 1:1 biomolecular interactions, using one of single-curve analysis (SCA), single-cycle kinetics (SCK) or multi-cycle kinetics (MCK)
#' @param input Data.frame, an excel, or a csv file (full path) - required
#' @param samples_names_file An optional data.frame, an excel, or a csv file (full path) containing the samples names.
#' If provided, it must have two columns, Name and ID. ID: names of  columns in the input file; Name: sample's names.
#' @param tstart Numeric value of time's starting point (default: minimum time point in the input)
#' @param tend Numeric value of time's ending point (default: maximum time point in the input)
#' @param tass Numeric value of association time - required
#' @param tdiss Numeric value of dissociation time - required
#' @param drift Boolean value, to apply drift correction (default: FALSE)
#' @param decay Boolean value, to apply surface decay correction (default: FALSE)
#' @param quiet Boolean value, to suppress notifications, messages and warnings (default: TRUE)
#' @param conc Numeric value, the used concentration of the analyte; should be in molar (see \code{\link{convert_toMolar}}) - required
#' @param method a character string indicating which fitting method to be used. One of "SCA", "SCK", or "MCK", case insensitive (default: SCA).
#' @param outdir Path and name of the output directory in which the results will be saved (default: NA)
#' @param generate_output a character string indicating what kind of output will be generated. One of "none", "all", or "customized", case insensitive (default: none).
#' If "all" or "customized" were given, \code{outdir} is required.
#' If "customized" was given, at least one of \code{generate_Plots}, \code{generate_Tables}, or/and \code{generate_Report} must be set to TRUE
#' @param generate_Report Boolean value, should anabel generate a summary report of the experiment? (default: FALSE)
#' @param generate_Plots Boolean value, should anabel generate plots? (default: FALSE).
#' \code{generate_output} must be set to "customized"
#' @param generate_Tables Boolean value, should anabel generate tables? (default: FALSE)
#' @param save_tables_as a character string indicating data format to save the tables with; could be "xlsx", "csv", "txt" or "rds", case insensitive, (default: xlsx)
#' @param debug_mode Boolean value, anabel will return additional fitting details for each curve and the estimated response (default: FALSE)
#'
#' @seealso \code{\link{convert_toMolar}}
#'
#' @return default returned value is a list of two data frames,
#'  the kinetics table and the fit value of each time point (fit_raw).
#'  If \code{dev_mode} was set to TRUE a third data frame will be returned containing the
#'  initial value of the parameters and the fitting function.
#'
#' @export
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_danger
#' @importFrom utils capture.output
#' @importFrom purrr pmap
#' @importFrom dplyr %>%
#'
#' @examples \donttest{
#' # To analyse data using MCK method:
#' run_anabel(
#'   input = MCK_dataset, tstart = 1, tass = 21, tdiss = 140,
#'   conc = c(3.9E-9, 1.6E-8, 6.2E-8, 2.5E-7, 1.0e-6), method = "MCK"
#' )
#' }
#'
#' @references
#' Determination of rate and equilibrium binding constants for macromolecular
#' interactions by surface plasmon resonance.
#' D J O'Shannessy, M Brigham-Burke, K K Soneson, P Hensley, I Brooks
#' Analytical biochemistry *212*, 457-468 (1993)
#'
#' Analyzing a kinetic titration series using affinity biosensors.
#' Robert Karlsson, Phinikoula S Katsamba, Helena Nordin, Ewa Pol, David G Myszka
#' Analytical Biochemistry *349*, 136–147 (2006)
#'
#' Anabel: an online tool for the real-time kinetic analysis of binding events.
#' Stefan D Krämer, Johannes Wöhrle , Christin Rath, Günter Roth
#' Bioinformatics and Biology Insights *13*,  1-10 (2019)


run_anabel <- function(input = NA, samples_names_file = NULL,
                       tstart = NA, tend = NA, tass = NA, tdiss = NA,
                       conc = NA, drift = FALSE, decay = FALSE, quiet = TRUE,
                       method = "SCA", outdir = NA,
                       generate_output = "none",
                       generate_Report = FALSE, generate_Plots = FALSE,
                       generate_Tables = FALSE, save_tables_as = "xlsx",
                       debug_mode = FALSE) {
  # catch the rslts or the error
  rslts <- tryCatch(
    {
      if (!quiet) message("Welcome to Anabel!")

      # + check input
      # general checks

      if (any(is.na(input)) | is.null(input)) stop("Missing input! Sensogram file is required.")
      if (any(is.na(tass), is.na(tdiss))) stop("Missing input! Time should be provided!")
      # stop if all model corrections were chosen
      if (isTRUE(drift) & isTRUE(decay)) stop("Invalid input! One model-correction can be chosen!")
      if (!tolower(generate_output) %in% c("none", "all", "customized")) stop("Invalid input! generate_output can be one of ('none', 'all', 'customized')!")

      if (any(
        isFALSE(is.logical(generate_Plots)), isFALSE(is.logical(generate_Tables)),
        isFALSE(is.logical(generate_Report)), isFALSE(is.logical(quiet)),
        isFALSE(is.logical(drift)), isFALSE(is.logical(decay))
      )) {
        stop("Invalid input! Make sure you provide correct input-type to each parameter!")
      }

      # output logic
      generate_output_orig <- generate_output
      generate_output <- ifelse(tolower(generate_output) %in% c("all", "customized"), TRUE, FALSE)

      if (tolower(generate_output_orig) == "all") {
        generate_Plots <- generate_Tables <- generate_Report <- TRUE
      }

      if (tolower(generate_output_orig) == "customized" & all(c(isFALSE(generate_Plots), isFALSE(generate_Tables), isFALSE(generate_Report)))) {
        generate_output_orig <- "none"
        generate_output <- FALSE
      }

      if (generate_output_orig != "all" & any(c(isTRUE(generate_Plots), isTRUE(generate_Tables), isTRUE(generate_Report)))) {
        generate_output <- TRUE
        generate_output_orig <- "customized"
      }

      if (tolower(generate_output_orig) == "none") {
        generate_Plots <- generate_Tables <- generate_Report <- FALSE
      }

      if (isTRUE(generate_output) & is.na(outdir)) stop("Missing input! Output directory is missing!")
      if (isTRUE(generate_output) & grepl("^\\s*$", outdir)) stop("Missing input! Output directory is missing!")
      if (isTRUE(generate_output) & !tolower(save_tables_as) %in% c("csv", "txt", "rds", "xlsx")) stop(paste("Invalid input! the format", save_tables_as, "is not supported!"))

      if (!quiet) message(ifelse(generate_output, "Reading input ... (1/3)", "Reading input ... (1/2)"))

      if (toupper(method) == "SCA") {
        suppressWarnings(
          rslts <- run_sca(
            input = input, samples_names_file = samples_names_file,
            tstart = tstart, tend = tend, tass = tass, tdiss = tdiss,
            conc = unlist(conc), drift = drift, decay = decay, quiet = quiet,
            generate_output = generate_output, generate_output_orig = generate_output_orig,
            generate_Plots = generate_Plots, generate_Tables = generate_Tables,
            save_tables_as = save_tables_as, generate_Report = generate_Report, outdir = outdir
          )
        )
      } else if (toupper(method) == "SCK") {
        suppressWarnings(
          rslts <- run_sck(
            input = input, samples_names_file = samples_names_file,
            tstart = tstart, tend = tend, tass = tass, tdiss = tdiss,
            conc = unlist(conc), drift = drift, decay = decay, quiet = quiet,
            generate_output = generate_output, generate_output_orig = generate_output_orig,
            generate_Plots = generate_Plots, generate_Tables = generate_Tables,
            save_tables_as = save_tables_as, generate_Report = generate_Report, outdir = outdir
          )
        )
      } else if (toupper(method) == "MCK") {
        suppressWarnings(
          rslts <- run_mck(
            input = input, samples_names_file = samples_names_file,
            tstart = tstart, tend = tend, tass = tass, tdiss = tdiss,
            conc = unlist(conc), drift = drift, decay = decay, quiet = quiet,
            generate_output = generate_output, generate_output_orig = generate_output_orig,
            generate_Plots = generate_Plots, generate_Tables = generate_Tables,
            save_tables_as = save_tables_as, outdir = outdir
          )
        )
      } else {
        stop("Invalid method's name! anabel provides SCA, SCK, and MCK! Check Anabel's help or vignette('anabel').")
      }
    },
    error = function(e) {
      stop(paste0("An error occurred:\n", gsub(".+: (Missing|Invalid)", "Invalid", e)))
    }
  )
  if (!quiet) message("Mischief managed!")
  if (!debug_mode) {
    list(
      kinetics = rslts$kinetics,
      fit_data = rslts$fit_data
    ) %>%
      return()
  } else {
    return(rslts)
  }
}
