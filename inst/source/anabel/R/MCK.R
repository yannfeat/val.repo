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


# excutes the data manipulation and checks required for mck methods
#' @importFrom rlang .data

run_mck <- function(input = NA, samples_names_file = NULL, tstart = NA, tend = NA, tass = NA, tdiss = NA,
                    conc = NA, drift = FALSE, decay = FALSE, quiet = TRUE,
                    generate_output = FALSE, generate_output_orig = "none", generate_Plots = FALSE,
                    generate_Tables = FALSE, save_tables_as = "xlsx", outdir = NA) {
  # if (steps <= 1) stop("Invalid input! The number of steps (cycles) seems to be invalid for the chosen method!")
  if (any(
    length(tass) != length(tstart), length(tass) != length(tdiss),
    length(tass) != length(tend), length(tend) != length(tstart),
    length(tass) != 1
  )) {
    stop("Invalid input! MCK expects single value for time. All cycles should start, associate, dissociate and end at the same time!")
  }

  conc <- as.numeric(conc)
  if (any(is.na(conc)) | length(conc) <= 1) stop("Invalid input! MCK expects a valid analyte-concentration for each cycle!")

  steps <- length(conc)

  # prep input for sanity checks (read input + rename "time" column to standard "Time" )
  bindingCurve_df <- check_input_file(input)
  bindingCurve_df <- manipulate_df(bindingCurve_df, "rename_time_col")

  # check sample name
  # read sample name file
  ## + sanity checks

  if (!is.null(samples_names_file)) {
    df <- map_names(names_file = samples_names_file, bc_df = bindingCurve_df)
    if (!is.data.frame(df)) {
      stop(df)
    } else {
      bindingCurve_df <- df
    }
  }

  init_df <- data.frame() # ?
  . <- NULL
  pass <- data.frame()

  if ((ncol(bindingCurve_df) - 1) != steps) stop("Invalid input! The number of analyte-concentrations does not match the input data!")
  time_check <- check_input_time(
    response_time = bindingCurve_df$Time,
    tstart = tstart, tass = tass, tdiss = tdiss, tend = tend, steps = 1
  )

  if (!is.data.frame(time_check)) stop(time_check)

  files_ticks <- data.frame(Name = colnames(bindingCurve_df)[colnames(bindingCurve_df) != "Time"], conc = conc)
  files_ticks <- as.data.frame(lapply(time_check, rep, steps)) %>%
    cbind(files_ticks, .)

  if (any(files_ticks$status == FALSE) | quiet == FALSE) {
    x <- purrr::pmap(
      list(files_ticks$Status, files_ticks$Description, files_ticks$Name, files_ticks$conc), # in case of minimal feedback, add time stamps if full table is wished
      function(s, d, n, a) {
        ifelse(s, cli::cli_alert_success(paste("Cycle", n, ", Analyte:", a, "M")),
          cli::cli_alert_danger(paste("Cycle", n, "--->", d))
        )
      }
    )
  }

  # crop and reshape the bc data
  bindingCurve_df <- manipulate_df(bindingCurve_df, "rename_colnames_roi") %>%
    manipulate_df("rename_time_col") %>%
    dplyr::filter(.data$Time >= files_ticks$tstart[1] & .data$Time <= files_ticks$tend[1]) %>%
    manipulate_df("melt_df")

  # generate param str to be printed and saved into the report
  if (!quiet | generate_output) {
    p_list <- print_params(list(
      input = input, samples_names_file = ifelse(is.null(samples_names_file), "None", samples_names_file),
      tstart = files_ticks$tstart[1], tend = files_ticks$tend[1], tass = files_ticks$tass[1], tdiss = files_ticks$tdiss[1],
      conc = paste(files_ticks$conc, collapse = ", "), drift = drift, decay = decay,
      method = "Multi-Cycle Kinetics (MCK)", generate_output = generate_output_orig,
      generate_Plots = generate_Plots, generate_Tables = generate_Tables,
      save_tables_as = save_tables_as, generate_Report = "No report available for this method", outdir = outdir
    ))
    if (!quiet) message(paste0("---\n", p_list, "\n---"))
  }

  if (!quiet) message(ifelse(generate_output, "Fitting started ... (2/3)", "Fitting started ... (2/2)"))

  colnames(bindingCurve_df) <- gsub("Time", "t", colnames(bindingCurve_df))
  kinetics <- data.frame()
  init_df <- data.frame()

  out_df <- tryCatch(
    {
      fit <- tryCatch(
        {
          fit <- model_fit(
            data = bindingCurve_df,
            n_steps = as.numeric(steps),
            mode = "MCK",
            surface_decay = decay,
            tass = files_ticks$tass,
            tdiss = files_ticks$tdiss,
            conc = conc,
            drift = drift,
            names = factor(unique(bindingCurve_df$variable)),
            Rmax_fitting = "global"
          )
        },
        error = function(e) {
          return(paste0("nlsLM fitting error: ", e))
        }
      )

      if (is.character(fit)) stop(fit)
      kinetics <- fit$val

      # remove invalid/negatives KD, kass & kdiss values
      kinetics <- remove_Neg_Kds(kntks = kinetics)

      # remove boundaries that were initiated with NA value
      fitting_param <- fit$fitting_param %>% dplyr::filter(!is.na(.data$lbs) & !is.na(.data$ubs))

      kinetics <- assess_fittingQuality(
        kntks = kinetics, fitting_param$lbs, fitting_param$ubs, to_check =
          as.character(fitting_param$name), fitting_param$isLog
      )

      # original fitting_param
      tmp_df <- fit$fitting_param[, c("name", "inits")] %>% tidyr::pivot_wider(., names_from = "name", values_from = "inits")
      tmp_df$isLog <- paste(fit$fitting_param$name[fit$fitting_param$isLog], collapse = ",")
      # tmp_df$ff <- fit$ff
      tmp_df$Response <- fit$Response
      tmp_df$Fit <- "Pass"
      init_df <- tmp_df

      # construct fitting array
      fit_raw <- data.frame(
        ID = bindingCurve_df$variable, Time = bindingCurve_df$t,
        Name = gsub("^\\d*\\_", "", bindingCurve_df$variable),
        Response = bindingCurve_df$value, fit = stats::predict(fit$model_fit),
        Drift = drift, Decay = decay,
        Fitting = "Passed"
      )
    },
    error = function(e) {
      if (!grepl("nlsLM", e, fixed = TRUE)) stop(paste0("Fatal error!", e))

      fit <- model_fit(
        data = bindingCurve_df,
        n_steps = as.numeric(steps),
        mode = "MCK",
        surface_decay = decay,
        tass = files_ticks$tass,
        tdiss = files_ticks$tdiss,
        conc = conc,
        drift = drift,
        names = factor(unique(bindingCurve_df$variable)),
        Rmax_fitting = "global",
        inits_only = TRUE
      )

      tmp_df <- fit$fitting_param[, c("name", "inits")] %>% tidyr::pivot_wider(., names_from = "name", values_from = "inits")
      tmp_df$isLog <- paste(fit$fitting_param$name[fit$fitting_param$isLog], collapse = ",")
      # tmp_df$ff <- fit$ff
      tmp_df$Response <- fit$Response
      tmp_df$Fit <- "Fail"
      init_df <- tmp_df

      fit_raw <- data.frame(
        ID = bindingCurve_df$variable, Name = gsub("^\\d*\\_", "", bindingCurve_df$variable),
        Time = bindingCurve_df$t, Response = bindingCurve_df$value, fit = NA,
        Fitting = "Failed"
      )
      return(list(fit_raw = fit_raw, init_df = data.frame(tmp_df)))
    }
  )

  if (is.data.frame(out_df)) {
    fit_raw <- merge(out_df, files_ticks[, c("Name", "conc")], by = "Name", all = T)
  } else if (is.list(out_df)) {
    fit_raw <- merge(out_df$fit_raw, files_ticks[, c("Name", "conc")], by = "Name", all = T)
    init_df <- rbind(init_df, out_df$init_df)
  }

  if (!quiet & nrow(kinetics) > 0 & any(kinetics$FittingQ == "Warning")) message("!!!Warning!!!\nThe fitting produced a warning or was not optiomal, please check the kinetics results!")

  if (generate_output) {
    outdir <- paste0(outdir, "/")
    if (!quiet) message("Generating results (3/3)")
    if (generate_Tables) {
      if (!quiet) message("Saving tables ...")
      save_tables(
        Dfs_list = list(kinetics = kinetics, fit_data = fit_raw),
        Path = outdir, save_tables_as = save_tables_as
      )
    }

    if (generate_Plots) {
      if (!quiet) message("Generating plots ...")
      plot_mck(fit_df = fit_raw, kntks_df = kinetics, outdir = outdir)
    }
  }

  list(
    kinetics = kinetics,
    fit_data = fit_raw,
    init_df = init_df
  ) %>%
    return()
}
