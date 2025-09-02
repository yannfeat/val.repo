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



# excutes the data manipulation and checks required for sck methods
#' @import dplyr
#' @import progress
#' @importFrom rlang .data
run_sca <- function(input = NA, samples_names_file = NULL, tstart = NA, tend = NA, tass = NA, tdiss = NA,
                    conc = NA, drift = FALSE, decay = FALSE, quiet = TRUE,
                    generate_output = FALSE, generate_output_orig = "none", generate_Plots = FALSE,
                    generate_Tables = FALSE, save_tables_as = "xlsx", generate_Report = FALSE, outdir = NA) {
  conc <- as.numeric(conc)

  if (is.na(conc) | length(conc) != 1) stop("Invalid input! The value of analyte-concentration seems to be invalid for the chosen method!")
  if (any(
    length(tass) != length(tstart), length(tass) != length(tdiss),
    length(tass) != length(tend), length(tend) != length(tstart),
    length(conc) != length(tstart)
  )) {
    stop("Invalid input! SCA expects single value for time and analyte-concentration!")
  }

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

  files_ticks <- check_input_time(
    response_time = bindingCurve_df$Time,
    tstart = tstart, tass = tass, tdiss = tdiss, tend = tend, steps = 1
  )

  if (!is.data.frame(files_ticks)) stop(files_ticks)
  if (!quiet) {
    if (files_ticks$Description != "") {
      cli::cli_alert_info(paste("tstart:", files_ticks$tstart, "tass:", files_ticks$tass, "tdiss:", files_ticks$tdiss, "tend:", files_ticks$tend, " --->", files_ticks$Description))
    } else {
      cli::cli_alert_success(paste("tstart:", files_ticks$tstart, "tass:", files_ticks$tass, "tdiss:", files_ticks$tdiss, "tend:", files_ticks$tend))
    }
  }

  if (!quiet | generate_output) {
    # generate param str to be printed and saved into the report
    p_list <- print_params(list(
      input = input, samples_names_file = ifelse(is.null(samples_names_file), "None", samples_names_file),
      tstart = files_ticks$tstart, tend = files_ticks$tend,
      tass = files_ticks$tass, tdiss = files_ticks$tdiss,
      conc = conc, drift = drift, decay = decay,
      method = "Single-curve analysis (SCA)", generate_output = generate_output_orig,
      generate_Plots = generate_Plots, generate_Tables = generate_Tables,
      save_tables_as = save_tables_as, generate_Report = generate_Report, outdir = outdir
    ))
    if (!quiet) message(paste0("---\n", p_list, "\n---"))
  }

  # crop and reshape the bc data
  bindingCurve_df <- manipulate_df(bindingCurve_df, "rename_colnames_roi") %>%
    manipulate_df("rename_time_col") %>%
    dplyr::filter(.data$Time >= files_ticks$tstart & .data$Time <= files_ticks$tend) %>%
    manipulate_df("melt_df")

  # prep data containers for fitting
  ## allocate fit_raw: expected dimension based from the data
  exp_dim <- length(bindingCurve_df$Time)

  fit_data <- data.frame(
    ID = factor(rep(levels(bindingCurve_df$variable), each = length(unique(bindingCurve_df$Time)))),
    Name = rep(NA, exp_dim),
    Time = rep(NA, exp_dim), Response = rep(NA, exp_dim), fit = rep(NA, exp_dim),
    Drift = rep(drift, exp_dim), Decay = rep(decay, exp_dim),
    Conc_M = rep(conc, exp_dim), Fitting = rep(NA, exp_dim)
  )

  init_df <- data.frame()
  . <- NULL
  kinetics <- data.frame()
  pass <- data.frame()
  cols <- c("ID", "Name", "Time", "Response", "fit", "Fitting")
  ids <- levels(bindingCurve_df$variable)
  Counter <- length(ids)
  colnames(bindingCurve_df) <- gsub("Time", "t", colnames(bindingCurve_df))

  if (!quiet) message(ifelse(generate_output, "Fitting started ... (2/3)", "Fitting started ... (2/2)"))
  if (!quiet) pb <- progress::progress_bar$new(format = "Fitting [:bar] :current/:total |:percent", total = Counter, width = 50, clear = FALSE)

  for (index in 1:Counter) {
    temp <- dplyr::filter(bindingCurve_df, bindingCurve_df$variable == ids[index])
    out_df <- tryCatch(
      {
        fit <- tryCatch({
          fit <- model_fit(
            data = temp,
            n_steps = 1,
            mode = "SCA",
            surface_decay = decay,
            drift = drift,
            tass = tass,
            tdiss = tdiss,
            conc = conc,
            names = ids[index],
            Rmax_fitting = "global"
          )
        })

        if (is.character(fit)) stop(fit)

        # remove invalid/negatives KD, kass & kdiss values
        fit$val <- remove_Neg_Kds(kntks = fit$val)
        fitting_param <- fit$fitting_param %>% dplyr::filter(!is.na(.data$lbs) & !is.na(.data$ubs))

        fit$val <- assess_fittingQuality(
          kntks = fit$val, fitting_param$lbs, fitting_param$ubs,
          to_check = as.character(fitting_param$name), fitting_param$isLog
        )

        kinetics <- cbind(ID = ids[index], fit$val) %>%
          rbind(kinetics, .) # if fit fails it will not be rbinded to kinetics

        fit$fitting_param$lbs <- fit$fitting_param$ubs <- fit$fitting_param$isFitted <- NULL
        tmp_df <- fit$fitting_param[, c("name", "inits")] %>%
          tidyr::pivot_wider(., names_from = "name", values_from = "inits")
        tmp_df$ID <- ids[index]
        tmp_df <- tmp_df %>%
          dplyr::relocate("ID")
        tmp_df$isLog <- paste(fit$fitting_param$name[fit$fitting_param$isLog], collapse = ",")
        # tmp_df$ff <- fit$ff
        tmp_df$Response <- fit$Response
        tmp_df$Fit <- "Pass"
        init_df <- rbind(init_df, tmp_df)

        fit_raw <- data.frame(
          ID = temp$variable, Name = gsub("^\\d*\\_", "", temp$variable),
          Time = temp$t, Response = temp$value, fit = stats::predict(fit$model_fit),
          Fitting = "Passed"
        )
      },
      error = function(e) {
        if (!grepl("nlsLM", e, fixed = TRUE)) stop(paste("Fatal error!", e))

        fit <- model_fit(
          data = temp,
          n_steps = 1,
          mode = "SCA",
          surface_decay = decay,
          tass = tass,
          tdiss = tdiss,
          conc = conc,
          drift = drift,
          names = ids[index],
          Rmax_fitting = "global",
          inits_only = TRUE
        )

        tmp_df <- fit$fitting_param[, c("name", "inits")] %>% tidyr::pivot_wider(., names_from = "name", values_from = "inits")
        tmp_df$ID <- ids[index]
        tmp_df <- tmp_df %>% relocate(.data$ID)
        tmp_df$isLog <- paste(fit$fitting_param$name[fit$fitting_param$isLog], collapse = ",")
        # tmp_df$ff <- fit$ff
        tmp_df$Response <- fit$Response
        tmp_df$Fit <- "Fail"

        fit_raw <- data.frame(
          ID = temp$variable, Name = gsub("^\\d*\\_", "", temp$variable),
          Time = temp$t, Response = temp$value, fit = NA,
          Fitting = "Failed"
        )

        return(list(fit_raw = fit_raw, init_df = data.frame(tmp_df)))
      }
    )
    if (is.data.frame(out_df)) {
      pass <- rbind(pass, out_df)
    } else if (is.list(out_df)) {
      pass <- rbind(pass, out_df$fit_raw)
      init_df <- rbind(init_df, out_df$init_df)
    }

    if (index %% 100 == 0) {
      fit_data[which(fit_data$ID %in% pass$ID), cols] <- pass
      pass <- data.frame()
    } else if (index == length(ids)) {
      fit_data[which(fit_data$ID %in% pass$ID), cols] <- pass
      rm(pass)
    }

    # Update progress-bar
    if (!quiet) pb$tick()
  }

  fit_data <- fit_data[order(fit_data$Name, fit_data$Time), ]
  if (!quiet & nrow(kinetics) > 0 & any(kinetics$FittingQ == "Warning")) message("!!!Warning!!!\nThe fitting produced a warning or was not optiomal for at least one sensogram, please check the kinetics results!")

  if (generate_output) {
    outdir <- paste0(outdir, "/")
    if (!quiet) message("Generating results (3/3)")
    if (generate_Tables) {
      if (!quiet) message("Saving tables ...")
      save_tables(
        Dfs_list = list(kinetics = kinetics, fit_data = fit_data),
        Path = outdir, save_tables_as = save_tables_as
      )
    }

    if (generate_Plots) {
      if (!quiet) message("Generating plots ...")
      prePlot(fit_df = fit_data, kntks_df = kinetics, method = "SCA", outdir = outdir, quiet = quiet)
    }

    if (generate_Report) {
      if (!quiet) message("Generating report ...")

      n_samples <- unique(fit_data$Name) %>% length()
      n <- ifelse(n_samples > 5, 5, n_samples) %>% as.numeric()
      n_sample <- sample(unique(fit_data$Name), n, replace = FALSE)
      df <- fit_data %>% dplyr::filter(.data$Name %in% n_sample)
      p <- preview_plot(df = df, tass = files_ticks$tass, tdiss = files_ticks$tdiss)

      ids <- NULL
      if (nrow(kinetics) > 1) {
        ids <- fit_data$ID[!fit_data$ID %in% kinetics$ID] %>%
          as.character() %>%
          unique()
        if (length(ids) == 0) ids <- NULL
      }

      rprt <- system.file("rmd", "SCA_report.Rmd", package = "anabel", mustWork = TRUE)
      rmarkdown::render(rprt,
        output_file = "SCA_Report.html", output_dir = outdir, quiet = TRUE,
        params = list(
          prev_plot = p, kinetics = kinetics, method_params = p_list,
          ids = ids, fit_data = df
        )
      )
    }
  }

  # return list of merged dfs
  list(
    kinetics = kinetics,
    fit_data = fit_data,
    init_df = data.frame(init_df) # ?
  ) %>% return()
}
