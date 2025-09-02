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



############## check input functions ################

# @importFrom openxlsx read.xlsx
# @importFrom utils read.csv

check_input_ext <- function(file) {
  # check file's validity
  # print(file)
  if (is.na(file)) stop("Invalid input! File must be a csv or an xlsx.")

  ext <- tools::file_ext(file)
  if (ext != "xlsx" & ext != "csv") {
    stop("Invalid input! File must be a csv or an xlsx.")
  }
  # read binding curve file
  if (ext == "xlsx") df <- openxlsx::read.xlsx(file)
  if (ext == "csv") df <- utils::read.csv(file)

  return(df)
}

check_input_file <- function(file) {
  # check file extension
  if (is.data.frame(file)) {
    df <- file
  } else {
    df <- check_input_ext(file)
  }

  # check file format
  msg <- check_input_file_content(df)
  if (msg != "") stop(msg)

  return(df)
}

# Applies sanity checks to the binding curve file
# description checks if the binding curve data are valid
# param df A data frame
# return The list of \code{df} and text message in case of error

check_input_file_content <- function(df) {
  if (is.null(df)) {
    msg <- "Empty file. Excution will be stopped!"
  } else if (length(grep("time", colnames(df), ignore.case = TRUE)) == 0) {
    msg <- "No Time column found. Excution will be stopped!"
  } else if (ncol(df) == 1) {
    msg <- "Missing data. Excution will be stopped!"
  } else if (nrow(df) < 1) {
    msg <- "No data found on worksheet. Excution will be stopped!"
  } else if (nrow(df) < 10) {
    msg <- "Very few data point. Excution will be stopped!"
  } else {
    msg <- ""
  }
  return(msg)
}

## check timestamps  ##
# sanity checks include:
# all available
# all numeric
# tstart < tass < tdiss < tend (SCA & MCK)
# tstart < tass_1 < tdiss_1 < ... < tass_n < tdiss_n < tend (SCK)

#   arcs within single step do not overlap (calls check_step_time)
#   arcs between multiple steps do not overlap
#   tstart and tend are within exp time
#
# expects:
#   vector of the experiment's time-point
#   a vector or a single value for each (tstart, tass, tdiss, tend)
#   number of steps

check_input_time <- function(response_time, tstart, tass, tdiss, tend, steps) {
  steps_ticks <- data.frame()
  response_time <- as.numeric(response_time)
  if (any(is.na(response_time))) {
    return("Invalid response time: missing time points are not allowed!")
  }
  if (length(unique(response_time)) != length(response_time)) {
    return("Invalid response time: duplicated time points are not allowed!")
  }
  if (any(sort(response_time) != response_time)) {
    return("Invalid response time order: sensogram data must be sorted by time!")
  }

  # general check for each function
  msg <- ""
  if (is.na(tstart) | tstart < min(response_time)) {
    tstart <- min(response_time)
    msg <- "\ntstart was reset! "
  }
  if (is.na(tend) | tend > max(response_time)) {
    tend <- max(response_time)
    msg <- paste0(msg, "\ntend was reset!")
  }

  tstart <- as.numeric(tstart)
  tend <- as.numeric(tend)
  tass <- unlist(tass) %>% as.numeric()
  tdiss <- unlist(tdiss) %>% as.numeric()

  if (any(
    is.na(tstart), is.na(tass),
    is.na(tdiss), is.na(tend)
  )) {
    return("Invalid time points: only numeric values are accepted!")
  }

  if (steps == 1) {
    bc_time <- c(tstart, tass, tdiss, tend)
    if (any(sort(bc_time) != bc_time)) {
      return(paste0(
        "Invalid time points! The following must hold: tstart < tass < tdiss < tend!", msg,
        "\nProvided time after check: tstart ", tstart, "; tass ", tass, ", tdiss ", tdiss, ", tend ", tend
      ))
    }
    steps_ticks <- data.frame(Step = 1, tstart = tstart, tass = tass, tdiss = tdiss, tend = tend, Status = TRUE, Description = gsub("\\\n", "", msg))
  } else {
    timestamps <- data.frame(tass = tass, tdiss = tdiss) # the length is checked in the main function
    j <- 2

    for (i in 1:steps) {
      bc_time <- c(tstart, timestamps$tass[i], timestamps$tdiss[i], tend)
      # print(bc_time)

      if (i == 1) {
        x <- (timestamps$tdiss[i] < timestamps$tass[j])
        j <- j + 1
      } else if (i == steps) {
        x <- timestamps$tass[i] > timestamps$tdiss[i - 1]
      } else {
        x <- (timestamps$tdiss[i] < timestamps$tass[j]) & (timestamps$tass[i] > timestamps$tdiss[i - 1]) # to ensure both steps are marked invalid
        j <- j + 1
      }

      if (any(sort(bc_time) != bc_time)) {
        steps_ticks <- rbind(steps_ticks, data.frame(
          Step = i, Status = FALSE,
          tstart = tstart, tass = timestamps$tass[i], tdiss = timestamps$tdiss[i], tend = tend,
          Description = "Invalid time points! The following must hold: tstart < tass < tdiss < tend!"
        ))
      } else if (!x) {
        steps_ticks <- rbind(steps_ticks, data.frame(
          Step = i, Status = FALSE,
          tstart = tstart, tass = timestamps$tass[i], tdiss = timestamps$tdiss[i], tend = tend,
          Description = "Invalid time points! Time of titration should be sequential!"
        ))
      } else {
        steps_ticks <- rbind(steps_ticks, data.frame(
          Step = i, Status = TRUE,
          tstart = tstart, tass = timestamps$tass[i], tdiss = timestamps$tdiss[i], tend = tend,
          Description = gsub("\\\n", "", msg)
        ))
      }
    }
  }
  return(steps_ticks)
}

# Checking provided sample name
# param bc_df: The binding curve data frame
# param names_file: Data frame containing samples names
# returns data frame after renaming its columns or error message
# in case of invalid input

map_names <- function(bc_df, names_file) {
  # if not a data.frame read the file according to the available allowed extensions
  if (is.data.frame(names_file)) {
    names_df <- names_file
  } else {
    names_df <- check_input_ext(names_file)
  }

  if (is.data.frame(names_df)) {
    x <- bc_df$Time
    bc_df$Time <- NULL

    if (!"Name" %in% colnames(names_df)) {
      return(msg = "Sample-names file must have column 'Name'.")
    }
    if (!"ID" %in% colnames(names_df)) {
      return(msg = "Sample-names file must have column 'ID'.")
    }
    if (nrow(names_df) != ncol(bc_df)) {
      return(msg = "Sample-names file mismatches experimental data.")
    }
    if (!all(colnames(bc_df) %in% names_df$ID)) {
      return(msg = "Experimental data mismatch the column 'ID' in names sample-names file.")
    }
    names_df <- names_df[match(colnames(bc_df), names_df$ID), ]
    colnames(bc_df) <- names_df$Name
    bc_df <- cbind(x, bc_df)
    colnames(bc_df)[1] <- "Time"
    return(bc_df)
  } else {
    return(names_df)
  }
}

############## data manipulation functions ################

# Apply basic manipulation to a data frame
#
# description takes in a df and based on the input, i.e. code, the data frame
# will be melted, its colnames renamed, etc.
#
# param df The data frame to be transformed
# param code Which action to take, which could be one of the following:
#   rename_time_col: find the column with word time in it, rename it to Time,
#   melt_df: melt the data frame by Time,
#   rename_colnames_roi: if mapping table between samples names and spot
#   number is provided, the column names will be then changed to it.
#
# return data frame after transformation
# @importFrom reshape2 melt

manipulate_df <- function(df, code) {
  switch(code,
    rename_time_col = {
      t <- grep("time", colnames(df), ignore.case = TRUE)
      colnames(df)[t] <- "Time"
      df
    },
    melt_df = reshape2::melt(df, id.vars = "Time"),
    rename_colnames_roi = {
      names(df) <- paste0(0:(ncol(df) - 1), "_", names(df))
      df
    }
  )
}


# print_params takes in the user's input and returns one string to be
# shown to the user

print_params <- function(ParamsList) {
  str <- ""

  str <- paste("Method = ", ParamsList$method, "\n")
  str <- paste0(str, "Input data = ", ifelse(is.data.frame(ParamsList$input), "data.frame", ParamsList$input), "\n")
  str <- paste0(str, "Samples names-file = ", ifelse(is.data.frame(ParamsList$samples_names_file), "data.frame", ParamsList$samples_names_file), "\n")
  str <- paste0(str, "Start time (tstart) = ", ParamsList$tstart, "\nEnd time (tend) = ", ParamsList$tend, "\n")
  str <- paste0(str, "Association starting time (tass) = ", ParamsList$tass, "\n")
  str <- paste0(str, "Dissociation starting time (tdiss) = ", ParamsList$tdiss, "\n")
  str <- paste0(str, "Sensogram drift correction (drift) = ", ParamsList$drift, "\n")
  str <- paste0(str, "Surface decay correction (decay) = ", ParamsList$decay, "\n")
  str <- paste0(str, "Analyte concentration in molar (conc) = ", ParamsList$conc, "\n")
  str <- paste0(str, "generate_output = ", ParamsList$generate_output, "\ngenerate_Plots = ", ParamsList$generate_Plots, "\n")
  str <- paste0(str, "generate_Tables = ", ParamsList$generate_Tables, "\nsave_tables_as = ", ParamsList$save_tables_as, "\n")
  str <- paste0(str, "generate_Report = ", ParamsList$generate_Report, "\noutdir = ", ParamsList$outdir)

  return(str)
}


########### Output functions ###########

# Saves the results on the server
# param \code{DFs_list} List of tables (fit_raw, kinetics)
# param Path File path
#
# return NULL
#' @importFrom utils write.csv write.table
#' @importFrom openxlsx write.xlsx


save_tables <- function(Dfs_list, Path, save_tables_as = "xlsx") {
  if (nrow(Dfs_list$kinetics) > 0) {
    switch(tolower(save_tables_as),
      xlsx = {
        write.xlsx(Dfs_list$kinetics, paste0(Path, "/kinetics.xlsx"))
      },
      csv = {
        write.csv(Dfs_list$kinetics, file = paste0(Path, "/kinetics.csv"), row.names = FALSE)
      },
      txt = {
        write.table(Dfs_list$kinetics, paste0(Path, "/kinetics.txt"), sep = "\t")
      },
      rds = {
        saveRDS(Dfs_list$kinetics, paste0(Path, "/kinetics.rds"))
      }
    )
  }

  if (nrow(Dfs_list$fit_data) > 0) {
    switch(tolower(save_tables_as),
      xlsx = {
        write.xlsx(Dfs_list$fit_data, paste0(Path, "/fit_data.xlsx"))
      },
      csv = {
        write.csv(Dfs_list$fit_data, file = paste0(Path, "/fit_data.csv"), row.names = FALSE)
      },
      txt = {
        write.table(Dfs_list$fit_data, paste0(Path, "/fit_data.txt"), sep = "\t")
      },
      rds = {
        saveRDS(Dfs_list$fit_data, paste0(Path, "/fit_data.rds"))
      }
    )
  }
  return(NULL)
}
