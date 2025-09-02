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



# The function is dedicated to initiate model's parameters
#' @importFrom rlang .data
#' @importFrom stats lm
#' @importFrom stats coef

# t_bounds_pos = 10, t_bounds_neg = 50
create_fitting_params <- function(n_steps, mode, dfl, names, tass, tdiss, conc, t_bounds_pos = 50,
                                  t_bounds_neg = 10, kdiss = 10^-2, Rmax = 10000,
                                  Rmax_fitting = "global", surface_decay = F, drift = F) {
  # Factors for ubs and lbs
  constant_rates_boundary_factor <- 1000
  decrease_boundary_factor <- NA # decrease only
  decay_boundary_factor <- NA
  rmax_boundary_factor <- NA

  # Predicting the init parameters globally
  # Do linear fits for log10(kobs) and log10(kdiss) to get good estimation for init values
  if (mode == "SCK") {
    i <- n_steps - 1
    dfl_temp <- dfl
    timecor <- (tass[i + 1] - tdiss[i]) * 0.1
  } else { # MCK and SCA
    i <- ceiling(n_steps / 2)
    dfl_temp <- dfl %>%
      dplyr::filter(.data$variable == names[i])

    # timecor <- (tass - tdiss) * 0.1
    timecor <- ((dfl_temp %>% slice(n()))$t - tdiss) * 0.1
    timecor <- unique(timecor)
  }

  # LINEAR FIT kDISS
  diss_df <- dfl_temp %>%
    filter(t >= tdiss[i] & t <= tdiss[i] + timecor)

  diss_df$value <- diss_df$value + abs(min(diss_df$value)) + 1
  diss_lm <- lm(log(diss_df$value) ~ diss_df$t)
  kdiss <- (abs(coef(diss_lm)[2]))
  kdiss <- as.numeric(kdiss)

  # LINEAR FIT kOBS, CALC INIT FOR kASS
  ass_df <- dfl_temp %>%
    filter(t >= tass[i] & t <= tdiss[i])
  ass_df$value <- ass_df$value + abs(min(ass_df$value)) + 1
  ass_lm <- lm(log(ass_df$value) ~ ass_df$t, ass_df)

  kass <- (abs(coef(ass_lm)[2] - kdiss) / conc[i])
  kass <- as.numeric(kass)

  # calculate kdec via half life
  v_tmin <- dfl_temp$value[which(dfl_temp$t == min(dfl_temp$t))]
  v_tmax <- dfl_temp$value[which(dfl_temp$t == max(dfl_temp$t))]
  v_half <- v_tmin + ((v_tmax - v_tmin) / 2)

  t_half <- dfl_temp$t[which(abs(dfl_temp$value - v_half) == min(abs(dfl_temp$value - v_half)))]
  t_half <- min(t_half)
  kdec <- log(2) / t_half

  # Init for Rmax is supposed to be the max of the binding curve times 2
  if (mode == "SCA") {
    Rmax <- max(dfl$value) - min(dfl$value)
  } else { # MCK and SCK
    Rmax <- abs(max(dfl_temp$value[which(dfl_temp$t >= tdiss[n_steps])]) - min(dfl_temp$value[which(dfl_temp$t >= tdiss[n_steps])]))
  }

  Decrease <- (kass * conc * Rmax) / (kass * conc + kdiss)
  Increase <- rep(Rmax, n_steps)

  new_kdiss <- c()
  kobs <- data.frame()

  p <- data.frame()
  ## Global parameter
  # kass
  p <- rbind(p, data.frame(name = "kass", inits = kass, lbs = kass / constant_rates_boundary_factor, ubs = kass * constant_rates_boundary_factor, isLog = T))
  # kdiss
  p <- rbind(p, data.frame(name = "kdiss", inits = kdiss, lbs = kdiss / constant_rates_boundary_factor, ubs = kdiss * constant_rates_boundary_factor, isLog = T))
  # Rmax
  if (Rmax_fitting == "global") {
    p <- rbind(p, data.frame(name = "Rmax", inits = Rmax, lbs = Rmax / rmax_boundary_factor, ubs = Rmax * rmax_boundary_factor, isLog = F))
  }
  # y_offset
  y_offset <- dfl_temp$value[which(dfl_temp$t == min(dfl_temp$t))]
  p <- rbind(p, data.frame(name = "y_offset", inits = y_offset, lbs = min(dfl$value), ubs = max(dfl$value), isLog = F))
  # Surface Decay
  if (surface_decay == T) {
    # Decay
    Decay <- (max(dfl$value) - min(dfl$value))
    p <- rbind(p, data.frame(name = "Decay", inits = Decay, lbs = Decay / decay_boundary_factor, ubs = Decay * decay_boundary_factor, isLog = F))
    # kdec
    p <- rbind(p, data.frame(name = "kdec", inits = kdec, lbs = kdec / constant_rates_boundary_factor, ubs = kdec * constant_rates_boundary_factor, isLog = T))
  }
  # Linear drift
  if (drift == T) {
    p <- rbind(p, data.frame(name = "drift", inits = 0, lbs = -1000, ubs = 1000, isLog = F)) # 1/1000
  }

  ## Step specific parameter
  for (i in 1:n_steps) {
    Rmax_step <- Rmax #* exp(-kdec * tdiss[i])
    # ta
    p <- rbind(p, data.frame(name = paste0("tass_", i), inits = tass[i], lbs = tass[i] - t_bounds_neg, ubs = tass[i] + t_bounds_pos, isLog = F))
    # tb
    p <- rbind(p, data.frame(name = paste0("tdiss_", i), inits = tdiss[i], lbs = tdiss[i] - t_bounds_neg, ubs = tdiss[i] + t_bounds_pos, isLog = F))
    # Decrease
    p <- rbind(p, data.frame(name = paste0("Decrease_", i), inits = Decrease[i], lbs = Decrease[i] / decrease_boundary_factor, ubs = Decrease[i] * decrease_boundary_factor, isLog = F))
    # Increase
    if (Rmax_fitting != "global") {
      p <- rbind(p, data.frame(name = paste0("Rmax_", i), inits = Increase[i], lbs = Increase[i] / rmax_boundary_factor, ubs = Increase[i] * rmax_boundary_factor, isLog = F))
    }
  }

  # adding strange deprecated columns
  p$isFitted <- T
  p$inits[p$isLog] <- log10(p$inits[p$isLog])
  p$lbs[p$isLog] <- log10(p$lbs[p$isLog])
  p$ubs[p$isLog] <- log10(p$ubs[p$isLog])

  return(p)
}

association <- function(step, conc, Rmax_fitting) {
  C <- conc[step]

  if (Rmax_fitting == "global") {
    Rmax <- "Rmax"
    Increase <- paste0("(10^kass*", C, "*", Rmax, ")/(10^kass*", C, "+10^kdiss)")
  } else {
    Rmax <- paste0("(Rmax", "_", step, ")")
    Increase <- Rmax
  }

  kobs <- paste0("(10^kass * ", C, " + 10^kdiss)")
  m <- paste0("(t > tass", "_", step, ") * (t < tdiss", "_", step, ") * ", Increase, " * (1 - exp(-(", kobs, ") * (t - tass", "_", step, ") * (t > tass", "_", step, ")))")
  last <- paste0("(t >= tdiss", "_", step, ") * ( ", Increase, " * (1 - exp(-(", kobs, ") * (tdiss", "_", step, " - tass", "_", step, ") * (t >= tdiss", "_", step, "))))")
  model <- paste0(m, " + ", last)

  return(model)
}

dissociation <- function(step, n_steps, mode, include_last = TRUE) {
  if (step < n_steps & mode == "SCK") {
    time <- paste0("(t >= tdiss", "_", step, ") * (t < tass", "_", step + 1, ")")
  } else {
    time <- paste0("(t >= tdiss", "_", step, ")")
  }

  Decrease <- paste0("Decrease", "_", step)
  m <- paste0(time, "* ", Decrease, "  * (1 - exp(-(10^(kdiss)) * (t - tdiss", "_", step, ") * ", time, "))")
  last <- paste0("(t >= tass", "_", step + 1, ") * ", Decrease, " * (1 - exp(-(10^(kdiss)) * (tass", "_", step + 1, " - tdiss", "_", step, ") * (t >= tass", "_", step + 1, ")))")

  if (step == n_steps) {
    last <- ""
  }

  if (include_last == TRUE) {
    model <- paste0(m, " - ", last)
  } else {
    model <- m
  }
  return(model)
}

# Fitting function
#' @param data data.frame
#' @param n_steps       Numeric
#' @param mode          String
#' @param tass          Numeric
#' @param tdiss         Numeric
#' @param conc          Vector
#' @param names         Vector
#' @param surface_decay Boolean
#' @param drift         Boolean
#' @param Rmax_fitting  String ["Global" , "Local"]
#' @param inits_plot    Boolean
#'
#' @return
#'  Case one: fitting
#'  list(model_fit = model_fit, val = val, fitting_param = fitting_param, init_plot = p, init_df = init_df, ff = fitting_func)
#'
#' Case two: init parameters only
#' list(fitting_param = fitting_param, init_plot = p, init_df = init_df, ff = fitting_func)
#
#' model_fit: data.frame of fitting's summary; val: estimated kinetics;
#' fitting_param: estimated best fitting parameters; init_plot = p,
#' init_df: data.frame of initial fitting parameters; ff: fitting function all put together

#' @importFrom  stats as.formula
#' @import ggplot2
#' @noRd

model_fit <- function(data, n_steps, mode, tass, tdiss, conc, names,
                      surface_decay = FALSE, drift = FALSE, Rmax_fitting = "global",
                      inits_plot = T, inits_only = F) {
  name_vector <- data$variable

  for (step in 1:n_steps) {
    mck_vector <- paste0("(name_vector == names[", step, "])")
    ma <- association(step = step, conc = conc, Rmax_fitting = Rmax_fitting)

    # Switch to IF (step == n_step){} ELSE {IF model ...}
    if (mode != "MCK") {
      mb <- dissociation(step = step, n_steps = n_steps, mode = mode)
    } else {
      mb <- dissociation(step = step, n_steps = n_steps, mode = mode, include_last = FALSE)
    }

    # If last step was reached don't include
    if (step == n_steps) {
      mb <- dissociation(step = step, n_steps = n_steps, mode = mode, include_last = FALSE)
    }

    if (step == 1) {
      model <- paste0("( ", ma, " - ", mb, ") *", mck_vector)
    } else {
      model <- paste0(model, " + ( ", ma, " - ", mb, ") *", mck_vector)
    }
  }

  # Add global parts to model
  if (surface_decay == TRUE) {
    model <- as.formula(paste0("value ~ 0 + y_offset - (Decay) * (1 - exp(-10^(kdec) * (t))) + ", model))
  } else if (drift == TRUE) {
    model <- as.formula(paste0("value ~ 0 + y_offset + drift * t + ", model))
  } else {
    model <- as.formula(paste0("value ~ 0 + y_offset + ", model))
  }

  fitting_param <- create_fitting_params(
    n_steps = n_steps,
    mode = mode,
    dfl = data,
    names = names,
    tass = tass,
    tdiss = tdiss,
    conc = conc,
    Rmax_fitting = Rmax_fitting,
    surface_decay = surface_decay,
    drift = drift
  )

  fitting_func <- NA

  if (inits_plot == TRUE) {
    model_tmp <- as.character(model)[3]
    fitting_param$name <- as.factor(fitting_param$name)
    for (i in levels(fitting_param$name)) {
      n <- i
      val <- fitting_param$inits[which(fitting_param$name == i)]
      model_tmp <- gsub(n, val, model_tmp)
    }

    fitting_func <- paste("model_function <- function(t) { return(", model_tmp, ")}", sep = "")
    f <- eval(parse(text = paste("model_function <- function(t) { return(", model_tmp, ")}", sep = "")))
    y <- f(data$t)
    Response <- y

    init_df <- data.frame(t = data$t, value = y, variable = data$variable)
    p <- ggplot(data = init_df, aes(x = t, y = .data$value, colour = .data$variable)) +
      geom_line()
  }
  if (inits_only == F) {
    inits <- fitting_param$inits %>% as.list()
    names(inits) <- fitting_param$name
    nlc <- stats::nls.control(maxiter = 1000, tol = 1e-20, warnOnly = TRUE)

    model_fit <- tryCatch(
      {
        model_fit <- minpack.lm::nlsLM(data = data, formula = model, start = inits, control = nlc, lower = fitting_param$lbs, upper = fitting_param$ubs)
      },
      error = function(e) {
        return(paste0("nlsLM fitting error: ", e$message))
      }
    )

    if (is.character(model_fit)) {
      return(model_fit)
    }

    out <- summary(model_fit)
    val <- as.data.frame(t(as.data.frame(out$coefficients)))[1, ]
    val[fitting_param$isLog == TRUE] <- 10^val[fitting_param$isLog == TRUE]

    sd <- as.data.frame(t(as.data.frame(out$coefficients)))[2, ]

    sd[fitting_param$isLog == TRUE] <- log(10) * val[fitting_param$isLog == TRUE] * sd[fitting_param$isLog == TRUE]

    names(sd) <- paste0("std_", names(val))
    row.names(sd) <- NULL
    row.names(val) <- NULL
    val <- cbind(val, sd)
    val$KD <- val$kdiss / val$kass
    val$std_KD <- sqrt(((val$std_kdiss^2) / (val$kdiss^2)) + ((val$std_kass^2) / (val$kass^2))) * abs(val$KD)

    # add_delta_value
    for (i in 1:n_steps) {
      if (n_steps == 1) {
        delta_step <- "delta"
        std_delta_step <- "std_delta"
      } else {
        delta_step <- paste0("delta_", i)
        std_delta_step <- paste0("std_delta_", i)
      }
      if (Rmax_fitting == "global") {
        Rmax_step <- "Rmax"
        std_Rmax_step <- "std_Rmax"
      } else {
        Rmax_step <- paste0("Rmax_", i)
        std_Rmax_step <- paste0("std_Rmax_", i)
      }
      tdiss_step <- paste0("tdiss_", i)
      tass_step <- paste0("tass_", i)
      std_tdiss_step <- paste0("std_tdiss_", i)
      std_tass_step <- paste0("std_tass_", i)

      delta_value <- (val$kass * conc[i] * val[, Rmax_step]) / (val$kass * conc[i] + val$kdiss) * (1 - exp(-(val$kass * conc[i] + val$kdiss) * (val[, tdiss_step] - val[, tass_step])))

      # add_std_delta
      # Calculate relative errors
      relative_error_kass <- val$std_kass / val$kass
      relative_error_Rmax <- val[, std_Rmax_step] / val[, Rmax_step]
      relative_error_kdiss <- abs(val$std_kdiss) / val$kdiss
      relative_error_tdiss <- val[, std_tdiss_step] / val[, tdiss_step]
      relative_error_tass <- val[, std_tass_step] / val[, tass_step]

      # Calculate relative error of term 1
      relative_error_term1 <- sqrt(relative_error_kass^2 + relative_error_Rmax^2)

      # Calculate relative error of term 2
      relative_error_term2 <- sqrt(relative_error_kass^2 + relative_error_kdiss^2)

      # Calculate relative error of entire equation
      relative_error_delta_value <- sqrt((relative_error_term1)^2 + (relative_error_term2)^2 + (abs(relative_error_kass + relative_error_Rmax + (val[, tdiss_step] - val[, tass_step]) * relative_error_kass + val$kdiss * val$std_kdiss / (val$kdiss * val$kass)))^2)

      # Calculate absolute uncertainty in delta_value
      delta_delta_value <- relative_error_delta_value * delta_value

      val[, delta_step] <- delta_value
      val[, std_delta_step] <- delta_delta_value
    }


    x <- colnames(val) %>% sort()
    val <- val[, match(x, colnames(val))]

    return(list(model_fit = model_fit, val = val, fitting_param = fitting_param, init_plot = p, init_df = init_df, Response = paste(Response, collapse = ","), ff = fitting_func))
  } else {
    return(list(fitting_param = fitting_param, init_plot = p, init_df = init_df, Response = paste(Response, collapse = ","), ff = fitting_func))
  }
}
