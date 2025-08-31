#' @title Temkin Isotherm Linear Analysis
#' @name fit_temkinLM
#'
#' @description Performs linear modeling for the Temkin adsorption isotherm.
#'
#' @param Ce numeric vector for equilibrium concentration
#' @param Qe numeric vector for adsorbed amount
#' @param Temp numeric value of temperature in Kelvin
#' @param verbose logical; if TRUE (default), prints summary and messages
#'
#' @import Metrics
#' @import stats
#' @import ggplot2
#' @import boot
#'
#' @importFrom Metrics rmse mae mse rae
#' @importFrom stats lm AIC BIC resid fitted
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon annotate labs theme_minimal theme
#' @importFrom boot boot
#'
#' @return A list containing the results of the  linear Temkin model fitting, including:
#' \itemize{
#'   \item \strong{Parameter estimates} for the Temkin model (aT and bT).
#'   \item \strong{Fit statistics} such as Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), and R-squared.
#'   \item \strong{Error metrics} including Root Mean Square Error (RMSE), Mean Absolute Error (MAE), Mean Squared Error (MSE), Relative Absolute Error (RAE), and standard error of estimates.
#'   \item A \strong{model fit plot} with bootstrapped 95% confidence intervals.
#'   \item A \strong{residual plot} for diagnostic assessment of model performance.}
#'
#' @examples
#' Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' Temp <- 298
#' fit_temkinLM(Ce, Qe, Temp, verbose = TRUE)
#' fit_temkinLM(Ce, Qe, Temp)
#'
#' @author Paul Angelo C. Manlapaz
#' @references Temkin, M.J., and Pyzhev, V. (1940). Kinetics of ammonia synthesis on promoted iron catalyst. Acta Phys. Chim. USSR 12, 327-356.
#' @export

utils::globalVariables(c("log_Ce", "Fit", "CI_lower", "CI_upper", "Fitted", "Residuals"))

fit_temkinLM <- function(Ce, Qe, Temp, verbose = TRUE) {

  # Constants
  R <- 8.314  # Universal gas constant

  # Data preparation
  log_Ce <- log(Ce)
  data <- data.frame(log_Ce = log_Ce, Qe = Qe)

  # Linear regression
  fit <- stats::lm(Qe ~ log_Ce, data = data)
  summary_fit <- summary(fit)
  coef_fit <- coef(fit)
  intercept <- coef_fit[1]
  slope <- coef_fit[2]

  # Temkin parameters
  bT <- 1 / (slope / (R * Temp))
  aT <- exp(intercept * (bT / (R * Temp)))

  # Residuals and fitted values
  fitted_vals <- stats::fitted(fit)
  residuals <- stats::resid(fit)

  # Fit statistics
  r_squared <- summary_fit$r.squared
  adj_r_squared <- summary_fit$adj.r.squared
  aic_val <- stats::AIC(fit)
  bic_val <- stats::BIC(fit)
  ss_res <- sum(residuals^2)
  n_obs <- length(Qe)
  p <- length(coef(fit))
  std_error <- sqrt(ss_res / (n_obs - p))

  # Error metrics
  error_metrics <- list(
    RMSE = Metrics::rmse(Qe, fitted_vals),
    MAE = Metrics::mae(Qe, fitted_vals),
    MSE = Metrics::mse(Qe, fitted_vals),
    RAE = Metrics::rae(Qe, fitted_vals),
    StdError = std_error
  )

  # Equation annotation
  equation_label <- paste0(
    "Q[e] == ", formatC(intercept, digits = 3, format = "f"), " + ", formatC(slope, digits = 3, format = "f"), " * ln(C[e])")

  # Bootstrapping for prediction interval
  boot_fun <- function(data, indices) {
    d <- data[indices, ]
    tryCatch({
      fit_boot <- stats::lm(Qe ~ log_Ce, data = d)
      predict(fit_boot, newdata = data.frame(log_Ce = sort(data$log_Ce)))
    }, error = function(e) rep(NA, nrow(data)))
  }

  boot_res <- boot::boot(data = data, statistic = boot_fun, R = 100)
  boot_preds <- boot_res$t[complete.cases(boot_res$t), ]
  ci_bounds <- apply(boot_preds, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

  plot_data <- data.frame(
    log_Ce = sort(data$log_Ce),
    Fit = predict(fit, newdata = data.frame(log_Ce = sort(data$log_Ce))),
    CI_lower = ci_bounds[1, ],
    CI_upper = ci_bounds[2, ]
  )

  # Plot 1: Model Fit
  model_fit_plot <- ggplot2::ggplot(data, ggplot2::aes(x = log_Ce, y = Qe)) +
    ggplot2::geom_point(color = "blue", size = 2) +
    ggplot2::geom_line(data = plot_data, ggplot2::aes(x = log_Ce, y = Fit), color = "red", linewidth = 1.2) +
    ggplot2::geom_ribbon(data = plot_data, ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "red") +
    ggplot2::annotate("text", x = max(log_Ce) * 0.95, y = min(Qe) * 1.05,
                      label = equation_label, parse = TRUE, size = 5, hjust = 1) +
    ggplot2::labs(title = "Temkin Isotherm Linear Model Fit", x = "ln(Ce)", y = "Qe") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )

  # Plot 2: Residuals
  residual_plot <- ggplot2::ggplot(data.frame(Fitted = fitted_vals, Residuals = residuals),
                                   ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point(color = "darkgreen", size = 4) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -1.96 * sd(residuals), ymax = 1.96 * sd(residuals)),
                         fill = "gray", alpha = 0.2) +
    ggplot2::annotate("text", x = max(fitted_vals) * 0.9, y = max(residuals) * 0.9,
                      label = paste("R2 =", round(r_squared, 3), "\nAdj. R2 =", round(adj_r_squared, 3)),
                      hjust = 1, size = 4) +
    ggplot2::labs(title = "Residual Plot for Temkin Isotherm Linear Form",
                  x = "Fitted Values", y = "Residuals") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )

  # Output if verbose
  if (verbose) {
  cat("=== Temkin Isotherm Linear Model Summary ===\n")
  print(summary_fit)

  cat("\nTemkin Model Parameters:\n")
  cat("aT:", round(aT, 8), "\n")
  cat("bT:", round(bT, 8), "\n")

  cat("\nModel Fit Statistics:\n")
  cat("R-squared:", r_squared, "\n")
  cat("Adjusted R-squared:", adj_r_squared, "\n")
  cat("AIC:", aic_val, "\n")
  cat("BIC:", bic_val, "\n")

  cat("\nError Metrics:\n")
  cat("RMSE:", round(error_metrics$RMSE, 8), "\n")
  cat("MAE:", round(error_metrics$MAE, 8), "\n")
  cat("MSE :", round(error_metrics$MSE, 8), "\n")
  cat("RAE :", round(error_metrics$RAE, 8), "\n")
  cat("Standard Error of Estimate:", round(error_metrics$StdError, 8), "\n")

  print(model_fit_plot)
  print(residual_plot)
  }

  # Return result list
  invisible(list(
    model = fit,
    summary = summary_fit,
    parameters = list(aT = aT, bT = bT),
    r_squared = r_squared,
    adjusted_r_squared = adj_r_squared,
    AIC = aic_val,
    BIC = bic_val,
    error_metrics = error_metrics,
    plots = list(model_fit = model_fit_plot, residuals = residual_plot)
  ))
}
