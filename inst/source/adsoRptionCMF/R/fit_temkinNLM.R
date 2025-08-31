#' @title Temkin Isotherm Non-Linear Analysis
#' @name fit_temkinNLM
#'
#' @description Performs non-linear modeling for the Temkin adsorption isotherm.
#'
#' @param Ce numeric vector for equilibrium concentration
#' @param Qe numeric vector for adsorbed amount
#' @param Temp numeric value for temperature (in Kelvin)
#'
#' @import nls2
#' @import Metrics
#' @import stats
#' @import ggplot2
#' @import boot
#'
#' @importFrom nls2 nls2
#' @importFrom Metrics rmse mae mse rae
#' @importFrom stats lm AIC BIC resid fitted
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon annotate labs theme_minimal theme
#' @importFrom boot boot
#'
#' @return A list containing the results of the  non-linear Temkin model fitting, including:
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
#' fit_temkinNLM(Ce, Qe, Temp)
#'
#' @author Paul Angelo C. Manlapaz
#' @references Temkin, M.J., and Pyzhev, V. (1940). Kinetics of ammonia synthesis on promoted iron catalyst. Acta Phys. Chim. USSR 12, 327-356.
#' @export

utils::globalVariables(c("logAT", "log_bT", "log_sigma", "Fit", "CI_lower", "CI_upper", "Fitted", "Residuals"))

fit_temkinNLM <- function(Ce, Qe, Temp) {

  # Prepare data
  data <- data.frame(Ce = Ce, Qe = Qe, Temp = Temp)

  # Temkin model equation
  R <- 8.314  # J/mol/K
  model_formula <- Qe ~ (R * Temp / bT) * log(AT * Ce)
  start_vals <- data.frame(AT = 1, bT = 1)

  fit <- tryCatch({
    nls2::nls2(model_formula, start = start_vals, data = data,
               algorithm = "port", control = stats::nls.control(maxiter = 500, warnOnly = TRUE))
  }, error = function(e) stop("Model fitting failed: ", e$message))

  # Model summary
  cat("=== Temkin Isotherm Non-linear Model Summary ===\n")
  summary_fit <- summary(fit)
  print(summary_fit)

  # Fitted values and residuals
  fitted_vals <- predict(fit)
  residuals <- data$Qe - fitted_vals

  # R-squared calculations
  ss_total <- sum((data$Qe - mean(data$Qe))^2)
  ss_res <- sum(residuals^2)
  r_squared <- 1 - (ss_res / ss_total)
  n_obs <- length(data$Qe)
  p <- length(coef(fit))
  adj_r_squared <- 1 - ((1 - r_squared) * (n_obs - 1) / (n_obs - p))

  # Fit statistics
  cat("\nModel Fit Statistics:\n")
  cat("R-squared:", r_squared, "\n")
  cat("Adjusted R-squared:", adj_r_squared, "\n")
  cat("AIC:", AIC(fit), "\n")
  cat("BIC:", BIC(fit), "\n")

  # Error metrics
  cat("\nError Metrics:\n")
  cat("RMSE:", Metrics::rmse(data$Qe, fitted_vals), "\n")
  cat("MAE:", Metrics::mae(data$Qe, fitted_vals), "\n")
  cat("MSE:", Metrics::mse(data$Qe, fitted_vals), "\n")
  cat("RAE:", Metrics::rae(data$Qe, fitted_vals), "\n")
  cat("Standard Error of Estimate:", sqrt(ss_res / (n_obs - p)), "\n")

  # Extract coefficients
  params <- coef(fit)
  AT <- params["AT"]
  bT <- params["bT"]

  # Equation annotation
  equation_label <- paste0("Q[e] == ", formatC((R * Temp / bT), digits = 3, format = "f"), " * log(", formatC(AT, digits = 3, format = "f"), " * C[e])")

  # Bootstrapped confidence intervals
  predict_fun <- function(formula, data, indices) {
    d <- data[indices, ]
    tryCatch({
      m <- nls2::nls2(formula, data = d, start = start_vals,
                      algorithm = "port", control = stats::nls.control(maxiter = 500, warnOnly = TRUE))
      predict(m, newdata = data.frame(Ce = sort(data$Ce)))
    }, error = function(e) rep(NA, length(data$Ce)))
  }

  boot_res <- boot::boot(data, statistic = predict_fun, R = 100, formula = model_formula)
  boot_preds <- boot_res$t[complete.cases(boot_res$t), ]
  ci_bounds <- apply(boot_preds, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

  plot_data <- data.frame(
    Ce = sort(data$Ce),
    Fit = predict(fit, newdata = data.frame(Ce = sort(data$Ce))),
    CI_lower = ci_bounds[1, ],
    CI_upper = ci_bounds[2, ]
  )

  # Plot 1: Model Fit
  model_fit_plot <- ggplot2::ggplot(data, ggplot2::aes(x = Ce, y = Qe)) +
    ggplot2::geom_point(color = "blue", size = 2) +
    ggplot2::geom_line(data = plot_data, ggplot2::aes(x = Ce, y = Fit), color = "red", linewidth = 1.2) +
    ggplot2::geom_ribbon(data = plot_data, ggplot2::aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "red") +
    ggplot2::annotate("text", x = max(data$Ce) * 0.95, y = min(data$Qe) * 1.05,
                      label = equation_label, parse = TRUE, size = 5, hjust = 1) +
    ggplot2::labs(title = "Temkin Isotherm Non-linear Model Fit",
                  x = "Equilibrium Concentration (Ce)",
                  y = "Adsorbed Amount (Qe)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )

  # Plot 2: Residuals (clean, shaded, annotated)
  residual_plot <- ggplot2::ggplot(data.frame(Fitted = fitted_vals, Residuals = residuals),
                                   ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point(color = "darkgreen", size = 4) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -1.96 * sd(residuals), ymax = 1.96 * sd(residuals)),
                         fill = "gray", alpha = 0.2) +
    ggplot2::annotate("text", x = max(fitted_vals) * 0.9, y = max(residuals) * 0.9,
                      label = paste("R2 =", round(r_squared, 3), "\n",
                                    "Adj. R2 =", round(adj_r_squared, 3)),
                      hjust = 1, size = 4, color = "black") +
    ggplot2::labs(title = "Residual Plot for Temkin Isotherm Non-linear Form",
                  x = "Fitted Values",
                  y = "Residuals") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12)
    )

  # Display plots
  print(model_fit_plot)
  print(residual_plot)

  # Return results for optional further use
  invisible(list(
    model = fit,
    summary = summary_fit,
    r_squared = r_squared,
    adjusted_r_squared = adj_r_squared,
    AIC = AIC(fit),
    BIC = BIC(fit),
    error_metrics = list(
      RMSE = Metrics::rmse(data$Qe, fitted_vals),
      MAE = Metrics::mae(data$Qe, fitted_vals),
      MSE = Metrics::mse(data$Qe, fitted_vals),
      RAE = Metrics::rae(data$Qe, fitted_vals),
      StdError = sqrt(ss_res / (n_obs - p))
    ),
    plots = list(model_fit = model_fit_plot, residuals = residual_plot)
  ))
}
