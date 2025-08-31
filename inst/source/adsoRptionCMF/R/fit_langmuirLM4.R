#' @title Langmuir Isotherm Linear (Form 4) Analysis
#' @name fit_langmuirLM4
#'
#' @description Performs linear modeling for the Langmuir adsorption isotherm using the linearized form Qe/Ce = bQmax - bQe.
#'
#' @param Ce Numeric vector for equilibrium concentrations
#' @param Qe Numeric vector for adsorbed amounts
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
#' @return A list containing the results of the linear Langmuir (Form 4) model fitting, including:
#' \itemize{
#'   \item \strong{Parameter estimates} for the Langmuir model (Qmax and Kl).
#'   \item \strong{Fit statistics} such as Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), and R-squared.
#'   \item \strong{Error metrics} including Root Mean Square Error (RMSE), Mean Absolute Error (MAE), Mean Squared Error (MSE), Relative Absolute Error (RAE), and standard error of estimates.
#'   \item A \strong{model fit plot} with bootstrapped 95% confidence intervals.
#'   \item A \strong{residual plot} for diagnostic assessment of model performance.}
#'
#' @examples
#' Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' fit_langmuirLM4(Ce, Qe, verbose = TRUE)
#' fit_langmuirLM4(Ce, Qe)
#'
#' @author Paul Angelo C. Manlapaz
#' @references Langmuir, I. (1918) <doi:10.1021/ja01269a066> The adsorption of gases on plane surfaces of glass, mics and platinum. Journal of the American Chemical Society, 1361-1403.
#' @export

utils::globalVariables(c("Qe_by_Ce", "Qe", "Fit", "CI_lower", "CI_upper", "Fitted", "Residuals"))

fit_langmuirLM4 <- function(Ce, Qe, verbose = TRUE) {

  # Basic validation
  if (any(Ce <= 0) || any(Qe <= 0)) stop("Ce and Qe must be positive for Form 4 transformation.")

  Qe_by_Ce <- Qe / Ce
  data <- data.frame(Qe_by_Ce = Qe_by_Ce, Qe = Qe)

  # Fit linear model: Qe/Ce = bQmax - bQe
  fit <- stats::lm(Qe_by_Ce ~ Qe, data = data)
  summary_fit <- summary(fit)
  coef_fit <- coef(fit)
  intercept <- coef_fit[1]  # b * Qmax
  slope <- coef_fit[2]      # -b

  # Langmuir parameters
  b <- -slope
  Qmax <- intercept / b

  # Fitted values and residuals
  fitted_vals <- stats::fitted(fit)
  residuals <- stats::resid(fit)

  # R-squared and adjusted R-squared
  r_squared <- summary_fit$r.squared
  adj_r_squared <- summary_fit$adj.r.squared

  # AIC/BIC
  aic_val <- stats::AIC(fit)
  bic_val <- stats::BIC(fit)

  # Error metrics
  ss_res <- sum(residuals^2)
  n_obs <- length(Qe)
  p <- length(coef(fit))
  std_error <- sqrt(ss_res / (n_obs - p))

  error_metrics <- list(
    RMSE = Metrics::rmse(Qe_by_Ce, fitted_vals),
    MAE = Metrics::mae(Qe_by_Ce, fitted_vals),
    MSE = Metrics::mse(Qe_by_Ce, fitted_vals),
    RAE = Metrics::rae(Qe_by_Ce, fitted_vals),
    StdError = std_error
  )

  # Equation annotation
  equation_label <- paste0("frac(Q[e], C[e]) == ", formatC(intercept, digits = 3, format = "f"), " - ", formatC(abs(slope), digits = 3, format = "f"), " * Q[e]")

  # Bootstrap for confidence intervals
  boot_fun <- function(data, indices) {
    d <- data[indices, ]
    tryCatch({
      fit_boot <- stats::lm(Qe_by_Ce ~ Qe, data = d)
      predict(fit_boot, newdata = data.frame(Qe = sort(data$Qe)))
    }, error = function(e) rep(NA, nrow(data)))
  }

  boot_res <- boot::boot(data = data, statistic = boot_fun, R = 100)
  boot_preds <- boot_res$t[complete.cases(boot_res$t), ]
  ci_bounds <- apply(boot_preds, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

  plot_data <- data.frame(
    Qe = sort(data$Qe),
    Fit = predict(fit, newdata = data.frame(Qe = sort(data$Qe))),
    CI_lower = ci_bounds[1, ],
    CI_upper = ci_bounds[2, ]
  )

  # Plot: Linear Fit with bootstrapped CI
  model_fit_plot <- ggplot2::ggplot(data, ggplot2::aes(x = Qe, y = Qe_by_Ce)) +
    ggplot2::geom_point(color = "blue", size = 2) +
    ggplot2::geom_line(data = plot_data, ggplot2::aes(x = Qe, y = Fit), color = "red", linewidth = 1.2) +
    ggplot2::geom_ribbon(data = plot_data, ggplot2::aes(ymin = CI_lower, ymax = CI_upper),
                         fill = "red", alpha = 0.2) +
    ggplot2::annotate("text", x = max(Qe) * 0.95, y = min(Qe_by_Ce) * 1.05,
                      label = equation_label, parse = TRUE, size = 5, hjust = 1) +
    ggplot2::labs(title = "Langmuir Isotherm Fourth Linear Model Fit (Qe/Ce vs. Qe)",
                  x = "Qe", y = "Qe/Ce") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )

  # Plot: Residuals
  residual_plot <- ggplot2::ggplot(data.frame(Fitted = fitted_vals, Residuals = residuals),
                                   ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point(color = "darkgreen", size = 4) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -1.96 * sd(residuals), ymax = 1.96 * sd(residuals)),
                         fill = "gray", alpha = 0.2) +
    ggplot2::annotate("text", x = max(fitted_vals) * 0.9, y = max(residuals) * 0.9,
                      label = paste("R2 =", round(r_squared, 3), "\nAdj. R2 =", round(adj_r_squared, 3)),
                      hjust = 1, size = 4) +
    ggplot2::labs(title = "Residual Plot for Langmuir Isotherm Fourth Linear Form",
                  x = "Fitted Values", y = "Residuals") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )

  # Output if verbose
  if (verbose) {
    cat("=== Langmuir Isotherm Fourth Linear Model Summary ===\n")
    print(summary_fit)

    cat("\nLangmuir Model Parameters:\n")
    cat("Qmax:", round(Qmax, 8), "\n")
    cat("b    :", round(b, 8), "\n")

    cat("\nModel Fit Statistics:\n")
    cat("R-squared:", r_squared, "\n")
    cat("Adjusted R-squared:", adj_r_squared, "\n")
    cat("AIC:", aic_val, "\n")
    cat("BIC:", bic_val, "\n")

    cat("\nError Metrics:\n")
    cat("RMSE:", round(error_metrics$RMSE, 8), "\n")
    cat("MAE:", round(error_metrics$MAE, 8), "\n")
    cat("MSE:", round(error_metrics$MSE, 8), "\n")
    cat("RAE:", round(error_metrics$RAE, 8), "\n")
    cat("Standard Error of Estimate:", round(error_metrics$StdError, 8), "\n\n")

    print(model_fit_plot)
    print(residual_plot)
  }

  invisible(list(
    model = fit,
    summary = summary_fit,
    parameters = list(Qmax = Qmax, b = b),
    r_squared = r_squared,
    adjusted_r_squared = adj_r_squared,
    AIC = aic_val,
    BIC = bic_val,
    error_metrics = error_metrics,
    plots = list(model_fit = model_fit_plot, residuals = residual_plot)
  ))
}
