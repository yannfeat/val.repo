#' @title Langmuir Isotherm Linear (Form 2) Analysis
#' @name fit_langmuirLM2
#'
#' @description Performs linear modeling for the Langmuir adsorption isotherm using the linearized form 1/Qe = (1/Qmax·b)(1/Ce) + (1/Qmax).
#'
#' @param Ce numeric vector for equilibrium concentration
#' @param Qe numeric vector for adsorbed amount
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
#' @return A list containing the results of the linear Langmuir (Form 2) model fitting, including:
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
#' fit_langmuirLM2(Ce, Qe, verbose = TRUE)
#' fit_langmuirLM2(Ce, Qe)
#'
#' @author Paul Angelo C. Manlapaz
#' @references Langmuir, I. (1918) <doi:10.1021/ja01269a066> The adsorption of gases on plane surfaces of glass, mics and platinum. Journal of the American Chemical Society, 1361-1403.
#' @export

utils::globalVariables(c("inv_Ce", "inv_Qe", "Fit", "CI_lower", "CI_upper", "Fitted", "Residuals"))

fit_langmuirLM2 <- function(Ce, Qe, verbose = TRUE) {

  # Basic validation
  if (any(Ce <= 0) || any(Qe <= 0)) stop("Ce and Qe must be positive to compute 1/Ce and 1/Qe.")

  inv_Ce <- 1 / Ce
  inv_Qe <- 1 / Qe
  data <- data.frame(inv_Ce = inv_Ce, inv_Qe = inv_Qe)

  # Fit linear model: 1/Qe = (1/Qmax·b)(1/Ce) + (1/Qmax)
  fit <- stats::lm(inv_Qe ~ inv_Ce, data = data)
  summary_fit <- summary(fit)
  coef_fit <- coef(fit)
  intercept <- coef_fit[1]
  slope <- coef_fit[2]

  # Langmuir parameters
  Qmax <- 1 / intercept
  b <- intercept / slope

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
    RMSE = Metrics::rmse(inv_Qe, fitted_vals),
    MAE = Metrics::mae(inv_Qe, fitted_vals),
    MSE = Metrics::mse(inv_Qe, fitted_vals),
    RAE = Metrics::rae(inv_Qe, fitted_vals),
    StdError = std_error
  )

  # Equation annotation
  equation_label <- paste0("frac(1, Q[e]) == ", formatC(intercept, digits = 3, format = "f"), ifelse(slope >= 0, " + ", " - "), formatC(abs(slope), digits = 3, format = "f"), " * frac(1, C[e])")

  # Bootstrap for confidence intervals
  boot_fun <- function(data, indices) {
    d <- data[indices, ]
    tryCatch({
      fit_boot <- stats::lm(inv_Qe ~ inv_Ce, data = d)
      predict(fit_boot, newdata = data.frame(inv_Ce = sort(data$inv_Ce)))
    }, error = function(e) rep(NA, nrow(data)))
  }

  boot_res <- boot::boot(data = data, statistic = boot_fun, R = 100)
  boot_preds <- boot_res$t[complete.cases(boot_res$t), ]
  ci_bounds <- apply(boot_preds, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

  plot_data <- data.frame(
    inv_Ce = sort(data$inv_Ce),
    Fit = predict(fit, newdata = data.frame(inv_Ce = sort(data$inv_Ce))),
    CI_lower = ci_bounds[1, ],
    CI_upper = ci_bounds[2, ]
  )

  # Plot: Linear Fit with bootstrapped CI
  model_fit_plot <- ggplot2::ggplot(data, ggplot2::aes(x = inv_Ce, y = inv_Qe)) +
    ggplot2::geom_point(color = "blue", size = 2) +
    ggplot2::geom_line(data = plot_data, ggplot2::aes(x = inv_Ce, y = Fit), color = "red", linewidth = 1.2) +
    ggplot2::geom_ribbon(data = plot_data, ggplot2::aes(ymin = CI_lower, ymax = CI_upper),
                         fill = "red", alpha = 0.2) +
    ggplot2::annotate("text", x = max(inv_Ce) * 0.95, y = min(inv_Qe) * 1.05,
                      label = equation_label, parse = TRUE, size = 5, hjust = 1) +
    ggplot2::labs(title = "Langmuir Isotherm Second Linear Model Fit (1/Qe vs. 1/Ce)",
                  x = "1/Ce", y = "1/Qe") +
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
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -1.96 * sd(residuals), ymax = 1.96 * sd(residuals)),
                         fill = "gray", alpha = 0.2) +
    ggplot2::annotate("text", x = max(fitted_vals) * 0.9, y = max(residuals) * 0.9,
                      label = paste("R2 =", round(r_squared, 3), "\nAdj. R2 =", round(adj_r_squared, 3)),
                      hjust = 1, size = 4) +
    ggplot2::labs(title = "Residual Plot for Langmuir Isotherm Second Linear Form",
                  x = "Fitted Values", y = "Residuals") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )

  # Output if verbose
  if (verbose) {
    cat("=== Langmuir Isotherm Second Linear Model Summary ===\n")
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
    cat("Standard Error of Estimate:", round(error_metrics$StdError, 8), "\n")

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
