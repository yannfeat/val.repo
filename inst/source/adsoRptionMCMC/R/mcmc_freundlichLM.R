#' @title MCMC Analysis for Freundlich Isotherm Linear Model
#' @name mcmc_freundlichLM
#' @description
#' Performs Bayesian parameter estimation using Markov Chain Monte Carlo (MCMC)
#' to estimate the parameters of the Freundlich isotherm based on its linearized form:
#' log(Qe) = log(Kf) + (1/n)log(Ce)
#' This method provides a probabilistic interpretation of the model parameters and accounts for their uncertainties.
#' It supports multiple MCMC chains and computes convergence diagnostics (Gelman-Rubin).
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of adsorbed amounts.
#' @param burnin Integer specifying the number of burn-in iterations (default is 1000).
#' @param mcmc Integer specifying the total number of MCMC iterations (default is 5000).
#' @param thin Integer specifying the thinning interval (default is 10).
#' @param verbose Integer controlling the frequency of progress updates (default is 100).
#' @param plot Logical; if TRUE, trace and density plots of the MCMC chains are shown (default is FALSE).
#' @param n_chains Number of independent MCMC chains (default = 2).
#' @param seed Optional integer for reproducibility.
#' @import MCMCpack
#' @import stats
#' @import coda
#' @import graphics
#' @return A list containing:
#' \describe{
#'   \item{mcmc_results}{An object of class \code{mcmc.list} containing posterior samples from all MCMC chains. Each chain includes samples of the intercept and slope.}
#'   \item{Kf_mean}{Posterior mean estimate of the Freundlich constant (Kf).}
#'   \item{n_mean}{Posterior mean estimate of the Freundlich exponent (n).}
#'   \item{logKf_mean}{Posterior mean of (log(Kf)) from the first MCMC chain.}
#'   \item{inv_n_mean}{Posterior mean of (1/n) (the slope) from the first MCMC chain.}
#'   \item{logKf_sd}{Posterior standard deviation of (log(Kf)), quantifying uncertainty in the intercept estimate.}
#'   \item{inv_n_sd}{Posterior standard deviation of (1/n), quantifying uncertainty in the slope estimate.}
#'   \item{logKf_ci}{95\% credible interval for (log(Kf)) from the first MCMC chain, representing the posterior uncertainty in the intercept.}
#'   \item{inv_n_ci}{95\% credible interval for (1/n) from the first MCMC chain, representing the posterior uncertainty in the slope.}
#'   \item{gelman_diag}{Gelman-Rubin diagnostic output from \code{coda::gelman.diag()}, used to assess convergence of the multiple MCMC chains. A potential scale reduction factor (PSRF) close to 1 indicates good convergence.}
#'   \item{mcmc_summary}{Summary statistics of the first MCMC chain, including means, standard deviations, quantiles, and sample sizes for each parameter.}
#' }
#' @examples Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' @examples Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' @examples
#' mcmc_freundlichLM(Ce, Qe, burnin = 1000, mcmc = 5000, thin = 10,
#'                   verbose = 100, plot = TRUE, n_chains = 2, seed = 123)
#' @author Paul Angelo C. Manlapaz
#' @references Gilks, W. R., Richardson, S., & Spiegelhalter, D. J. (1995). \emph{Markov Chain Monte Carlo in Practice}. Chapman and Hall/CRC.
#' @importFrom MCMCpack MCMCregress
#' @importFrom stats quantile
#' @importFrom coda mcmc.list traceplot densplot gelman.diag
#' @importFrom graphics par abline legend
#' @export

utils::globalVariables(c("log_Ce", "log_Qe"))

mcmc_freundlichLM <- function(Ce, Qe,
                              burnin = 1000,
                              mcmc = 5000,
                              thin = 10,
                              verbose = 100,
                              plot = FALSE,
                              n_chains = 2,
                              seed = NULL) {

  # Check required packages
  if (!requireNamespace("MCMCpack", quietly = TRUE)) {
    stop("Package 'MCMCpack' is required. Please install it with install.packages('MCMCpack').")
  }
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required. Please install it with install.packages('coda').")
  }

  # Input validation
  if (any(Ce <= 0) || any(Qe <= 0)) {
    stop("Ce and Qe must be positive for log-transformation.")
  }

  # Optional reproducibility
  if (!is.null(seed)) set.seed(seed)

  # Log-transform
  log_Ce <- log10(Ce)
  log_Qe <- log10(Qe)
  data <- data.frame(log_Ce = log_Ce, log_Qe = log_Qe)

  # Run multiple MCMC chains
  chains <- vector("list", n_chains)

  for (i in seq_len(n_chains)) {
    chains[[i]] <- MCMCpack::MCMCregress(
      log_Qe ~ log_Ce,
      data = data,
      burnin = burnin,
      mcmc = mcmc,
      thin = thin,
      verbose = verbose,
      progressbar = TRUE
    )
  }

  # Combine chains
  mcmc_list <- coda::mcmc.list(chains)
  main_chain <- chains[[1]]

  # Posterior summaries
  logKf_mean <- mean(main_chain[, 1])
  inv_n_mean <- mean(main_chain[, 2])
  logKf_sd   <- sd(main_chain[, 1])
  inv_n_sd   <- sd(main_chain[, 2])
  logKf_ci   <- quantile(main_chain[, 1], probs = c(0.025, 0.975))
  inv_n_ci   <- quantile(main_chain[, 2], probs = c(0.025, 0.975))

  # Derived estimates
  Kf_mean <- 10^logKf_mean
  n_mean  <- 1 / inv_n_mean

  # Gelman-Rubin convergence diagnostic
  gelman_diag <- tryCatch(
    coda::gelman.diag(mcmc_list, autoburnin = FALSE),
    error = function(e) {
      warning("Gelman-Rubin diagnostic failed: ", e$message)
      NULL
    }
  )

  # Plot if requested
  if (plot) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)

    param_names <- colnames(mcmc_list[[1]])

    for (param in param_names) {
      if (verbose > 0) {
        message("Plotting trace and density for: ", param)
      }

      values <- unlist(lapply(mcmc_list, function(chain) chain[, param]))
      post_mean <- mean(values)
      post_sd <- sd(values)

      # TRACEPLOT
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
      coda::traceplot(mcmc_list[, param], main = paste("Traceplot:", param))
      abline(h = post_mean, col = "#FF6347", lwd = 2, lty = 2)
      legend("topleft",
             legend = c(sprintf("Posterior Mean = %.4f", post_mean),
                        sprintf("Posterior SD = %.4f", post_sd)),
             col = "#FF6347", lty = 2, lwd = 2, bty = "n", cex = 0.8)

      # DENSITY PLOT
      coda::densplot(mcmc_list[, param], main = paste("Density:", param))
      abline(v = post_mean, col = "#FF6347", lwd = 2, lty = 2)
      legend("topright",
             legend = c(sprintf("Posterior Mean = %.4f", post_mean),
                        sprintf("Posterior SD = %.4f", post_sd)),
             col = "#FF6347", lty = 2, lwd = 2, bty = "n", cex = 0.8)
    }
  }

  # Return results
  return(list(
    mcmc_results = mcmc_list,
    Kf_mean = Kf_mean,
    n_mean = n_mean,
    logKf_mean = logKf_mean,
    inv_n_mean = inv_n_mean,
    logKf_sd = logKf_sd,
    inv_n_sd = inv_n_sd,
    logKf_ci = logKf_ci,
    inv_n_ci = inv_n_ci,
    gelman_diag = gelman_diag,
    mcmc_summary = summary(main_chain)
  ))
}
