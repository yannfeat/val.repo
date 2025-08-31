#' @title MCMC Analysis for Temkin Isotherm Linear Model
#' @name mcmc_temkinLM
#' @description
#' Performs Bayesian parameter estimation using Markov Chain Monte Carlo (MCMC) to estimate the parameters of the Temkin isotherm based on its linearized form:
#' Qe = aT + bT * log(Ce)
#' This method provides a probabilistic interpretation of the model parameters and accounts for their uncertainties.
#' It supports multiple MCMC chains and computes convergence diagnostics (Gelman-Rubin).
#' @param Ce Numeric vector of equilibrium concentrations.
#' @param Qe Numeric vector of adsorbed amounts.
#' @param Temp Numeric value of temperature in Kelvin.
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
#'   \item{mcmc_results}{An object of class \code{mcmc.list} containing posterior samples from all MCMC chains.}
#'   \item{aT_mean}{Posterior mean estimate of Temkin constant (aT).}
#'   \item{bT_mean}{Posterior mean estimate of Temkin constant (bT).}
#'   \item{aT_raw_mean}{Posterior mean of the intercept (aT) from the linear model.}
#'   \item{bT_raw_mean}{Posterior mean of the slope (b_T) from the linear model.}
#'   \item{aT_sd}{Posterior standard deviation of (aT).}
#'   \item{bT_sd}{Posterior standard deviation of (bT).}
#'   \item{aT_ci}{95\% credible interval for (aT).}
#'   \item{bT_ci}{95\% credible interval for (bT ).}
#'   \item{gelman_diag}{Gelman-Rubin convergence diagnostic.}
#'   \item{mcmc_summary}{Summary statistics from the first chain.}
#' }
#' @examples Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' @examples Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' @examples
#' mcmc_temkinLM(Ce, Qe, 298, burnin = 1000, mcmc = 5000, thin = 10,
#'               verbose = 100, plot = TRUE, n_chains = 2, seed = 123)
#' @author Paul Angelo C. Manlapaz
#' @references Gilks, W. R., Richardson, S., & Spiegelhalter, D. J. (1995). \emph{Markov Chain Monte Carlo in Practice}. Chapman and Hall/CRC.
#' @importFrom MCMCpack MCMCregress
#' @importFrom coda mcmc.list traceplot densplot gelman.diag
#' @importFrom stats quantile
#' @importFrom graphics par abline legend
#' @export

utils::globalVariables(c("log_Ce"))

mcmc_temkinLM <- function(Ce, Qe, Temp,
                          burnin = 1000,
                          mcmc = 5000,
                          thin = 10,
                          verbose = 100,
                          plot = FALSE,
                          n_chains = 2,
                          seed = NULL) {

  if (!requireNamespace("MCMCpack", quietly = TRUE)) stop("Package 'MCMCpack' is required.")
  if (!requireNamespace("coda", quietly = TRUE)) stop("Package 'coda' is required.")
  if (any(Ce <= 0)) stop("All Ce values must be positive (required for log-transformation).")

  R <- 8.314  # J/mol·K

  log_Ce <- log(Ce)
  data <- data.frame(Qe = Qe, log_Ce = log_Ce)

  run_chain <- function(i) {
    if (!is.null(seed)) set.seed(seed + i)
    MCMCpack::MCMCregress(Qe ~ log_Ce,
                          data = data,
                          burnin = burnin,
                          mcmc = mcmc,
                          thin = thin,
                          verbose = verbose,
                          progressbar = TRUE)
  }

  chains <- lapply(seq_len(n_chains), run_chain)
  mcmc_list <- coda::mcmc.list(chains)
  main_chain <- as.data.frame(chains[[1]])

  intercept_mean <- mean(main_chain[, 1])  # aT raw
  slope_mean <- mean(main_chain[, 2])      # bT
  intercept_sd <- sd(main_chain[, 1])
  slope_sd <- sd(main_chain[, 2])
  intercept_ci <- quantile(main_chain[, 1], probs = c(0.025, 0.975))
  slope_ci <- quantile(main_chain[, 2], probs = c(0.025, 0.975))

  bT_mean <- slope_mean
  aT_mean <- exp(intercept_mean * (bT_mean / (R * Temp)))  # aT = exp(a_raw * bT / RT)

  gelman_diag <- tryCatch({
    coda::gelman.diag(mcmc_list, autoburnin = FALSE)
  }, error = function(e) {
    warning("Gelman-Rubin diagnostic failed: ", e$message)
    NULL
  })

  if (plot) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar), add = TRUE)

    param_names <- colnames(mcmc_list[[1]])
    for (param in param_names) {
      values <- unlist(lapply(mcmc_list, function(chain) chain[, param]))
      post_mean <- mean(values)
      post_sd <- sd(values)

      graphics::par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
      coda::traceplot(mcmc_list[, param], main = paste("Traceplot:", param))
      graphics::abline(h = post_mean, col = "#FF6347", lty = 2, lwd = 2)
      graphics::legend("topleft",
                       legend = sprintf("Posterior Mean = %.4f\nPosterior SD = %.4f", post_mean, post_sd),
                       col = "#FF6347", lty = 2, lwd = 2, bty = "n", cex = 0.8)

      coda::densplot(mcmc_list[, param], main = paste("Density:", param))
      graphics::abline(v = post_mean, col = "#FF6347", lty = 2, lwd = 2)
      graphics::legend("topright",
                       legend = sprintf("Posterior Mean = %.4f\nPosterior SD = %.4f", post_mean, post_sd),
                       col = "#FF6347", lty = 2, lwd = 2, bty = "n", cex = 0.8)
    }
  }

  return(list(
    mcmc_results = mcmc_list,
    aT_mean = aT_mean,
    bT_mean = bT_mean,
    aT_raw_mean = intercept_mean,
    bT_raw_mean = slope_mean,
    aT_sd = intercept_sd,
    bT_sd = slope_sd,
    aT_ci = intercept_ci,
    bT_ci = slope_ci,
    gelman_diag = gelman_diag,
    mcmc_summary = summary(coda::mcmc(main_chain))
  ))
}
