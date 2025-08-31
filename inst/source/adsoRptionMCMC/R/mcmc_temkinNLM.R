#' @title MCMC Analysis for Temkin Isotherm Non-linear Model
#' @name mcmc_temkinNLM
#' @description
#' Performs Bayesian parameter estimation using Markov Chain Monte Carlo (MCMC)
#' for the non-linear Temkin isotherm model:
#' Qe = (R * T / b_T) * ln(A_T * Ce)
#' This approach is applied to obtain a probabilistic distribution of the model parameters, capturing uncertainties
#' and potential correlations between them.
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
#' @import coda
#' @import stats
#' @import graphics
#' @return A list containing:
#' \describe{
#'   \item{AT_mean}{Posterior mean estimate of Temkin constant (AT).}
#'   \item{bT_mean}{Posterior mean estimate of Temkin constant (bT).}
#'   \item{logAT_mean}{Posterior mean of (log(AT)).}
#'   \item{bT_sd}{Posterior standard deviation for (bT).}
#'   \item{logAT_sd}{Posterior standard deviation for (log(AT)).}
#'   \item{logAT_ci}{95\% credible interval for (log(AT)).}
#'   \item{bT_ci}{95\% credible interval for (bT).}
#'   \item{gelman_diag}{Gelman-Rubin diagnostics (only if multiple chains).}
#'   \item{mcmc_summary}{Summary statistics for each parameter.}
#' }
#' @examples Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' @examples Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' @examples
#' mcmc_temkinNLM(Ce, Qe, 298, burnin = 1000, mcmc = 5000, thin = 10,
#'                verbose = 100, plot = TRUE, n_chains = 2, seed = 123)
#' @author Paul Angelo C. Manlapaz
#' @references
#' Gilks, W. R., Richardson, S., & Spiegelhalter, D. J. (1995). \emph{Markov Chain Monte Carlo in Practice}. Chapman and Hall/CRC.
#' @importFrom MCMCpack MCMCmetrop1R
#' @importFrom coda mcmc mcmc.list gelman.diag traceplot densplot
#' @importFrom stats quantile
#' @importFrom graphics par abline legend
#' @export

utils::globalVariables(c("logAT", "log_bT", "log_sigma"))

mcmc_temkinNLM <- function(Ce, Qe, Temp,
                           burnin = 1000,
                           mcmc = 5000,
                           thin = 10,
                           verbose = 100,
                           n_chains = 2,
                           plot = FALSE,
                           seed = NULL) {

  if (any(Ce <= 0) || any(Qe <= 0)) stop("Ce and Qe must be positive.")
  R <- 8.314  # J/mol·K

  log_likelihood <- function(params) {
    logAT <- params[1]
    logbT <- params[2]
    log_sigma <- params[3]

    sigma <- exp(log_sigma)
    A_T <- exp(logAT)
    b_T <- exp(logbT)

    pred <- (R * Temp / b_T) * log(A_T * Ce)

    if (any(!is.finite(pred)) || any(!is.finite(sigma))) return(-1e10)
    sum(dnorm(Qe, mean = pred, sd = sigma, log = TRUE))
  }

  run_chain <- function(i) {
    if (!is.null(seed)) set.seed(seed + i)
    theta.init <- c(log(1), log(1000), log(sd(Qe)))
    MCMCpack::MCMCmetrop1R(
      fun = log_likelihood,
      theta.init = theta.init,
      mcmc = mcmc,
      burnin = burnin,
      thin = thin,
      verbose = verbose,
      force.samp = TRUE,
      V = diag(c(0.01, 0.01, 0.01))
    )
  }

  chains <- vector("list", n_chains)
  for (i in seq_len(n_chains)) {
    chains[[i]] <- run_chain(i)
    colnames(chains[[i]]) <- c("logAT", "log_bT", "log_sigma")
  }

  mcmc_list <- coda::mcmc.list(lapply(chains, coda::mcmc))
  main_chain <- as.data.frame(chains[[1]])

  logAT_mean <- mean(main_chain$logAT)
  log_bT_mean <- mean(main_chain$log_bT)
  logAT_sd <- sd(main_chain$logAT)
  log_bT_sd <- sd(main_chain$log_bT)
  logAT_ci <- quantile(main_chain$logAT, probs = c(0.025, 0.975))
  log_bT_ci <- quantile(main_chain$log_bT, probs = c(0.025, 0.975))

  AT_mean <- exp(logAT_mean)
  bT_mean <- exp(log_bT_mean)

  gelman_diag <- if (n_chains > 1) {
    tryCatch(coda::gelman.diag(mcmc_list, autoburnin = FALSE),
             error = function(e) {
               warning("Gelman-Rubin diagnostic failed: ", e$message)
               NULL
             })
  } else {
    matrix(NA, nrow = 3, ncol = 2,
           dimnames = list(c("logAT", "log_bT", "log_sigma"),
                           c("Point est.", "Upper C.I.")))
  }

  if (plot) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar), add = TRUE)

    for (param in c("logAT", "log_bT", "log_sigma")) {
      values <- unlist(lapply(chains, function(chain) chain[, param]))
      post_mean <- mean(values)
      post_sd <- sd(values)

      graphics::par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
      coda::traceplot(mcmc_list[, param], main = paste("Traceplot:", param))
      graphics::abline(h = post_mean, col = "#FF6347", lty = 2, lwd = 2)
      graphics::legend("topleft", legend = sprintf("Posterior mean = %.4f\nPosterior SD = %.4f",
                                                    post_mean, post_sd),
                       col = "#FF6347", lty = 2, bty = "n", cex = 0.8)

      coda::densplot(mcmc_list[, param], main = paste("Density:", param))
      graphics::abline(v = post_mean, col = "#FF6347", lty = 2, lwd = 2)
      graphics::legend("topright", legend = sprintf("Posterior mean = %.4f\nPosterior SD = %.4f",
                                                    post_mean, post_sd),
                       col = "#FF6347", lty = 2, bty = "n", cex = 0.8)
    }
  }

  return(list(
    AT_mean = AT_mean,
    bT_mean = bT_mean,
    logAT_mean = logAT_mean,
    bT_sd = log_bT_sd,
    logAT_sd = logAT_sd,
    logAT_ci = logAT_ci,
    bT_ci = exp(log_bT_ci),
    gelman_diag = gelman_diag,
    mcmc_summary = summary(coda::mcmc(main_chain))
  ))
}
