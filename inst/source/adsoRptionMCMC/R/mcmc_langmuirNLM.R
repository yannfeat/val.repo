#' @title MCMC Analysis for Langmuir Isotherm Non-linear Model
#' @name mcmc_langmuirNLM
#' @description
#' This function performs Bayesian parameter estimation using Markov Chain Monte Carlo (MCMC) simulation
#' to estimate the parameters of the Langmuir isotherm using the non-linear model:
#' Qe = (Qmax * KL * Ce) / (1 + KL * Ce)
#' This approach is applied to obtain a probabilistic distribution of the model parameters, capturing uncertainties
#' and potential correlations between them.
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
#' @import coda
#' @import stats
#' @import graphics
#' @return A list containing:
#' \describe{
#'   \item{Qmax_mean}{Posterior mean estimate of the Langmuir maximum adsorption capacity (Qmax).}
#'   \item{Kl_mean}{Posterior mean estimate of the Langmuir constant (Kl).}
#'   \item{Qmax_sd}{Posterior standard deviation for (Qmax).}
#'   \item{Kl_sd}{Posterior standard deviation for (Kl).}
#'   \item{Qmax_ci}{95\% credible interval for (Qmax).}
#'   \item{Kl_ci}{95\% credible interval for (Kl).}
#'   \item{gelman_diag}{Gelman-Rubin diagnostics (only if multiple chains).}
#'   \item{mcmc_summary}{Summary statistics for each parameter.}
#' }
#' @examples Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' @examples Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' @examples
#' mcmc_langmuirNLM(Ce, Qe, burnin = 1000, mcmc = 5000, thin = 10,
#'                  verbose = 100, plot = TRUE, n_chains = 2, seed = 123)
#' @author Paul Angelo C. Manlapaz
#' @importFrom MCMCpack MCMCmetrop1R
#' @importFrom coda mcmc mcmc.list gelman.diag traceplot densplot
#' @importFrom stats quantile
#' @importFrom graphics par abline legend
#' @export

utils::globalVariables(c("logQmax", "logKl", "log_sigma"))

mcmc_langmuirNLM <- function(Ce, Qe,
                             burnin = 1000,
                             mcmc = 5000,
                             thin = 10,
                             verbose = 100,
                             n_chains = 2,
                             plot = FALSE,
                             seed = NULL) {

  if (!requireNamespace("MCMCpack", quietly = TRUE)) stop("Please install 'MCMCpack'.")
  if (!requireNamespace("coda", quietly = TRUE)) stop("Please install 'coda'.")
  if (any(Ce <= 0) || any(Qe <= 0)) stop("All Ce and Qe values must be positive.")

  log_likelihood <- function(params) {
    logQmax <- params[1]
    logKl <- params[2]
    sigma <- exp(params[3])

    Qmax <- exp(logQmax)
    Kl <- exp(logKl)

    pred <- Qmax * Kl * Ce / (1 + Kl * Ce)
    sum(dnorm(Qe, mean = pred, sd = sigma, log = TRUE))
  }

  run_chain <- function(chain_id) {
    if (!is.null(seed)) set.seed(seed + chain_id)
    MCMCpack::MCMCmetrop1R(
      fun = log_likelihood,
      theta.init = c(log10(max(Qe)), log(1), log(sd(Qe))),
      mcmc = mcmc,
      burnin = burnin,
      thin = thin,
      verbose = verbose
    )
  }

  chains <- lapply(seq_len(n_chains), run_chain)
  for (i in seq_along(chains)) {
    colnames(chains[[i]]) <- c("logQmax", "logKl", "log_sigma")
  }

  mcmc_list <- coda::mcmc.list(lapply(chains, coda::mcmc))
  main_chain <- as.data.frame(chains[[1]])

  logQmax_mean <- mean(main_chain$logQmax)
  logKl_mean <- mean(main_chain$logKl)
  logQmax_sd <- sd(main_chain$logQmax)
  logKl_sd <- sd(main_chain$logKl)
  logQmax_ci <- stats::quantile(main_chain$logQmax, probs = c(0.025, 0.975))
  logKl_ci <- stats::quantile(main_chain$logKl, probs = c(0.025, 0.975))

  Qmax_mean <- exp(logQmax_mean)
  Kl_mean <- exp(logKl_mean)

  gelman_diag <- tryCatch({
    coda::gelman.diag(mcmc_list, autoburnin = FALSE)
  }, error = function(e) {
    warning("Gelman-Rubin diagnostic failed: ", e$message)
    NULL
  })

  if (plot) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar), add = TRUE)

    param_names <- c("logQmax", "logKl", "log_sigma")
    for (param in param_names) {
      values <- unlist(lapply(chains, function(chain) chain[, param]))
      post_mean <- mean(values)
      post_sd <- sd(values)

      graphics::par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
      coda::traceplot(mcmc_list[, param], main = paste("Traceplot:", param))
      graphics::abline(h = post_mean, col = "#FF6347", lty = 2, lwd = 2)
      graphics::legend("topleft", legend = sprintf("Posterior mean = %.4f\nPosterior SD = %.4f", post_mean, post_sd),
                       col = "#FF6347", lty = 2, lwd = 2, bty = "n", cex = 0.8)

      coda::densplot(mcmc_list[, param], main = paste("Density:", param))
      graphics::abline(v = post_mean, col = "#FF6347", lty = 2, lwd = 2)
      graphics::legend("topright", legend = sprintf("Posterior mean = %.4f\nPosterior SD = %.4f", post_mean, post_sd),
                       col = "#FF6347", lty = 2, lwd = 2, bty = "n", cex = 0.8)
    }
  }

  return(list(
    mcmc_results = mcmc_list,
    Qmax_mean = Qmax_mean,
    Kl_mean = Kl_mean,
    logQmax_mean = logQmax_mean,
    logKl_mean = logKl_mean,
    logQmax_sd = logQmax_sd,
    logKl_sd = logKl_sd,
    logQmax_ci = logQmax_ci,
    logKl_ci = logKl_ci,
    gelman_diag = gelman_diag,
    mcmc_summary = summary(coda::mcmc(main_chain))
  ))
}
