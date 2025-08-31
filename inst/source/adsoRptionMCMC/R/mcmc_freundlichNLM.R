#' @title MCMC Analysis for Freundlich Isotherm Non-linear Model
#' @name mcmc_freundlichNLM
#' @description
#' Performs Bayesian parameter estimation using Markov Chain Monte Carlo (MCMC)
#' for the non-linear Freundlich isotherm model:
#' Qe = Kf * Ce^(1/n)
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
#'   \item{Kf_mean}{Posterior mean estimate of Freundlich constant (Kf).}
#'   \item{n_mean}{Posterior mean estimate of Freundlich exponent (n).}
#'   \item{logKf_mean}{Posterior mean of (log(K_f)).}
#'   \item{inv_n_mean}{Posterior mean of (1/n).}
#'   \item{logKf_sd}{Posterior standard deviation for (log(Kf)).}
#'   \item{inv_n_sd}{Posterior standard deviation for (1/n).}
#'   \item{logKf_ci}{95\% credible interval for (log(Kf)).}
#'   \item{inv_n_ci}{95\% credible interval for (1/n).}
#'   \item{gelman_diag}{Gelman-Rubin diagnostics (only if multiple chains).}
#'   \item{mcmc_summary}{Summary statistics for each parameter.}
#' }
#' @examples Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' @examples Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' @examples
#' mcmc_freundlichNLM(Ce, Qe, burnin = 1000, mcmc = 5000, thin = 10,
#'                    verbose = 100, plot = TRUE, n_chains = 2, seed = 123)
#' @author Paul Angelo C. Manlapaz
#' @references Gilks, W. R., Richardson, S., & Spiegelhalter, D. J. (1995). \emph{Markov Chain Monte Carlo in Practice}. Chapman and Hall/CRC.
#' @importFrom MCMCpack MCMCmetrop1R
#' @importFrom coda mcmc mcmc.list gelman.diag traceplot densplot
#' @importFrom stats quantile
#' @importFrom graphics par abline legend
#' @export

utils::globalVariables(c("logKf", "inv_n", "log_sigma"))

mcmc_freundlichNLM <- function(Ce, Qe,
                               burnin = 1000,
                               mcmc = 5000,
                               thin = 10,
                               verbose = 100,
                               n_chains = 2,
                               plot = FALSE,
                               seed = NULL) {

  if (any(Ce <= 0) || any(Qe <= 0)) stop("Ce and Qe must be positive.")

  log_likelihood <- function(params) {
    logKf <- params[1]
    inv_n <- params[2]
    sigma <- exp(params[3])
    pred <- 10^logKf * Ce^inv_n
    sum(dnorm(Qe, mean = pred, sd = sigma, log = TRUE))
  }

  run_chain <- function(init_seed) {
    if (!is.null(init_seed)) set.seed(init_seed)
    MCMCpack::MCMCmetrop1R(
      fun = log_likelihood,
      theta.init = c(log10(mean(Qe)), 0.5, log(sd(Qe))),
      mcmc = mcmc,
      burnin = burnin,
      thin = thin,
      verbose = verbose
    )
  }

  chains <- vector("list", n_chains)
  for (i in seq_len(n_chains)) {
    chains[[i]] <- run_chain(if (!is.null(seed)) seed + i else NULL)
    colnames(chains[[i]]) <- c("logKf", "inv_n", "log_sigma")
  }

  mcmc_list <- coda::mcmc.list(lapply(chains, coda::mcmc))
  main_chain <- as.data.frame(chains[[1]])

  logKf_mean <- mean(main_chain$logKf)
  inv_n_mean <- mean(main_chain$inv_n)
  logKf_sd   <- sd(main_chain$logKf)
  inv_n_sd   <- sd(main_chain$inv_n)
  logKf_ci   <- quantile(main_chain$logKf, probs = c(0.025, 0.975))
  inv_n_ci   <- quantile(main_chain$inv_n, probs = c(0.025, 0.975))

  Kf_mean <- 10^logKf_mean
  n_mean  <- 1 / inv_n_mean

  gelman_diag <- if (n_chains > 1) {
    tryCatch(coda::gelman.diag(mcmc_list, autoburnin = FALSE),
             error = function(e) {
               warning("Gelman-Rubin diagnostic failed: ", e$message)
               NULL
             })
  } else {
    matrix(NA, nrow = 3, ncol = 2,
           dimnames = list(c("logKf", "inv_n", "log_sigma"), c("Point est.", "Upper C.I.")))
  }

  if (plot) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)

    param_names <- c("logKf", "inv_n", "log_sigma")
    for (param in param_names) {
      if (verbose > 0) message("Plotting trace and density for: ", param)
      values <- unlist(lapply(chains, function(chain) chain[, param]))
      post_mean <- mean(values)
      post_sd <- sd(values)

      # Traceplot
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
      coda::traceplot(mcmc_list[, param], main = paste("Traceplot:", param))
      abline(h = post_mean, col = "#FF6347", lty = 2, lwd = 2)
      legend("topleft",
             legend = c(sprintf("Posterior Mean = %.4f", post_mean),
                        sprintf("Posterior SD = %.4f", post_sd)),
             col = "#FF6347", lty = 2, bty = "n", cex = 0.8)

      # Density plot
      coda::densplot(mcmc_list[, param], main = paste("Density:", param))
      abline(v = post_mean, col = "#FF6347", lty = 2, lwd = 2)
      legend("topright",
             legend = c(sprintf("Posterior Mean = %.4f", post_mean),
                        sprintf("Posterior SD = %.4f", post_sd)),
             col = "#FF6347", lty = 2, bty = "n", cex = 0.8)
    }
  }

  return(list(
    Kf_mean = Kf_mean,
    n_mean = n_mean,
    logKf_mean = logKf_mean,
    inv_n_mean = inv_n_mean,
    logKf_sd = logKf_sd,
    inv_n_sd = inv_n_sd,
    logKf_ci = logKf_ci,
    inv_n_ci = inv_n_ci,
    gelman_diag = gelman_diag,
    mcmc_summary = summary(coda::mcmc(main_chain))
  ))
}
