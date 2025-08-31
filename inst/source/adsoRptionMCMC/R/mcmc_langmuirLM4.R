#' @title MCMC Analysis for Langmuir Isotherm Linear (Form 4) Model
#' @name mcmc_langmuirLM4
#' @description
#' Performs Bayesian parameter estimation using Markov Chain Monte Carlo (MCMC) to estimate the parameters of the Langmuir isotherm using its fourth linear form:
#' Qe / Ce = b * Qmax - b * Qe
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
#' @return A list with:
#' \describe{
#'   \item{mcmc_results}{Combined posterior samples (mcmc.list).}
#'   \item{Qmax_mean}{Posterior mean of Qmax.}
#'   \item{b_mean}{Posterior mean of b.}
#'   \item{intercept_mean}{Posterior mean of intercept (b * Qmax).}
#'   \item{intercept_sd}{Posterior standard deviation of intercept.}
#'   \item{intercept_ci}{95\% credible interval for intercept.}
#'   \item{slope_mean}{Posterior mean of slope (-b).}
#'   \item{slope_sd}{Posterior standard deviation of slope.}
#'   \item{slope_ci}{95\% credible interval for slope.}
#'   \item{gelman_diag}{Gelman-Rubin convergence diagnostics.}
#'   \item{mcmc_summary}{Summary of the first MCMC chain.}
#' }
#' @examples Ce <- c(0.01353, 0.04648, 0.13239, 0.27714, 0.41600, 0.63607, 0.80435, 1.10327, 1.58223)
#' @examples Qe <- c(0.03409, 0.06025, 0.10622, 0.12842, 0.15299, 0.15379, 0.15735, 0.15735, 0.16607)
#' @examples
#' mcmc_langmuirLM4(Ce, Qe, burnin = 1000, mcmc = 10000, thin = 10,
#'                  verbose = 100, plot = TRUE, n_chains = 2, seed = 123)
#' @author Paul Angelo C. Manlapaz
#' @references Gilks, W. R., Richardson, S., & Spiegelhalter, D. J. (1995). \emph{Markov Chain Monte Carlo in Practice}. Chapman and Hall/CRC.
#' @importFrom MCMCpack MCMCregress
#' @importFrom coda mcmc.list traceplot densplot gelman.diag
#' @importFrom stats quantile
#' @importFrom graphics par abline legend
#' @export

utils::globalVariables(c("Qe_by_Ce", "Qe"))

mcmc_langmuirLM4 <- function(Ce, Qe,
                             burnin = 1000,
                             mcmc = 5000,
                             thin = 10,
                             verbose = 100,
                             plot = FALSE,
                             n_chains = 2,
                             seed = NULL) {

  if (!requireNamespace("MCMCpack", quietly = TRUE)) stop("Package 'MCMCpack' required but not installed.")
  if (!requireNamespace("coda", quietly = TRUE)) stop("Package 'coda' required but not installed.")

  if (any(Qe <= 0) || any(Ce <= 0)) stop("All Ce and Qe values must be positive.")

  Qe_by_Ce <- Qe / Ce
  data <- data.frame(Qe_by_Ce = Qe_by_Ce, Qe = Qe)

  chains <- vector("list", n_chains)
  for (i in seq_len(n_chains)) {
    if (!is.null(seed)) set.seed(seed + i)
    chains[[i]] <- MCMCpack::MCMCregress(Qe_by_Ce ~ Qe,
                                         data = data,
                                         burnin = burnin,
                                         mcmc = mcmc,
                                         thin = thin,
                                         verbose = verbose,
                                         progressbar = TRUE)
  }

  mcmc_list <- coda::mcmc.list(lapply(chains, coda::mcmc))
  main_chain <- chains[[1]]

  intercept_mean <- mean(main_chain[, 1])  # b * Qmax
  slope_mean <- mean(main_chain[, 2])      # -b

  intercept_sd <- sd(main_chain[, 1])
  slope_sd <- sd(main_chain[, 2])

  intercept_ci <- stats::quantile(main_chain[, 1], probs = c(0.025, 0.975))
  slope_ci <- stats::quantile(main_chain[, 2], probs = c(0.025, 0.975))

  b_mean <- -slope_mean
  Qmax_mean <- intercept_mean / b_mean

  gelman_diag <- tryCatch(
    coda::gelman.diag(mcmc_list, autoburnin = FALSE),
    error = function(e) {
      warning("Gelman-Rubin diagnostic failed: ", e$message)
      NULL
    }
  )

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
      graphics::abline(h = post_mean, col = "#FF6347", lwd = 2, lty = 2)
      graphics::legend("topleft",
                       legend = sprintf("Posterior Mean = %.4f\nPosterior SD = %.4f", post_mean, post_sd),
                       col = "#FF6347", lty = 2, lwd = 2, bty = "n", cex = 0.8)

      coda::densplot(mcmc_list[, param], main = paste("Density:", param))
      graphics::abline(v = post_mean, col = "#FF6347", lwd = 2, lty = 2)
      graphics::legend("topright",
                       legend = sprintf("Posterior Mean = %.4f\nPosterior SD = %.4f", post_mean, post_sd),
                       col = "#FF6347", lty = 2, lwd = 2, bty = "n", cex = 0.8)
    }
  }

  return(list(
    mcmc_results = mcmc_list,
    Qmax_mean = Qmax_mean,
    b_mean = b_mean,
    intercept_mean = intercept_mean,
    intercept_sd = intercept_sd,
    intercept_ci = intercept_ci,
    slope_mean = slope_mean,
    slope_sd = slope_sd,
    slope_ci = slope_ci,
    gelman_diag = gelman_diag,
    mcmc_summary = summary(main_chain)
  ))
}
