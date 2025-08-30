#' Reestimate the sample size for the adaptive design in bioequivalence (BE) studies using mixed criterion.
#' 
#' This function reestimates the sample size using mixed criterion required for target power, using binary search. The power (passing rate) function of mixed criterion testing lacks a closed-form expression. Thus, sample size (re-)estimation requires a binary search, after identifying an `n` where the passing rate exceeds the desired level.
#' @author Daeyoung Lim, \email{Daeyoung.Lim@fda.hhs.gov}
#' @param n The number of donors in each simulation.
#' @param r The number of replicates from each donor for each simulated dataset.
#' @param S_WR The estimated standard deviation of the reference measurements. The reference-scaled average bioequivalence approach is used if S_WR > 0.249 and the average bioequivalence approach otherwise.
#' @param params (Optional) The list of true parameters to be assumed in data generation.
#' 
#' + `sigma_W0` - A regulatory constant set by the FDA. Defaults to 0.25.
#' + `GMR` - The geometric mean ratio of the test and reference values of the pharmacokinetic measures (e.g., Jmax or AUC). If the test-formulation measure is greater than that of the reference formulation, then GMR is typically set to 1.05, which is the initial value of this function. If the reference-formulation measure is bigger, then GMR is typically 0.95. Defaults to 0.95.
#' + `m` - Another regulatory constant that determines the bounds within which the estimated GMR should fall for bioequivalence to be established. Defaults to 1.25, representing 80-125% average BE limits, which is the FDA recommendation.
#' + `sig_level` - The significance level (alpha-level).
#' + `nmax` - The upper limit for sample size reestimation. If the sample size exceeds `nmax` inside estimation procedure, the function will return `nmax`.
#' + `target_power` - The threshold for power (or passing rate) for a hypothesis test to be considered powerful. Typically set at 80% and defaults to 0.8.
#' 
#' @param nsim (Optional) The number of total simulations to be conducted. Defaults to 1,000.
#' @param ncores (Optional) The number of CPU cores to use for parallel processing (OpenMP). If R hasn't been installed with OpenMP configured, this will not take effect. When OpenMP is available, it should not exceed the number of existing cores. If unspecified, it will default to 2 cores or the number of existing cores, whichever is smaller.
#' 
#' @return A list of lists
#' 
#' + `parameters` - A list of true parameter settings.
#' + `rss` - The reestimated sample size.
#' + `runtime` - The total elapsed time charged for the execution of the program.
#' 
#' @examples
#' out <- rss(10, 6, S_WR = 0.22, nsim = 2)
#' 
#' @references
#' Potvin, D., DiLiberti, C. E., Hauck, W. W., Parr, A. F., Schuirmann, D. J., & Smith, R. A. (2008). Sequential design approaches for bioequivalence studies with crossover designs. Pharmaceutical Statistics: The Journal of Applied Statistics in the Pharmaceutical Industry, 7(4), 245-262.
#' Lim, D., Rantou, E., Kim, J., Choi, S., Choi, N. H., & Grosser, S. (2023). Adaptive designs for IVPT data with mixed scaled average bioequivalence. Pharmaceutical Statistics, 22(6), 1116-1134.
#' 
#' @md
#' @export
rss <- function(n, r, S_WR, params = list(), nsim = 1000, ncores = NULL) {
    parameters <- list(sigma_W0 = 0.25, GMR = 0.95,
                       m = 1.25, sig_level = 0.05, nmax = 100, target_power = 0.8)
    parameters[names(params)] <- params
    sigma_W0 <- parameters$sigma_W0
    GMR <- parameters$GMR
    m <- parameters$m
    sig_level <- parameters$sig_level
    nmax <- parameters$nmax
    target_power <- parameters$target_power

    if (!is.null(ncores)) {
        ncores_ <- parallel::detectCores()
        if (ncores > ncores_) {
            stop(paste0("The number of cores cannot exceed ", ncores_))
        }
    } else {
        ncores <- min(2, parallel::detectCores())
    }

    runtime <- system.time({
        fout <- .Call(
            `_adaptIVPT_reestimate_samplesize`,
            as.integer(n),
            as.integer(r),
            as.double(log(GMR)),
            as.double(S_WR^2),
            as.double(sig_level),
            as.double(m),
            as.double(sigma_W0),
            as.double(target_power),
            as.integer(nmax),
            as.integer(nsim),
            as.integer(ncores))
    })[3]
    list(parameters = parameters,
         rss = fout,
         runtime = runtime)
}