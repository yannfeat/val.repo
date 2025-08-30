#' Compute the passing rate for the mixed scaling approach in bioequivalence (BE) studies
#' 
#' This function runs Monte Carlo simulations to compute the passing rate (PR) of the mixed scaling (MS) approach.
#' @author Daeyoung Lim, \email{Daeyoung.Lim@fda.hhs.gov}
#' @param n The number of donors in each simulation.
#' @param r The number of replicates from each donor for each simulated dataset.
#' @param params (Optional) The list of true parameters to be assumed in data generation.
#' 
#' + `sigma_W0` - A regulatory constant set by the FDA. Defaults to 0.25.
#' + `sigma_WT` - The true standard deviation of the test formulation population.
#' + `sigma_WR` - The true standard deviation of the reference formulation population.
#' + `GMR` - The geometric mean ratio of the test and reference values of the pharmacokinetic measures (e.g., Jmax or AUC). If the test-formulation measure is greater than that of the reference formulation, then GMR is typically set to 1.05, which is the initial value of this function. If the reference-formulation measure is bigger, then GMR is typically 0.95. Defaults to 0.95.
#' + `m` - Another regulatory constant that determines the bounds within which the estimated GMR should fall for bioequivalence to be established. Defaults to 1.25, representing 80-125% average BE limits, which is the FDA recommendation.
#' + `sig_level` - The significance level (alpha-level). Defaults to 0.05.
#' 
#' @param nsim (Optional) The number of total simulations to be conducted. Defaults to 1,000.
#' @param ncores (Optional) The number of CPU cores to use for parallel processing (OpenMP). If R hasn't been installed with OpenMP configured, this will not take effect. When OpenMP is available, it should not exceed the number of existing cores. If unspecified, it will default to 2 cores or the number of existing cores, whichever is smaller.
#' @references
#' Davit, B. M., Chen, M. L., Conner, D. P., Haidar, S. H., Kim, S., Lee, C. H., Lionberger, R. A., Makhlouf, F. T., Nwakama, P. E., Patel, D. T., Schuirmann, D. J., & Yu, L. X. (2012). Implementation of a reference-scaled average bioequivalence approach for highly variable generic drug products by the US Food and Drug Administration. The AAPS journal, 14(4), 915-924.
#' Lim, D., Rantou, E., Kim, J., Choi, S., Choi, N. H., & Grosser, S. (2023). Adaptive designs for IVPT data with mixed scaled average bioequivalence. Pharmaceutical Statistics, 22(6), 1116-1134.
#' 
#' @return A list of lists
#' 
#' + `parameters` - A list of true parameter settings.
#' + `passing_rate` - The estimated passing rate.
#' + `runtime` - The total elapsed time charged for the execution of the program.
#' 
#' @examples
#' out <- prms(10, 6, nsim = 2)
#' 
#' @md
#' @export
prms <- function(n, r, params = list(), nsim = 1000, ncores = NULL) {
    parameters <- list(sigma_W0 = 0.25, sigma_WT = 0.5, sigma_WR = 0.5, GMR = 0.95,
                       m = 1.25, sig_level = 0.05)
    parameters[names(params)] <- params
    sigma_W0 <- parameters$sigma_W0
    sigma_WR <- parameters$sigma_WR
    sigma_WT <- parameters$sigma_WT
    GMR <- parameters$GMR
    m <- parameters$m
    sig_level <- parameters$sig_level

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
            `_adaptIVPT_prms`,
            as.integer(n),
            as.integer(r),
            as.double(sigma_WT^2),
            as.double(sigma_WR^2),
            as.double(log(GMR)),
            as.double(m),
            as.double(sigma_W0),
            as.double(sig_level),
            as.integer(nsim),
            as.integer(ncores))
    })[3]
    list(parameters = parameters,
         passing_rate = fout,
         runtime = runtime)
}