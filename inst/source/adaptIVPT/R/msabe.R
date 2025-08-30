#' Run the mixed scaling approach in bioequivalence (BE) studies
#' 
#' This function runs hypothesis testing for bioequivalence using the mixed criterion
#' @author Daeyoung Lim, \email{Daeyoung.Lim@fda.hhs.gov}
#' @param Test An n-by-r matrix of test product data. `n` is the number of donors and `r` is the number of skin section replicates.
#' @param Reference An n-by-r matrix of reference product data.
#' @param params (Optional) The list of tuning parameters for running the test.
#' 
#' + `sigma_W0` - A regulatory constant set by the FDA. Defaults to 0.25.
#' + `m` - Another regulatory constant that determines the bounds within which the estimated GMR should fall for bioequivalence to be established. Defaults to 1.25, representing 80-125% average BE limits, which is the FDA recommendation.
#' + `sig_level` - The significance level (alpha-level).
#' 
#' @references
#' Davit, B. M., Chen, M. L., Conner, D. P., Haidar, S. H., Kim, S., Lee, C. H., Lionberger, R. A., Makhlouf, F. T., Nwakama, P. E., Patel, D. T., Schuirmann, D. J., & Yu, L. X. (2012). Implementation of a reference-scaled average bioequivalence approach for highly variable generic drug products by the US Food and Drug Administration. The AAPS journal, 14(4), 915-924.
#' Lim, D., Rantou, E., Kim, J., Choi, S., Choi, N. H., & Grosser, S. (2023). Adaptive designs for IVPT data with mixed scaled average bioequivalence. Pharmaceutical Statistics, 22(6), 1116-1134.
#' 
#' @return A list of lists
#' 
#' + `parameters` - A list of true parameter settings.
#' + `fout` - The test result and related estimators.
#' + `runtime` - The total elapsed time charged for the execution of the program.
#' 
#' @examples
#' n <- 6
#' r <- 3
#' Test <- matrix(runif(n*r), nrow = n, ncol = r)
#' Reference <- matrix(runif(n*r), nrow = n, ncol = r)
#' out <- msabe(Test, Reference)
#' 
#' @md
#' @export
msabe <- function(Test, Reference, params = list()) {
    parameters <- list(sigma_W0 = 0.25, m = 1.25, sig_level = 0.05)
    parameters[names(params)] <- params
    sigma_W0 <- parameters$sigma_W0
    m <- parameters$m
    sig_level <- parameters$sig_level

    runtime <- system.time({
        fout <- .Call(
            `_adaptIVPT_msabe`,
            as.matrix(Test),
            as.matrix(Reference),
            as.double(m),
            as.double(sigma_W0),
            as.double(sig_level))
    })[3]
    fout <- drop(fout)
    names(fout) <- c("test", "Ibar", "S2_WR", "S2_I", "lowerbound", "upperbound")
    out <- list(parameters = parameters,
         fout = fout,
         runtime = runtime)

    class(out) <- "msabe"
    out
}

