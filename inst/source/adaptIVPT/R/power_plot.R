#' Plot the passing-rate curve and the passing-rate surface in IVPT
#' 
#' This function plots the power (passing-rate) curve and power (passing-rate) surface of the mixed scaling (MS) approach. A power curve shows the statistical power across different effect sizes. In IVPT studies, the effect size is captured by the difference between the means of log-measurements of the test and reference products (i.e., logGMR). For the passing-rate surface, the corresponding function considers different values of the standard deviation.
#' @author Daeyoung Lim, \email{Daeyoung.Lim@fda.hhs.gov}
#' @param n The number of donors in each simulation.
#' @param r The number of replicates from each donor for each simulated dataset.
#' @param observed_GMR The observed (estimated) GMR of the user's data. Along with the observed sigmaWR, the corresponding passing rate will be displayed in the 3D plot as a vertical line parallel to the z-axis.
#' @param observed_sigmaWR The observed (estimated) sigmaWR of the user's data. Along with the observed GMR, the corresponding passing rate will be displayed in the 3D plot as a vertical line parallel to the z-axis.
#' @param GMR_grid The grid of GMR values to be used for plotting the 3D surface of passing rates.
#' @param sigmaWR_grid The grid of sigmaWR values to be used for plotting the 3D surface of passing rates.
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
#' @param verbose (Optional) A logical value (`TRUE`/`FALSE`) indicating whether to display the progress bar.
#' @param plot (Optional) A logical value (`TRUE`/`FALSE`) indicating whether to generate a 3D interactive plot of the surface. If `FALSE`, the function will return the (x, y, z) values as a `list`.
#' @references
#' Davit, B. M., Chen, M. L., Conner, D. P., Haidar, S. H., Kim, S., Lee, C. H., Lionberger, R. A., Makhlouf, F. T., Nwakama, P. E., Patel, D. T., Schuirmann, D. J., & Yu, L. X. (2012). Implementation of a reference-scaled average bioequivalence approach for highly variable generic drug products by the US Food and Drug Administration. The AAPS journal, 14(4), 915-924.
#' Lim, D., Rantou, E., Kim, J., Choi, S., Choi, N. H., & Grosser, S. (2023). Adaptive designs for IVPT data with mixed scaled average bioequivalence. Pharmaceutical Statistics, 22(6), 1116-1134.
#' 
#' @return A list
#' 
#' + `GMR` - A list of true parameter settings.
#' + `passing_rate` - The estimated passing rate.
#' + `runtime` - The total elapsed time charged for the execution of the program.
#' 
#' @examples
#' out <- PRsurface(6, 3, GMR_grid = c(0.90, 1), sigmaWR_grid = c(0.2, 0.5), nsim = 2, plot = FALSE)
#' 
#' @md
#' @importFrom rgl persp3d plot3d
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
PRsurface <- function(n, r, observed_GMR = 0.95, observed_sigmaWR = 0.294, GMR_grid = seq(0.75, 1.30, length.out = 100), sigmaWR_grid = seq(0.2, 1, length.out = 100),
                      params = list(), nsim = 1000, ncores = NULL, verbose = FALSE, plot = TRUE) {
    parameters <- list(sigma_W0 = 0.25, sigma_WT = 0.5, sigma_WR = 0.5, GMR = 0.95,
                       m = 1.25, sig_level = 0.05)
    parameters[names(params)] <- params
    z <- matrix(0, nrow = length(GMR_grid), ncol = length(sigmaWR_grid))
    if (verbose) {
        pb <- txtProgressBar(style=3)
    }
    iprog <- 0
    nprog <- length(GMR_grid) * length(sigmaWR_grid)
    for (i in 1:length(GMR_grid)) {
        GMR <- GMR_grid[i]
        parameters$GMR <- GMR
        for (j in 1:length(sigmaWR_grid)) {
            sigmaWR <- sigmaWR_grid[j]
            parameters$sigma_WR <- parameters$sigma_WT <- sigmaWR
            z[i,j] <- prms(n, r, parameters, nsim, ncores)$passing_rate
            if (verbose) {
                iprog <- iprog + 1
                setTxtProgressBar(pb, iprog / nprog)
            }
        }
    }
    if (verbose) {
        close(pb)
    }
    if (plot) {
        persp3d(GMR_grid, sigmaWR_grid, z, main="Passing Rate Surface",
                zlab = "Passing Rate", xlab="GMR", ylab=expression(sigma[WR]),
                theta = 30, phi = 15, col = "orange", shade = 0.4)
        ml <- max(length(GMR_grid), length(sigmaWR_grid))
        observed_pr <- prms(n, r, list(sigma_WR=observed_sigmaWR, sigma_WT = observed_sigmaWR, GMR = observed_GMR))$passing_rate
        plot3d(rep(observed_GMR, ml), rep(observed_sigmaWR, ml), seq(0, observed_pr, length.out = ml), type = 'l', col="red", lwd = 2, add = TRUE)
    }
    invisible(list(GMR = GMR_grid, sigmaWR = sigmaWR_grid, z = z))
}