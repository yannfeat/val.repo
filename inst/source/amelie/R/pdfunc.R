#' Compute the probability density function of a matrix of features.
#'
#' @param x A matrix of numeric features.
#' @param univariate Logical indicating whether the univariate pdf should be
#' computed.
#'
#' @return A vector with values of the density function.
#'
#' @details
#'
#' \code{pdfunc} computes univariate or multivariate probabilities for a set of
#' observations.
#'
#' All columns of a row are used in computing the pdf.
#'
#' Variance and covariance are computed using \code{var} and \code{cov}, where
#' the denominator \code{n-1} is used.
#'
#' @examples
#' dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
#' pdfunc(dmat,TRUE)
#'
#'#'@importFrom stats cov

#' @rdname pdfunc
#' @export
pdfunc <- function(x, univariate = TRUE) {

  #check univariate
  if (!is.logical(univariate)) stop("univariate must be logical.")

  #compute mean and sd
  x_mean <- .mean2(x)

  if (univariate == TRUE) {
    x_var <- .var2(x)
    .univariate_pdf(x,x_mean,x_var)
  } else {
    x_sd <- cov(x)
    .multivariate_pdf(x,x_mean,x_sd)
  }
}
