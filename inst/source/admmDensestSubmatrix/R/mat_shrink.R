#' Soft threshholding operator.
#'
#' Applies the shrinkage operator for singular value tresholding.
#' @param K matrix
#' @param tau regularization parameter
#' @return Matrix
#' @export
#' @examples
#' mat_shrink(matrix(c(1,0,0,0,1,1,1,1,1), nrow=3, ncol=3, byrow=TRUE),0.35)
#

mat_shrink <- function (K, tau){

  r <- dim(K)[1]
  c <- dim(K)[2]

  s <- svd(K, nu=r, nv=c)
  # Test GITHUB CHANGES.
  L <- pmax(s$d-tau,0)

  if (r < c) {
    K <- s$u %*% diag(L) %*% t(s$v[,1:r])
  } else {
    K <- s$u[,1:c] %*% diag(L) %*% t(s$v)
  }
  return(K)
  #  return(list(K=K,L=L, s = s))
}
