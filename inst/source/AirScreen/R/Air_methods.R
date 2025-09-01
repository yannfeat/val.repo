
#' Generate normal samples (Compound Symmetry)
#'
#' \code{rnormCS} efficiently generates samples from a multivariate
#' normal distribution with compound symmetry correlation structure (all
#' features equally correlated).
#'
#' @param n Number of observations.
#' @param p Number of features.
#' @param rho Common correlation coefficient.
#' @param means Numeric vector of feature means (length \eqn{1} or \eqn{p}).
#' @param variances Numeric vector of feature variances (length \eqn{1} or
#' \eqn{p}).
#' @return A numeric \eqn{n \times p} matrix with the specified correlation,
#' means, and variances.
#' @examples
#' X1 <- rnormCS(10, 5)
#' X2 <- rnormCS(10, 5, rho = 0.3, means = 2, variances = 4)
#' X3 <- rnormCS(10, 5, rho = 0.4, means = 1:5, variances = 3:7)
#' @importFrom stats rnorm
#' @export
rnormCS <- function(n, p, rho = 0.5,
                        means = 0, variances = 1) {
  ## processing inputs
  if (length(means) == 1) {
    means <- rep(means, p)
  } else if (length(means) != p) {
    stop("means must be length 1 or p")
  }
  if (length(variances) == 1) {
    variances <- rep(variances, p)
  } else if (length(variances) != p) {
    stop("variances must be length 1 or p")
  }

  ## generating standardized compound-symmetry normal samples
  z <- rnorm(n) * sqrt(rho)
  Z <- matrix(rnorm(n * p), n, p) * sqrt(1 - rho)
  X0 <- z + Z

  ## transforming samples
  X1 <- sweep(X0, 2, sqrt(variances), FUN = "*")
  X2 <- sweep(X1, 2, means, FUN = "+")
  X2
}


#' Generate normal samples (Autoregressive AR(1) correlation)
#'
#' \code{rnormAR1} efficiently generates samples from a multivariate
#' normal distribution with AR(1)
#' correlation \eqn{cor(x_i, x_j) = \rho^{\lvert i-j \rvert}}.
#'
#' @param n Number of observations.
#' @param p Number of variables.
#' @param rho Autoregressive correlation coefficient.
#' @param means Numeric vector of feature means (length \eqn{1} or \eqn{p}).
#' @param variances Numeric vector of feature variances (length \eqn{1} or
#' \eqn{p}).
#' @return A numeric \eqn{n \times p} matrix with the specified correlation,
#' means, and variances.
#' @importFrom stats filter
#' @examples
#' X1 <- rnormAR1(10, 5)
#' X2 <- rnormAR1(10, 5, rho = 0.3, means = 2, variances = 4)
#' X3 <- rnormAR1(10, 5, rho = 0.4, means = 1:5, variances = 3:7)
#' @importFrom stats rnorm
#' @export
rnormAR1 <- function(n, p, rho = 0.5,
                         means = 0, variances = 1) {
  ## processing inputs
  if (length(means) == 1) {
    means <- rep(means, p)
  } else if (length(means) != p) {
    stop("`means` must be length 1 or length p")
  }
  if (length(variances) == 1) {
    variances <- rep(variances, p)
  } else if (length(variances) != p) {
    stop("`variances` must be length 1 or length p")
  }

  ## generating standardized AR(1) normals
  U <- matrix(rnorm(n * p), n, p)
  if (p > 1) {
    U[, -1] <- U[, -1] * sqrt(1 - rho^2)
    X0 <- t(apply(U, 1, function(x) stats::filter(x, filter = rho,
                                                  method = "recursive")))
  } else {
    X0 <- matrix(U, ncol = 1)
  }

  ## transforming samples
  X1 <- sweep(X0, 2, sqrt(variances), FUN = "*")
  X2 <- sweep(X1, 2, means, FUN = "+")
  X2
}


#' Newton-Raphson root finder
#'
#' @param f Function whose root is sought.
#' @param fp Derivative function of \code{f}.
#' @param x Numeric starting value.
#' @param tol Convergence tolerance (default \code{1e-3}).
#' @param m Maximum number of iterations (default \eqn{100}).
#' @return The estimated root.
#' @details
#' Iterates \eqn{x_{new} = x - f(x)/f'(x)} until the change is below \code{tol}
#' or \code{m} iterations are reached (then issues a warning).
#' @examples
#' # Solve x^2 - 2 = 0
#' newton(function(x) x^2 - 2, function(x) 2*x, 1)
#' @export
newton <- function(f, fp, x, tol = 1e-3, m = 100) {
  iter <- 0
  oldx <- x
  x    <- oldx + 10 * tol

  while (abs(x - oldx) > tol) {
    iter <- iter + 1
    if (iter > m) {
      warning("Maximum iterations (m = ", m,
              ") reached: returning last estimate", call. = FALSE)
      break
    }
    oldx <- x
    fx   <- as.numeric(f(oldx))
    fpx  <- as.numeric(fp(oldx))
    x    <- oldx - fx / fpx
  }

  return(as.numeric(x))
}


#' Adaptive Iterative Ridge HOLP Screening
#'
#' This function ranks features with the Adaptive Iterative Ridge
#' High-dimensional Ordinary Least-squares Projection (Air-HOLP) method of
#' Joudah *et al.* (2025) and returns both the per-feature ranks and the
#' ordered feature indices. AirHOLP is intended for the high-dimensional
#' case \eqn{p \ge n}. When \eqn{n > p}, use \code{\link{AirOLS}} instead.
#'
#' @details
#' The \code{Threshold} parameter controls how many coefficients are kept at each
#' iteration of the adaptive-penalty procedure.
#' The default value \eqn{\lceil n/\log(n)\rceil} performs well in most
#' settings; changing it can reduce stability, so we recommend keeping the
#' default unless you have a specific reason to adjust it.
#' The parameters Lambda, \code{Un}, and \code{XUn} are helpful to run
#' \code{AirHOLP} on \eqn{2} or more different \code{y} vectors for the
#' same \code{X} (to avoid repeated heavy computations).
#'
#' @param X Numeric predictor matrix of dimension \eqn{n \times p}.
#' @param y Numeric response vector of length \eqn{n}.
#' @param Threshold Integer specifying the number of coefficients retained at
#'   each adaptive-penalty iteration (default \eqn{n/\log(n)} capped at
#'   \eqn{p-1}).
#' @param r0 Numeric initial ridge penalty (default \eqn{10}).
#' @param adapt Logical; set to \code{TRUE} (default) to enable adaptive penalty
#'   selection.
#' @param iter Integer; maximum number of iterations for adaptive-penalty
#'   selection (default \eqn{10}).
#' @param Lambda Eigenvalues of \eqn{XX^T}, if missing the function will compute it.
#' @param Un Eigenvectors of \eqn{XX^T}, if missing the function will compute it.
#' @param XUn \code{X} transpose times \code{Un}, if missing the function will
#' compute it.
#'
#' @return An object of class \code{AirResult} containing
#' \describe{
#'   \item{\code{order_r}}{Integer vector of feature indices sorted by absolute
#'     Air-HOLP score, from largest to smallest.}
#'   \item{\code{index_r}}{Integer vector of feature ranks matching
#'     \code{order_r}.}
#'   \item{\code{Beta_r}}{Numeric vector of Air-HOLP coefficient estimates.}
#'   \item{\code{r}}{Final ridge-penalty value used.}
#'   \item{\code{iter_last}}{Number of iterations performed for adaptive
#'     penalty selection.}
#' }
#'
#' @examples
#' # Example 1 (default parameters)
#' set.seed(314)
#' X <- matrix(rnorm(10000), nrow = 50, ncol = 200)
#' y <- X[, 1] + X[, 10] + rnorm(50)
#' result <- AirHOLP(X, y)
#' str(result)
#' result$order_r[1:7] # the top 7 features
#' result$index_r[c(1, 10),] # ranks of the true features (x1, and x10)
#'
#' # Example 2 (multiple responses, same X)
#' set.seed(314)
#' X <- matrix(rnorm(2000000), nrow = 1000, ncol = 2000)
#' y1 <- X[, 1] + X[, 2] + 6*rnorm(1000)
#' y2 <- X[, 1] - X[, 2] + 12*rnorm(1000)
#' y3 <- X[, 1] + X[, 2] - X[, 3] + 3*rnorm(1000)
#' y4 <- X[, 1] - X[, 2] + X[, 3] + 9*rnorm(1000)
#' XXT <- tcrossprod(X)
#' eXXT <- eigen(XXT)
#' Lambda <- eXXT$values
#' Un <- eXXT$vectors
#' XUn <- crossprod(X,Un)
#' result1 <- AirHOLP(X, y1, Lambda = Lambda, Un = Un, XUn = XUn)
#' result1$order_r[1:7] # the top 7 features
#' result1$index_r[1:2,] # ranks of the true features (x1 and x2)
#' result2 <- AirHOLP(X, y2, Lambda = Lambda, Un = Un, XUn = XUn)
#' result2$order_r[1:7] # the top 7 features
#' result2$index_r[1:2,] # ranks of the true features (x1 and x2)
#' result3 <- AirHOLP(X, y3, Lambda = Lambda, Un = Un, XUn = XUn)
#' result3$order_r[1:7] # the top 7 features
#' result3$index_r[1:3,] # ranks of the true features (x1, x2, and x3)
#' result4 <- AirHOLP(X, y4, Lambda = Lambda, Un = Un, XUn = XUn)
#' result4$order_r[1:7] # the top 7 features
#' result4$index_r[1:3,] # ranks of the true features (x1, x2, and x3)
#'
#' # Example 3 (multiple fixed penalties)
#' set.seed(314)
#' X <- matrix(rnorm(10000), nrow = 100, ncol = 200)
#' y <- X[, 1] - X[, 2] + X[, 3] + 2*rnorm(100)
#' result <- AirHOLP(X, y, r0 = c(1, 100, 10000), adapt = FALSE)
#' str(result)
#' result$order_r0[1:7,] # the top 7 features (for each penalty)
#' result$index_r0[1:3,] # ranks of the true features (x1, x2, and x3)
#'
#'
#' @references
#' Joudah, I., Muller, S., and Zhu, H. (2025).
#' "Air-HOLP: Adaptive Regularized Feature Screening for High-Dimensional Data."
#' *Statistics and Computing*. \doi{10.1007/s11222-025-10599-6}
#'
#' @export
AirHOLP <- function(X, y, Threshold = min(ncol(X)-1, ceiling(nrow(X)/log(nrow(X)))),
                    r0 = 10, adapt = TRUE, iter = 10, Lambda, Un, XUn) {

  n <- nrow(X) # sample size
  p <- ncol(X) # number of features
  q <- length(r0) # number of penalty parameters
  iter_temp2 <- 0*(1:q) # used for calculating iter_last
  iter_temp1 <- iter_temp2 - 1 # used for calculating iter_last

  # Standardizing X and y:
  X <- as.matrix(scale(X))
  y <- as.vector(y)

  if(adapt){
    # Main computations:
    if(missing(Lambda)|missing(Un)){
      XXT <- tcrossprod(X)
      eXXT <- eigen(XXT)
      Lambda <- eXXT$values
      Un <- eXXT$vectors
    }
    if(missing(XUn)){
      XUn <- crossprod(X,Un)
    }
    UnTy <- crossprod(Un,y)
    yUnD2UnTy <- UnTy^2*(Lambda^2)

    # Penalty selection:
    r_max <- 1000*sqrt(n) # maximum penalty
    max.iter <- 30 # maximum number of iterations for Newtons method
    index_r <- matrix(1:(p*q), nrow = p, ncol = q)
    index_r0 <- index_r
    order_r <- index_r
    order_r0 <- index_r
    Beta_r <- index_r
    Beta_r0 <- index_r
    r <- r0
    r_temp <- r0
    for (j in 1:iter) {
      for (i in 1:q) {
        # Initial screening:
        Beta_temp <- XUn%*%((Lambda+r[i])^(-1)*UnTy)
        index_temp <- match(1:p,rank(-abs(Beta_temp), ties.method = c("first")))
        if(j<2) {
          Beta_r0[,i] <- Beta_temp
          index_r0[,i] <- rank(-abs(Beta_temp), ties.method = c("first"))
          order_r0[,i] <- order(-abs(Beta_temp))
        }

        # Estimating the expected response:
        Xs <- X[,index_temp[1:Threshold]]
        ys <- Xs%*%(solve(crossprod(Xs) +
                            diag(Threshold)*10^-12)%*%crossprod(Xs,y))

        # MSE functions:
        ysUnDUnTy <- t(crossprod(ys,Un)*Lambda)*UnTy
        Z <- function(lam) { # The function we minimize
          t((Lambda+lam)^-2)%*%yUnD2UnTy - 2*t((Lambda+lam)^-1)%*%ysUnDUnTy
        }
        Z1 <- function(lam) { # First derivative
          -2*t((Lambda+lam)^-3)%*%yUnD2UnTy + 2*t((Lambda+lam)^-2)%*%ysUnDUnTy
        }
        Z2 <- function(lam) { # Second derivative
          6*t((Lambda+lam)^-4)%*%yUnD2UnTy - 4*t((Lambda+lam)^-3)%*%ysUnDUnTy
        }

        # MSE minimization:
        sol <- newton(Z1, Z2, 0.0001, tol = 0.001, m = max.iter)
        r[i] <- sol
        if(r[i] > r_max) {r[i] <- r_max}
        if(r[i] < 0.0001) {r[i] <- 0.0001}
        if(Z(r_max) < Z(r[i])) {r[i] <- r_max} # Checking boundaries
        if(Z(0.0001) < Z(r[i])) {r[i] <- 0.0001}

        # Feature screening:
        Beta_r[,i] <- XUn%*%((Lambda+r[i])^(-1)*UnTy)
        index_r[,i] <- rank(-abs(Beta_r[,i]), ties.method = c("first"))
        order_r[,i] <- order(-abs(Beta_r[,i]))

        # Calculations for the number of iterations:
        if(abs(r[i] - r_temp[i]) < 0.01*r[i]){ # Checking relative error
          iter_temp1[i] <- j
          iter_temp2[i] <- iter_temp2[i] + 1
        }
      }
      if(sum(abs(r - r_temp) < 0.01*r) == q){ # Checking relative error
        break
      }
      r_temp <- r
    }
    iter_last <- iter_temp1 - iter_temp2 + 1 # Number of iterations
    AirHOLP <- list(index_r = index_r, index_r0 = index_r0, order_r = order_r,
                    order_r0 = order_r0, Beta_r = Beta_r, Beta_r0 = Beta_r0,
                    r = r, iter_last = iter_last)
  } else{
    if(q < 2) {
      # Feature screening:
      if(missing(Lambda)|missing(Un)){
        Beta_r0 <- crossprod(X, solve(tcrossprod(X)+r0*diag(n),y))
      } else{
        UnTy <- crossprod(Un,y)
        Beta_r0 <- XUn%*%((Lambda+r0)^(-1)*UnTy)
      }
      index_r0 <- rank(-abs(Beta_r0), ties.method = c("first"))
      order_r0 <- order(-abs(Beta_r0))
      AirHOLP <- list(index_r0 = index_r0, order_r0 = order_r0,
                      Beta_r0 = Beta_r0)
    } else{
      # Main computations:
      if(missing(Lambda)|missing(Un)){
        XXT <- tcrossprod(X)
        eXXT <- eigen(XXT)
        Lambda <- eXXT$values
        Un <- eXXT$vectors
      }
      if(missing(XUn)){
        XUn <- crossprod(X,Un)
      }
      UnTy <- crossprod(Un,y)

      # Feature screening:
      index_r0 <- matrix(1:(p*q), nrow = p, ncol = q)
      order_r0 <- index_r0
      Beta_r0 <- index_r0
      for (i in 1:q) {
        Beta_r0[,i] <- XUn%*%((Lambda+r0[i])^(-1)*UnTy)
        index_r0[,i] <- rank(-abs(Beta_r0[,i]), ties.method = c("first"))
        order_r0[,i] <- order(-abs(Beta_r0[,i]))
      }
      AirHOLP <- list(index_r0 = index_r0, order_r0 = order_r0,
                      Beta_r0 = Beta_r0)
    }
  }
}


#' Adaptive Iterative Ridge OLS Screening
#'
#' This function ranks features with the Adaptive Iterative Ridge Ordinary
#' Least Squares (Air-OLS) method of Joudah *et al.* (2025) and returns both
#' the per-feature ranks and the ordered feature indices. AirHOLP is intended
#' for the high-dimensional case \eqn{n \ge p}. When \eqn{p > n}, use
#' \code{\link{AirHOLP}} instead.
#'
#' @details
#' The \code{Threshold} parameter controls how many coefficients are kept at each
#' iteration of the adaptive-penalty procedure.
#' The default value \eqn{\lceil n/\log(n)\rceil} performs well in most
#' settings; changing it can reduce stability, so we recommend keeping the
#' default unless you have a specific reason to adjust it.
#' The parameters Lambda, \code{Up}, and \code{XUp} are helpful to run
#' \code{AirOLS} on \eqn{2} or more different \code{y} vectors for the
#' same \code{X} (to avoid repeated heavy computations).
#'
#' @param X Numeric predictor matrix of dimension \eqn{n \times p}.
#' @param y Numeric response vector of length \eqn{n}.
#' @param Threshold Integer specifying the number of coefficients retained at
#'   each adaptive-penalty iteration (default \eqn{n/\log(n)} capped at
#'   \eqn{p-1}).
#' @param r0 Numeric initial ridge penalty (default \eqn{10}).
#' @param adapt Logical; set to \code{TRUE} (default) to enable adaptive penalty
#'   selection.
#' @param iter Integer; maximum number of iterations for adaptive-penalty
#'   selection (default \eqn{10}).
#' @param Lambda Eigenvalues of \eqn{X^T X}, if missing the function will compute it.
#' @param Up Eigenvectors of \eqn{X^T X}, if missing the function will compute it.
#' @param XUp \code{X} times \code{Up}, if missing the function will compute it.
#'
#' @return An object of class \code{AirResult} containing
#' \describe{
#'   \item{\code{order_r}}{Integer vector of feature indices sorted by absolute
#'     Air-OLS score, from largest to smallest.}
#'   \item{\code{index_r}}{Integer vector of feature ranks matching
#'     \code{order_r}.}
#'   \item{\code{Beta_r}}{Numeric vector of Air-OLS coefficient estimates.}
#'   \item{\code{r}}{Final ridge-penalty value used.}
#'   \item{\code{iter_last}}{Number of iterations performed for adaptive
#'     penalty selection.}
#' }
#'
#' @examples
#' # Example 1 (default parameters)
#' set.seed(314)
#' X <- matrix(rnorm(10000), nrow = 200, ncol = 50)
#' y <- X[, 1] + X[, 10] + 2*rnorm(200)
#' result <- AirOLS(X, y)
#' str(result)
#' result$order_r[1:7] # the top 7 features
#' result$index_r[c(1, 10),] # ranks of the true features (x1, and x10)
#'
#' # Example 2 (multiple responses, same X)
#' set.seed(314)
#' X <- matrix(rnorm(2000000), nrow = 2000, ncol = 1000)
#' y1 <- X[, 1] + X[, 2] + 6*rnorm(2000)
#' y2 <- X[, 1] - X[, 2] + 12*rnorm(2000)
#' y3 <- X[, 1] + X[, 2] - X[, 3] + 5*rnorm(2000)
#' y4 <- X[, 1] - X[, 2] + X[, 3] + 10*rnorm(2000)
#' XTX <- crossprod(X)
#' eXTX <- eigen(XTX)
#' Lambda <- eXTX$values
#' Up <- eXTX$vectors
#' XUp <- X%*%Up
#' result1 <- AirOLS(X, y1, Lambda = Lambda, Up = Up, XUp = XUp)
#' result1$order_r[1:7] # the top 7 features
#' result1$index_r[1:2,] # ranks of the true features (x1 and x2)
#' result2 <- AirOLS(X, y2, Lambda = Lambda, Up = Up, XUp = XUp)
#' result2$order_r[1:7] # the top 7 features
#' result2$index_r[1:2,] # ranks of the true features (x1 and x2)
#' result3 <- AirOLS(X, y3, Lambda = Lambda, Up = Up, XUp = XUp)
#' result3$order_r[1:7] # the top 7 features
#' result3$index_r[1:3,] # ranks of the true features (x1, x2, and x3)
#' result4 <- AirOLS(X, y4, Lambda = Lambda, Up = Up, XUp = XUp)
#' result4$order_r[1:7] # the top 7 features
#' result4$index_r[1:3,] # ranks of the true features (x1, x2, and x3)
#'
#' # Example 3 (multiple fixed penalties)
#' set.seed(314)
#' X <- matrix(rnorm(10000), nrow = 200, ncol = 100)
#' y <- X[, 1] - X[, 2] + X[, 3] + 3*rnorm(200)
#' result <- AirOLS(X, y, r0 = c(1, 100, 10000), adapt = FALSE)
#' str(result)
#' result$order_r0[1:7,] # the top 7 features for each penalty
#' result$index_r0[1:3,] # ranks of the true features (x1, x2, and x3)
#'
#'
#' @references
#' Joudah, I., Muller, S., and Zhu, H. (2025).
#' "Air-HOLP: Adaptive Regularized Feature Screening for High-Dimensional Data."
#' *Statistics and Computing*. \doi{10.1007/s11222-025-10599-6}
#'
#' @export
AirOLS <- function(X, y, Threshold = min(ncol(X)-1, ceiling(nrow(X)/log(nrow(X)))),
                   r0 = 10, adapt = TRUE, iter = 10, Lambda, Up, XUp) {

  n <- nrow(X) # sample size
  p <- ncol(X) # number of features
  q <- length(r0) # number of penalty parameters
  iter_temp2 <- 0*(1:q) # used for calculating iter_last
  iter_temp1 <- iter_temp2 - 1 # used for calculating iter_last

  # Standardizing X and y:
  X <- as.matrix(scale(X))
  y <- as.vector(y)

  if(adapt){
    # Main computations:
    if(missing(Lambda)|missing(Up)){
      XTX <- crossprod(X)
      eXTX <- eigen(XTX)
      Lambda <- eXTX$values
      Up <- eXTX$vectors
    }
    if(missing(XUp)){
      XUp <- X%*%Up
    }
    UpXTy <- crossprod(Up, crossprod(X,y))
    yUpDUpXTy <- UpXTy^2*Lambda

    # Penalty selection:
    r_max <- 1000*sqrt(n) # maximum penalty
    max.iter <- 30 # maximum number of iterations for Newtons method
    index_r <- matrix(1:(p*q), nrow = p, ncol = q)
    index_r0 <- index_r
    order_r <- index_r
    order_r0 <- index_r
    Beta_r <- index_r
    Beta_r0 <- index_r
    r <- r0
    r_temp <- r0
    for (j in 1:iter) {
      for (i in 1:q) {
        # Initial screening:
        Beta_temp <- Up%*%((Lambda+r[i])^(-1)*UpXTy)
        index_temp <- match(1:p,rank(-abs(Beta_temp), ties.method = c("first")))
        if(j<2) {
          Beta_r0[,i] <- Beta_temp
          index_r0[,i] <- rank(-abs(Beta_temp), ties.method = c("first"))
          order_r0[,i] <- order(-abs(Beta_temp))
        }

        # Estimating the expected response:
        Xs <- X[,index_temp[1:Threshold]]
        ys <- Xs%*%(solve(crossprod(Xs) +
                            diag(Threshold)*10^-12)%*%crossprod(Xs,y))

        # MSE functions:
        ysXUpUpXTy <- t(crossprod(crossprod(X,ys),Up))*UpXTy
        Z <- function(lam) { # The function we minimize
          t((Lambda+lam)^-2)%*%yUpDUpXTy - 2*t((Lambda+lam)^-1)%*%ysXUpUpXTy
        }
        Z1 <- function(lam) { # First derivative
          -2*t((Lambda+lam)^-3)%*%yUpDUpXTy + 2*t((Lambda+lam)^-2)%*%ysXUpUpXTy
        }
        Z2 <- function(lam) { # Second derivative
          6*t((Lambda+lam)^-4)%*%yUpDUpXTy - 4*t((Lambda+lam)^-3)%*%ysXUpUpXTy
        }

        # MSE minimization:
        sol <- newton(Z1, Z2, 0.0001, tol = 0.001, m = max.iter)
        r[i] <- sol
        if(r[i] > r_max) {r[i] <- r_max}
        if(r[i] < 0.0001) {r[i] <- 0.0001}
        if(Z(r_max) < Z(r[i])) {r[i] <- r_max} # Checking boundaries
        if(Z(0.0001) < Z(r[i])) {r[i] <- 0.0001}

        # Feature screening:
        Beta_r[,i] <- Up%*%((Lambda+r[i])^(-1)*UpXTy)
        index_r[,i] <- rank(-abs(Beta_r[,i]), ties.method = c("first"))
        order_r[,i] <- order(-abs(Beta_r[,i]))

        # Calculations for the number of iterations:
        if(abs(r[i] - r_temp[i]) < 0.01*r[i]){ # Checking relative error
          iter_temp1[i] <- j
          iter_temp2[i] <- iter_temp2[i] + 1
        }
      }
      if(sum(abs(r - r_temp) < 0.01*r) == q){ # Checking relative error
        break
      }
      r_temp <- r
    }
    iter_last <- iter_temp1 - iter_temp2 + 1 # Number of iterations
    AirOLS <- list(index_r = index_r, index_r0 = index_r0, order_r = order_r,
                   order_r0 = order_r0, Beta_r = Beta_r, Beta_r0 = Beta_r0,
                   r = r, iter_last = iter_last)
  } else{
    if(q < 2) {
      # Feature screening:
      if(missing(Lambda)|missing(Up)){
        Beta_r0 <- solve(crossprod(X) + r0*diag(p), crossprod(X, y))
      } else{
        UpXTy <- crossprod(Up, crossprod(X,y))
        Beta_r0 <- Up%*%((Lambda+r0)^(-1)*UpXTy)
      }
      index_r0 <- rank(-abs(Beta_r0), ties.method = c("first"))
      order_r0 <- order(-abs(Beta_r0))
      AirOLS <- list(index_r0 = index_r0, order_r0 = order_r0,
                     Beta_r0 = Beta_r0)
    } else{
      # Main computations:
      if(missing(Lambda)|missing(Up)){
        XTX <- crossprod(X)
        eXTX <- eigen(XTX)
        Lambda <- eXTX$values
        Up <- eXTX$vectors
      }
      if(missing(XUp)){
        XUp <- X%*%Up
      }
      UpXTy <- crossprod(Up, crossprod(X,y))

      # Feature screening:
      index_r0 <- matrix(1:(p*q), nrow = p, ncol = q)
      order_r0 <- index_r0
      Beta_r0 <- index_r0
      for (i in 1:q) {
        Beta_r0[,i] <- Up%*%((Lambda+r0[i])^(-1)*UpXTy)
        index_r0[,i] <- rank(-abs(Beta_r0[,i]), ties.method = c("first"))
        order_r0[,i] <- order(-abs(Beta_r0[,i]))
      }
      AirOLS <- list(index_r0 = index_r0, order_r0 = order_r0,
                     Beta_r0 = Beta_r0)
    }
  }
}


#' Unified Interface for \code{\link{AirHOLP}} and \code{\link{AirOLS}}
#'
#' \code{Air} is a high-level wrapper that applies either the
#' \code{\link{AirHOLP}} or \code{\link{AirOLS}} methods based on data
#' dimensions. It returns an \code{AirResult} object ready for inspection with
#' \code{\link{summary}}.
#'
#' @details
#' \itemize{
#'   \item When \code{method = "auto"} (default), \code{\link{AirHOLP}} is used
#'     if \eqn{p \ge n}; otherwise \code{\link{AirOLS}} is chosen.
#'   \item \code{penalty_type} chooses whether the ridge penalty
#'     is selected adaptively, fixed at the supplied value(s), or
#'     both (returning two sets of ranks).
#'   \item Air checks the validity of inputs and substitute by defaults
#'     when invalid.
#' }
#'
#' @param X Numeric predictor matrix (\eqn{n \times p}).
#' @param y Numeric response vector of length \eqn{n}.
#' @param m Integer specifying the number of coefficients retained at each
#'   adaptive-penalty iteration (default \eqn{n/\log(n)} capped at \eqn{p-1}).
#' @param screening_threshold Integer specifying the number of screened
#'   features to display in summary.
#' @param penalty Numeric scalar or vector with ridge penalty value(s)
#'   (default \eqn{10}).
#' @param penalty_type One of \code{"adaptive"}, \code{"fixed"},
#'   or \code{"both"} (default \code{"adaptive"}).
#' @param method \code{"auto"}, \code{"AirHOLP"}, or \code{"AirOLS"}
#'   (default \code{"auto"}).
#' @return An object of class \code{"AirResult"}, a named list that may
#'   contain the following elements (depending on \code{penalty_type}):
#'   \describe{
#'     \item{\code{order}, \code{order_adaptive}, \code{order_fixed}}{Integer
#'       vector of feature indices sorted by absolute Air-HOLP or Air-OLS score,
#'       from largest to smallest.}
#'     \item{\code{rank}, \code{rank_adaptive}, \code{rank_fixed}}{Integer
#'       vector of feature ranks matching \code{order}.}
#'     \item{\code{Beta}, \code{Beta_adaptive}, \code{Beta_fixed}}{Numeric
#'       vector of Air-HOLP or Air-OLS coefficient estimates.}
#'     \item{\code{penalty}, \code{penalty_adaptive}, \code{penalty_fixed}}{
#'       Final ridge penalty value(s) used.}
#'   }
#' The helper \code{\link{summary.AirResult}} prints a concise summary.
#'
#' @examples
#' ## simple example (p > n  ->  AirHOLP)
#' set.seed(314)
#' X <- matrix(rnorm(100000), nrow = 200, ncol = 500)
#' y <- X[, 1] + X[, 2] + X[, 3] + X[, 4] + 3*rnorm(200)
#' result <- Air(X, y, penalty_type = "both")
#' summary(result)
#'
#' ## multiple fixed penalty values
#' result2 <- Air(X, y, penalty_type = "fixed", penalty = c(1, 100))
#' summary(result2)
#'
#' @references
#' Joudah, I., Muller, S., and Zhu, H. (2025).
#' "Air-HOLP: Adaptive Regularized Feature Screening for High-Dimensional Data."
#' *Statistics and Computing*. \doi{10.1007/s11222-025-10599-6}
#'
#' @export
#' @importFrom stats setNames
Air <- function(X, y, m = NULL, screening_threshold = NULL, penalty = 10,
                penalty_type = c("adaptive", "fixed", "both"),
                method = c("auto", "AirHOLP", "AirOLS")) {

  ## ---------- 1. Input checking and coercion ---------------------------------
  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.numeric(X))
    stop("X must contain only numeric values (coercion failed).")

  n <- nrow(X);  p <- ncol(X)

  if (length(y) != n)
    stop("Length of y (response) must equal nrow(X).")
  y <- as.numeric(y)

  ## --- ensure m is a positive integer 1 <= m <= p-1 -----------------------
  if (is.null(m)) {
    m <- min(p - 1, ceiling(n/log(n)))
  }
  if (!is.numeric(m) || length(m) != 1) {
    warning("m must be a single numeric value;
            using default m = min(p - 1, n/log(n)).")
    m <- ceiling(min(p - 1, n/log(n)))
  }
  if (m != ceiling(m)) {
    warning("m is not an integer; using m = ceiling(m).")
    m <- ceiling(m)
  }
  if (m < 1 || m > p - 1) {
    warning("m must satisfy 1 <= m <= p-1;
            using default m = min(p - 1, n/log(n)).")
    m <- min(p - 1, n/log(n))
    m <- ceiling(m)
  }
  m <- as.integer(m)

  ## --- ensure screening_threshold is a positive integer < p ----------------
  if (is.null(screening_threshold)) {
    screening_threshold <- m
  }
  if (!is.numeric(screening_threshold) || length(screening_threshold) != 1) {
    warning("screening_threshold must be a single numeric value;
            using screening_threshold = m.")
    screening_threshold <- m
  }
  if (screening_threshold != floor(screening_threshold)) {
    warning("screening_threshold is not an integer;
            using screening_threshold = ceiling(screening_threshold).")
    screening_threshold <- ceiling(screening_threshold)
  }
  if (screening_threshold < 1 || screening_threshold >= p) {
    warning("screening_threshold must satisfy 1 <= screening_threshold < p;
            using screening_threshold = m.")
    screening_threshold <- m
  }
  screening_threshold <- as.integer(screening_threshold)

  ## --- penalty checks ------------------------------------------------------
  if (!is.numeric(penalty)) {
    warning("penalty is non-numeric; using default penalty = 10.")
    penalty <- 10
  } else if (any(penalty < 0)) {
    warning("negative penalty value(s) provided; using as-is.")
  }

  penalty_type <- match.arg(penalty_type)

  method <- match.arg(method)
  if (method == "auto")
    method <- if (p >= n) "AirHOLP" else "AirOLS"

  # pick underlying engine
  engine <- switch(method, AirHOLP = AirHOLP, AirOLS = AirOLS)

  ## ---------- 2. Internal helpers ------------------------------------------
  run_engine <- function(adapt_flag) {
    engine(X, y, Threshold = m, r0 = penalty, adapt = adapt_flag)
  }

  ## ---------- 3. Run the requested analyses --------------------------------
  res_adapt <- res_fixed <- NULL
  if (penalty_type %in% c("adaptive", "both"))
    res_adapt <- run_engine(TRUE)

  if (penalty_type %in% c("fixed", "both"))
    res_fixed <- run_engine(FALSE)

  ## ---------- 4. Assemble the returned object ------------------------------
  build_part <- function(z, adaptive = TRUE) {
    if (adaptive) {
      list(rank    = z$index_r,
           order   = z$order_r,
           Beta    = z$Beta_r,
           penalty = z$r)
    } else {
      list(rank    = z$index_r0,
           order   = z$order_r0,
           Beta    = z$Beta_r0,
           penalty = penalty)
    }
  }

  obj <- switch(penalty_type,
                adaptive = build_part(res_adapt, TRUE),
                fixed    = build_part(res_fixed,  FALSE),
                both     = c(
                  setNames(build_part(res_adapt, TRUE),
                           paste0(names(build_part(res_adapt, TRUE)), "_adaptive")),
                  setNames(build_part(res_fixed,  FALSE),
                           paste0(names(build_part(res_fixed, FALSE)), "_fixed"))
                )
  )

  ## attach metadata required for summary()
  class(obj) <- "AirResult"
  attr(obj, "penalty_type")        <- penalty_type
  attr(obj, "method")              <- method
  attr(obj, "screening_threshold") <- screening_threshold
  attr(obj, "iter_last") <- if (!is.null(res_adapt$iter_last))
    res_adapt$iter_last
  else NA_integer_
  attr(obj, "colnames") <- colnames(X)

  invisible(obj)
}


#' Summarise an \code{AirResult} Object
#'
#' Produces a compact, human-readable summary of the ranking results
#' returned by \code{\link{Air}}.
#'
#' @param object An object of class \code{"AirResult"}.
#' @param ... Additional arguments (ignored; included for S3 compatibility).
#'
#' @return Invisibly returns \code{object}. The function is invoked for
#'   its printing side-effect.
#'
#' @examples
#' set.seed(314)
#' X <- matrix(rnorm(500), 50, 100)
#' y <- X[, 1] - 2*X[, 3] + rnorm(50)
#' res <- Air(X, y, penalty_type = "both")
#' summary(res)
#'
#' @export
summary.AirResult <- function(object, ...) {
  pt   <- attr(object, "penalty_type")          # "adaptive" / "fixed" / "both"
  mth  <- attr(object, "method")                # "AirHOLP" / "AirOLS"
  k    <- attr(object, "screening_threshold")
  its  <- attr(object, "iter_last")
  cnm  <- attr(object, "colnames")

  # derive friendly labels
  base <- if (mth == "AirHOLP") "HOLP" else "OLS"
  scr_method <- switch(pt,
                       adaptive = paste0("Air-",   base),
                       fixed    = paste0("Ridge-", base),
                       both     = paste0("Air-", base, " & Ridge-", base))
  disp_pt <- if (pt == "both") "adaptive & fixed" else pt

  # header
  cat("\n==================  Feature Screening Summary  ==================\n")
  cat("Screening method  : ", scr_method, "\n", sep = "")
  cat("Penalty type      : ", disp_pt,    "\n", sep = "")
  cat("Screen threshold  : ", k,          "\n\n", sep = "")

  # helper to print blocks
  show_block <- function(lbl, ord, pen) {
    if (length(pen) > 1 && is.matrix(ord)) {
      for (i in seq_along(pen)) {
        sel   <- ord[seq_len(min(k, nrow(ord))), i]
        feats <- if (!is.null(cnm)) cnm[sel] else sel
        cat(lbl, " (penalty = ", pen[i], ")\n", sep = "")
        cat("  Top ", length(sel), " screened features:\n", sep = "")
        print(t(feats))
      }
      cat("  Selected penalty values: ",
          paste(pen, collapse = ", "), "\n\n", sep = "")
    } else {
      # either single penalty or a vector but non-matrix ord
      if (is.matrix(ord)) {
        sel <- ord[seq_len(min(k, nrow(ord))), 1]
      } else {
        sel <- ord[seq_len(min(k, length(ord)))]
      }
      feats <- if (!is.null(cnm)) cnm[sel] else sel
      cat(lbl, "\n")
      cat("  Top ", length(sel), " screened features:\n", sep = "")
      print(t(feats))
      cat("  Selected penalty value(s): ",
          paste(pen, collapse = ", "), "\n\n", sep = "")
    }
  }

  # body
  if (pt == "adaptive") {
    show_block("[Adaptive penalty]", object$order, object$penalty)
  } else if (pt == "fixed") {
    show_block("[Fixed penalty]",    object$order, object$penalty)
  } else {  # both
    show_block("[Adaptive penalty]", object$order_adaptive, object$penalty_adaptive)
    show_block("[Fixed penalty]",    object$order_fixed,    object$penalty_fixed)
  }

  # footer
  if (!is.null(its) && !all(is.na(its))) {
    cat("Number of iterations: ",
        paste(its, collapse = ", "), "\n", sep = "")
  }
  cat("=============================================================\n")
  invisible(object)
}

