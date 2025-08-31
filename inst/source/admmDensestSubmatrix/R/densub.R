#' densub
#'
#' Iteratively solves the convex optimization problem using ADMM.
#'
#'       \eqn{min    |X|_* + gamma* |Y|_1 + 1_Omega_W (W) + 1_Omega_Q (Q) + 1_Omega_Z (Z)}
#'
#'       s.t    \eqn{X - Y = 0}, \eqn{X = W}, \eqn{X = Z},
#'
#' where \eqn{Omega_W (W)}, \eqn{Omega_Q (Q)}, \eqn{Omega_Z (Z)} are the sets:
#'       \eqn{Omega_W = {W in R^MxN | e^TWe = mn}}

#'
#'       \eqn{Omega_Q = {Q in R^MxN | Projection of Q on not N = 0}}
#'
#'       \eqn{Omega_Z = {Z in R^MxN | Z_ij <= 1 for all (i,j) in M x N}}
#'
#'
#'       \eqn{Omega_Q = {Q in R^MxN | Projection of Q on not N = 0}}
#'
#'       \eqn{Omega_Z = {Z in R^MxN | Z_ij <= 1 for all (i,j) in M x N}}
#'
#' \eqn{1_S} is the indicator function of the set \eqn{S} in \eqn{R^MxN} such that \eqn{1_S(X) = 0} if \eqn{X} in \eqn{S} and +infinity otherwise
#'
#' @param G sampled binary matrix
#' @param m number of rows in dense submatrix
#' @param n number of columns in dense submatrix
#' @param gamma  \eqn{l_1} regularization parameter
#' @param tau    penalty parameter for equality constraint violation
#' @param opt_tol    stopping tolerance in algorithm
#' @param maxiter maximum number of iterations of the algorithm to run
#' @param quiet toggles between displaying intermediate statistics
#' @importFrom stats runif
#' @importFrom utils tail
#' @return Rank one matrix with \eqn{mn} nonzero entries, matrix \eqn{Y} that is used to count the number of disagreements between \eqn{G} and \eqn{X}
#' @export


densub <- function(G, m, n, tau = 0.35, gamma = 6/(sqrt(m*n)*(q-p)), opt_tol = 1.0e-4,maxiter, quiet = TRUE){

  mu <- 1/tau
  M <- dim(G)[1]
  N <- dim(G)[2]

  for(i in 1:M)
  {
    for(j in 1:M)
    {
      if(is.na(G[i,j])|(!(G[i,j] == 1 | G[i,j] == 0)))
      {
        stop('The argument "G" must be binary matrix')
      }
    }
  }

  if(m > M | n > N){
    warning ('Dimensions of dense submatrix exceed dimensions of  initial matrix G')
  }

  #Initialize
  W <- matrix(1L, nrow = M, ncol = N)*m*n/(M*N)
  X <- W
  Y <- X
  Z <- X
  Q <- X - Y
  LambdaQ <- matrix(0L, nrow=nrow(X), ncol=ncol(X))
  LambdaZ <- matrix(0L, nrow=nrow(X), ncol=ncol(X))
  LambdaW <- matrix(0L, nrow=nrow(X), ncol=ncol(X))


  convergence <- 0
  iter <- 0

  while (iter < maxiter && convergence==0){

    ##Update Q
    Q_old <- Q
    Q <- X - Y + mu*LambdaQ
    Q <- Q*G

    ##Update X
    mat<- 1/3*(Y + Q + Z + W - mu*(LambdaQ + LambdaW + LambdaZ))
    X = mat_shrink(K=mat, tau=1/(3*tau))

    ##Update Y
    A <-X - Q - gamma*matrix(1L, nrow = M, ncol = N)*mu + LambdaQ*mu
    B <- matrix(0L, nrow = M, ncol = N)
    Y <- pmax(A, B)

    ##Update W
    W_old <- W
    newW <- X + mu*LambdaW
    alfa <- (tail(m, n=1)*tail(n, n=1) - sum(as.vector(newW)))/(M*N)
    W <- newW + alfa*matrix(1L, nrow = M, ncol = N)

    ##Update Z
    Z_old <- Z
    Z <- X + mu*LambdaZ
    D <- matrix(0L, M, N)
    Z1 <- pmax(Z, D)
    E <- matrix(1L, M, N)
    Z <- pmin(Z1, E)

    ##Update dual variables
    LambdaQ <- LambdaQ + tau*(X - Y - Q)
    LambdaW <- LambdaW + tau*(X - W)
    LambdaZ <- LambdaZ + tau*(X - Z)

    ##Check convergence
    ##primal
    NZ <- norm(X - Z, type="F")
    NW <- norm(X - W, type="F")
    NQ <- norm(X - Y - Q, type="F")

    errP <- max(NZ, NW, NQ)/norm(X, type="F")

    ##Dual feasibility
    NDz <- norm(Z - Z_old, type="F")
    NDw <- norm(W - W_old, type="F")
    NDp <- norm(Q - Q_old, type="F")

    errD <- max(NDz, NDw, NDp)/norm(X, type="F")

    if(errP < opt_tol && errD < opt_tol) {
      convergence=1
      break
    }
    else {
      convergence=0
    }
    iter <- iter +1

    print(sprintf('iter: %d, primal_gap: %e, dual_gap: %e',iter, errP, errD))


  }
  results <- list(X=X, Y=Y)
  return(results)

}
