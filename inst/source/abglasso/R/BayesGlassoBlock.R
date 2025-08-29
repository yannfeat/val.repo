#' Adaptive Bayesian graphical lasso MCMC sampler
#'
#' A Bayesian adaptive graphical lasso  data-augmented block Gibbs sampler. The sampler is adapted from the MATLAB routines used in Wang (2012).
#'
#' @param X Numeric matrix.
#' @param burnin An integer specifying the number of burn-in iterations.
#' @param nmc An integer specifying the number of MCMC samples.
#'
#' @return list containing:
#' \describe{
#' \item{Sig}{A \code{p} by \code{p} by \code{nmc} array of saved posterior samples of covariance matrices.}
#' \item{Omega}{A \code{p} by \code{p} by nmc array of saved posterior samples of precision matrices.}
#' \item{Lambda}{A 1 by \code{nmc} vector of saved posterior samples of lambda values.}
#' }
#'
#' @importFrom pracma triu zeros inv Reshape
#' @importFrom stats rgamma rnorm
#' @importFrom statmod rinvgauss
#'
#' @references Wang, H. (2012). Bayesian graphical lasso models and efficient
#' posterior computation. \emph{Bayesian Analysis, 7}(4). \doi{10.1214/12-BA729}.
#'
#' @examples
#' \donttest{
#' # Generate true covariance matrix:
#' p             <- 10
#' n             <- 50
#' SigTrue       <- pracma::Toeplitz(c(0.7^rep(1:p-1)))
#' CTrue         <- pracma::inv(SigTrue)
#' # Generate expected value vector:
#' mu            <- rep(0,p)
#' # Generate multivariate normal distribution:
#' set.seed(123)
#' X             <- MASS::mvrnorm(n,mu=mu,Sigma=SigTrue)
#' abglasso_post <- BayesGlassoBlock(X,burnin = 1000,nmc = 2000)
#'}
#' @export

BayesGlassoBlock <- function(X,burnin = 1000,nmc = 2000){

  # Total iterations, n, p, sum of product matrix, sample covariance matrix, precision matrix:
  total_it<- burnin+nmc
  n       <- nrow(X)
  p       <- ncol(X)
  S       <- t(X)%*%X
  Sig     <- S/n
  C       <- inv(Sig)

  # Extract locations of upper triangular part of a matrix:
  indmx    <- Reshape(1:p^2,p,p)
  upperind <- indmx[triu(indmx,1)>0]

  # Extract locations of lower triangular part of a matrix:
  indmx_t  <- t(indmx)
  lowerind <- indmx_t[triu(indmx_t,1)>0]

  # Initialize arrays for storing each precision matrix estimate, covariance matrix estimate and latent tau estimate for each MCMC iteration:
  C_save   <- array(rep(0, p*p*nmc), dim<-c(p, p, nmc))
  Sig_save <- C_save
  tau      <- zeros(p,p)

  # Indicator matrix for looping through columns & rows ("blocks"):
  ind_noi_all <- zeros(p-1,p)

  for(i in 1:p){
    if(i==1) ind_noi <- t(2:p)
    else if (i==p) ind_noi <- t(1:(p-1))
    else ind_noi <- t(c(1:(i-1),(i+1):(p)))
    ind_noi_all[,i] <- ind_noi
  }

# Set hyperparameters for the Gamma distirbution of the shrinkage parameter (lambda_{ij}):
s <- 1e-2
t <- 1e-6

# Set lambda_{ii}:
lambda_ii <- 1

# Main block sampling loop:
for(iter in 1:(total_it)){

  # Print progress:
  if(iter%%100 == 0) {
    cat("Total iterations= ", iter, "Iterations since burn in= ",
        ifelse(iter - burnin > 0, iter - burnin, 0),
        "\n")
  }

  Cadjust <- pmax(abs(as.vector(C)[upperind]),10^-12)

  # Sample off-diagonal lambda:
  s_post <- 1+s
  t_post <- Cadjust+t
  lambda <- sapply(t_post,function(x) rgamma(1,shape=s_post, x))

  # Sample off-diagonal tau:
  lambda_prime <- lambda^2
  mu_prime     <- pmin(lambda/Cadjust,1e12)
  tau_temp     <- pmax(1e-12,1/sapply(1:length(mu_prime), function(x) rinvgauss(1,mu_prime[x],lambda_prime[x])))
  tau[upperind]<- tau_temp
  tau[lowerind]<- tau_temp

  # Sample covariance matrix (Sig) and precision matrix (C):
  for(i in 1:p){
    ind_noi      <- ind_noi_all[,i]
    tau_temp     <- tau[ind_noi,i]
    Sig11        <- Sig[ind_noi,ind_noi]; Sig12 = Sig[ind_noi,i]
    invC11       <- Sig11 - Sig12%*%t(Sig12)/Sig[i,i]
    Ci           <- (S[i,i]+lambda_ii)*invC11+diag(1/tau_temp,length(tau_temp),length(tau_temp))
    Ci           <- (Ci+t(Ci))/2
    Ci_chol      <- chol(Ci)
    mu_i         <- -inv(Ci)%*%S[ind_noi,i]
    beta         <- mu_i+ inv(Ci_chol)%*%rnorm(p-1)
    C[ind_noi,i] <- beta
    C[i,ind_noi] <- beta
    gam          <- rgamma(1,(n/2)+1,1/(2/(S[i,i]+lambda_ii)))
    C[i,i]       <- gam+t(beta)%*%invC11%*%beta

    # Update covariance matrix according to the one-column change of the precision matrix:
    invC11beta           <- invC11%*%beta
    Sig[ind_noi,ind_noi] <- invC11+invC11beta%*%t(invC11beta)/gam
    Sig12                <- -invC11beta/gam
    Sig[ind_noi,i]       <- Sig12
    Sig[i,ind_noi]       <- t(Sig12)
    Sig[i,i]             <- 1/gam

  }

  # Save covariance and precision matrices:
  if (iter > burnin){
   Sig_save[,,iter-burnin] <- Sig
   C_save[,,iter-burnin]   <- C
  }
}

return(list("Sig" = Sig_save, "Omega" = C_save))
}
