###########################
####################
#Weighted trawl simulation
#Using vectorisation for simulation
#Simulate a trawl process
#@title sim_weighted_trawl_dev
#@param n number of grid points to be simulated
#@param Delta grid-width
#@param trawlfct the trawl function used in the simulation (Exp, supIG or LM)
#@param trawlfct_par parameter vector of trawl function
#(Exp: lambda, supIG: delta, gamma, LM: alpha, H)
#@param distr marginal distribution
#@param distr_par parameters of the marginal distribution:
#(Gaussian: mu, sigma, Poisson: v, NegBin: m, theta)
#@param kernelfct the kernel function used in the ambit process
#@details Simulation using slices and vectorisation.
#@return path Simulated  path
#@return path_Rcpp simulated path using Rcpp in addition
#@return slice_sizes slice sizes used
#@return S_matrix Matrix of all slices
sim_weighted_trawl_dev <- function(n, Delta, trawlfct, trawlfct_par,
                                   distr, distr_par, kernelfct)
{
  if(trawlfct=="Exp"){
    f <- function(x) {trawl_Exp(x,trawlfct_par[1])}}
  if(trawlfct=="supIG"){
    f <- function(x) {trawl_supIG(x,trawlfct_par[1],trawlfct_par[2])}}
  if(trawlfct=="LM"){
    f <- function(x) {trawl_LM(x,trawlfct_par[1],trawlfct_par[2])}}

  #Compute the Lebesgue measure of the individual slices
  slice_sizes <- ComputeSliceSizes(n, Delta, f)

  #Generate the random slices
  S_matrix <- matrix(0, n+1, n+1)

  if(distr == "Gauss"){

    for(k in 1:n){
      #"Middle" section of matrix
      S_matrix[k, 1:(n+1-k)] <- stats::rnorm(n+1-k,
                                mean=distr_par[1]*slice_sizes[k,2],
                                sd=distr_par[2]*sqrt(slice_sizes[k,2]))
      #Diagonal elements
      S_matrix[k,(n+1-k+1)] <- stats::rnorm(1,
                                mean=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                sd=distr_par[2]*sqrt(slice_sizes[k,(n+1-k+1)]))
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      S_matrix[i,1] <-stats::rnorm(1,
                                mean=distr_par[1]*slice_sizes[i,1],
                                sd=distr_par[2]*sqrt(slice_sizes[i,1]))
    }

  }

  if(distr == "Poi"){
    for(k in 1:n){
      #"Middle" section of matrix
      S_matrix[k, 1:(n+1-k)] <- stats::rpois(n+1-k,
                                          distr_par[1]*slice_sizes[k,2])
      #Diagonal elements
      S_matrix[k,(n+1-k+1)] <- stats::rpois(1,
                                          distr_par[1]*slice_sizes[k,(n+1-k+1)])
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      S_matrix[i,1] <-stats::rpois(1, distr_par[1]*slice_sizes[i,1])
    }

  }

  if(distr == "NegBin"){
    for(k in 1:n){
      #"Middle" section of matrix
      S_matrix[k, 1:(n+1-k)] <- stats::rnbinom(n+1-k,
                                  size=distr_par[1]*slice_sizes[k,2],
                                  prob=1-distr_par[2])
      #Diagonal elements
      S_matrix[k,(n+1-k+1)] <- stats::rnbinom(1,
                                  size=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                  prob=1-distr_par[2])
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      S_matrix[i,1] <-stats::rnbinom(1,
                                     size=distr_par[1]*slice_sizes[i,1],
                                     prob=1-distr_par[2])
    }

  }

  #Create weight vector
  weights <-numeric(n+1)
  for(i in 1:(n+1)){
    weights[i]<-kernelfct(i*Delta)
  }

  path <- AddWeightedSlices(S_matrix, weights)
  path_Rcpp <- AddWeightedSlices_Rcpp(S_matrix, weights)
  return(list("path"=path, "path_Rcpp"=path_Rcpp,
              "slice_sizes"=slice_sizes, "S_matrix"=S_matrix,
              "kernelwights"=weights))

}


#######################
#'This function simulates a weighted trawl process for various
#'choices of the trawl function and the marginal distribution.
#'@title Simulation of a weighted trawl process
#'@param n number of grid points to be simulated (excluding the starting value)
#'@param Delta grid-width
#'@param trawlfct the trawl function a used in the simulation (Exp, supIG or LM)
#'@param trawlfct_par parameter vector of trawl function
#'(Exp: lambda, supIG: delta, gamma, LM: alpha, H)
#'@param distr marginal distribution. Choose from "Gamma" (Gamma),
#'"Gauss" (Gaussian), "Cauchy"
#'(Cauchy), "NIG" (Normal Inverse Gaussian),
#'Poi" (Poisson), "NegBin" (Negative Binomial)
#'@param distr_par parameters of the marginal distribution:
#'(Gamma: shape, scale;
#'Gauss: mu, sigma (i.e. the second parameter is the standard deviation, not the
#'variance);
#' Cauchy: l, s;
#' NIG: alpha, beta, delta, mu;
#' Poi: v, NegBin: m, theta)
#'@param kernelfct the kernel function p used in the ambit process
#'@details This functions simulates a sample path from a weighted trawl process
#'given by
#'\deqn{	Y_t =\int_{(-\infty,t]\times (-\infty, \infty)}
#'p(t-s)I_{(0,a(t-s))}(x)L(dx,ds),}
#'  for   \eqn{t \ge 0},
#' and returns \eqn{Y_0, Y_{\Delta}, \ldots, Y_{n\Delta}}.
#'@return path Simulated  path
#'@return slice_sizes slice sizes used
#'@return S_matrix Matrix of all slices
#'@return kernelweights kernel weights used
#'@example man/examples/sim_weighted_trawl.R
#'@export
sim_weighted_trawl <- function(n, Delta, trawlfct, trawlfct_par,
                               distr, distr_par, kernelfct=NULL)
{
  if(trawlfct=="Exp"){
    f <- function(x) {trawl_Exp(x,trawlfct_par[1])}}
  if(trawlfct=="supIG"){
    f <- function(x) {trawl_supIG(x,trawlfct_par[1],trawlfct_par[2])}}
  if(trawlfct=="LM"){
    f <- function(x) {trawl_LM(x,trawlfct_par[1],trawlfct_par[2])}}

  #Compute the Lebesgue measure of the individual slices
  slice_sizes <- ComputeSliceSizes(n, Delta, f)

  #Generate the random slices
  S_matrix <- matrix(0, n+1, n+1)

  if(distr == "Gamma"){

    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- stats::rgamma(n+1-k,
                                    shape=distr_par[1]*slice_sizes[k,2],
                                    scale=distr_par[2])
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rgamma(1,
                                    shape=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                    scale=distr_par[2])
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rgamma(1,
                                    shape=distr_par[1]*slice_sizes[i,1],
                                    scale=distr_par[2])
      }
    }

  }

  if(distr == "Gauss"){

    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
          S_matrix[k, 1:(n+1-k)] <- stats::rnorm(n+1-k,
                                    mean=distr_par[1]*slice_sizes[k,2],
                                    sd=distr_par[2]*sqrt(slice_sizes[k,2]))
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
          S_matrix[k,(n+1-k+1)] <- stats::rnorm(1,
                                mean=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                sd=distr_par[2]*sqrt(slice_sizes[k,(n+1-k+1)]))
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rnorm(1, mean=distr_par[1]*slice_sizes[i,1],
                                     sd=distr_par[2]*sqrt(slice_sizes[i,1]))
      }
    }

  }

  if(distr == "Cauchy"){

    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- stats::rcauchy(n+1-k,
                                  location=distr_par[1]*slice_sizes[k,2],
                                  scale=distr_par[2]*slice_sizes[k,2])
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rcauchy(1,
                                location=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                scale=distr_par[2]*slice_sizes[k,(n+1-k+1)])
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rcauchy(1,
                                location=distr_par[1]*slice_sizes[i,1],
                                scale=distr_par[2]*slice_sizes[i,1])
      }
    }

  }

  if(distr == "NIG"){

    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- base::suppressWarnings(fBasics::rnig(n+1-k,
                                    alpha=distr_par[1],
                                    beta=distr_par[2],
                                    delta=distr_par[3]*slice_sizes[k,2],
                                    mu =distr_par[4]*slice_sizes[k,2]))

      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- base::suppressWarnings(fBasics::rnig(1,
                                  alpha=distr_par[1],
                                  beta=distr_par[2],
                                  delta=distr_par[3]*slice_sizes[k,(n+1-k+1)],
                                  mu=distr_par[4]*slice_sizes[k,(n+1-k+1)]))
      }

    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <- base::suppressWarnings(fBasics::rnig(1,
                                  alpha=distr_par[1],
                                  beta=distr_par[2],
                                  delta=distr_par[3]*slice_sizes[i,1],
                                  mu=distr_par[4]*slice_sizes[i,1]))
      }
    }
    #For small slices and hence delta*slice small
    #the rnig functions produces NAs, replace them by 0
    S_matrix[is.na(S_matrix)]<-0
  }

  if(distr == "Poi"){
    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- stats::rpois(n+1-k,
                                               distr_par[1]*slice_sizes[k,2])
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rpois(1,
                                          distr_par[1]*slice_sizes[k,(n+1-k+1)])
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rpois(1, distr_par[1]*slice_sizes[i,1])
      }
    }

  }

  if(distr == "NegBin"){
    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
          S_matrix[k, 1:(n+1-k)] <- stats::rnbinom(n+1-k,
                                            size=distr_par[1]*slice_sizes[k,2],
                                            prob=1-distr_par[2])
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rnbinom(1,
                                    size=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                    prob=1-distr_par[2])
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rnbinom(1,
                                    size=distr_par[1]*slice_sizes[i,1],
                                    prob=1-distr_par[2])
      }
    }

  }

  #Trawl simulation:
  if(is.null(kernelfct)){
    path <- AddSlices_Rcpp(S_matrix)
  }

  #Weighted trawl simulation:
  else {

  #Create weight vector
  weights <-numeric(n+1)

  for(i in 1:(n+1)){
    weights[i]<-kernelfct(i*Delta)
  }

  path <- AddWeightedSlices_Rcpp(S_matrix, weights)

  }

  return(list("path"=path, "slice_sizes"=slice_sizes,
              "S_matrix"=S_matrix, "kernelweights"=weights))

}


#######################
#'This function simulates a weighted trawl process for a generic trawl
#'function and various
#'choices the marginal distribution. The specific trawl function
#'to be used can be supplied
#'directly by the user.
#'@title Simulation of a weighted trawl process with generic trawl function
#'@param n number of grid points to be simulated (excluding the starting value)
#'@param Delta grid-width
#'@param trawlfct_gen the trawl function a used in the simulation
#'@param distr marginal distribution. Choose from "Gamma" (Gamma),
#'"Gauss" (Gaussian), "Cauchy"
#'(Cauchy), "NIG" (Normal Inverse Gaussian),
#'Poi" (Poisson), "NegBin" (Negative Binomial)
#'@param distr_par parameters of the marginal distribution:
#'(Gamma: shape, scale;
#'Gauss: mu, sigma (i.e. the second parameter is the standard deviation, not the
#'variance);
#' Cauchy: l, s;
#' NIG: alpha, beta, delta, mu;
#' Poi: v, NegBin: m, theta)
#'@param kernelfct the kernel function p used in the ambit process
#'@details This functions simulates a sample path from a weighted trawl process
#'given by
#'\deqn{	Y_t =\int_{(-\infty,t]\times (-\infty, \infty)}
#'p(t-s)I_{(0,a(t-s))}(x)L(dx,ds),} for \eqn{ t \ge 0},
#' and returns \eqn{Y_0, Y_{\Delta}, \ldots, Y_{n\Delta}}.
#' The user needs to ensure that trawlfct_gen is a monotonic function.
#'@return path Simulated  path
#'@return slice_sizes slice sizes used
#'@return S_matrix Matrix of all slices
#'@return kernelweights kernel weights used
#'@example man/examples/sim_weighted_trawl_gen.R
#'@export
sim_weighted_trawl_gen <- function(n, Delta, trawlfct_gen,
                               distr, distr_par, kernelfct=NULL)
{
  f <- function(x) {trawlfct_gen(-x)}

  #Compute the Lebesgue measure of the individual slices
  slice_sizes <- ComputeSliceSizes(n, Delta, f)

  #Generate the random slices
  S_matrix <- matrix(0, n+1, n+1)

  if(distr == "Gamma"){

    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- stats::rgamma(n+1-k,
                                                shape=distr_par[1]*slice_sizes[k,2],
                                                scale=distr_par[2])
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rgamma(1,
                                               shape=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                               scale=distr_par[2])
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rgamma(1,
                                      shape=distr_par[1]*slice_sizes[i,1],
                                      scale=distr_par[2])
      }
    }

  }

  if(distr == "Gauss"){

    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- stats::rnorm(n+1-k,
                                               mean=distr_par[1]*slice_sizes[k,2],
                                               sd=distr_par[2]*sqrt(slice_sizes[k,2]))
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rnorm(1,
                                              mean=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                              sd=distr_par[2]*sqrt(slice_sizes[k,(n+1-k+1)]))
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rnorm(1, mean=distr_par[1]*slice_sizes[i,1],
                                     sd=distr_par[2]*sqrt(slice_sizes[i,1]))
      }
    }

  }

  if(distr == "Cauchy"){

    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- stats::rcauchy(n+1-k,
                                                 location=distr_par[1]*slice_sizes[k,2],
                                                 scale=distr_par[2]*slice_sizes[k,2])
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rcauchy(1,
                                                location=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                                scale=distr_par[2]*slice_sizes[k,(n+1-k+1)])
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rcauchy(1,
                                       location=distr_par[1]*slice_sizes[i,1],
                                       scale=distr_par[2]*slice_sizes[i,1])
      }
    }

  }

  if(distr == "NIG"){

    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- base::suppressWarnings(fBasics::rnig(n+1-k,
                                                                       alpha=distr_par[1],
                                                                       beta=distr_par[2],
                                                                       delta=distr_par[3]*slice_sizes[k,2],
                                                                       mu =distr_par[4]*slice_sizes[k,2]))

      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- base::suppressWarnings(fBasics::rnig(1,
                                                                      alpha=distr_par[1],
                                                                      beta=distr_par[2],
                                                                      delta=distr_par[3]*slice_sizes[k,(n+1-k+1)],
                                                                      mu=distr_par[4]*slice_sizes[k,(n+1-k+1)]))
      }

    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <- base::suppressWarnings(fBasics::rnig(1,
                                                              alpha=distr_par[1],
                                                              beta=distr_par[2],
                                                              delta=distr_par[3]*slice_sizes[i,1],
                                                              mu=distr_par[4]*slice_sizes[i,1]))
      }
    }
    #For small slices and hence delta*slice small
    #the rnig functions produces NAs, replace them by 0
    S_matrix[is.na(S_matrix)]<-0
  }

  if(distr == "Poi"){
    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- stats::rpois(n+1-k,
                                               distr_par[1]*slice_sizes[k,2])
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rpois(1,
                                              distr_par[1]*slice_sizes[k,(n+1-k+1)])
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rpois(1, distr_par[1]*slice_sizes[i,1])
      }
    }

  }

  if(distr == "NegBin"){
    for(k in 1:n){
      #"Middle" section of matrix
      if(slice_sizes[k,2]>0){
        S_matrix[k, 1:(n+1-k)] <- stats::rnbinom(n+1-k,
                                                 size=distr_par[1]*slice_sizes[k,2],
                                                 prob=1-distr_par[2])
      }
      #Diagonal elements
      if(slice_sizes[k,(n+1-k+1)]>0){
        S_matrix[k,(n+1-k+1)] <- stats::rnbinom(1,
                                                size=distr_par[1]*slice_sizes[k,(n+1-k+1)],
                                                prob=1-distr_par[2])
      }
    }
    #Column of first slices:
    for(i in 1:(n+1)){
      if(slice_sizes[i,1]>0){
        S_matrix[i,1] <-stats::rnbinom(1,
                                       size=distr_par[1]*slice_sizes[i,1],
                                       prob=1-distr_par[2])
      }
    }

  }

  #Trawl simulation:
  if(is.null(kernelfct)){
    path <- AddSlices_Rcpp(S_matrix)
  }

  #Weighted trawl simulation:
  else {

    #Create weight vector
    weights <-numeric(n+1)

    for(i in 1:(n+1)){
      weights[i]<-kernelfct(i*Delta)
    }

    path <- AddWeightedSlices_Rcpp(S_matrix, weights)

  }

  return(list("path"=path, "slice_sizes"=slice_sizes,
              "S_matrix"=S_matrix, "kernelweights"=weights))

}
