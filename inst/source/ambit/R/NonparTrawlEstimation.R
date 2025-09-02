#This file contains the relevant functions based on the
# non-parameteric trawl function estimation
# proposed in Sauri and Veraart (2022)

#######################
#'This function implements the nonparametric trawl estimation proposed in
#'Sauri and Veraart (2022).
#'@title Nonparametric estimation of the trawl function
#'@name nonpar_trawlest
#'@param data Data to be used in the trawl function estimation.
#'@param Delta Width of the grid on which we observe the data
#'@param lag The lag until which the trawl function should be estimated
#'@details Estimation of the trawl function using the methodology proposed in
#'Sauri and Veraart (2022). Suppose the data is observed on the grid
#'0, Delta, 2Delta, ..., (n-1)Delta. Given the path contained in data,
#'the function  returns the lag-dimensional
#' vector \deqn{(\hat a(0), \hat a(\Delta), \ldots, \hat a((lag-1) \Delta)).}
#' In the case when lag=n, the n-1 dimensional vector
#' \deqn{(\hat a(0), \hat a(\Delta), \ldots, \hat a((n-2) \Delta))} is returned.
#'@return ahat Returns the lag-dimensional vector
#' \eqn{(\hat a(0), \hat a(\Delta), \ldots, \hat a((lag-1) \Delta)).}
#' Here, \eqn{\hat a(0)} is estimated
#' based on the realised variance estimator.
#'@return a0_alt Returns the alternative estimator of a(0) using
#'the same methodology as the one used for
#' t>0. Note that this is not the recommended estimator to use, but can be
#' used for comparison purposes.
#' @example man/examples/nonpar_trawlest.R
#'@export
nonpar_trawlest <- function(data, Delta, lag=100){
  a_hat <- vector(mode = "numeric", length = lag)
  cov <- stats::acf(data, lag=lag+1,type = c("covariance"), plot = FALSE)$acf
  for(i in 1:lag){
    a_hat[i]<- -(cov[i+1]-cov[i])/Delta
  }
  a0_alt <- a_hat[1]

  #Cover the case when lag==n and remove the last NA value
  n <- base::length(data) #it includes time points 0, Delta, ...(n-1) Delta
  if(lag==n){
    a_hat <- a_hat[1:n-1]
  }


  #Estimating a(0) using the recommended RV-based estimator:
  incr <- base::diff(data)
  a_hat[1] <-base::sum(incr^2)/(2*n*Delta)

  #note that a_hat[1] corresponds to ^a(0) etc
  return(list("a_hat"=a_hat, "a0_alt"=a0_alt))
}

#'This function estimates the size of the trawl set given by Leb(A).
#'@title Nonparametric estimation of the trawl set Leb(A)
#'@name LebA_est
#'@param data Data to be used in the trawl function estimation.
#'@param Delta Width of the grid on which we observe the data
#'@param biascor A binary variable determining whether a bias correction should
#'be computed, the default is FALSE
#'@details Estimation of the trawl function using the methodology proposed in
#'Sauri and Veraart (2022).
#'@return The estimated Lebesgue measure of the trawl set
#'@example man/examples/LebA_est.R
#'@export
LebA_est <- function(data, Delta, biascor=FALSE){

  n<- base::length(data)

  avector <- nonpar_trawlest(data, Delta=Delta, lag=n)$a_hat

  #Estimate the derivative of the trawl function for the bias correction
  deriv <- c(trawl_deriv_mod(data, Delta=Delta, lag=n-2),0) #add a 0 at the end
  bias <- vector(mode = "numeric", length = n-1)

    if(biascor==TRUE){
      for(i in 1:(n-1)){
        bias[i] <- 0.5*Delta*deriv[i]
      }
    }

  my_avector <- avector - bias

  lebA <- sum(my_avector)*Delta

  return(lebA)

}

#' This function estimates Leb(A), Leb(A intersection A_h), Leb(A\ A_h).
#'@title Nonparametric estimation of the trawl (sub-) sets
#'Leb(A), Leb(A intersection A_h), Leb(A setdifference A_h)
#'@name LebA_slice_est
#'@param data Data to be used in the trawl function estimation.
#'@param Delta Width of the grid on which we observe the data
#'@param h  Time point used in A intersection A_h and the setdifference
#'A setdifference A_h
#'@param biascor A binary variable determining whether a bias correction should
#'be computed, the default is FALSE
#'@details Estimation of the trawl function using the methodology proposed in
#'Sauri and Veraart (2022).
#'@return LebA
#'@return LebAintersection
#'@return LebAsetdifference
#'@example man/examples/LebA_slice_est.R
#'@export
LebA_slice_est <- function(data, Delta, h, biascor=FALSE){

  n<- base::length(data)

  k <- base::floor(h/Delta)

  avector <- nonpar_trawlest(data, Delta, lag=n)$a_hat

  #Estimate the derivative of the trawl function for the bias correction
  deriv <- c(trawl_deriv_mod(data, Delta=Delta, lag=n-2),0) #add a 0 at the end
  bias <- vector(mode = "numeric", length = n-1)

  if(biascor==TRUE){
    for(i in 1:(n-1)){
      bias[i] <- 0.5*Delta*deriv[i]
    }
  }

  my_avector <- avector - bias

  lebA <-sum(my_avector)*Delta
  #Estimate Leb(A intersection A_h)
  lebAintersection <-sum(my_avector[(k+1):n-1])*Delta

  if(lebAintersection>lebA){lebAintersection <- lebA}

  lebAsetdifference <- lebA-lebAintersection

  #return list
  return(list("LebA"=lebA, "LebAintersection"=lebAintersection,
              "LebAsetdifference"=lebAsetdifference))

}

#'This function estimates the ratios
#' Leb(A intersection A_h)/Leb(A), Leb(A\ A_h)/Leb(A).
#'@title Nonparametric estimation of the ratios
#'Leb(A intersection A_h)/Leb(A), Leb(A setdifference A_h)/Leb(A)
#'@name LebA_slice_ratio_est_acfbased
#'@param data Data to be used in the trawl function estimation.
#'@param Delta Width of the grid on which we observe the data
#'@param h  Time point used in A intersection A_h and the setdifference
#'A setdifference A_h
#'@details Estimation of the trawl function using the methodology proposed in
#'Sauri and Veraart (2022) which is based on the empirical acf.
#'@return LebAintersection_ratio: LebAintersection/LebA
#'@return LebAsetdifference_ratio: LebAsetdifference/LebA
#'@example man/examples/LebA_slice_ratio_est_acfbased.R
#'@export
LebA_slice_ratio_est_acfbased <- function(data, Delta, h){

  n<- base::length(data)

  k <- base::floor(h/Delta)

  my_acf <- stats::acf(data, plot=FALSE)$acf[k+1]

  #Estimate Leb(A intersection A_h)/Leb(A)
  lebAintersectionratio <- my_acf

  if(lebAintersectionratio<0){lebAintersectionratio <- 0}

  lebAsetdifferenceratio <- 1-my_acf

  #return list
  return(list("LebAintersection_ratio"=lebAintersectionratio,
              "LebAsetdifference_ratio"=lebAsetdifferenceratio))

}
#Functions for the CLT
#'This function computes the theoretical asymptotic variance
#'appearing in the CLT of the trawl process for a given trawl function and
#'fourth cumulant.
#'@title Computing the true asymptotic variance in the CLT of the
#'trawl estimation
#'@name asymptotic_variance
#'@param t Time point at which the asymptotic variance is computed
#'@param c4 The fourth cumulant of the Levy seed of the trawl process
#'@param varlevyseed The variance of the Levy seed of the trawl process,
#' the default is 1
#'@param trawlfct The trawl function for which the
#'asymptotic variance will be computed (Exp, supIG or LM)
#'@param trawlfct_par The parameter vector of the trawl function
#'(Exp: lambda, supIG: delta, gamma, LM: alpha, H)
#'@details As derived in
#'Sauri and Veraart (2022), the asymptotic variance in the central limit
#'theorem for the trawl function estimation is given by
#' \deqn{\sigma_{a}^{2}(t)=c_{4}(L')a(t)+2\{ \int_{0}^{\infty}a(s)^{2}ds+
#' \int_{0}^{t}a(t-s)a(t+s)ds-\int_{t}^{\infty}a(s-t)a(t+s)ds\},}
#' for \eqn{t>0}.
#' The integrals in the above formula are approximated numerically.
#'@return The function returns \eqn{\sigma_{a}^{2}(t)}.
#'@example man/examples/asymptotic_variance.R
#'@export
asymptotic_variance <- function(t, c4, varlevyseed=1, trawlfct, trawlfct_par)
{
  if(trawlfct=="Exp"){
    a <- function(x) {trawl_Exp(-x,trawlfct_par[1])}}
  if(trawlfct=="supIG"){
    a <- function(x) {trawl_supIG(-x,trawlfct_par[1],trawlfct_par[2])}}
  if(trawlfct=="LM"){
    a <- function(x) {trawl_LM(-x,trawlfct_par[1],trawlfct_par[2])}}


  v1  <- c4*a(t)

  g2 <- function(x){(a(x))^2}
  v2  <- 2*varlevyseed^2*stats::integrate(g2, 0, Inf)$value

  g3 <- function(x){a(t-x)*a(t+x)}
  v3  <- 2*varlevyseed^2*stats::integrate(g3, 0, t)$value

  g4 <- function(x){a(x-t)*a(t+x)}
  v4  <- -2*varlevyseed^2*stats::integrate(g4, t, Inf)$value

  v <- v1+v2+v3+v4

  return(list("v"=v, "v1"=v1, "v2"=v2, "v3"=v3, "v4"=v4))
}


###Checking the asymptotic normality: Infeasible statistics
#'This function computes the infeasible test statistic appearing in the CLT
#'for the trawl function estimation.
#'@title Computing the infeasible test statistic from the trawl function
#'estimation CLT
#'@name test_asymnorm
#'@param ahat The term \eqn{\hat a(k \Delta_n)} in the CLT
#'@param n The number n of observations in the sample
#'@param Delta The width Delta of the observation grid
#'@param k The time point in \eqn{0, 1, \ldots, n-1};
#'the test statistic will be computed for the time point
#'\eqn{k*\Delta_n}.
#'@param c4 The fourth cumulant of the Levy seed of the trawl process
#'@param varlevyseed The variance of the Levy seed of the trawl process,
#' the default is 1
#'@param trawlfct The trawl function for which the
#'asymptotic variance will be computed (Exp, supIG or LM)
#'@param trawlfct_par The parameter vector of the trawl function
#'(Exp: lambda, supIG: delta, gamma, LM: alpha, H)
#'@details As derived in
#'Sauri and Veraart (2022), the infeasible test statistic is given by
#' \deqn{\frac{\sqrt{n\Delta_{n}}}{\sqrt{\sigma_{a}^2(k \Delta_n)}}
#' \left(\hat{a}(k\Delta_n)-a(k \Delta_n)\right),}
#' for \eqn{k \in \{0, 1, \ldots, n-1\}}.
#'@return The function returns the infeasible test statistic specified above.
#'@example man/examples/test_asymnorm.R
#'@export
test_asymnorm <- function(ahat, n, Delta, k, c4, varlevyseed=1,
                          trawlfct, trawlfct_par)
{
  if(trawlfct=="Exp"){
    a <- function(x) {trawl_Exp(-x,trawlfct_par[1])}}
  if(trawlfct=="supIG"){
    a <- function(x) {trawl_supIG(-x,trawlfct_par[1],trawlfct_par[2])}}
  if(trawlfct=="LM"){
    a <- function(x) {trawl_LM(-x,trawlfct_par[1],trawlfct_par[2])}}

  #Computing the asymptotic variance
  v<- asymptotic_variance(k*Delta, c4, varlevyseed=1, trawlfct, trawlfct_par)$v

  x <- sqrt(n*Delta)*(ahat-a(k*Delta))/sqrt(v)
  return(x)
}


#####################
#Functions for the feasible CLT:
#'This function computes the scaled realised quarticity of a time series
#'for a given width of the observation grid.
#'@title Computing the scaled realised quarticity
#'@name rq
#'@param data The data set used to compute the scaled realised quarticity
#'@param Delta The width Delta of the observation grid
#'@details According to
#'Sauri and Veraart (2022), the scaled realised quarticity for
#'\eqn{X_0, X_{\Delta_n}, \ldots, X_{(n-1)\Delta_n}} is given by
#' \deqn{RQ_n:=\frac{1}{\sqrt{2 n\Delta_{n}}}
#' \sum_{k=0}^{n-2}(X_{(k+1)\Delta_n}-X_{k\Delta_n})^4.}
#'@return The function returns the scaled realised quarticity RQ_n.
#'@example man/examples/rq.R
#'@export
rq <- function(data, Delta)
{
  incr <- base::diff(data)
  n <- base::length(data) #it includes time points 0, Delta, ...(n-1) Delta
  rq <-base::sum(incr^4)/(2*n*Delta)
  return(rq)
}

#'This function estimates the fourth cumulant of the trawl process.
#'@title Estimating the fourth cumulant of the trawl process
#'@name c4est
#'@param data The data set used to estimate the fourth cumulant
#'@param Delta The width Delta of the observation grid
#'@details According to
#'Sauri and Veraart (2022), estimator based on
#'\eqn{X_0, X_{\Delta_n}, \ldots, X_{(n-1)\Delta_n}} is given by
#'\deqn{\hat c_4(L')=RQ_n/\hat a(0),}
#'where
#' \deqn{RQ_n:=\frac{1}{\sqrt{2 n\Delta_{n}}}
#' \sum_{k=0}^{n-2}(X_{(k+1)\Delta_n}-X_{k\Delta_n})^4,}
#' and
#' \deqn{\hat a(0)=\frac{1}{2\Delta_{n}n}
#' \sum_{k=0}^{n-2}(X_{(k+1)\Delta_n}-X_{k\Delta_n})^{2}.}
#'@return The function returns the estimated fourth cumulant of the Levy seed:
#'\eqn{\hat c_4(L')}.
#'@example man/examples/c4est.R
#'@export
c4est <-function(data, Delta){
  my_rq <- rq(data, Delta)
  my_a0 <- nonpar_trawlest(data, Delta, lag=100)$a_hat[1]
  return(my_rq/my_a0)
}




#'This function estimates the asymptotic variance which appears in the
#'CLT for the trawl function estimation.
#'@title Estimating the asymptotic variance in the trawl function CLT
#'@name asymptotic_variance_est
#'@param t The time point at which to compute the asymptotic variance
#'@param c4 The fourth cumulant of the Levy seed of the trawl process
#'@param varlevyseed The variance of the Levy seed of the trawl process,
#' the default is 1
#'@param avector The vector \eqn{(\hat a(0), \hat a(\Delta_n),
#'..., \hat a((n-1)\Delta_n))}
#'@param Delta The width Delta of the observation grid
#'@param N The optional parameter to specify the upper bound \eqn{N_n} in the
#'computations of the estimators
#'@details As derived in
#'Sauri and Veraart (2022), the estimated asymptotic variance is given by
#'\deqn{\hat \sigma^2_a(t)=\hat v_1(t)+\hat v_2(t)+\hat v_3(t)+\hat v_4(t),}
#'where
#'\deqn{\hat{v}_{1}(t):=\widehat{c_{4}(L')}\hat{a}(t)=RQ_n\hat{a}(t)/
#'\hat{a}(0),}
#'for
#'\deqn{RQ_n:=\frac{1}{\sqrt{2 n\Delta_{n}}}
#' \sum_{k=0}^{n-2}(X_{(k+1)\Delta_n}-X_{k\Delta_n})^4,}
#' and
#' \deqn{	\hat{v}_{2}(t):=2\sum_{l=0}^{N_{n}}\hat{a}^{2}(l\Delta_{n})
#' \Delta_{n},}
#' \deqn{	\hat{v}_{3}(t):=2\sum_{l=0}^{\min\{i,n-1-i\}}\hat{a}((i-l)\Delta_{n})
#' \hat{a}((i+l)\Delta_{n})\Delta_{n},}
#' \deqn{	\hat{v}_{4}(t):=-2\sum_{l=i}^{N_{n}-i}\hat{a}((l-i)\Delta_{n})
#' \hat{a}((i+l)\Delta_{n})\Delta_{n}.}
#'@return The estimated asymptotic variance \eqn{\hat v=\hat \sigma_a^2(t)}
#'and its components \eqn{\hat v_1, \hat v_2, \hat v_3, \hat v_4}.
#'@example man/examples/asymptotic_variance_est.R
#'@export
asymptotic_variance_est <- function(t, c4, varlevyseed=1, Delta, avector,
                                    N=NULL)
{
  #avector contains the estimates for
  #a(0), a(Delta), a(2Delta),...,a((n-1)Delta)
  n <- base::length(avector)#length of original vector =n

  if(is.null(N)){
    my_avector <-avector
    my_avector_length <- n
  }
  else{
    if((N+1)>n){N <- (n-1)} #Ensure that N is in the correct range
    my_avector <-avector[1:(N+1)] #"n=N+1, n-1=N"
    my_avector_length <- N+1
  }

  i <- floor(t/Delta)

  if(i==0){
    v1 <- c4*avector[1]
    v2 <- 0
    v3 <- 0
    v4 <- 0
    v <- v1+v2+v3+v4
  }
  else{

    v1  <- c4*avector[i+1]

    #For v2, only sum up from 1 to N+1 (corresponding to 0 to N):
    v2 <- 2*varlevyseed^2*sum(my_avector^2)*Delta

    tmp3<-0
    for(l in 0:(min(i,n-1-i))){ #Note that avector_length=n
      tmp3<- tmp3+avector[i-l+1]*avector[i+l+1]
    }
    v3  <- 2*varlevyseed^2*tmp3*Delta

    #For v4, only sum up from i+1 to N+1-i (corresponding to i to N-i)
    tmp4<-0
    #Note that "avector_length-1=n-1 = N"
    for(l in i:(my_avector_length-1-i)){
      if(i>(my_avector_length-1-i)) break
      tmp4<- tmp4+my_avector[l-i+1]*my_avector[i+l+1]
    }
    v4  <- -2*varlevyseed^2*tmp4*Delta

    v <- v1+v2+v3+v4
  }
  return(list("v"=v, "v1"=v1, "v2"=v2, "v3"=v3, "v4"=v4))
}

#'This function computes the feasible test statistic appearing in the CLT
#'for the trawl function estimation.
#'@title Computing the feasible statistic of the trawl function CLT
#'@name test_asymnorm_est_dev
#'@param ahat The estimated trawl function at time t: \eqn{\hat{a}(t)}
#'@param n The number of observations in the data set
#'@param Delta The width Delta of the observation grid
#'@param k The time point in \eqn{0, 1, \ldots, n-1};
#'the test statistic will be computed for the time point \eqn{k * \Delta_n}.
#'@param c4 The fourth cumulant of the Levy seed of the trawl process
#'@param varlevyseed The variance of the Levy seed of the trawl process,
#' the default is 1
#'@param trawlfct The trawl function for which the
#'asymptotic variance will be computed (Exp, supIG or LM)
#'@param trawlfct_par The parameter vector of the trawl function
#'(Exp: lambda, supIG: delta, gamma, LM: alpha, H)
#'@param avector The vector \eqn{(\hat a(0), \hat a(Delta_n), ...,
#'\hat a((n-1)\Delta_n))}
#'@details As derived in
#'Sauri and Veraart (2022), the feasible statistic is given by
#'\deqn{T(k \Delta_n)_n:=\frac{\sqrt{n\Delta_{n}}}{
#'\sqrt{\widehat{\sigma_{a}^2( \Delta_n)}}}
#'\left(\hat{a}( \Delta_n)-a( \Delta_n)\right)}.
#'@return The function returns the feasible statistic \eqn{T( \Delta_n)_n}
#'if the estimated asymptotic variance is positive and 999 otherwise.
# #'@export

test_asymnorm_est_dev <- function(ahat, n, Delta, k, c4, varlevyseed=1,
                                  trawlfct, trawlfct_par, avector)
{
  if(trawlfct=="Exp"){
    a <- function(x) {trawl_Exp(-x,trawlfct_par[1])}}
  if(trawlfct=="supIG"){
    a <- function(x) {trawl_supIG(-x,trawlfct_par[1],trawlfct_par[2])}}
  if(trawlfct=="LM"){
    a <- function(x) {trawl_LM(-x,trawlfct_par[1],trawlfct_par[2])}}

  #Computing the asymptotic variance
  av<- asymptotic_variance_est(k*Delta, c4, varlevyseed=1, Delta, avector)
  v <- av$v
  v1 <- av$v1
  v2 <- av$v2
  v3 <- av$v3
  v4 <- av$v4
  if(v<=0){x <- 999}#{v<-v2}
  else{
    x <- sqrt(n*Delta)*(ahat-a(k*Delta))/sqrt(v)
  }
  return(list("x"=x, "v"=v, "v1"=v1, "v2"=v2, "v3"=v3, "v4"=v4))
}

#######Bias correction
#'This function estimates the derivative of the trawl function using
#'the modified version proposed in Sauri and Veraart (2022).
#'@title Estimating the derivative of the trawl function
#'@name trawl_deriv_mod
#'@param data The data set used to compute the derivative of the trawl function
#'@param Delta The width Delta of the observation grid
#'@param lag The lag until which the trawl function should be estimated
#'@details According to
#'Sauri and Veraart (2022), the derivative of the trawl function can
#'be estimated based on observations
#'\eqn{X_0, X_{\Delta_n}, \ldots, X_{(n-1)\Delta_n}}  by
#' \deqn{\widehat a(t)=\frac{1}{\sqrt{ n\Delta_{n}^2}}
#' \sum_{k=l+1}^{n-2}(X_{(k+1)\Delta_n}-X_{k\Delta_n})
#' (X_{(k-l+1)\Delta_n}-X_{(k-l)\Delta_n}),}
#' for \eqn{\Delta_nl\leq t < (l+1)\Delta_n}.
#'@return The function returns the lag-dimensional vector
#' \eqn{(\hat a'(0), \hat a'(\Delta), \ldots, \hat a'((lag-1) \Delta)).}
#'@example man/examples/trawl_deriv_mod.R
#'@export
trawl_deriv_mod <- function(data, Delta, lag=100)
{
  incr <- base::diff(data)
  n <- base::length(data) #it includes time points 0, Delta, ...(n-1) Delta

  a_deriv <- vector(mode = "numeric", length = lag)

  for(j in 1:lag){
    a_deriv[j] <-base::sum(incr[(1+j):(n-1)]*incr[1:((n-1)-j)])/(n*Delta^2)
  }

  return(a_deriv)
}

#'This function estimates the derivative of the trawl function using
#'the empirical derivative of the trawl function.
#'@title Estimating the derivative of the trawl function using the
#'empirical derivative
#'@name trawl_deriv
#'@param data The data set used to compute the derivative of the trawl function
#'@param Delta The width Delta of the observation grid
#'@param lag The lag until which the trawl function should be estimated
#'@details According to
#'Sauri and Veraart (2022), the derivative of the trawl function can
#'be estimated based on observations
#'\eqn{X_0, X_{\Delta_n}, \ldots, X_{(n-1)\Delta_n}}  by
#' \deqn{\widehat a(t)=\frac{1}{\Delta_{n}}
#'(\hat a(t+\Delta_n)-\hat a(\Delta_n)),}
#' for \eqn{\Delta_nl\leq t < (l+1)\Delta_n}.
#'@return The function returns the lag-dimensional vector
#' \eqn{(\hat a'(0), \hat a'(\Delta), \ldots, \hat a'((lag-1) \Delta)).}
#'@example man/examples/trawl_deriv.R
#'@export
trawl_deriv <- function(data, Delta, lag=100)
{

  my_lag <- lag
  ahat <- nonpar_trawlest(data, Delta, lag=(my_lag+1))$a_hat

  a_deriv <-  base::diff(ahat)/Delta

  return(a_deriv)
}



#'This function computes the feasible statistics associated with the
#'CLT for the trawl function estimation.
#'@title Computing the feasible statistic of the trawl function CLT
#'@name test_asymnorm_est
#'@param data The data set based on observations of
#'\eqn{X_0, X_{\Delta_n}, \ldots, X_{(n-1)\Delta_n}}
#'@param Delta The width Delta of the observation grid
#'@param trawlfct The trawl function for which the
#'asymptotic variance will be computed (Exp, supIG or LM)
#'@param trawlfct_par The parameter vector of the trawl function
#'(Exp: lambda, supIG: delta, gamma, LM: alpha, H)
#'@param biascor A binary variable determining whether a bias correction should
#'be computed, the default is FALSE
#'@param k The optional parameter specifying the time point in
#'\eqn{0, 1, \ldots, n-1};
#'the test statistic will be computed for the time point \eqn{k \Delta_n}.
#'@details As derived in
#'Sauri and Veraart (2022), the feasible statistic, for \eqn{t>0},  is given by
#'\deqn{T(t)_n:=\frac{\sqrt{n\Delta_{n}}}{\sqrt{\widehat{\sigma_{a}^2(t)}}}
#'\left(\hat{a}(t)-a(t)-bias(t)\right).}
#'For \eqn{t=0}, we have \deqn{T(t)_n:=\frac{\sqrt{n\Delta_{n}}}{\sqrt{RQ_n}}
#'\left(\hat{a}(0)-a(0)-bias(0)\right),} where
#'\deqn{RQ_n:=\frac{1}{\sqrt{2 n\Delta_{n}}}
#' \sum_{k=0}^{n-2}(X_{(k+1)\Delta_n}-X_{k\Delta_n})^4.}
#'We set \eqn{bias(t)=0} in the case
#'when biascor==FALSE and \eqn{bias(t)=0.5 * \Delta * \hat a'(t)} otherwise.
#'@return The function returns the vector of the feasible statistics
#' \eqn{(T(0)_n, T((\Delta)_n, \ldots, T((n-2)\Delta_n))} if no bias correction
#' is required and \eqn{(T(0)_n, T((\Delta)_n, \ldots, T((n-3)\Delta_n))} if
#' bias correction is required if k is not provided, otherwise it returns the
#' value \eqn{T(k \Delta_n)_n}. If the estimated asymptotic variance is <= 0,
#' the value of the test statistic is set to 999.
#'@example man/examples/test_asymnorm_est.R
#'@export

test_asymnorm_est <- function(data, Delta, trawlfct, trawlfct_par,
                              biascor=FALSE, k=NULL)
{

  n <- base::length(data) #it includes time points 0, Delta, ...(n-1) Delta

  if(trawlfct=="Exp"){
    a <- function(x) {trawl_Exp(-x,trawlfct_par[1])}}
  if(trawlfct=="supIG"){
    a <- function(x) {trawl_supIG(-x,trawlfct_par[1],trawlfct_par[2])}}
  if(trawlfct=="LM"){
    a <- function(x) {trawl_LM(-x,trawlfct_par[1],trawlfct_par[2])}}


  #Set the variance of the Levy seed to 1
  varlevyseed <- 1

  #Estimate c4 from the data
  c4 <- c4est(data, Delta)

  #Estimate the trawl function vector
  #(\hat a(0), \hat a(Delta_n), ..., \hat a((n-2)\Delta_n))
  avector <- nonpar_trawlest(data, Delta, lag=n-1)$a_hat

  #Estimate the derivative of the trawl function for the bias correction
  deriv <- c(trawl_deriv_mod(data, Delta=Delta, lag=n-2),0) #add a 0 at the end



  if(is.null(k)){
    #Computing the asymptotic variance and test statistic vector
    av <- vector(mode = "numeric", length = n-1)
    teststats <- vector(mode = "numeric", length = n-1)
    bias <- vector(mode = "numeric", length = n-1)

    #Covering the case k=0
    if(biascor==TRUE){
      bias[1] <-0.5*Delta*deriv[1]
    }
    teststats[1] <- sqrt(n*Delta)*
      (avector[1]-a(0)-bias[1])/sqrt(rq(data, Delta))

  for(i in 2:(n-1)){
    av[i] <-
      asymptotic_variance_est((i-1)*Delta, c4, varlevyseed=1, Delta, avector)$v
    if(av[i]>0){
      if(biascor==TRUE){
        bias[i] <-0.5*Delta*deriv[i]
      }
      teststats[i] <-
        sqrt(n*Delta)*(avector[i]-a((i-1)*Delta)-bias[i])/sqrt(av[i])
    }
    else{
      teststats[i] <- 999
    }
  }
    if(biascor==TRUE){
      teststats <- teststats[1:(n-2)]
    }
  }
  else{ #if k has been provided

    #Covering the case k=0
    if(k==0){
      if(biascor==TRUE){
        bias <-0.5*Delta*deriv[1]
      }
      else{
        bias <- 0
      }
      teststats <- sqrt(n*Delta)*(avector[1]-a(0)-bias)/sqrt(rq(data, Delta))
    }
    else{
      av <-
        asymptotic_variance_est((k*Delta), c4, varlevyseed=1, Delta, avector)$v

      if(biascor==TRUE){
        bias <- 0.5*Delta*deriv[k+1]
      }
      else{
        bias <- 0
      }
      if(av > 0){
        teststats <- sqrt(n*Delta)*(avector[k+1]-a(k*Delta)-bias)/sqrt(av)
      }
      else{
        teststats <- 999
      }
    }

  }

  return(teststats)
}
