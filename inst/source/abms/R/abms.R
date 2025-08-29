#' For internal use

#' Womack prior
#' @description Womack probability mass function with 'K' predictors and parameter 'rho'.
#' @param K Number of predictors. It must be a positive integer.
#' @param rho Value for the "rho" parameter. It must be positive real number
#' @return Given that all models of the same hierarchy has the same prior probability, this function returns one value for each hierarchy.
#' including the null model (size=0)
#' @references {Womack, A., Fuentes, C., and Rodriguez-Taylor, D. (2015). "Model Space Priors for Objective Sparse Bayesian Regression." arXiv:1511.04745. 8:24}
#' @export
#' @examples #Fixing rho=1 and 3 predictors
#' womack(K=3, rho=1)
#' #it returns Womack prior for all models of size 0 (the null model), 1,2 and 3 (the full model)

womack = function(K, rho)
{
  if(length(rho)!=1){stop(paste("'rho' must be a vector of size 1")) }
  if(rho<=0){stop(paste("'rho' must be >0")) }
  if((round(K)!=K) || K<=0){ stop(paste("'K' must be a positive integer")) }
  out = base::rep( -Inf, K + 1 )
  base::names(out) = 0 : K
  out[ K + 1 ] = 0
  for( k in (K-1):0 )
  {
    j = (k+1):K
    bb = out[j+1] + base::lgamma(j+1) - base::lgamma(k+1) - base::lgamma(j+1-k) + base::log(rho)
    out[k+1] = base::log( base::sum( exp(bb - base::max(bb) )) ) + base::max(bb)
    out = out - base::max(out) -base::log(base::sum( exp(out - base::max(out) ) ) )
  }
  base::exp( out + base::lbeta(c(0:K) + 1, K - c(0:K) + 1 ) + base::log(K+1) )
}


#' Title
#' @description Sampling from the Chines Restaurant distribution.
#' @param n Number of observations. It must be a positive integer.
#' @param b Parameter distribution, a non-negative integer. The number of Bernoulli independent variables that are added. It can be a vector
#' @param c Parameter distribution, a positive real number. Used calculate the success probability the j-th Bernoulli independent variable, that is, c/(c +j -1). It can be a vector
#' @references {Pitman, Jim (1995). "Exchangeable and Partially Exchangeable Random Partitions". Probability Theory and Related Fields. 102 (2): 145:158}
#' @return This function generates \code{n} random variables from the \eqn{CRT}(\eqn{b,c}) distribution
#' @export
#' @examples #Generating 4 random variables with parameters b=2 and c=1
#' rCRT(4,2,1)
rCRT<-function(n,b,c)
{
  if((round(n)!=n) || n<=0 || length(n) > 1){ stop(paste("'n' must be a positive integer")) }
  if(any(b<0)){stop(paste("'b' must be a non-negative integer")) }
  if(any(c<=0)){stop(paste("'c' must be >0")) }
  if(1!=length(b) || 1!=length(c)){stop(paste("'b' and 'c' must be of size 1")) }

  x<-base::vector(length=n)
  ifelse(b!=0, aux<-matrix(nrow=n, ncol=b), aux<-matrix(nrow=n, ncol=1) )
  for(j in 1:b)
  {
    p =c/(c +j -1)
    if(p>1){p<-1}
    base::ifelse(b!=0, aux[,j]<-stats::rbinom(n,size=1, prob=p), aux[,j]<-base::rep(0,n))
  }
  x<-apply(aux,1,sum)
  return(x)

}


#' Bayesian variable selection models via a spike-and-slab methodology.
#' @description A Bayesian model selection methodology based on the spike-and-slab strategy and an augmentation technique for Linear, Logistic, Negative Binomial, Quantile, and Skew Normal Regression.
#' The model considers a response vector \eqn{y} of size \eqn{n} and \eqn{p} predictors to perform coefficient estimation and asses which ones are relevant to explain the response distribution. Other parameters related to the family selected are also estimated.
#' Summary results can be provided using the \code{summary_gibbs()} R function.
#' @param y A vector of size \eqn{n} with observed responses. It can also be a (\eqn{n x 1}) matrix.
#' @param Covariates A data.frame object with the predictors (without the intercept) for which we want to test if they are relevant to the response variable. It can also be a (\eqn{n x p}) matrix.
#' @param family A character object that describes the hierarchical regression model that will be used.
#' If \code{family="LiR"}, then a Linear regresion model will be fitted (gaussian errors).
#' If \code{family="LoR"}, then a Logistic regresion model will be fitted (binomial distribution).
#' If \code{family="NBR"}, then a Negative Binomial regresion model will be fitted (mean \eqn{r(1-p)/p}).
#' If \code{family="QR"}, then a Quantile regresion model will be fitted (Asymmetric Laplace errors).
#' If \code{family="SNR"}, then a Skew normal regresion model will be fitted (Skew-Normal errors).
#' The argument is fixed at \code{family="LiR"} by default.
#' @param first_excluded A non-negative integer that indicates which first columns will not be tested. For example, if \code{first_excluded=2}, the two first columns of \code{Covariates} will not be tested. Intercept is always excluded for the selection process.
#' @param nchain The Gibbs sampler's chain size, it must be a non-negative integer. The default value is 10,000
#' @param burnin The burn-in period of the Gibbs sampler, it must be a non-negative integer and greater than \code{nchain}. The default value is 2,000
#' @param tau2 The variance prior of each coefficient, it must be a positive real number. Fixed at 1 by deafault
#' @param rho The parameter of the Womack prior, it must be a positive real number. Fixed at 1 by deafault
#' @param ni For Logistic regression only. A vector of size \eqn{n} that represent the i-th individual size (the size parameter of the Binomial distribution) that it must be a positive integer. It can also be a (\eqn{n x 1}) matrix. For default, all individual size are fixed at 1.
#' @param alpha For Quantile regression only. The desired quantile for which we want to perform Quantile regression. \code{alpha} must be between (\eqn{0,1}). By default, \code{alpha=0.5}, that is, median regression.
#' @param a0 This argument depends on the family choosen.
#' For \code{family="LiR"}, is the shape hyper-parameter of the \eqn{Gamma} prior to the variance parameter (\eqn{\sigma^2}) of the Gaussian distribution.
#' For \code{family="NBR"} is the shape hyper-parameter of the \eqn{Gamma} prior to the parameter \eqn{r}  the Negative Binomial distribution (the number of successes until the experiment is stopped).
#' For \code{family="QR"} is the shape hyper-parameter of the \eqn{Gamma} prior to thevariance parameter (\eqn{\sigma^2}) of the Asymmetric Laplace distribution.
#' Note thas this argument do not exist for \code{family=LoR} and \code{family=SNR}. For all hierarchical regression models, it must be a positive real number and its fixed at 1 by deafault.
#' @param b0 This argument depends on the family choosen.
#' For \code{family="LiR"} is the scale hyper-parameter of the \eqn{Gamma} prior to the variance parameter (\eqn{\sigma^2}) of the Gaussian distribution.
#' For \code{family="NBR"} is the scale hyper-parameter of the \eqn{Gamma} prior to the parameter \eqn{r}  the Negative Binomial distribution (the number of successes until the experiment is stopped).
#' For \code{family="QR"} is the scale hyper-parameter of the \eqn{Gamma} prior to the variance parameter (\eqn{\sigma^2}) of the Asymmetric Laplace distribution.
#' Note thas this argument do not exist for \code{family=LoR} and \code{family=SNR}. For all hierarchical regression models, it must be a positive real number and its fixed at 1 by deafault.
#' @param d For the Skew-Normal regression only. It is the location hyper-parameter of the t-student prior to the parameter \eqn{lambda} (asymmetric parameter of the Skew-Normal distribution). By default is fixed at 2, which is recommended.
#' @param b2 For the Skew-Normal regression only. It is the scale hyper-parameter of the t-student prior to the parameter lambda (asymmetric parameter of the Skew-Normal distribution). By default is fixed at 1/2, which is recommended.
#' @param model_fixed Either \code{NULL} or a vector that indicates which model will be fixed to perform parameter estimation only under such a model. For example, if there are only three predictors and \code{model.fixed=c(1,3)}, only parameter estimation will be performed where only the first and third predictors are included. If \code{NULL}, model selection will also be performed. Fixed at \code{NULL} by default.
#' @param WomackPrior A logical argument. If \code{TRUE}, the Womack prior for the model space will be used. Otherwise, the Beta-Binomial prior with shape parameters \code{a_bb} and \code{b_bb} will be used. Fixed at \code{TRUE} by default
#' @param a_bb A numeric vector of length 1. The first shape parameter of the Beta-Binomial prior. Recomended value is \code{a_bb=1}.
#' @param b_bb A numeric vector of length 1. The second shape parameter of the Beta-Binomial prior. Recomended value is \code{b_bb=p_selection^u}, where \code{u}>1, and \code{p_selection} is the number of predictors under the selection process.
#' @param count.iteration A logical argument. If \code{TRUE}, a counter for the Gibbs sampler iterations will be displayed. Fixed at \code{TRUE} by deafult.
#' @return A abms object with the following variables:
#' @return \item{family}{This character object prints the name of the fitted hierarchical regression model. It needs to be extracted from the list 'Default'.}
#' @return \item{prednames}{A character object that prints the predictors names, using the columns names of the \code{Covariates} argument. It needs to be extracted from the list 'Default'.}
#' @return \item{Seconds}{How many seconds the method took. It needs to be extracted from the list 'Default'.}
#' @return \item{tau2}{The \code{tau2} that was used as argument.}
#' @return \item{y}{The \code{y} response vector that was used as argument.}
#' @return \item{Covariates}{The \code{Covariates} data frame or matrix that was used as argument.}
#' @return \item{beta_chain}{The coefficients sample for each Gibbs sampler iteration. A (\code{nchain} x \eqn{p}) matrix}
#' @return \item{sigma2_chain}{For the Linear, Quantile and Skew-Normal regression only. The variance parameter (\eqn{\sigma^2}) sample for each Gibbs sampler iteration. A (\code{nchain} x 1) matrix}
#' @return \item{r_chain}{For the Negative-Binomial regression only. The number of failure parameter (\eqn{r}) sample for each Gibbs sampler iteration. A (\code{nchain} x 1) matrix}
#' @return \item{lambda_chain}{For the Skew-Normal regression only. The asymmetric parameter (\eqn{\lambda}) sample for each Gibbs sampler iteration. A (\code{nchain} x 1) matrix}
#' @return \item{model_chain}{The model selected at each Gibbs sampler iteration. A (\code{nchain} x \eqn{p}) matrix.}
#' @return \item{Z_chain}{For internal use.}
#' @return \item{t_chain}{For internal use.}
#'
#' @references{Azzalini (1985). A class of distributions which includes the normal ones, Scandinavian Journal of Statistics 12(2): 171:178.}
#' @references{Bayes, C. and Branco, M. (2007). Bayesian inference for the skewness parameter of the scalar skew-normal distribution. Brazilian Journal of Probability and Statistics. 21: 141:163.}
#' @references{Kotz, S., Kozubowski, T. and Podgorski, K. (2001). The Laplace Distribution and Generalization, first edn, Birkhauser Basel.}
#' @references{Polson, N., Scott, J., and Windle, J. (2013). Bayesian Inference for Logistic Models Using Polya Gamma Latent Variables. Journal of the American Statistical Association, 108: 1339:1349.}
#' @references{Zhou, W. and Carin, L. (2013). Negative Binomial Process Count and Mixture Modeling. arXiv:1405.0506v1.}
#'
#' @export
#' @examples
#' ##################################################
#' ## 	    	Gibbs for Linear Regression 		##
#' ##################################################
#'
#' ## Simulating data
#' set.seed(31415)
#' N<-200
#' r_beta<-as.matrix(c(1, 0, 2, 0))
#' r_p<-length(r_beta)
#' r_sigma2<-1.5
#' X<-matrix( c(rep(1, N), rnorm((r_p -1)*N)), ncol=r_p )
#' Xbeta<-X%*%r_beta
#' y<-rnorm(N, mean=Xbeta , sd=sqrt(r_sigma2))
#' Covariates<-X[,2:(length(r_beta))];
#' colnames(Covariates)<-c("X1", "X2", "X3")
#'
#' ## Fitting the model
#' fit<- gibbs_abms(y, Covariates, family="LiR", first_excluded=0, nchain=1000, burnin=20,
#'  a0=1, b0=1)
#'
#' summary_gibbs(fit, BF=FALSE)	#Summary results
#'
#' ## For more examples, see "Model Ilustrations.R" file in
#' ## https://github.com/SirCornflake/BMS


gibbs_abms<-function(y, Covariates, family="LiR", first_excluded=0, nchain=10000, burnin=2000, tau2=1000, rho=1, ni=rep(1, length(y)), alpha=0.5,
                     a0=1, b0=1, d=2, b2=1/2, model_fixed=NULL, WomackPrior=TRUE, a_bb=1, b_bb=1, count.iteration=TRUE )
{
  print.abms <- function(m){
    print(m$Default)
  }

  if(( !inherits(y,"numeric") && !inherits(y,"integer") )  || is.vector(y)==FALSE){stop(paste("'y' must be a numeric or integer vector")) }
  if(!inherits(Covariates, "data.frame") && !inherits(Covariates, "matrix")){stop(paste("'Covariates' must be a data.frame or a matrix")) }
  if(length(first_excluded)>1 || first_excluded<0){stop(paste("'first_excluded' must be a non-negative integer")) }
  if(length(nchain)>1 || nchain<=0){stop(paste("'nchain' must be a positive integer")) }
  if(burnin<0 || burnin>nchain){stop(paste("'burnin' must be a non-negative integer that equal less than nchain ")) }
  if(tau2<=0){stop(paste("'tau2' must be a positive integer")) }
  if(tau2<=10) warning("It is recomended for 'tau2' equal at least 1000.")
  if(rho<=0){stop(paste("'rho' must be a positive integer")) }
  if(!inherits(model_fixed,"NULL") && !inherits(model_fixed,"numeric")){ stop(paste("'model_fixed' must be NULL or a vector of size 'p-1' at most")) }
  if(inherits(model_fixed,"numeric") && length(model_fixed)>(ncol(Covariates)) ){ stop(paste("'model_fixed' must be a vector of size 'p-1' at most")) }
  if(WomackPrior!=TRUE && WomackPrior!=FALSE){stop(paste("'WomackPrior' must be either TRUE or FALSE")) }
  if(count.iteration!=TRUE && count.iteration!=FALSE){stop(paste("'count.iteration' must be either TRUE or FALSE")) }

  if(a0<=0){stop(paste("'a0' must be a positive real number")) }
  if(b0<=0){stop(paste("'b0' must be a positive real number")) }
  if(inherits(family,"LoR") && (length(ni)!=length(y) || any(ni<0)) ){stop(paste("'ni' must be a positive integer vector with same size as 'y'")) }
  if(alpha>1 || alpha<0){stop(paste("'alpha' must be between 0 and 1")) }
  if(d<=0){stop(paste("'d' must be a positive real number")) }
  if(b2<=0){stop(paste("'b2' must be a positive real number")) }
  if(a_bb<=0){stop(paste("'a_bb' must be a positive real number")) }
  if(b_bb<=0){stop(paste("'b_bb' must be a positive real number")) }

  t0<-proc.time()

  ## Beta-Binomial pior2 PDF
  BetaBinomialPrior<-function(p, a, b)
  {
    aux<-vector(length=p+1)
    for(i in 0:p)
    {
      aux[i+1]<-beta(a=a +i, b=p +b -i)/beta(a=a,b=b)
    }
    return(aux)
  }


  ##############################
  ##	   Initiating	    ##
  ##############################
  N <- length(y)
  p<-length(Covariates[1,]) +1
  if(length(colnames(Covariates))==0)
  {
    aux_prednames<-base::vector(length=ncol(Covariates)); for(i in 1:ncol(Covariates)){aux_prednames[i]<-c(paste0("X",i))}
    colnames(Covariates)<-aux_prednames
  }

  X<-cbind(base::rep(1,N), Covariates)
  X<-as.matrix(X)
  colnames(X)<-NULL
  intercept_first_excluded<- first_excluded +1
  p_selection<-p -intercept_first_excluded

  beta.chain<-matrix(1, nrow=nchain, ncol=p, byrow=TRUE)
  beta<-beta.chain[1,]
  Z.chain<-matrix(NA, nrow=nchain, ncol=N, byrow=TRUE)
  t.chain<-matrix(NA, nrow=nchain, ncol=N, byrow=TRUE)
  gammaOld<-base::rep(1, p_selection)

  if(family=="LiR")
  {
    invsigma2.chain<-matrix(1, nrow=nchain, ncol=1, byrow=TRUE)
    invsigma2<-invsigma2.chain[1,]
    sigma2.chain<-matrix(0, nrow=nchain, ncol=1, byrow=TRUE)
    Z<-matrix(y)
    t.chain[1,]<-invsigma2
  }

  if(family=="LoR")
  {
    w<-rep(1,length(y))
    kappa <- y - ni/2
    Z<-kappa/w
    t.chain[1,]<-w
  }

  if(family=="NBR")
  {
    y_unique<-sort(unique(y))
    y_unique_freq<-as.integer(table(y))
    r.chain<-matrix(1, nrow=nchain, ncol=1, byrow=TRUE)
    r<-r.chain[1,]
    l<-1
    w<-rep(1,length(y))
    Z<-matrix((y-r)/(2*w))
    t.chain[1,]<-w
  }

  if(family=="QR")
  {
    invsigma2.chain<-matrix(1, nrow=nchain, ncol=1, byrow=TRUE)
    invsigma2<-invsigma2.chain[1,]
    sigma2.chain<-matrix(0, nrow=nchain, ncol=1, byrow=TRUE)
    w<-rep(1,length(y))
    Z.chain<-matrix(NA, nrow=nchain, ncol=N, byrow=TRUE)
    t.chain<-matrix(NA, nrow=nchain, ncol=N, byrow=TRUE)
    delta2<-2/(alpha*(1-alpha))
    xi<-(1 -2*alpha)/(alpha*(1-alpha))
    Z<-matrix(y -xi*w)
    t.chain[1,]<-1/(delta2*w)*invsigma2
  }

  if(family=="SNR")
  {
    lambda.chain<-matrix(1, nrow=nchain, ncol=1, byrow=TRUE)
    sigma2.chain<-matrix(2, nrow=nchain, ncol=1, byrow=TRUE)
    w<-rep(1,length(y))
    lambda<-lambda.chain[1,]
    kappa<-1
    invzeta2<-1
    zeta2<-1
    v<-1
    Z<-matrix(y -kappa*w)
    t.chain[1,]<-invzeta2
  }

  model_chain<-matrix(NA, nrow=(nchain- burnin), ncol=p_selection)

  log_tau2<-base::log(tau2)
  frac_tau2<-1/tau2
  if(WomackPrior==TRUE){ log_gamma_prior<-base::log(womack(p_selection,rho)) }else{
    log_gamma_prior<-log( BetaBinomialPrior(p=p_selection, a=a_bb, b=b_bb) )
  }

  InvDiag_tau2_general<-diag(frac_tau2, p)
  Omega_modified_general<-matrix(base::rep(t.chain[1,],each = p), nrow = p, ncol = N, byrow = F)
  tXgamma_Omega_general<-t(X)*Omega_modified_general[1:ncol(X),]
  NotInverse_V_general<-tXgamma_Omega_general%*%X + InvDiag_tau2_general
  tXgamma_Omega_Z_general<- tXgamma_Omega_general%*%Z

  X_index_aux<-base::vector("list", length = 2)
  X_gamma_aux<-base::vector("list", length = 2)
  tXgamma_Omega_aux<-base::vector("list", length = 2)
  chol_V_aux<-base::vector("list", length = 2)
  V_aux<-base::vector("list", length = 2)
  m_aux<-base::vector("list", length = 2)
  X_index_aux_excluded<-c(1:intercept_first_excluded)
  DAlog_weight_model<-base::vector(length=2)


  ######################
  ## 	 Gibbs	  ##
  ######################
  for(i in 1:nchain)
  {
    if(count.iteration==TRUE){cat("  Iteracion", i, "de", nchain, "\r")}

    if(is.null(model_fixed)==TRUE)
    {

      ## Add-delete algorithm
      aux_gamma<-sample(1:p_selection, size=1)
      gammaCandidate<-gammaOld
      gammaCandidate[aux_gamma]<-1 -gammaOld[aux_gamma]

      for(k in 1:2)
      {
        if(k==1){gamma_aux<-gammaOld}else{gamma_aux<-gammaCandidate}
        q_aux<-base::sum(gamma_aux)
        X_index_aux[[k]]<-unique(c(X_index_aux_excluded, intercept_first_excluded +which(gamma_aux==1)))
        X_gamma_aux[[k]]<-X[,X_index_aux[[k]]]
        tXgamma_Omega_aux[[k]]<- tXgamma_Omega_general[X_index_aux[[k]],]
        tXgamma_Omega_aux_Z<- tXgamma_Omega_Z_general[X_index_aux[[k]]]
        InvDiag_tau2_aux<-InvDiag_tau2_general[1:(intercept_first_excluded +q_aux), 1:(intercept_first_excluded +q_aux)]
        chol_V_aux[[k]]<- chol(NotInverse_V_general[X_index_aux[[k]], X_index_aux[[k]]])
        V_aux[[k]]<- chol2inv(chol_V_aux[[k]])
        det_V_aux<-1/prod(diag(chol_V_aux[[k]]))^(2)
        if(det_V_aux<1e-10){det_V_aux<-1e-10}
        m_aux[[k]]<-V_aux[[k]]%*%tXgamma_Omega_aux_Z
        DAlog_weight_model[k]<-as.vector( (-q_aux/2)*log_tau2 +(0.5)*base::log(det_V_aux) +
                                            (0.5)*t(m_aux[[k]])%*%tXgamma_Omega_aux_Z +log_gamma_prior[q_aux +1] )
      }
      A<- DAlog_weight_model -base::max(DAlog_weight_model)
      b<-exp(A)
      DAgamma_prob<-b/base::sum(b)

      p_gammaOld<-DAgamma_prob[1]
      p_gammaCandidate<-DAgamma_prob[2]
      p_selectionCandidate<-min(p_gammaCandidate/p_gammaOld,1)
      selection<-sample(c("Candidate","Old"), size=1, prob=c(p_selectionCandidate, 1-p_selectionCandidate))
      if(selection=="Candidate"){gamma<-gammaCandidate; k<-2}else{gamma<-gammaOld; k<-1}
      q<-base::sum(gamma)
    }else{
      gamma<-rep(0, p-1)
      gamma[model_fixed]<-1
      k<-1
      X_index_aux[[k]]<-unique(c(X_index_aux_excluded, intercept_first_excluded +which(gamma==1)))
      X_gamma_aux[[k]]<-X[,X_index_aux[[k]]]
      tXgamma_Omega_aux[[k]]<- tXgamma_Omega_general[X_index_aux[[k]],]
      tXgamma_Omega_aux_Z<- tXgamma_Omega_Z_general[X_index_aux[[k]]]
      chol_V_aux[[k]]<- chol(NotInverse_V_general[X_index_aux[[k]], X_index_aux[[k]]])
      V_aux[[k]]<- chol2inv(chol_V_aux[[k]])
      m_aux[[k]]<-V_aux[[k]]%*%tXgamma_Omega_aux_Z
    }

    ## Updating beta
    beta_index_0<-setdiff(1:p, X_index_aux[[k]])
    if(length(beta_index_0)==0){beta_index_0<-0}
    beta[X_index_aux[[k]]]<-t(mvtnorm::rmvnorm(n = 1, mean = m_aux[[k]], sigma = V_aux[[k]], method="chol"))
    beta[beta_index_0]<-base::rep(0,length(beta_index_0))
    beta.chain[i,] <- beta
    if(i>burnin){model_chain[i -burnin,]<-gamma}

    ## Updating gamma
    gammaOld<-gamma
    X_beta <- as.matrix(X_gamma_aux[[k]])%*%as.matrix(beta[X_index_aux[[k]]])

    if(family=="LiR")
    {
      ## Updating invsigma2=1/sigma2
      a1<-a0 +N/2
      b1<-b0 +base::sum( (y -X_beta)^2 )/2
      invsigma2<-stats::rgamma( 1, shape= a1, rate= b1 )
      invsigma2.chain[i,]<-invsigma2

      ## Updating gamma
      gammaOld<-gamma

      ## Updating other quantities
      Z<-matrix(y)
      Z.chain[i,]<-Z
      t.chain[i,]<-invsigma2
    }

    if(family=="LoR")
    {
      ## Updating w
      w <- BayesLogit::rpg.devroye(num = N, h = ni, z = as.numeric(X_beta))

      ## Updating other quantities
      Z<-kappa/w
      Z.chain[i,]<-Z
      t.chain[i,]<-w
    }

    if(family=="NBR")
    {
      ## Updating w
      w <- BayesLogit::rpg(num = N, h = y +r, z = as.numeric(X_beta))

      ## Updating l
      for(j in 1:length(y_unique))
      {
        aux_l<-rCRT(y_unique_freq[j], b=y_unique[j], c=r)
        index_l<-which(y==y_unique[j])
        l[index_l]<-aux_l
      }

      ## Updating r
      e_Xbeta<-as.vector(exp(X%*%beta))
      r<-stats::rgamma( 1, shape= a0 +base::sum(l), rate= b0 +base::sum(base::log(1 +e_Xbeta)) )
      r.chain[i,]<-r

      ## Updating other quantities
      Z<-matrix((y-r)/(2*w))
      Z.chain[i,]<-Z
      t.chain[i,]<-w
    }

    if(family=="QR")
    {
      ## Updating w
      chi<-invsigma2*((y -X_beta)^2)/delta2; psi<- invsigma2*(xi^2 +2*delta2)/delta2
      for(j in 1:length(y))
      {
        w[j]<-GIGrvg::rgig(1, lambda=1/2, chi=chi[j], psi=psi)
      }

      ## Updating invsigma2=1/sigma2
      a1<-a0 +3*N/2
      b1<-b0 +base::sum( ((y -X_beta -xi*w)^2 +2*delta2*w^2)/(2*delta2*w) )
      invsigma2<-stats::rgamma( 1, shape= a1, rate= b1 )
      invsigma2.chain[i,]<-invsigma2

      ## Updating other quantities
      Z<-matrix(y -xi*w)
      Z.chain[i,]<-Z
      t.chain[i,]<-1/(delta2*w)*invsigma2
    }

    if(family=="SNR")
    {
      ## Updating w
      mu_w<-(y*kappa -X_beta*kappa)/(kappa^2 +zeta2); sigma2_w<- (zeta2)/(kappa^2 +zeta2 )
      w<-truncnorm::rtruncnorm(n=N, a=0, b=Inf, mean=mu_w, sd=sqrt(sigma2_w))

      ## Updating invzeta2=1/zeta2
      aux_zeta1<-0.5*(N+1) +1
      aux_zeta2<-0.5*( (v*kappa^2/b2) +base::sum( (y -X_beta -kappa*w)^2 ) )
      invzeta2<-stats::rgamma( 1, shape= aux_zeta1, rate= aux_zeta2 )
      zeta2<-1/invzeta2

      ## Updating kappa
      mu_kappa<-base::sum(y*w -X_beta*w)/(v/b2 +base::sum(w^2))
      sigma2_kappa<-(zeta2)/(v/b2 +base::sum(w^2))
      kappa<-stats::rnorm(1, mean=mu_kappa, sd=sqrt(sigma2_kappa))

      ## Updating v
      shape_v<- (d+1)/2
      rate_v<- 0.5*( invzeta2*(kappa^2)/b2 +d )
      v<-stats::rgamma(1, shape=shape_v, rate=rate_v)

      ## Updating other quantities
      Z<-matrix(y -kappa*w)
      Z.chain[i,]<-Z
      t.chain[i,]<-invzeta2

      ## Saving original SN parameters
      lambda<-kappa/sqrt(zeta2)
      lambda.chain[i,]<-lambda
      sigma2<-zeta2 +kappa^2
      sigma2.chain[i,]<-sigma2
    }

    Omega_modified_general<-matrix(base::rep(t.chain[i,],each = p), nrow = p, ncol = N, byrow = F)
    tXgamma_Omega_general<-t(X)*Omega_modified_general[1:ncol(X),]
    NotInverse_V_general<-tXgamma_Omega_general%*%X + InvDiag_tau2_general
    tXgamma_Omega_Z_general<- tXgamma_Omega_general%*%Z
  }

  t1<-proc.time()
  Time<-(t1-t0)[3]

  if(first_excluded!=0)
  {
    prednames<-colnames(Covariates)[-(1:first_excluded)]
  }else{prednames<-colnames(Covariates)}

  beta.chain<-beta.chain[(burnin +1):nchain,]

  if(family=="LiR" || family=="QR")
  {
    sigma2.chain<-1/invsigma2.chain[(burnin +1):nchain,]
    Output<-list(tau2=tau2, y=y, Covariates=Covariates, Z_chain=Z.chain, t_chain=t.chain, beta_chain=beta.chain, sigma2_chain=sigma2.chain, model_chain=model_chain, Default=list(family=family, prednames=prednames, Seconds=Time))
  }

  if(family=="LoR")
  {
    Output<-list(tau2=tau2, Covariates=Covariates, Z_chain=Z.chain, t_chain=t.chain, beta_chain=beta.chain, model_chain=model_chain, Default=list(family=family, prednames=prednames, Seconds=Time))
  }

  if(family=="NBR")
  {
    r.chain<-r.chain[(burnin +1):nchain,]
    Output<-list(tau2=tau2, y=y, Covariates=Covariates, Z_chain=Z.chain, t_chain=t.chain, beta_chain=beta.chain, r_chain=r.chain, model_chain=model_chain, Default=list(family=family, prednames=prednames, Seconds=Time))
  }

  if(family=="SNR")
  {
    lambda.chain<-lambda.chain[(burnin +1):nchain,]
    sigma2.chain<-sigma2.chain[(burnin +1):nchain,]
    Output<- list(tau2=tau2, y=y, Covariates=Covariates, Z_chain=Z.chain, t_chain=t.chain, beta_chain=beta.chain, sigma2_chain=sigma2.chain, lambda_chain=lambda.chain, model_chain=model_chain, Default=list(family=family, prednames=prednames, Seconds=Time))
  }

  class(Output) <- 'abms'
  Output
}



#' Summary function for abms objects
#' @description For abms objects, it returns the posterior mean, standard deviation, and 95% centered credible interval for each parameter. Additionally, it provides all explored models alongside the conditional Bayes factors and marginal Bayes factors estimator between the most probable model and the others that have arisen.
#' @param fit An abms object. Such object is obtained by fitting a regression model with the \code{gibbs_abms()} function.
#' @param BF A logical object. if \code{TRUE}, then the Bayes factor comparison is shown.\code{BF=FALSE} by default.
#'
#' @return A summary of the inference performed by the Bayesian model obtained by the \code{gibbs_abms()} function. The variables are:
#' @return \item{Mean_IC}{A table with the posterior mean, standard deviation, and 95% centered credible interval for each parameter}
#' @return \item{Explored_Models}{A table with all explored models. If \code{BF=TRUE}, the conditional Bayes factors and marginal Bayes factors estimator between the most probable model and the others that have arisen are displayed.}
#' @export
#'
#' @examples
#' ## See \code{gibbs_abms()} help page function

summary_gibbs<-function(fit, BF=FALSE)
{
  if(!inherits(fit,"abms")){stop(paste("'fit' must be a 'abms' class object")) }
  if(BF!=TRUE && BF!=FALSE){stop(paste("'BF' must be either TRUE or FALSE")) }

  ## Auxiliar function for Summary table for Gibbs sampler
  aux_summary_gibbs<-function(fit)
  {
    ExploredModels<-unique(fit$model_chain)
    TableExploredModels<-data.frame(ExploredModels)
    TableExploredModels[,ncol(TableExploredModels) +1]<-base::rep(0,nrow(TableExploredModels))
    colnames(TableExploredModels)<-c(fit$Default$prednames, "Proportion")

    for(j in 1:nrow(ExploredModels))
    {
      num_selected<-0
      for(i in 1:nrow(fit$model_chain))
      {
        if(all(fit$model_chain[i,]==ExploredModels[j,])==TRUE){num_selected<-num_selected +1}
      }
      TableExploredModels[j,length(fit$Default$prednames)+1]<-num_selected/nrow(fit$model_chain)

    }
    TableExploredModels<-TableExploredModels[order(TableExploredModels$Proportion, decreasing=TRUE),]
  }

  aux_BF<-function(fit,ExploredModels)
  {
    aux_ExploredModels<- ExploredModels[, -ncol(ExploredModels)]
    Indexes<-vector(mode="list", length=nrow(aux_ExploredModels))
    loglik<-vector(mode="list", length=nrow(aux_ExploredModels))
    X_index_aux<-vector(mode="list", length=nrow(aux_ExploredModels))
    X_gamma<-vector(mode="list", length=nrow(aux_ExploredModels))
    InvDiag_tau2_aux<-vector(mode="list", length=nrow(aux_ExploredModels))
    X_index_aux<-vector(mode="list", length=nrow(aux_ExploredModels))
    tau2<-fit$tau2
    Covariates<-fit$Covariates
    p<-ncol(Covariates) +1
    first_excluded<-abs(ncol(Covariates) -length(aux_ExploredModels))
    intercept_first_excluded<- first_excluded +1
    p_selection<-p -intercept_first_excluded

    N<-nrow(Covariates)
    X<-as.matrix(cbind(base::rep(1,N), Covariates))
    InvDiag_tau2_general<-diag(1/tau2, p)
    X_index_aux_excluded<-c(1:intercept_first_excluded)

    for(j in 1:nrow(aux_ExploredModels))
    {
      q_aux<-base::sum(aux_ExploredModels[j,])
      X_gamma[[j]]<-as.matrix(fit$Covariates[,which(aux_ExploredModels[j,]==1)])
      InvDiag_tau2_aux[[j]]<-InvDiag_tau2_general[1:(intercept_first_excluded +q_aux), 1:(intercept_first_excluded +q_aux)]
      X_index_aux[[j]]<-unique(c(X_index_aux_excluded, intercept_first_excluded +which(aux_ExploredModels[j,]==1)))

    }
    for(i in 1:nrow(fit$beta_chain))
    {
      Z<-fit$Z_chain[i,]; t_Omega<-fit$t_chain[i,]
      Omega_modified_general<-matrix(base::rep(t_Omega,each = p), nrow = p, ncol = N, byrow = F)
      tXgamma_Omega_general<-t(X)*Omega_modified_general[1:ncol(X),]
      NotInverse_V_general<-tXgamma_Omega_general%*%X + InvDiag_tau2_general
      tXgamma_Omega_Z_general<- tXgamma_Omega_general%*%Z
      for(j in 1:nrow(aux_ExploredModels))
      {

        tXgamma_Omega_aux_Z<- tXgamma_Omega_Z_general[X_index_aux[[j]]]
        chol_V_aux<- chol(NotInverse_V_general[X_index_aux[[j]], X_index_aux[[j]]])  #Cholesky of "not inverted" V
        V_aux<- chol2inv(chol_V_aux)
        m_aux<-V_aux%*%tXgamma_Omega_aux_Z
        det_V_aux<-1/prod(diag(chol_V_aux))^(2)

        loglik[[j]][i]<-0.5*log(det_V_aux) +(0.5)*t(m_aux)%*%tXgamma_Omega_aux_Z -0.5*p_selection*log(tau2)
      }
    }
    New_ExploredModels<-ExploredModels
    for(j in 1:nrow(aux_ExploredModels))
    {
      Marginal_BF<-mean(loglik[[1]]) -mean(loglik[[j]])
      New_ExploredModels[j,ncol(ExploredModels) +1]<-Marginal_BF
    }
    colnames(New_ExploredModels)[-(1:ncol(ExploredModels))]<-c("Log_Marginal_BF_Estimator")
    return(New_ExploredModels)
  }

  ExploredModels<-aux_summary_gibbs(fit)
  if(BF==TRUE)
  {
    ExploredModels<-aux_BF(fit,ExploredModels)
    most_model<-ExploredModels[1,-((ncol(ExploredModels) -1):ncol(ExploredModels)) ]
  }else{most_model<-ExploredModels[1,-ncol(ExploredModels)]}

  beta_index<-c()
  for(j in 1:nrow(fit$beta_chain))
  {
    if(prod(most_model==fit$model_chain[j,])==1){beta_index<-c(beta_index,j)}
  }
  beta_chain<-fit$beta_chain[beta_index,]
  if(fit$Default$family=="LiR"){parameters<-cbind(fit$sigma2_chain[beta_index], beta_chain); aux_rownames<-c("sigma2")}
  if(fit$Default$family=="LoR"){parameters<-cbind(beta_chain); aux_rownames<-c()}
  if(fit$Default$family=="NBR"){parameters<-cbind(fit$r_chain[beta_index], beta_chain); aux_rownames<-c("r")}
  if(fit$Default$family=="QR"){parameters<-cbind(fit$sigma2_chain[beta_index], beta_chain); aux_rownames<-c("sigma2")}
  if(fit$Default$family=="SNR"){parameters<-cbind(fit$sigma2_chain[beta_index], fit$lambda_chain[beta_index], beta_chain); aux_rownames<-c("sigma2", "lambda")}

  p<-ncol(beta_chain)
  Mean<-apply(parameters, 2, mean)
  Quantile<-t(apply(parameters, 2, stats::quantile, prob=c(0.025,0.975)))
  SD<-t(apply(parameters, 2, stats::sd))
  Table<-as.data.frame(matrix(c(Mean,Quantile, SD), byrow=FALSE, ncol=4))
  colnames(Table)<-c("Mean", "2.5% quantile", "97.5% quantile", "SD")
  rownames(Table)<-c(aux_rownames, "intercept", fit$Default$prednames)

  list(Mean_IC=Table, Explored_Models=ExploredModels)
}


#' Logistic Regression Data generator
#' @description It generates \code{N} observations of the Binomial distribution with parameters \code{ni} (the i-th's individual sizes) and \code{p} (the success probability), where the coefficients are indexed on \code{p} via the logistic function.
#' @param beta A vector of coefficients including the intercept. It can be a matrix.
#' @param Covariates A data.frame object with the predictors (without intercept) for which we want to test if they are relevant to the response variable. It can also be a (\eqn{n x p}) matrix.
#' @param N The number of observations that will be generated. It must be a positive integer.
#' @param ni A vector of size \eqn{n} that represent the i-th individual size (the size parameter of the binomial distribution). It can also be a (\eqn{n x 1}) matrix. For default, all individual size are fixed at 1.
#'
#' @return The function return a table with the sample of size N from the Binomial distribution indexed with the predictors indicated in the \code{Covariates} argument, the \code{ni}, the number of failures (\code{ni} - \code{y}), and the predictors for each individual .
#' @export
#'
#' @examples
#' N<-200    #Number of extractions
#' beta<-c(1, 0, 2, 0, 3, 2)    #Coefficient vector
#' p<-length(beta)
#' aux_cov<-rnorm((p-1)*N, 0,1)
#' Covariates<-data.frame(matrix(aux_cov, ncol=p-1, nrow=N))   #Generating the Covariates data.frame
#' colnames(Covariates)<-c("X1", "X2", "X3", "X4", "X5")
#' base<-gen_base_binomial_reg(N, beta, Covariates, ni=rep(1, N))    #Generating the data
#' base
gen_base_binomial_reg<- function(N, beta, Covariates, ni=rep(1, N))
{
  if(N<=0){stop(paste("'N' must be a positive integer")) }
  if(length(beta)!=(ncol(Covariates) +1)){stop(paste("'beta' must be have same size as 'ncol(Covariates)'")) }
  if(!inherits(Covariates, "data.frame") && !inherits(Covariates, "matrix")){stop(paste("'Covariates' must be a data.frame or a matrix")) }


  if(length(ni)!=N || any(ni<0)){stop(paste("'ni' must be a positive integer vector with same size as 'y'")) }

  p<-length(Covariates[1,]) +1; x<-c()
  for(i in 1:(p -1))
  {
    x<-c(x,Covariates[,i])
  }
  X<-matrix(c(rep(1,N),x), ncol = p, nrow = N, byrow=FALSE)


  beta_matrix<-as.matrix(beta)
  prob<- exp(X%*%beta_matrix)/(1 + exp(X%*%beta_matrix))

  y<-vector(length=N)
  for(i in 1:N)
  {
    y[i] <- stats::rbinom(n = 1, size = ni[i], prob = prob[i])
  }

  base <- data.frame(y = y,
                     n_failure = ni - y,
                     ni = ni,  #
                     Covariates = Covariates)
  colnames(base)[-(1:3)]<-colnames(Covariates)
  return(base)
}



#' Negative Binomial Regression Data generator
#' @description It generates \code{N} observations of the Negative binomial distribution with parameters \code{r} (number of success) and \eqn{p} (success probability), where the coefficients are indexed on \eqn{p} via the logistic function.
#' @param N The number of observations that will be generated. It must be a positive integer.
#' @param beta A vector of coefficients including the intercept. It can be a matrix.
#' @param r The number of success parameter. It must be a positive integer.
#' @param Covariates A data.frame object with the predictors (without intercept) for which we want to test if they are relevant to the response variable. It can also be a (\eqn{n x p}) matrix.
#'
#' @return The function return a sample of size N from the Negative binomial distribution indexed with the predictors indicated in the \code{Covariates} argument, and the predictors for each individual.
#' @export
#'
#' @examples
#' N<-10   #Number of extractions
#' beta<-c(0.5, -0.8,  1.0,  0,  0.4, -0.7)  #Coefficient vector
#' p<-length(beta)
#' r<-2    #Number of success parameter
#' aux_cov<-rnorm((p-1)*N, 0,1)
#' Covariates<-data.frame(matrix(aux_cov, ncol=p-1, nrow=N))   #Generating the Covariates data.frame
#' colnames(Covariates)<-c("X1", "X2", "X3", "X4", "X5")
#' base<-gen_base_NegBinomial_reg(N, beta, r, Covariates)    #Generating the data
#' base
gen_base_NegBinomial_reg<- function(N, beta, r, Covariates)
{
  if(N<=0){stop(paste("'N' must be a positive integer")) }
  if(length(beta)!=(ncol(Covariates) +1)){stop(paste("'beta' must be have same size as 'ncol(Covariates)'")) }
  if(r<=0){stop(paste("'r' must be a positive integer")) }
  if(!inherits(Covariates, "data.frame") && !inherits(Covariates, "matrix")){stop(paste("'Covariates' must be a data.frame or a matrix")) }


  X<-as.matrix(Covariates)
  X<-cbind(rep(1,N),X)
  colnames(X)<-NULL

  beta_matrix<-as.matrix(beta)
  prob<- 1/(1 + exp(X%*%beta_matrix))

  y<-vector(length=N)

  for(i in 1:N)
  {
    y[i]<-stats::rnbinom(1, size=r, prob= prob[i])
  }

  base <- data.frame(y = y,
                     Covariates = Covariates)
  colnames(base)[-1]<-colnames(Covariates)
  return(base)
}


