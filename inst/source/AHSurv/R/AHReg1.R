#--------------------------------------------------------------------------------------------------------------------------
#' New Generalized Log-logistic (GLL) hazard function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param eta     : shape parameter
#' @param zeta    : shape parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the NGLL hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hNGLL(t=t, kappa=0.5, alpha=0.35, eta=0.7, zeta=1.4, log=FALSE)
#'

hNGLL<- function(t,kappa,alpha,eta,zeta, log=FALSE){
  pdf0 <-   ((alpha*kappa)*((t*kappa)^(alpha-1)))/(1+zeta*((t*eta)^alpha))^(((kappa^alpha)/(zeta*(eta^alpha)))+1)
  cdf0 <-  (1-((1+zeta*((t*eta)^alpha))^(-((kappa^alpha)/(zeta*(eta^alpha))))))
  cdf0 <- ifelse(cdf0==1,0.9999999,cdf0)
  log.h <- log(pdf0) - log(1-cdf0)
  ifelse(log, return(log.h), return(exp(log.h)))
}


#--------------------------------------------------------------------------------------------------------------------------
#' New Generalized Log-logistic (GLL) cumulative hazard function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha     : shape parameter
#' @param eta       : shape parameter
#' @param zeta      : shape parameter
#' @param t         : positive argument
#' @return the value of the NGLL cumulative hazard function
#' @references Hassan Muse, A. A new generalized log-logistic distribution with increasing, decreasing, unimodal and bathtub-shaped hazard rates: properties and applications, in Proceedings of the Symmetry 2021 - The 3rd International Conference on Symmetry, 8–13 August 2021, MDPI: Basel, Switzerland, doi:10.3390/Symmetry2021-10765.
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHNGLL(t=t, kappa=0.5, alpha=0.35, eta=0.7, zeta=1.4)
#'

CHNGLL <- function(t,kappa,alpha,eta,zeta){
  cdf0 <-  (1-((1+zeta*((t*eta)^alpha))^(-((kappa^alpha)/(zeta*(eta^alpha))))))
  return(-log(1-cdf0))
}


#----------------------------------------------------------------------------------------
#' Kumaraswamy Weibull (KW) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param alpha   : scale parameter
#' @param kappa   : shape parameter
#' @param eta     : shape parameter
#' @param zeta    : shape parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the KW hazard function
#' @references Cordeiro, G. M., Ortega, E. M., & Nadarajah, S. (2010). The Kumaraswamy Weibull distribution with application to failure data. Journal of the Franklin Institute, 347(8), 1399-1429.
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hKW(t=t, alpha=0.35, kappa=0.5, eta=1.20, zeta=1.5, log=FALSE)
#'
hKW <- function(t,alpha,kappa,eta,zeta,log=FALSE){
  log.h <-  (log(kappa)+log(zeta)+log(eta)+log(alpha)+((zeta-1)*log(1-exp(-alpha*t^kappa)))+((kappa-1)*log(t)+log(exp(-alpha*t^kappa))))-(log(1-(1-exp(-alpha*t^kappa))^zeta))
  ifelse(log, return(log.h), return(exp(log.h)))
}

#----------------------------------------------------------------------------------------
#' Kumaraswamy Weibull (KW) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param alpha   : scale parameter
#' @param kappa   : shape parameter
#' @param eta     : shape parameter
#' @param zeta    : shape parameter
#' @param t       : positive argument
#' @return the value of the KW cumulative hazard function
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHKW(t=t, alpha=0.35, kappa=0.5, eta=1.20, zeta=1.5)
#'
CHKW<- function(t,alpha,kappa,eta,zeta){
  sf <- (1-(1-exp(-alpha*t^kappa))^zeta)^eta
  return(-log(sf))
}
#--------------------------------------------------------------------------------------------------------------------------
#' Generalized Log-logistic (GLL) hazard function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the GLL hazard function
#' @references Muse, A. H., Mwalili, S., Ngesa, O., Alshanbari, H. M., Khosa, S. K., & Hussam, E. (2022). Bayesian and frequentist approach for the generalized log-logistic accelerated failure time model with applications to larynx-cancer patients. Alexandria Engineering Journal, 61(10), 7953-7978.
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hGLL(t=t, kappa=0.5, alpha=0.35, eta=0.7, log=FALSE)
#'

hGLL<- function(t, kappa,alpha,eta, log = FALSE){
  val<-log(kappa)+log(alpha)+(alpha-1)*log(kappa*t)-log(1+(eta*t)^alpha)
  if(log) return(val) else return(exp(val))
}

#--------------------------------------------------------------------------------------------------------------------------
#' Generalized Log-logistic (GLL) cumulative hazard function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the GLL cumulative hazard function
#' @references Muse, A. H., Mwalili, S., Ngesa, O., Almalki, S. J., & Abd-Elmougod, G. A. (2021). Bayesian and classical inference for the generalized log-logistic distribution with applications to survival data. Computational intelligence and neuroscience, 2021.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHGLL(t=t, kappa=0.5, alpha=0.35, eta=0.9)
#'

CHGLL <- function(t, kappa,alpha, eta){
  val <- ((kappa^alpha)/(eta^alpha))*log(1+((eta*t)^alpha))
  return(val)
}
#----------------------------------------------------------------------------------------
#' Modified Kumaraswamy Weibull (MKW) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param alpha   : inverse scale parameter
#' @param kappa   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the MKW hazard function
#' @references Khosa, S. K. (2019). Parametric Proportional Hazard Models with Applications in Survival analysis (Doctoral dissertation, University of Saskatchewan).
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hMKW(t=t, alpha=0.35, kappa=0.7, eta=1.4, log=FALSE)
#'
hMKW <- function(t,alpha,kappa,eta,log=FALSE){
  log.h <-  (log(kappa)+log(eta)+log(alpha)+((eta-1)*log(1-exp(-t^kappa)))+((kappa-1)*log(t)+log(exp(-t^kappa))))-(log(1-(1-exp(-t^kappa))^eta))
  ifelse(log, return(log.h), return(exp(log.h)))
}

#----------------------------------------------------------------------------------------
#' Modified Kumaraswamy Weibull (MKW) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param alpha   : Inverse scale parameter
#' @param kappa   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @return the value of the MKW cumulative hazard function
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHMKW(t=t,alpha=0.35, kappa=0.7, eta=1.4)
#'
CHMKW<- function(t,alpha,kappa,eta){
  sf <- (1-(1-exp(-t^kappa))^eta)^alpha
  return(-log(sf))
}
#----------------------------------------------------------------------------------------
#' Exponentiated Weibull (EW) Probability Density Function.
#----------------------------------------------------------------------------------------
#' @param lambda   : scale parameter
#' @param kappa    : shape parameter
#' @param alpha    : shape parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the EW probability density function
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' dexpweibull(t=t, lambda=0.6,kappa=0.5, alpha=0.45, log=FALSE)
#'

dexpweibull<- function(t,lambda,kappa,alpha,log=FALSE){
  log.pdf <-  log(alpha) + (alpha-1)*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) +
    dweibull(t,scale=lambda,shape=kappa,log=TRUE)
  ifelse(log, return(log.pdf), return(exp(log.pdf)))
}

#----------------------------------------------------------------------------------------
#' Exponentiated Weibull (EW) Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param lambda   : scale parameter
#' @param kappa    : shape parameter
#' @param alpha    : shape parameter
#' @param t       : positive argument
#' @param log.p     :log scale (TRUE or FALSE)
#' @return the value of the EW cumulative distribution function
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pexpweibull(t=t, lambda=0.65,kappa=0.45, alpha=0.25, log.p=FALSE)
#'

pexpweibull<- function(t,lambda,kappa,alpha,log.p=FALSE){
  log.cdf <- alpha*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE)
  ifelse(log.p, return(log.cdf), return(exp(log.cdf)))
}

#----------------------------------------------------------------------------------------
#' Exponentiated Weibull (EW) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param lambda  : scale parameter
#' @param kappa   : shape parameter
#' @param alpha   : shape parameter
#' @param t      : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the EW hazard function
#' @references Khan, S. A. (2018). Exponentiated Weibull regression for time-to-event data. Lifetime data analysis, 24(2), 328-354.
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hEW(t=t, lambda=0.9, kappa=0.5, alpha=0.75, log=FALSE)
#'
hEW <- function(t,lambda,kappa,alpha,log=FALSE){
  log.pdf <-  log(alpha) + (alpha-1)*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) +
    dweibull(t,scale=lambda,shape=kappa,log=TRUE)
  cdf <- exp(alpha*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) )
  log.h <- log.pdf - log(1-cdf)
  ifelse(log, return(log.h), return(exp(log.h)))
}

#----------------------------------------------------------------------------------------
#' Exponentiated Weibull (EW) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param lambda   : scale parameter
#' @param kappa   : shape parameter
#' @param alpha    : shape parameter
#' @param t       : positive argument
#' @return the value of the EW cumulative hazard function
#' @references Rubio, F. J., Remontet, L., Jewell, N. P., & Belot, A. (2019). On a general structure for hazard-based regression models: an application to population-based cancer research. Statistical methods in medical research, 28(8), 2404-2417.
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHEW(t=t, lambda=0.9, kappa=0.5, alpha=0.75)
#'
CHEW<- function(t,lambda,kappa,alpha){
  cdf <- exp(alpha*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) )
  return(-log(1-cdf))
}

#--------------------------------------------------------------------------------------------------------------------------
#' Modified Log-logistic (MLL) hazard function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the MLL hazard function
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hMLL(t=t, kappa=0.75, alpha=0.5, eta=0.9,log=FALSE)
#'

hMLL<- function(t,kappa,alpha,eta,log=FALSE){
  log.h <- log(kappa*(kappa*t)^(alpha-1)*exp(eta*t)*(alpha+eta*t)/(1+((kappa*t)^alpha)*exp(eta*t)))
  ifelse(log, return(log.h), return(exp(log.h)))
}

#--------------------------------------------------------------------------------------------------------------------------
#' Modified Log-logistic (MLL) cumulative hazard function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the MLL cumulative hazard function
#' @references Kayid, M. (2022). Applications of Bladder Cancer Data Using a Modified Log-Logistic Model. Applied Bionics and Biomechanics, 2022.
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHMLL(t=t, kappa=0.75, alpha=0.5, eta=0.9)
#'

CHMLL<- function(t,kappa,alpha,eta){
  sf <-  1/(1+((kappa*t)^alpha)*exp(eta*t))
  return(-log(sf))
}

#--------------------------------------------------------------------------------------------------------------------------
#' Power Generalised Weibull (PGW) hazard function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the PGW hazard function
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hPGW(t=t, kappa=0.5, alpha=1.5, eta=0.6,log=FALSE)
#'

hPGW <- function(t, kappa,alpha, eta, log = FALSE){
  val <- log(alpha) - log(eta) - alpha*log(kappa) + (alpha-1)*log(t) +
    (1/eta - 1)*log( 1 + (t/kappa)^alpha )
  if(log) return(val) else return(exp(val))
}

#--------------------------------------------------------------------------------------------------------------------------
#' Power Generalised Weibull (PGW) cumulative hazard function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the PGW cumulative hazard function
#' @references Alvares, D., & Rubio, F. J. (2021). A tractable Bayesian joint model for longitudinal and survival data. Statistics in Medicine, 40(19), 4213-4229.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHPGW(t=t, kappa=0.5, alpha=1.5, eta=0.6)
#'

CHPGW <- function(t, kappa, alpha, eta){
  val <- -1 + ( 1 + (t/kappa)^alpha)^(1/eta)
  return(val)
}


#----------------------------------------------------------------------------------------
#' Generalised Gamma (GG) Probability Density Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the GG probability density function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' dggamma(t=t, kappa=0.5, alpha=0.35, eta=0.9,log=FALSE)
#'

dggamma <- function(t, kappa, alpha, eta, log = FALSE){
  val <- log(eta) - alpha*log(kappa) - lgamma(alpha/eta) + (alpha - 1)*log(t) -
    (t/kappa)^eta
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Generalised Gamma (GG) Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @param log.p     :log scale (TRUE or FALSE)
#' @return the value of the GG cumulative distribution function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pggamma(t=t, kappa=0.5, alpha=0.35, eta=0.9,log.p=FALSE)
#'

pggamma <- function(t, kappa, alpha, eta, log.p = FALSE){
  val <- pgamma( t^eta, shape = alpha/eta, scale = kappa^eta, log.p = TRUE)
  if(log.p) return(val) else return(exp(val))
}


#----------------------------------------------------------------------------------------
#' Generalised Gamma (GG) Survival Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @param log.p     :log scale (TRUE or FALSE)
#' @return the value of the GG survival function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sggamma(t=t, kappa=0.5, alpha=0.35, eta=0.9,log.p=FALSE)
#'
sggamma <- function(t, kappa, alpha, eta, log.p = FALSE){
  val <- pgamma( t^eta, shape = alpha/eta, scale = kappa^eta, log.p = TRUE, lower.tail =  FALSE)
  if(log.p) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Generalised Gamma (GG) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa  : scale parameter
#' @param alpha  : shape parameter
#' @param eta    : shape parameter
#' @param t      : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the GG hazard function
#' @references Agarwal, S. K., & Kalla, S. L. (1996). A generalized gamma distribution and its application in reliabilty. Communications in Statistics-Theory and Methods, 25(1), 201-210.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hGG(t=t, kappa=0.5, alpha=0.35, eta=0.9,log=FALSE)
#'
hGG <- function(t, kappa, alpha, eta, log = FALSE){
  val <- dggamma(t, kappa, alpha, eta, log = TRUE) - sggamma(t, kappa, alpha, eta, log.p = TRUE)
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Generalised Gamma (GG) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @return the value of the GG cumulative hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHGG(t=t, kappa=0.5, alpha=0.35, eta=0.9)
#'
CHGG <- function(t, kappa, alpha, eta){
  val <- -pgamma( t^eta, shape = alpha/eta, scale = kappa^eta, log.p = TRUE, lower.tail =  FALSE)
  return(val)
}





#----------------------------------------------------------------------------------------
#' Log-logistic (LL) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha    : shape parameter
#' @param t        : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the LL hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hLL(t=t, kappa=0.5, alpha=0.35,log=FALSE)
#'
hLL<- function(t,kappa,alpha, log = FALSE){
  pdf0 <-  dllogis(t,shape=alpha,scale=kappa)
  cdf0 <- pllogis(t,shape=alpha,scale=kappa)
  val<-log(pdf0)-log(1-cdf0)
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Log-logistic  (LL) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha    : shape parameter
#' @param t       : positive argument
#' @return the value of the LL cumulative hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHLL(t=t, kappa=0.5, alpha=0.35)
#'
CHLL<- function(t,kappa,alpha){
  cdf <- pllogis(t,shape=alpha,scale=kappa)
  val<--log(1-cdf)
  return(val)
}


#----------------------------------------------------------------------------------------
#' Weibull (W) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha    : shape parameter
#' @param t        : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the w hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hW(t=t, kappa=0.75, alpha=0.5,log=FALSE)
#'
hW<- function(t,kappa,alpha, log = FALSE){
  val<- log(alpha*kappa*t^(alpha-1))
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Weibull  (W) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param t       : positive argument
#' @return the value of the W cumulative hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHW(t=t, kappa=0.75, alpha=0.5)
#'
CHW<- function(t,kappa,alpha){
  val <- kappa*t^alpha
  return(val)
}



#----------------------------------------------------------------------------------------
#' Lognormal (LN) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : meanlog parameter
#' @param alpha   : sdlog parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the LN hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hLN(t=t, kappa=0.5, alpha=0.75,log=FALSE)
#'

hLN <- function(t,kappa,alpha, log = FALSE){
  pdf0 <- dlnorm(t,meanlog=kappa,sdlog=alpha)
  cdf0 <- plnorm(t,meanlog=kappa,sdlog=alpha)
  val<-log(pdf0)-log(1-cdf0)
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Lognormal (LN) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : meanlog parameter
#' @param alpha   : sdlog parameter
#' @param t       : positive argument
#' @return the value of the LN cumulative hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHLN(t=t, kappa=0.75, alpha=0.95)
#'
CHLN<- function(t,kappa,alpha){
  cdf <- plnorm(t,meanlog=kappa,sdlog=alpha)
  val<--log(1-cdf)
  return(val)
}


#----------------------------------------------------------------------------------------
#' Burr-XII (BXII) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha    : shape parameter
#' @param t        : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the BXII hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hBXII(t=t, kappa=0.85, alpha=0.45,log=FALSE)
#'
hBXII<- function(t,kappa,alpha, log = FALSE){
  h0<-(alpha*kappa*t^(kappa-1))/(1+t^kappa)
  val <- log(h0)
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Burr-XII  (BXII) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param t       : positive argument
#' @return the value of the BXII cumulative hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHBXII(t=t, kappa=0.5, alpha=0.35)
#'
CHBXII<- function(t,kappa,alpha){
  cdf0 <- (1-((1+t^kappa))^(-alpha))
  H0<--log(1-cdf0)
  return(H0)
}

#----------------------------------------------------------------------------------------
#' Gamma (G) Hazard Function.
#----------------------------------------------------------------------------------------
#' @param shape   : shape parameter
#' @param scale   : scale parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the G hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' hG(t=t, shape=0.5, scale=0.85,log=FALSE)
#'
hG <- function(t, shape, scale, log = FALSE){
  lpdf0 <-  dgamma(t, shape = shape, scale = scale, log = T)
  ls0 <- pgamma(t, shape = shape, scale = scale, lower.tail = FALSE, log.p = T)
  val <- lpdf0 - ls0
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Gamma (G) Cumulative Hazard Function.
#----------------------------------------------------------------------------------------
#' @param shape   : shape parameter
#' @param scale      : scale parameter
#' @param t       : positive argument
#' @return the value of the G cumulative hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' CHG(t=t, shape=0.85, scale=0.5)
#'
CHG <- function(t, shape, scale){
  H0 <- -pgamma(t, shape = shape, scale = scale, lower.tail = FALSE, log.p = TRUE)
  return(H0)
}

###############################################################################################
###############################################################################################
###############################################################################################
#' Overall Survival AH model.
###############################################################################################
###############################################################################################
###############################################################################################

########################################################################################################
#' @description The flexible parametric accelerated hazards (AH) model's maximum likelihood estimation, log-likelihood, and information criterion.
#' Baseline hazards: NGLL, GLL,KW, EW, MLL, PGW, GG, MKW, Log-logistic, Weibull,  Log-normal, Burr-XII, and Gamma
########################################################################################################
#' @param init  : initial points for optimisation
#' @param z      : design matrix for covariates (p x n), p >= 1
#' @param delta : vital indicator (0-alive,1 - dead,)
#' @param time   : survival times
#' @param basehaz : {baseline hazard structure including baseline
#' (NGLLAH,GLLAH,EWAH,KWAH,MLLAH,PGWAH,GGAH,
#' MKWAH,LLAH,WAH,GAH,LNAH,BXIIAH)}
#' @param method :"nlminb" or a method from "optim"
#' @param n : The number of the observations of the data set
#' @param maxit :The maximum number of iterations. Defaults to 1000
#' @param log     :log scale (TRUE or FALSE)
#' @details The function AHMLE returns MLE estimates and information criterion.
#' @format By default the function calculates the following values:
#' \itemize{
#'   \item AIC:  Akaike Information Criterion;
#'    \item CAIC: Consistent Akaikes Information Criterion;
#'     \item BIC:  Bayesian Information Criterion;
#'      \item BCAIC:  Bozdogan’s Consistent Akaike Information Criterion;
#'      \item HQIC:  Hannan-Quinn information criterion;
#'      \item par:  maximum likelihood estimates;
#'      \item Value:  value of the likelihood function;
#'     \item Convergence:  0 indicates successful completion and 1 indicates that the iteration limit maxit.
#'  }
#' @return a list containing the output of the optimisation (OPT) and the information criterion including (AIC, BIC, CAIC, BCAIC, and HQIC).
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' #Example #1
#' data(ipass)
#' time<-ipass$time
#' delta<-ipass$status
#' z<-ipass$arm
#' AHMLE(init = c(1.0,1.0,1.0,0.5),time = time,delta = delta,n=nrow(z),
#' basehaz = "GLLAH",z = z,method = "Nelder-Mead",
#' maxit = 1000)
#'
#' #Example #2
#' data(bmt)
#' time<-bmt$Time
#' delta<-bmt$Status
#' z<-bmt$TRT
#' AHMLE(init = c(1.0,1.0,1.0,0.5),time = time,delta = delta,n=nrow(z),
#' basehaz = "GLLAH",z = z,method = "Nelder-Mead",
#' maxit = 1000)
#'
#'#Example #3
#'data("e1684")
#'time<-e1684$FAILTIME
#'delta<-e1684$FAILCENS
#'TRT<-e1684$TRT
#'AGE<-e1684$TRT
#'z<-as.matrix(cbind(scale(TRT), scale(AGE) ))
#'AHMLE(init = c(1.0,1.0,1.0,0.5,0.75),time = time,delta = delta,n=nrow(z),
#'basehaz = "GLLAH",z = z,method = "Nelder-Mead",maxit = 1000)
#'
#'#Example #4
#'data("LeukSurv")
#'time<-LeukSurv$time
#'delta<-LeukSurv$cens
#'age<-LeukSurv$age
#'wbc<-LeukSurv$wbc
#'tpi<-LeukSurv$tpi
#'z<-as.matrix(cbind(scale(age), scale(tpi),scale(wbc) ))
#'AHMLE(init = c(1.0,1.0,1.0,1.0,0.5,0.65,0.85),time = time,delta = delta,n=nrow(z),
#'basehaz = "NGLLAH",z = z,method = "Nelder-Mead",maxit = 1000)
#'



AHMLE<- function(init, time, delta,n, basehaz, z, method = "Nelder-Mead", maxit = 1000,log=FALSE){
  # Required variables
  time <- as.vector(time)
  delta <- as.vector(as.logical(delta))
  z <- as.matrix(z)
  n<-nrow(z)
  time.obs <- time[delta]
  if(!is.null(z))  z.obs <- z[delta,]
  p0 <- dim(z)[2]

  # NGLL - AH Model
  if(basehaz == "NGLLAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); de0<-exp(par[4]);beta <- par[5:(4+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hNGLL(time.obs*exp.z.beta.obs,ae0,be0,ce0,de0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHNGLL(time*exp.z.beta,ae0,be0,ce0,de0)/exp.z.beta)
      return(sum(val))
    }
  }
  # KW- AH Model
  if(basehaz == "KWAH"){
    log.lik <- function(par){
      ae0 <- par[1]; be0 <- par[2];  ce0 <- par[3]; de0<-par[4];beta <- par[5:(4+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hKW(time.obs*exp.z.beta.obs,ae0,be0,ce0,de0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHKW(time*exp.z.beta,ae0,be0,ce0,de0)/exp.z.beta)
      return(sum(val))
    }
  }
  # GLL - AH Model
  if(basehaz == "GLLAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hGLL(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHGLL(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }

  # EW - AH Model
  if(basehaz == "EWAH"){
    log.lik <- function(par){
      ae0 <- par[1]; be0 <- par[2];  ce0 <- par[3]; beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hEW(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHEW(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }

  # MLL - AH Model
  if(basehaz == "MLLAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hMLL(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHMLL(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }

  # PGW - AH Model
  if(basehaz == "PGWAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hPGW(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHPGW(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }

  # GG - AH Model
  if(basehaz == "GGAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hGG(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHGG(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }
  # MKW - AH Model
  if(basehaz == "MKWAH"){
    log.lik <- function(par){
      ae0 <- par[1]; be0 <- par[2];  ce0<-par[3]; beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hMKW(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHMKW(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }
  # Loglogistic - AH Model
  if(basehaz == "LLAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hLL(time.obs*exp.z.beta.obs,ae0,be0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHLL(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }
  # Weibull - AH Model
  if(basehaz == "WAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hW(time.obs*exp.z.beta.obs,ae0,be0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHW(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }
  # Gamma - AH Model
  if(basehaz == "GAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hG(time.obs*exp.z.beta.obs,ae0,be0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHG(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }

  # Lognormal - AH Model
  if(basehaz == "LNAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hLN(time.obs*exp.z.beta.obs,ae0,be0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHLN(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }

  # BURXII - AH Model
  if(basehaz == "BXIIAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- hBXII(time.obs*exp.z.beta.obs,ae0,be0, log = TRUE)
      val <- - sum(lhaz0) + sum(CHBXII(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }
  if(method != "nlminb") OPT <- optim(init,log.lik,control=list(maxit=maxit), method = method)
  if(method == "nlminb") OPT <- nlminb(init,log.lik,control=list(iter.max=maxit))
  p=length(OPT$par)
  l=OPT$value
  AIC=2*l + 2*p
  CAIC=AIC+(2*p*(p+1)/(n-l+1))
  HQIC= 2*l+2*log(log(n))*p
  BCAIC=2*l+(p*(log(n)+1))
  BIC=(2*l)+(p*(log(n)))
  result = (list("AIC" = AIC , "CAIC " = CAIC,
                 "BIC" = BIC, "HQIC" = HQIC, "BCAIC" = BCAIC))
  OUT <- list(OPT = OPT, result=result)
  return(OUT)
}



###############################################################################################
###############################################################################################
###############################################################################################
#' Relative Survival AH model.
###############################################################################################
###############################################################################################
###############################################################################################

########################################################################################################
#' @description  The flexible parametric accelerated excess hazards (AEH) model's maximum likelihood estimation, log-likelihood, and information criterion.
#' Baseline hazards:NGLL, GLL, KW,EW, MLL, PGW, GG, MKW, Log-logistic, Weibull,  Log-normal, Burr-XII, and Gamma
########################################################################################################
#' @param init  : initial points for optimisation
#' @param z : design matrix for covariates (p x n), p >= 1
#' @param delta : vital indicator (0-alive,1 - dead)
#' @param time  : survival times
#' @param basehaz : {baseline hazard structure including baseline
#' (NGLLAEH,GLLAEH,EWAEH,KWAEH,MLLAEH,
#' PGWAEH,GGAEH,MKWAEH,LLAEH,WAEH,GAEH,
#' LNAEH,BXIIAEEH)}
#' @param hp.obs  : population hazards (for uncensored individuals)
#' @param n : The number of the observations of the data set
#' @param method :"nlminb" or a method from "optim"
#' @param log     :log scale (TRUE or FALSE)
#' @param maxit :The maximum number of iterations. Defaults to 1000
#' @format By default the function calculates the following values:
#' \itemize{
#'   \item AIC:  Akaike Information Criterion;
#'    \item CAIC: Consistent Akaikes Information Criterion;
#'     \item BIC:  Bayesian Information Criterion;
#'      \item BCAIC:  Bozdogan’s Consistent Akaike Information Criterion;
#'      \item HQIC:  Hannan-Quinn information criterion;
#'      \item par:  maximum likelihood estimates;
#'      \item Value:  value of the likelihood function;
#'     \item Convergence:  0 indicates successful completion and 1 indicates that the iteration limit maxit.
#'  }
#' @return a list containing the output of the optimisation (OPT) and the information criterion including (AIC, BIC, CAIC, BCAIC, and HQIC).
#' @export
#'
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Mutua Kilai, \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' data(bmt)
#' time<-bmt$Time
#' delta<-bmt$Status
#' z<-bmt$TRT
#' AEHMLE(init = c(1.0,0.5,1.0,0.5),time = time,delta = delta,n=nrow(z),
#' basehaz = "GLLAEH",z = z,hp.obs=0.6,method = "Nelder-Mead",
#' maxit = 1000)
#'

AEHMLE <- function(init, time, delta, n,basehaz, z, hp.obs, method = "Nelder-Mead", maxit = 1000, log=FALSE){
  # Required variables
  time <- as.vector(time)
  delta <- as.vector(as.logical(delta))
  z <- as.matrix(z)
  n<-nrow(z)
  time.obs <- time[delta]
  if(!is.null(z))  z.obs <- z[delta,]
  hp.obs <- as.vector(hp.obs)
  p0 <- dim(z)[2]

  # NGLL - AH Model
  if(basehaz == "NGLLAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); de0<-exp(par[4]);beta <- par[5:(4+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hNGLL(time.obs*exp.z.beta.obs,ae0,be0,ce0,de0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHNGLL(time*exp.z.beta,ae0,be0,ce0,de0)/exp.z.beta)
      return(sum(val))
    }
  }
  # KW - AH Model
  if(basehaz == "KWAH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); de0<-exp(par[4]);beta <- par[5:(4+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hKW(time.obs*exp.z.beta.obs,ae0,be0,ce0,de0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHKW(time*exp.z.beta,ae0,be0,ce0,de0)/exp.z.beta)
      return(sum(val))
    }
  }
  # GLL - AEH Model
  if(basehaz == "GLLAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hGLL(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHGLL(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }

  # EW - AEH Model
  if(basehaz == "EWAEH"){
    log.lik <- function(par){
      ae0 <- par[1]; be0 <- par[2];  ce0 <- par[3]; beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hEW(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHEW(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }

  # MLL - AEH Model
  if(basehaz == "MLLAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hMLL(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHMLL(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }

  # PGW - AEH Model
  if(basehaz == "PGWAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hPGW(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHPGW(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }

  # GG - AEH Model
  if(basehaz == "GGAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hGG(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHGG(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }
  # MKW- AEH Model
  if(basehaz == "MKWAEH"){
    log.lik <- function(par){
      ae0 <- par[1]; be0 <- par[2];  ce0<-par[3];beta <- par[4:(3+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hMKW(time.obs*exp.z.beta.obs,ae0,be0,ce0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHMKW(time*exp.z.beta,ae0,be0,ce0)/exp.z.beta)
      return(sum(val))
    }
  }
  # Loglogistic - AEH Model
  if(basehaz == "LLAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hLL(time.obs*exp.z.beta.obs,ae0,be0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHLL(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }
  # Weibull - AEH Model
  if(basehaz == "WAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hW(time.obs*exp.z.beta.obs,ae0,be0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHW(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }
  # Gamma - AEH Model
  if(basehaz == "GAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hG(time.obs*exp.z.beta.obs,ae0,be0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHG(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }

  # Lognormal - AEH Model
  if(basehaz == "LNAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+hLN(time.obs*exp.z.beta.obs,ae0,be0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHLN(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }

  # BURXII - AEH Model
  if(basehaz == "BXIIAEH"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      z.beta <- as.vector(z%*%beta)
      exp.z.beta <- exp(z.beta)
      exp.z.beta.obs <- exp(z.beta[delta])
      lhaz0 <- log(hp.obs+ hBXII(time.obs*exp.z.beta.obs,ae0,be0, log = FALSE))
      val <- - sum(lhaz0) + sum(CHBXII(time*exp.z.beta,ae0,be0)/exp.z.beta)
      return(sum(val))
    }
  }
  if(method != "nlminb") OPT <- optim(init,log.lik,control=list(maxit=maxit), method = method)
  if(method == "nlminb") OPT <- nlminb(init,log.lik,control=list(iter.max=maxit))
  p=length(OPT$par)
  l=OPT$value
  AIC=2*l + 2*p
  CAIC=AIC+(2*p*(p+1)/(n-l+1))
  HQIC= 2*l+2*log(log(n))*p
  BCAIC=2*l+(p*(log(n)+1))
  BIC=(2*l)+(p*(log(n)))
  result = (list("AIC" = AIC , "CAIC " = CAIC,
                 "BIC" = BIC, "HQIC" = HQIC, "BCAIC" = BCAIC))
  OUT <- list(OPT = OPT, result=result)
  return(OUT)
}

