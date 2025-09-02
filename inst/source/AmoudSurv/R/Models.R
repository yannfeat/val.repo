#----------------------------------------------------------------------------------------
#' Arcsine-Log-logistic (ASLL)  Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the ASLL Cumulative Distribution Function.
#' @references Tung, Y. L., Ahmad, Z., & Mahmoudi, E. (2021). The Arcsine-X Family of Distributions with Applications to Financial Sciences. Comput. Syst. Sci. Eng., 39(3), 351-363.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pASLL(t=t, alpha=0.7, beta=0.5)
#'
pASLL<-function(t,alpha,beta){
  cdf0<-(2/pi)*(asin((t^(beta))/(alpha^(beta)+t^(beta))))
  return(cdf0)
}

#----------------------------------------------------------------------------------------
#' Arcsine-Log-logistic (ASLL)  Survival Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the ASLL Survival Function.
#' @references Tung, Y. L., Ahmad, Z., & Mahmoudi, E. (2021). The Arcsine-X Family of Distributions with Applications to Financial Sciences. Comput. Syst. Sci. Eng., 39(3), 351-363.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sASLL(t=t, alpha=0.7, beta=0.5)
#'

sASLL<-function(t,alpha,beta){
  cdf0<-(2/pi)*(asin((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-1-cdf0
  return(val)
}

#----------------------------------------------------------------------------------------
#' Arcsine-Log-logistic (ASLL)  Hazard Rate Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the ASLL Hazard Rate  Function.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rSLL(t=t, alpha=0.7, beta=0.5,log=FALSE)
#'
rASLL<-function(t,alpha,beta,log=FALSE){
  cdf0<-(2/pi)*(asin((t^(beta))/(alpha^(beta)+t^(beta))))
  cdf0 <- ifelse(cdf0==1,0.9999999,cdf0)
  pdf0<-((2/pi)*(((beta/alpha)*(t/alpha)^(beta-1))/((1+(t/alpha)^(beta))^2))/sqrt(1-((t^(beta))/(alpha^(beta)+t^(beta)))^2))
  log.h<-log(pdf0)-log(1-cdf0)
  ifelse(log, return(log.h), return(exp(log.h)))
}

#----------------------------------------------------------------------------------------
#' Sine-Log-logistic (SLL)  Survivor Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the SLL Survivor function
#' @references Souza, L., Junior, W., De Brito, C., Chesneau, C., Ferreira, T., & Soares, L. (2019). On the Sin-G class of distributions: theory, model and application. Journal of Mathematical Modeling, 7(3), 357-379.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sSLL(t=t, alpha=0.7, beta=0.5)
sSLL<-function(t,alpha,beta){
  cdf0<-sin((pi/2)*((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-1-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Sine-Log-logistic (SLL)  Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the SLL Cumulative Distribution function
#' @references Souza, L., Junior, W., De Brito, C., Chesneau, C., Ferreira, T., & Soares, L. (2019). On the Sin-G class of distributions: theory, model and application. Journal of Mathematical Modeling, 7(3), 357-379.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pSLL(t=t, alpha=0.7, beta=0.5)
#'
pSLL<-function(t,alpha,beta){
  cdf0<-sin((pi/2)*((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Sine-Log-logistic (SLL)  Hazard  Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the SLL hazard function
#' @references Souza, L. (2015). New trigonometric classes of probabilistic distributions. esis, Universidade Federal Rural de Pernambuco, Brazil.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rSLL(t=t, alpha=0.7, beta=0.5,log=FALSE)
#'
rSLL<-function(t,alpha,beta,log=FALSE){
  cdf0<-sin((pi/2)*((t^(beta))/(alpha^(beta)+t^(beta))))
  cdf0 <- ifelse(cdf0==1,0.9999999,cdf0)
  pdf0<-((pi/2))*((beta/alpha)*(t/alpha)^(beta-1)/((1+(t/alpha)^(beta))^2))*cos((pi/2)*((t^(beta))/(alpha^(beta)+t^(beta))))
  log.h<-log(pdf0)-log(1-cdf0)
  ifelse(log, return(log.h), return(exp(log.h)))
}


#----------------------------------------------------------------------------------------
#' Cosine-Log-logistic (CLL)  Survivor Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the CLL Survivor function
#' @references Mahmood, Z., M Jawa, T., Sayed-Ahmed, N., Khalil, E. M., Muse, A. H., & Tolba, A. H. (2022). An Extended Cosine Generalized Family of Distributions for Reliability Modeling: Characteristics and Applications with Simulation Study. Mathematical Problems in Engineering, 2022.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sCLL(t=t, alpha=0.7, beta=0.5)
sCLL<-function(t,alpha,beta){
  cdf0<-1-cos((pi/2)*((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-1-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Cosine-Log-logistic (SLL)  Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the CLL Cumulative Distribution function
#' @references Souza, L., Junior, W. R. D. O., de Brito, C. C. R., Ferreira, T. A., & Soares, L. G. (2019). General properties for the Cos-G class of distributions with applications. Eurasian Bulletin of Mathematics (ISSN: 2687-5632), 63-79.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pCLL(t=t, alpha=0.7, beta=0.5)
#'
pCLL<-function(t,alpha,beta){
  cdf0<-1-cos((pi/2)*((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Cosine-Log-logistic (CLL)  Hazard  Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the CLL hazard function
#' @references Souza, L., Junior, W. R. D. O., de Brito, C. C. R., Ferreira, T. A., & Soares, L. G. (2019). General properties for the Cos-G class of distributions with applications. Eurasian Bulletin of Mathematics (ISSN: 2687-5632), 63-79.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rCLL(t=t, alpha=0.7, beta=0.5,log=FALSE)
#'
rCLL<-function(t,alpha,beta,log=FALSE){
  cdf0<-1-cos((pi/2)*((t^(beta))/(alpha^(beta)+t^(beta))))
  cdf0 <- ifelse(cdf0==1,0.9999999,cdf0)
  pdf0<-((pi/2))*((beta/alpha)*(t/alpha)^(beta-1)/((1+(t/alpha)^(beta))^2))*sin((pi/2)*((t^(beta))/(alpha^(beta)+t^(beta))))
  log.h<-log(pdf0)-log(1-cdf0)
  ifelse(log, return(log.h), return(exp(log.h)))
}


#----------------------------------------------------------------------------------------
#' Tangent-Log-logistic (TLL)  Survivor Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the TLL Survivor function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sTLL(t=t, alpha=0.7, beta=0.5)
sTLL<-function(t,alpha,beta){
  cdf0<-tan((pi/4)*((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-1-cdf0
  return(val)
}

#----------------------------------------------------------------------------------------
#' Tangent-Log-logistic (TLL)  Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the TLL Cumulative Distribution function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pTLL(t=t, alpha=0.7, beta=0.5)
#'
pTLL<-function(t,alpha,beta){
  cdf0<-tan((pi/4)*((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Tangent-Log-logistic (TLL)  Hazard  Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the TLL hazard function
#' @references Muse, A. H., Tolba, A. H., Fayad, E., Abu Ali, O. A., Nagy, M., & Yusuf, M. (2021). Modelling the COVID-19 mortality rate with a new versatile modification of the log-logistic distribution. Computational Intelligence and Neuroscience, 2021.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rTLL(t=t, alpha=0.7, beta=0.5,log=FALSE)
#'
rTLL<-function(t,alpha,beta,log=FALSE){
  cdf0<-tan((pi/4)*((t^(beta))/(alpha^(beta)+t^(beta))))
  cdf0 <- ifelse(cdf0==1,0.9999999,cdf0)
  pdf0<-((pi/4))*((beta/alpha)*(t/alpha)^(beta-1)/((1+(t/alpha)^(beta))^2))*sec((pi/4)*((t^(beta))/(alpha^(beta)+t^(beta))))^2
  log.h<-log(pdf0)-log(1-cdf0)
  ifelse(log, return(log.h), return(exp(log.h)))
}

#----------------------------------------------------------------------------------------
#' Secant-log-logistic (SCLL) Survivor Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the SCLL Survivor function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sSCLL(t=t, alpha=0.7, beta=0.5)
sSCLL<-function(t,alpha,beta){
  cdf0<-sec((pi/3)*((t^(beta))/(alpha^(beta)+t^(beta))))-1
  val<-1-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Secant-log-logistic (SCLL) Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the SCLL Cumulative Distribution function
#' @references Souza, L., de Oliveira, W. R., de Brito, C. C. R., Chesneau, C., Fernandes, R., & Ferreira, T. A. (2022). Sec-G class of distributions: Properties and applications. Symmetry, 14(2), 299.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pSCLL(t=t, alpha=0.7, beta=0.5)
#'
pSCLL<-function(t,alpha,beta){
  cdf0<-sec((pi/3)*((t^(beta))/(alpha^(beta)+t^(beta))))-1
  val<-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Secant-log-logistic (SCLL) Hazard  Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the SCLL hazard function
#' @references Souza, L., de Oliveira, W. R., de Brito, C. C. R., Chesneau, C., Fernandes, R., & Ferreira, T. A. (2022). Sec-G class of distributions: Properties and applications. Symmetry, 14(2), 299.
#' @references Tung, Y. L., Ahmad, Z., & Mahmoudi, E. (2021). The Arcsine-X Family of Distributions with Applications to Financial Sciences. Comput. Syst. Sci. Eng., 39(3), 351-363.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rSCLL(t=t, alpha=0.7, beta=0.5,log=FALSE)
#'
rSCLL<-function(t,alpha,beta,log=FALSE){
  cdf0<-sec((pi/3)*((t^(beta))/(alpha^(beta)+t^(beta))))-1
  cdf0 <- ifelse(cdf0==1,0.9999999,cdf0)
  pdf0<-(pi/3)*((beta/alpha)*(t/alpha)^(beta-1)/((1+(t/alpha)^(beta))^2))*tan((pi/3)*((t^(beta))/(alpha^(beta)+t^(beta))))*sec((pi/3)*((t^(beta))/(alpha^(beta)+t^(beta))))
  log.h<-log(pdf0)-log(1-cdf0)
  ifelse(log, return(log.h), return(exp(log.h)))
}

#----------------------------------------------------------------------------------------
#' Arctangent-Log-logistic (ATLL)  Survivor Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the ATLL Survivor function
#' @references Alkhairy, I., Nagy, M., Muse, A. H., & Hussam, E. (2021). The Arctan-X family of distributions: Properties, simulation, and applications to actuarial sciences. Complexity, 2021.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sATLL(t=t, alpha=0.7, beta=0.5)
sATLL<-function(t,alpha,beta){
  cdf0<-(4/pi)*(atan((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-1-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Arctangent-Log-logistic (ATLL)  Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @return the value of the ATLL Cumulative Distribution function
#' @references Alkhairy, I., Nagy, M., Muse, A. H., & Hussam, E. (2021). The Arctan-X family of distributions: Properties, simulation, and applications to actuarial sciences. Complexity, 2021.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pATLL(t=t, alpha=0.7, beta=0.5)
#'
pATLL<-function(t,alpha,beta){
  cdf0<-(4/pi)*(atan((t^(beta))/(alpha^(beta)+t^(beta))))
  val<-cdf0
  return(val)
}
#----------------------------------------------------------------------------------------
#' Arctangent-Log-logistic (ATLL)  Hazard  Function.
#----------------------------------------------------------------------------------------
#' @param beta   : shape parameter
#' @param alpha   : scale parameter
#' @param t       : positive argument
#' @param log     :log scale (TRUE or FALSE)
#' @return the value of the ATLL hazard function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rATLL(t=t, alpha=0.7, beta=0.5,log=FALSE)
#'
rATLL<-function(t,alpha,beta,log=FALSE){
  cdf0<-(4/pi)*(atan((t^(beta))/(alpha^(beta)+t^(beta))))
  cdf0 <- ifelse(cdf0==1,0.9999999,cdf0)
  pdf0<-((4/pi)*(((beta/alpha)*(t/alpha)^(beta-1))/((1+(t/alpha)^(beta))^2))/(1+((t^(beta))/(alpha^(beta)+t^(beta)))^2))
  log.h<-log(pdf0)-log(1-cdf0)
  ifelse(log, return(log.h), return(exp(log.h)))
}
#--------------------------------------------------------------------------------------------------------------------------
#' New Generalized Log-logistic (NGLL) hazard function.
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rNGLL(t=t, kappa=0.5, alpha=0.35, eta=0.7, zeta=1.4, log=FALSE)
#'

rNGLL<- function(t,kappa,alpha,eta,zeta, log=FALSE){
  pdf0 <-   ((alpha*kappa)*((t*kappa)^(alpha-1)))/(1+zeta*((t*eta)^alpha))^(((kappa^alpha)/(zeta*(eta^alpha)))+1)
  cdf0 <-  (1-((1+zeta*((t*eta)^alpha))^(-((kappa^alpha)/(zeta*(eta^alpha))))))
  cdf0 <- ifelse(cdf0==1,0.9999999,cdf0)
  log.h <- log(pdf0) - log(1-cdf0)
  ifelse(log, return(log.h), return(exp(log.h)))
}

#--------------------------------------------------------------------------------------------------------------------------
#' New Generalized Log-logistic (NGLL) cumulative distribution function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha     : shape parameter
#' @param eta       : shape parameter
#' @param zeta      : shape parameter
#' @param t         : positive argument
#' @return the value of the NGLL cumulative distribution function
#' @references Hassan Muse, A. A new generalized log-logistic distribution with increasing, decreasing, unimodal and bathtub-shaped hazard rates: properties and applications, in Proceedings of the Symmetry 2021 - The 3rd International Conference on Symmetry, 8–13 August 2021, MDPI: Basel, Switzerland, doi:10.3390/Symmetry2021-10765.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pNGLL(t=t, kappa=0.5, alpha=0.35, eta=0.7, zeta=1.4)
#'

pNGLL <- function(t,kappa,alpha,eta,zeta){
  cdf0 <-  (1-((1+zeta*((t*eta)^alpha))^(-((kappa^alpha)/(zeta*(eta^alpha))))))
  return(-log(1-cdf0))
}
#--------------------------------------------------------------------------------------------------------------------------
#' New Generalized Log-logistic (NGLL) survivor function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha     : shape parameter
#' @param eta       : shape parameter
#' @param zeta      : shape parameter
#' @param t         : positive argument
#' @return the value of the NGLL survivor function
#' @references Hassan Muse, A. A new generalized log-logistic distribution with increasing, decreasing, unimodal and bathtub-shaped hazard rates: properties and applications, in Proceedings of the Symmetry 2021 - The 3rd International Conference on Symmetry, 8–13 August 2021, MDPI: Basel, Switzerland, doi:10.3390/Symmetry2021-10765.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' SNGLL(t=t, kappa=0.5, alpha=0.35, eta=0.7, zeta=1.4)
#'

SNGLL <- function(t,kappa,alpha,eta,zeta){
  cdf0 <-  (1-((1+zeta*((t*eta)^alpha))^(-((kappa^alpha)/(zeta*(eta^alpha))))))
  return(1-cdf0)
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rGLL(t=t, kappa=0.5, alpha=0.35, eta=0.7, log=FALSE)
#'

rGLL<- function(t, kappa,alpha,eta, log = FALSE){
  val<-log(kappa)+log(alpha)+(alpha-1)*log(kappa*t)-log(1+(eta*t)^alpha)
  if(log) return(val) else return(exp(val))
}



#--------------------------------------------------------------------------------------------------------------------------
#' Generalized Log-logistic (GLL) cumulative distribution function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the GLL cumulative distribution function
#' @references Muse, A. H., Mwalili, S., Ngesa, O., Almalki, S. J., & Abd-Elmougod, G. A. (2021). Bayesian and classical inference for the generalized log-logistic distribution with applications to survival data. Computational intelligence and neuroscience, 2021.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pGLL(t=t, kappa=0.5, alpha=0.35, eta=0.9)
#'

pGLL <- function(t, kappa,alpha, eta){
  val <- (1-((1+((t*eta)^alpha))^(-((kappa^alpha)/(eta^alpha)))))
  return(val)
}

#--------------------------------------------------------------------------------------------------------------------------
#' Generalized Log-logistic (GLL) survivor function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the GLL survivor function
#' @references Muse, A. H., Mwalili, S., Ngesa, O., Alshanbari, H. M., Khosa, S. K., & Hussam, E. (2022). Bayesian and frequentist approach for the generalized log-logistic accelerated failure time model with applications to larynx-cancer patients. Alexandria Engineering Journal, 61(10), 7953-7978.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sGLL(t=t, kappa=0.5, alpha=0.35, eta=0.9)
#'

sGLL <- function(t, kappa,alpha, eta){
  sf0 <- (1+((t*eta)^alpha))^(-((kappa^alpha)/(eta^alpha)))
  val<-sf0
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rMKW(t=t, alpha=0.35, kappa=0.7, eta=1.4, log=FALSE)
#'
rMKW <- function(t,alpha,kappa,eta,log=FALSE){
  log.h <-  (log(kappa)+log(eta)+log(alpha)+((eta-1)*log(1-exp(-t^kappa)))+((kappa-1)*log(t)+log(exp(-t^kappa))))-(log(1-(1-exp(-t^kappa))^eta))
  ifelse(log, return(log.h), return(exp(log.h)))
}

#----------------------------------------------------------------------------------------
#' Modified Kumaraswamy Weibull (MKW) Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param alpha   : Inverse scale parameter
#' @param kappa   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @return the value of the MKW cumulative distribution function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pMKW(t=t,alpha=0.35, kappa=0.7, eta=1.4)
#'
pMKW<- function(t,alpha,kappa,eta){
  cdf0 <-1- (1-(1-exp(-t^kappa))^eta)^alpha
  return(cdf0)
}

#----------------------------------------------------------------------------------------
#' Modified Kumaraswamy Weibull (MKW) Survivor Function.
#----------------------------------------------------------------------------------------
#' @param alpha   : Inverse scale parameter
#' @param kappa   : shape parameter
#' @param eta     : shape parameter
#' @param t       : positive argument
#' @return the value of the MKW survivor function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sMKW(t=t,alpha=0.35, kappa=0.7, eta=1.4)
#'
sMKW<- function(t,alpha,kappa,eta){
  sf <- (1-(1-exp(-t^kappa))^eta)^alpha
  return(sf)
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pEW(t=t, lambda=0.65,kappa=0.45, alpha=0.25, log.p=FALSE)
#'

pEW<- function(t,lambda,kappa,alpha,log.p=FALSE){
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rEW(t=t, lambda=0.9, kappa=0.5, alpha=0.75, log=FALSE)
#'
rEW <- function(t,lambda,kappa,alpha,log=FALSE){
  log.pdf <-  log(alpha) + (alpha-1)*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) +
    dweibull(t,scale=lambda,shape=kappa,log=TRUE)
  cdf <- exp(alpha*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) )
  log.h <- log.pdf - log(1-cdf)
  ifelse(log, return(log.h), return(exp(log.h)))
}

#----------------------------------------------------------------------------------------
#' Exponentiated Weibull (EW) Survivor Function.
#----------------------------------------------------------------------------------------
#' @param lambda   : scale parameter
#' @param kappa   : shape parameter
#' @param alpha    : shape parameter
#' @param t       : positive argument
#' @return the value of the EW survivor function
#' @references Rubio, F. J., Remontet, L., Jewell, N. P., & Belot, A. (2019). On a general structure for hazard-based regression models: an application to population-based cancer research. Statistical methods in medical research, 28(8), 2404-2417.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sEW(t=t, lambda=0.9, kappa=0.5, alpha=0.75)
#'
sEW<- function(t,lambda,kappa,alpha){
  cdf <- exp(alpha*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) )
  return(1-cdf)
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rMLL(t=t, kappa=0.75, alpha=0.5, eta=0.9,log=FALSE)
#'

rMLL<- function(t,kappa,alpha,eta,log=FALSE){
  log.h <- log(kappa*(kappa*t)^(alpha-1)*exp(eta*t)*(alpha+eta*t)/(1+((kappa*t)^alpha)*exp(eta*t)))
  ifelse(log, return(log.h), return(exp(log.h)))
}
#--------------------------------------------------------------------------------------------------------------------------
#' Modified Log-logistic (MLL) cumulative distribution function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the MLL cumulative distribution function
#' @references Kayid, M. (2022). Applications of Bladder Cancer Data Using a Modified Log-Logistic Model. Applied Bionics and Biomechanics, 2022.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pMLL(t=t, kappa=0.75, alpha=0.5, eta=0.9)
#'

pMLL<- function(t,kappa,alpha,eta){
  cdf0 <- 1- (1/(1+((kappa*t)^alpha)*exp(eta*t)))
  return(cdf0)
}
#--------------------------------------------------------------------------------------------------------------------------
#' Modified Log-logistic (MLL) survivor function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the MLL survivor function
#' @references Kayid, M. (2022). Applications of Bladder Cancer Data Using a Modified Log-Logistic Model. Applied Bionics and Biomechanics, 2022.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sMLL(t=t, kappa=0.75, alpha=0.5, eta=0.9)
#'

sMLL<- function(t,kappa,alpha,eta){
  sf <-  1/(1+((kappa*t)^alpha)*exp(eta*t))
  return(sf)
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rPGW(t=t, kappa=0.5, alpha=1.5, eta=0.6,log=FALSE)
#'

rPGW <- function(t, kappa,alpha, eta, log = FALSE){
  val <- log(alpha) - log(eta) - alpha*log(kappa) + (alpha-1)*log(t) +
    (1/eta - 1)*log( 1 + (t/kappa)^alpha )
  if(log) return(val) else return(exp(val))
}

#--------------------------------------------------------------------------------------------------------------------------
#' Power Generalised Weibull (PGW) cumulative distribution function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the PGW cumulative distribution function
#' @references Alvares, D., & Rubio, F. J. (2021). A tractable Bayesian joint model for longitudinal and survival data. Statistics in Medicine, 40(19), 4213-4229.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pPGW(t=t, kappa=0.5, alpha=1.5, eta=0.6)
#'

pPGW <- function(t, kappa, alpha, eta){
  cdf0 <- 1-exp(1-( 1 + (t/kappa)^alpha )^(1/eta))
  return(cdf0)
}

#--------------------------------------------------------------------------------------------------------------------------
#' Power Generalised Weibull (PGW) survivor function.
#--------------------------------------------------------------------------------------------------------------------------
#' @param kappa     : scale parameter
#' @param alpha      : shape parameter
#' @param eta   : shape parameter
#' @param t       : positive argument
#' @return the value of the PGW survivor function
#' @references Alvares, D., & Rubio, F. J. (2021). A tractable Bayesian joint model for longitudinal and survival data. Statistics in Medicine, 40(19), 4213-4229.
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sPGW(t=t, kappa=0.5, alpha=1.5, eta=0.6)
#'

sPGW <- function(t, kappa, alpha, eta){
  sf <- exp(1 - ( 1 + (t/kappa)^alpha)^(1/eta))
 return(sf)
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pdGG(t=t, kappa=0.5, alpha=0.35, eta=0.9,log=FALSE)
#'

pdGG <- function(t, kappa, alpha, eta, log = FALSE){
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pGG(t=t, kappa=0.5, alpha=0.35, eta=0.9,log.p=FALSE)
#'

pGG <- function(t, kappa, alpha, eta, log.p = FALSE){
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sGG(t=t, kappa=0.5, alpha=0.35, eta=0.9,log.p=FALSE)
#'
sGG <- function(t, kappa, alpha, eta, log.p = FALSE){
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rGG(t=t, kappa=0.5, alpha=0.35, eta=0.9,log=FALSE)
#'
rGG <- function(t, kappa, alpha, eta, log = FALSE){
  val <- dggamma(t, kappa, alpha, eta, log = TRUE) - sggamma(t, kappa, alpha, eta, log.p = TRUE)
  if(log) return(val) else return(exp(val))
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rLL(t=t, kappa=0.5, alpha=0.35,log=FALSE)
#'
rLL<- function(t,kappa,alpha, log = FALSE){
  pdf0 <-  dllogis(t,shape=alpha,scale=kappa)
  cdf0 <- pllogis(t,shape=alpha,scale=kappa)
  val<-log(pdf0)-log(1-cdf0)
  if(log) return(val) else return(exp(val))
}


#----------------------------------------------------------------------------------------
#' Log-logistic  (LL) Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha    : shape parameter
#' @param t       : positive argument
#' @return the value of the LL cumulative distribution function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pLL(t=t, kappa=0.5, alpha=0.35)
#'
pLL<- function(t,kappa,alpha){
  cdf0<- pllogis(t,shape=alpha,scale=kappa)
  val<-cdf0
  return(val)
}

#----------------------------------------------------------------------------------------
#' Log-logistic  (LL) Survivor Function.
#----------------------------------------------------------------------------------------
#' @param kappa    : scale parameter
#' @param alpha    : shape parameter
#' @param t       : positive argument
#' @return the value of the LL survivor function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sLL(t=t, kappa=0.5, alpha=0.35)
#'
sLL<- function(t,kappa,alpha){
  cdf0<- pllogis(t,shape=alpha,scale=kappa)
  val<-1-cdf0
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rW(t=t, kappa=0.75, alpha=0.5,log=FALSE)
#'
rW<- function(t,kappa,alpha, log = FALSE){
  val<- log(alpha*kappa*t^(alpha-1))
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Weibull  (W) Survivor Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param t       : positive argument
#' @return the value of the W Survivor function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sW(t=t, kappa=0.75, alpha=0.5)
#'
sW<- function(t,kappa,alpha){
  val <- exp(-kappa*t^alpha)
  return(val)
}

#----------------------------------------------------------------------------------------
#' Weibull  (W) Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : scale parameter
#' @param alpha   : shape parameter
#' @param t       : positive argument
#' @return the value of the W Cumulative Distribution function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pW(t=t, kappa=0.75, alpha=0.5)
#'
pW<- function(t,kappa,alpha){
  val <-exp(1-(-kappa*t^alpha))
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rLN(t=t, kappa=0.5, alpha=0.75,log=FALSE)
#'

rLN <- function(t,kappa,alpha, log = FALSE){
  pdf0 <- dlnorm(t,meanlog=kappa,sdlog=alpha)
  cdf0 <- plnorm(t,meanlog=kappa,sdlog=alpha)
  val<-log(pdf0)-log(1-cdf0)
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Lognormal (LN) Survivor Hazard Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : meanlog parameter
#' @param alpha   : sdlog parameter
#' @param t       : positive argument
#' @return the value of the LN Survivor function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sLN(t=t, kappa=0.75, alpha=0.95)
#'
sLN<- function(t,kappa,alpha){
  cdf <- plnorm(t,meanlog=kappa,sdlog=alpha)
  val<-(1-cdf)
  return(val)
}

#----------------------------------------------------------------------------------------
#' Lognormal (LN) Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param kappa   : meanlog parameter
#' @param alpha   : sdlog parameter
#' @param t       : positive argument
#' @return the value of the LN cumulative distribution function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pLN(t=t, kappa=0.75, alpha=0.95)
#'
pLN<- function(t,kappa,alpha){
  cdf <- plnorm(t,meanlog=kappa,sdlog=alpha)
  val<-cdf
  return(val)
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
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' rG(t=t, shape=0.5, scale=0.85,log=FALSE)
#'
rG <- function(t, shape, scale, log = FALSE){
  lpdf0 <-  dgamma(t, shape = shape, scale = scale, log = T)
  ls0 <- pgamma(t, shape = shape, scale = scale, lower.tail = FALSE, log.p = T)
  val <- lpdf0 - ls0
  if(log) return(val) else return(exp(val))
}

#----------------------------------------------------------------------------------------
#' Gamma (G) Cumulative Distribution Function.
#----------------------------------------------------------------------------------------
#' @param shape   : shape parameter
#' @param scale      : scale parameter
#' @param t       : positive argument
#' @return the value of the G Cumulative Distribution function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' pG(t=t, shape=0.85, scale=0.5)
#'
pG <- function(t, shape, scale){
  p0 <- pgamma(t, shape = shape, scale = scale)
  return(p0)
}

#----------------------------------------------------------------------------------------
#' Gamma (G) Survivor Function.
#----------------------------------------------------------------------------------------
#' @param shape   : shape parameter
#' @param scale      : scale parameter
#' @param t       : positive argument
#' @return the value of the G Survivor function
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau   \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#' t=runif(10,min=0,max=1)
#' sG(t=t, shape=0.85, scale=0.5)
#'
sG <- function(t, shape, scale){
  s0 <- 1-pgamma(t, shape = shape, scale = scale)
  return(s0)
}

###############################################################################################
###############################################################################################
###############################################################################################
#' General Odds (GO) Model.
###############################################################################################
###############################################################################################
###############################################################################################

########################################################################################################
#' @description A Tractable Parametric General Odds (GO) model's Log-likelihood, MLE and information criterion values.
#' Baseline hazards: NGLL,GLL,MLL,PGW, GG, EW, MKW, LL, TLL, SLL,CLL,SCLL,ATLL, and ASLL
########################################################################################################
#' @param init  : initial points for optimisation
#' @param zt : design matrix for time-dependent effects (q x n), q >= 1
#' @param z  : design matrix for odds-level effects (p x n), p >= 1
#' @param status : vital status (1 - dead, 0 - alive)
#' @param n : The number of the data set
#' @param times  : survival times
#' @param basehaz : baseline hazard structure including baseline (New generalized log-logistic general  odds "NGLLGO" model,  generalized log-logisitic general odds "GLLGO" model,
#' modified log-logistic general odds "MLLGO" model,exponentiated Weibull general odds "EWGO" model,
#' power generalized weibull general  odds "PGWGO" model, generalized gamma general odds "GGGO" model,
#' modified kumaraswamy Weibull general odds "MKWGO" model, log-logistic general odds "LLGO" model,
#' tangent-log-logistic general odds "TLLGO" model, sine-log-logistic general odds "SLLGO" model,
#' cosine log-logistic general  odds "CLLGO" model,secant-log-logistic general odds "SCLLGO" model,
#' arcsine-log-logistic general odds "ASLLGO" model, arctangent-log-logistic general odds "ATLLGO" model,
#' Weibull general odds "WGO" model, gamma general odds "WGO" model, and log-normal general odds "ATLNGO" model.)
#' @param hessian :A function to return (as a matrix) the hessian for those methods that can use this information.
#' @param conf.int : confidence level
#' @param method :"optim" or a method from "nlminb".The methods supported are: BFGS (default), "L-BFGS", "Nelder-Mead", "SANN", "CG", and "Brent".
#' @param maxit :The maximum number of iterations. Defaults to 1000
#' @param log     :log scale (TRUE or FALSE)
#' @return a list containing the output of the optimisation (OPT) and the log-likelihood function (loglik)
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#'
#' #Example #1
#' data(alloauto)
#' time<-alloauto$time
#' delta<-alloauto$delta
#' z<-alloauto$type
#' MLEGO(init = c(1.0,0.50,0.50,0.5,0.5),times = time,status = delta,n=nrow(z),
#' basehaz = "PGWGO",z = z,zt=z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'
#' #Example #2
#' data(bmt)
#' time<-bmt$Time
#' delta<-bmt$Status
#' z<-bmt$TRT
#' MLEGO(init = c(1.0,0.50,0.45,0.5),times = time,status = delta,n=nrow(z),
#' basehaz = "TLLGO",z = z,zt=z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,
#' log=FALSE)
#'
#' #Example #3
#' data("gastric")
#' time<-gastric$time
#' delta<-gastric$status
#' z<-gastric$trt
#' MLEGO(init = c(1.0,1.0,0.50,0.5,0.5),times = time,status = delta,n=nrow(z),
#' basehaz = "GLLGO",z = z,zt=z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'
#'
MLEGO <- function(init, times, status, n,basehaz, z, zt,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000, log=FALSE){
  # Required variables
  times <- as.vector(times)
  status <- as.vector(as.logical(status))
  z <- as.matrix(z)
  zt <- as.matrix(zt)
  n<-nrow(z)
  conf.int<-0.95
  hessian<- TRUE
  times.obs <- times[status]
  if(!is.null(z))  z.obs <- z[status,]
  if(!is.null(zt))  zt.obs <- zt[status,]
  p0 <- dim(z)[2]
  p1 <- dim(zt)[2]

  # NGLL - GO Model
  if(basehaz == "NGLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); de0<-exp(par[4]);beta1 <- par[5:(4+p0)];beta2 <- par[(5+p0):(4+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rNGLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0,de0, log=FALSE)))/((exp.x.dif2.obs*pNGLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0,de0))+SNGLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0,de0)))
      lsf0<-(1/(1+(exp.x.dif2*((pNGLL(times*exp.x.beta1,ae0,be0,ce0,de0)/SNGLL(times*exp.x.beta1,ae0,be0,ce0,de0))))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }

  # GLL - GO Model
  if(basehaz == "GLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]);beta1 <- par[4:(3+p0)];beta2 <- par[(4+p0):(3+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rGLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.dif2.obs*pGLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0))+sGLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0)))
      lsf0<-(1/(1+(exp.x.dif2*((pGLL(times*exp.x.beta1,ae0,be0,ce0)/sGLL(times*exp.x.beta1,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # MLL - GO Model
  if(basehaz == "MLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]);beta1 <- par[4:(3+p0)];beta2 <- par[(4+p0):(3+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rMLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.dif2.obs*pMLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0))+sMLL(times.obs*exp.x.beta1.obs,ae0,be0,ce0)))
      lsf0<-(1/(1+(exp.x.dif2*((pMLL(times*exp.x.beta1,ae0,be0,ce0)/sMLL(times*exp.x.beta1,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # GG - GO Model
  if(basehaz == "GGGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]);beta1 <- par[4:(3+p0)];beta2 <- par[(4+p0):(3+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rGG(times.obs*exp.x.beta1.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.dif2.obs*pGG(times.obs*exp.x.beta1.obs,ae0,be0,ce0))+sGG(times.obs*exp.x.beta1.obs,ae0,be0,ce0)))
      lsf0<-(1/(1+(exp.x.dif2*((pGG(times*exp.x.beta1,ae0,be0,ce0)/sGG(times*exp.x.beta1,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # EW - GO Model
  if(basehaz == "EWGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]);beta1 <- par[4:(3+p0)];beta2 <- par[(4+p0):(3+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rEW(times.obs*exp.x.beta1.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.dif2.obs*pEW(times.obs*exp.x.beta1.obs,ae0,be0,ce0))+sEW(times.obs*exp.x.beta1.obs,ae0,be0,ce0)))
      lsf0<-(1/(1+(exp.x.dif2*((pEW(times*exp.x.beta1,ae0,be0,ce0)/sEW(times*exp.x.beta1,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # PGW - GO Model
  if(basehaz == "PGWGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]);beta1 <- par[4:(3+p0)];beta2 <- par[(4+p0):(3+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rPGW(times.obs*exp.x.beta1.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.dif2.obs*pPGW(times.obs*exp.x.beta1.obs,ae0,be0,ce0))+sPGW(times.obs*exp.x.beta1.obs,ae0,be0,ce0)))
      lsf0<-(1/(1+(exp.x.dif2*((pPGW(times*exp.x.beta1,ae0,be0,ce0)/sPGW(times*exp.x.beta1,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # MKW - GO Model
  if(basehaz == "MKWGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]);beta1 <- par[4:(3+p0)];beta2 <- par[(4+p0):(3+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rMKW(times.obs*exp.x.beta1.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.dif2.obs*pMKW(times.obs*exp.x.beta1.obs,ae0,be0,ce0))+sMKW(times.obs*exp.x.beta1.obs,ae0,be0,ce0)))
      lsf0<-(1/(1+(exp.x.dif2*((pMKW(times*exp.x.beta1,ae0,be0,ce0)/sMKW(times*exp.x.beta1,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # TLL - GO Model
  if(basehaz == "TLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rTLL(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pTLL(times.obs*exp.x.beta1.obs,ae0,be0))+sTLL(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pTLL(times*exp.x.beta1,ae0,be0)/sTLL(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # CLL - GO Model
  if(basehaz == "CLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rCLL(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pCLL(times.obs*exp.x.beta1.obs,ae0,be0))+sCLL(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pCLL(times*exp.x.beta1,ae0,be0)/sCLL(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # SLL - GO Model
  if(basehaz == "SLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rSLL(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pSLL(times.obs*exp.x.beta1.obs,ae0,be0))+sSLL(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pSLL(times*exp.x.beta1,ae0,be0)/sSLL(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # SCLL - GO Model
  if(basehaz == "SCLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rSCLL(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pTLL(times.obs*exp.x.beta1.obs,ae0,be0))+sSCLL(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pSCLL(times*exp.x.beta1,ae0,be0)/sSCLL(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # ATLL - GO Model
  if(basehaz == "ATLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rATLL(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pATLL(times.obs*exp.x.beta1.obs,ae0,be0))+sATLL(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pATLL(times*exp.x.beta1,ae0,be0)/sATLL(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # ASLL - GO Model
  if(basehaz == "ASLLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rASLL(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pASLL(times.obs*exp.x.beta1.obs,ae0,be0))+sASLL(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pASLL(times*exp.x.beta1,ae0,be0)/sASLL(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # LL - GO Model
  if(basehaz == "LLGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rLL(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pLL(times.obs*exp.x.beta1.obs,ae0,be0))+sLL(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pLL(times*exp.x.beta1,ae0,be0)/sLL(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # W - GO Model
  if(basehaz == "WGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rW(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pW(times.obs*exp.x.beta1.obs,ae0,be0))+sW(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pW(times*exp.x.beta1,ae0,be0)/sW(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # G - GO Model
  if(basehaz == "GGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rG(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pG(times.obs*exp.x.beta1.obs,ae0,be0))+sG(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pG(times*exp.x.beta1,ae0,be0)/sG(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # LN - GO Model
  if(basehaz == "LNGO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);beta1 <- par[3:(2+p0)];beta2 <- par[(3+p0):(2+p0+p1)];
      x.beta1 <- as.vector(zt%*%beta1)
      x.beta2 <- as.vector(z%*%beta2)
      x.betadiff<- as.vector(z%*%(beta2-beta1))
      exp.x.beta1 <- exp(x.beta1)
      exp.x.beta1.obs <- exp(x.beta1[status])
      exp.x.beta2 <- exp(x.beta2)
      exp.x.beta2.obs <- exp(x.beta2[status])
      exp.x.dif2 <- exp(x.betadiff)
      exp.x.dif2.obs<- exp(x.betadiff[status])
      lhaz0 <- log((exp.x.beta2.obs*(rLN(times.obs*exp.x.beta1.obs,ae0,be0, log=FALSE)))/((exp.x.dif2.obs*pLN(times.obs*exp.x.beta1.obs,ae0,be0))+sLN(times.obs*exp.x.beta1.obs,ae0,be0)))
      lsf0<-(1/(1+(exp.x.dif2*((pLN(times*exp.x.beta1,ae0,be0)/sLN(times*exp.x.beta1,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  if(method != "optim") OPT <-   optim(init,log.lik,control=list(maxit=maxit), method = method,hessian = TRUE,)
  if(method == "optim") OPT <- nlminb(init,log.lik,hessian=TRUE,control=list(iter.max=maxit))

  pars=length(OPT$par)
  l=OPT$value
  MLE<-OPT$par
  MLE<-MLE
  hessian<-OPT$hessian
  se<-sqrt(diag(solve(hessian)))
  SE<-se
  zval <- pars/ se
  zvalue=zval
  ci.lo<-MLE-SE*qnorm((1-conf.int)/2,lower.tail=FALSE)
  ci.lo<-ci.lo
  ci.up<-MLE+SE*qnorm((1-conf.int)/2,lower.tail=FALSE)
  ci.up<-ci.up
  p.value = 2*stats::pnorm(-abs(zval))
  p.value=p.value
  estimates<-data.frame(MLE,SE,zvalue,
                        "p.value"=ifelse(p.value<0.001,"<0.001",round(p.value,digits=3)),lower.95=ci.lo,upper.95=ci.up)
  AIC=2*l + 2*pars
  CAIC=AIC+(2*pars*(pars+1)/(n-l+1))
  HQIC= 2*l+2*log(log(n))*pars
  BCAIC=2*l+(pars*(log(n)+1))
  BIC=(2*l)+(pars*(log(n)))
  informationcriterions=cbind(AIC,CAIC,BCAIC,BIC,HQIC)
  value<-OPT$value
  convergence<-OPT$convergence
  counts<-OPT$counts
  message<-OPT$message
  loglikelihood<-cbind(value,convergence,message)
  counts<-cbind(counts)
  result <- list(estimates=estimates, informationcriterions=informationcriterions,loglikelihood=loglikelihood,counts=counts)
  return(result)
}




###############################################################################################
###############################################################################################
###############################################################################################
#' Accelerated Odds (AO) Model.
###############################################################################################
###############################################################################################
###############################################################################################

########################################################################################################
#' @description  A Tractable Parametric Accelerated Odds (AO) model's maximum likelihood estimates,log-likelihood, and Information Criterion values.
#'  Baseline hazards: NGLL,GLL,MLL,PGW, GG, EW, MKW, LL, TLL, SLL,CLL,SCLL,ATLL, and ASLL
########################################################################################################
#' @param init  : Initial parameters to maximize the likelihood function;
#' @param z     : design matrix for covariates (p x n), p >= 1
#' @param status : vital status (1 - dead, 0 - alive)
#' @param n : The number of the data set
#' @param times  : survival times
#' @param basehaz : baseline hazard structure including baseline (New generalized log-logistic accelerated odds "NGLLAO" model,  generalized log-logisitic accelerated odds "GLLAO" model,
#' modified log-logistic accelerated odds "MLLAO" model,exponentiated Weibull accelerated odds "EWAO" model,
#' power generalized weibull accelerated odds "PGWAO" model, generalized gamma accelerated odds "GGAO" model,
#' modified kumaraswamy Weibull accelerated odds "MKWAO" model, log-logistic accelerated odds "LLAO" model,
#' tangent-log-logistic accelerated odds "TLLAO" model, sine-log-logistic accelerated odds "SLLAO" model,
#' cosine log-logistic accelerated odds "CLLAO" model,secant-log-logistic accelerated odds "SCLLAO" model,
#' arcsine-log-logistic accelerated odds "ASLLAO" model,arctangent-log-logistic accelerated odds "ATLLAO" model,
#' Weibull accelerated odds "WAO" model, gamma accelerated odds "WAO" model, and log-normal accelerated odds "ATLNAO" model.)
#' @param hessian :A function to return (as a matrix) the hessian for those methods that can use this information.
#' @param conf.int : confidence level
#' @param method :"optim" or a method from "nlminb".The methods supported are: BFGS (default), "L-BFGS", "Nelder-Mead", "SANN", "CG", and "Brent".
#' @param maxit :The maximum number of iterations. Defaults to 1000
#' @param log     :log scale (TRUE or FALSE)
#' @return a list containing the output of the optimisation (OPT) and the log-likelihood function (loglik)
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#'
#'#Example #1
#'data(alloauto)
#'time<-alloauto$time
#'delta<-alloauto$delta
#'z<-alloauto$type
#'MLEAO(init = c(1.0,0.40,0.50,0.50),times = time,status = delta,n=nrow(z),
#'basehaz = "GLLAO",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'
#'#Example #2
#'data(bmt)
#'time<-bmt$Time
#'delta<-bmt$Status
#'z<-bmt$TRT
#'MLEAO(init = c(1.0,1.0,0.5),times = time,status = delta,n=nrow(z),
#'basehaz = "CLLAO",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,
#'log=FALSE)
#'
#'#Example #3
#'data("gastric")
#'time<-gastric$time
#'delta<-gastric$status
#'z<-gastric$trt
#'MLEAO(init = c(1.0,1.0,0.5),times = time,status = delta,n=nrow(z),
#'basehaz = "LNAO",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'
#'#Example #4
#'data("larynx")
#'time<-larynx$time
#'delta<-larynx$delta
#'larynx$age<-as.numeric(scale(larynx$age))
#'larynx$diagyr<-as.numeric(scale(larynx$diagyr))
#'larynx$stage<-as.factor(larynx$stage)
#'z<-model.matrix(~ stage+age+diagyr, data = larynx)
#'MLEAO(init = c(1.0,1.0,0.5,0.5,0.5,0.5,0.5,0.5),times = time,status = delta,n=nrow(z),
#'basehaz = "ASLLAO",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'

MLEAO <- function(init, times, status, n,basehaz, z, method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000, log=FALSE){
  # Required variables
  times <- as.vector(times)
  status <- as.vector(as.logical(status))
  z <- as.matrix(z)
  conf.int<-0.95
  hessian<- TRUE
  n<-nrow(z)
  times.obs <- times[status]
  if(!is.null(z))  z.obs <- z[status,]
  p0 <- dim(z)[2]

  # NGLL - AO Model
  if(basehaz == "NGLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); de0<-exp(par[4]);beta <- par[5:(4+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rNGLL(times.obs*exp.x.beta.obs,ae0,be0,ce0,de0, log=FALSE)))/((pNGLL(times.obs*exp.x.beta.obs,ae0,be0,ce0,de0)/exp.x.beta.obs)+SNGLL(times.obs*exp.x.beta.obs,ae0,be0,ce0,de0)))
      lsf0<-(1/(1+(1/exp.x.beta*((pNGLL(times*exp.x.beta,ae0,be0,ce0,de0)/SNGLL(times*exp.x.beta,ae0,be0,ce0,de0))))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }

  # GLL - AO Model
  if(basehaz == "GLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rGLL(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=FALSE)))/((pGLL(times.obs*exp.x.beta.obs,ae0,be0,ce0)/exp.x.beta.obs)+(sGLL(times.obs*exp.x.beta.obs,ae0,be0,ce0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pGLL(times*exp.x.beta,ae0,be0,ce0)/sGLL(times*exp.x.beta,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # MLL - AO Model
  if(basehaz == "MLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rMLL(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=FALSE)))/((pMLL(times.obs*exp.x.beta.obs,ae0,be0,ce0)/exp.x.beta.obs)+(sMLL(times.obs*exp.x.beta.obs,ae0,be0,ce0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pMLL(times*exp.x.beta,ae0,be0,ce0)/sMLL(times*exp.x.beta,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # GG - AO Model
  if(basehaz == "GGAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rGG(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=FALSE)))/((pGG(times.obs*exp.x.beta.obs,ae0,be0,ce0)/exp.x.beta.obs)+(sGG(times.obs*exp.x.beta.obs,ae0,be0,ce0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pGG(times*exp.x.beta,ae0,be0,ce0)/sGG(times*exp.x.beta,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # EW - AO Model
  if(basehaz == "EWAO"){
    log.lik <- function(par){
      ae0 <- par[1]; be0 <- par[2];  ce0 <- par[3]; beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rEW(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=FALSE)))/((pEW(times.obs*exp.x.beta.obs,ae0,be0,ce0)/exp.x.beta.obs)+(sEW(times.obs*exp.x.beta.obs,ae0,be0,ce0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pEW(times*exp.x.beta,ae0,be0,ce0)/sEW(times*exp.x.beta,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # PGW - AO Model
  if(basehaz == "PGWAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rPGW(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=FALSE)))/((pPGW(times.obs*exp.x.beta.obs,ae0,be0,ce0)/exp.x.beta.obs)+(sPGW(times.obs*exp.x.beta.obs,ae0,be0,ce0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pPGW(times*exp.x.beta,ae0,be0,ce0)/sPGW(times*exp.x.beta,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # MKW - AO Model
  if(basehaz == "MKWAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rMKW(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=FALSE)))/((pMKW(times.obs*exp.x.beta.obs,ae0,be0,ce0)/exp.x.beta.obs)+(sMKW(times.obs*exp.x.beta.obs,ae0,be0,ce0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pMKW(times*exp.x.beta,ae0,be0,ce0)/sMKW(times*exp.x.beta,ae0,be0,ce0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # TLL - AO Model
  if(basehaz == "TLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rTLL(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pTLL(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sTLL(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pTLL(times*exp.x.beta,ae0,be0)/sTLL(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # CLL - AO Model
  if(basehaz == "CLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rCLL(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pCLL(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sCLL(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pCLL(times*exp.x.beta,ae0,be0)/sCLL(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # SLL - AO Model
  if(basehaz == "SLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rSLL(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pSLL(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sSLL(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pSLL(times*exp.x.beta,ae0,be0)/sSLL(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # SCLL - AO Model
  if(basehaz == "SCLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rSCLL(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pSCLL(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sSCLL(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pSCLL(times*exp.x.beta,ae0,be0)/sCLL(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # ATLL - AO Model
  if(basehaz == "ATLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rATLL(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pATLL(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sATLL(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pATLL(times*exp.x.beta,ae0,be0)/sATLL(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # ASLL - AO Model
  if(basehaz == "ASLLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rASLL(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pASLL(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sASLL(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pASLL(times*exp.x.beta,ae0,be0)/sASLL(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # LL - AO Model
  if(basehaz == "LLAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rLL(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pLL(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sLL(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pLL(times*exp.x.beta,ae0,be0)/sLL(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # LN - AO Model
  if(basehaz == "LNAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rLN(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pLN(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sLN(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pLN(times*exp.x.beta,ae0,be0)/sLN(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # W - AO Model
  if(basehaz == "WAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rW(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pW(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sW(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pW(times*exp.x.beta,ae0,be0)/sW(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # G - AO Model
  if(basehaz == "GAO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log(((rG(times.obs*exp.x.beta.obs,ae0,be0, log=FALSE)))/((pG(times.obs*exp.x.beta.obs,ae0,be0)/exp.x.beta.obs)+(sG(times.obs*exp.x.beta.obs,ae0,be0))))
      lsf0<-(1/(1+(1/exp.x.beta*((pG(times*exp.x.beta,ae0,be0)/sG(times*exp.x.beta,ae0,be0))))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  if(method != "optim") OPT <-optim(init,log.lik,control=list(maxit=maxit), method = method,hessian = TRUE)
  if(method == "optim") OPT <- nlminb(init,log.lik,hessian=TRUE,control=list(iter.max=maxit))

  pars=length(OPT$par)
  l=OPT$value
  MLE<-OPT$par
  MLE<-MLE
  hessian<-OPT$hessian
  se<-sqrt(diag(solve(hessian)))
  SE<-se
  zval <- pars/ se
  zvalue=zval
  ci.lo<-MLE-SE*qnorm((1-conf.int)/2,lower.tail=FALSE)
  ci.lo<-ci.lo
  ci.up<-MLE+SE*qnorm((1-conf.int)/2,lower.tail=FALSE)
  ci.up<-ci.up
  p.value = 2*stats::pnorm(-abs(zval))
  p.value=p.value
  estimates<-data.frame(MLE,SE,zvalue,
                        "p.value"=ifelse(p.value<0.001,"<0.001",round(p.value,digits=3)),lower.95=ci.lo,upper.95=ci.up)
  AIC=2*l + 2*pars
  CAIC=AIC+(2*pars*(pars+1)/(n-l+1))
  HQIC= 2*l+2*log(log(n))*pars
  BCAIC=2*l+(pars*(log(n)+1))
  BIC=(2*l)+(pars*(log(n)))
  informationcriterions=cbind(AIC,CAIC,BCAIC,BIC,HQIC)
  value<-OPT$value
  convergence<-OPT$convergence
  counts<-OPT$counts
  message<-OPT$message
  loglikelihood<-cbind(value,convergence,message)
  counts<-cbind(counts)
  result <- list(estimates=estimates, informationcriterions=informationcriterions,loglikelihood=loglikelihood,counts=counts)
  return(result)
}



###############################################################################################
###############################################################################################
###############################################################################################
#' Proportional Odds (PO) model.
###############################################################################################
###############################################################################################
###############################################################################################

########################################################################################################
#' @description  Tractable Parametric Proportional Odds (PO) model's maximum likelihood estimation, log-likelihood, and information criterion.
#'  Baseline hazards: NGLL,GLL,MLL,PGW, GG, EW, MKW, LL, TLL, SLL,CLL,SCLL,ATLL, and ASLL
########################################################################################################
#' @param init  : initial points for optimisation
#' @param z     : design matrix for covariates (p x n), p >= 1
#' @param status : vital status (1 - dead, 0 - alive)
#' @param n : The number of the data set
#' @param times  : survival times
#' @param basehaz : baseline hazard structure including baseline (New generalized log-logistic proportional odds "NGLLPO" model,  generalized log-logisitic proportional odds "GLLPO" model, modified log-logistic proportional odds "MLLPO" model,
#' exponentiated Weibull proportional odds "EWPO" model, power generalized weibull proportional odds "PGWPO" model, generalized gamma proportional odds "GGPO" model,
#' modified kumaraswamy Weibull proportional odds "MKWPO" model, log-logistic proportional odds "PO" model,
#' tangent-log-logistic proportional odds "TLLPO" model, sine-log-logistic proportional odds "SLLPO" model, cosine log-logistic proportional odds "CLLPO" model,
#' secant-log-logistic proportional odds "SCLLPO" model, arcsine-log-logistic proportional odds "ASLLPO" model, and arctangent-log-logistic proportional odds "ATLLPO" model,
#' Weibull proportional odds "WPO" model, gamma proportional odds "GPO" model, and log-normal proportional odds "LNPO" model.)
#' @param hessian :A function to return (as a matrix) the hessian for those methods that can use this information.
#' @param conf.int : confidence level
#' @param method :"optim" or a method from "nlminb".The methods supported are: BFGS (default), "L-BFGS", "Nelder-Mead", "SANN", "CG", and "Brent".
#' @param maxit :The maximum number of iterations. Defaults to 1000
#' @param log     :log scale (TRUE or FALSE)
#' @return a list containing the output of the optimisation (OPT) and the log-likelihood function (loglik)
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#'
#' #Example #1
#' data(alloauto)
#' time<-alloauto$time
#' delta<-alloauto$delta
#' z<-alloauto$type
#' MLEPO(init = c(1.0,0.40,1.0,0.50),times = time,status = delta,n=nrow(z),
#' basehaz = "GLLPO",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'
#' #Example #2
#' data(bmt)
#' time<-bmt$Time
#' delta<-bmt$Status
#' z<-bmt$TRT
#' MLEPO(init = c(1.0,1.0,0.5),times = time,status = delta,n=nrow(z),
#' basehaz = "SLLPO",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'
#' #Example #3
#' data("gastric")
#' time<-gastric$time
#' delta<-gastric$status
#' z<-gastric$trt
#' MLEPO(init = c(1.0,0.50,1.0,0.75),times = time,status = delta,n=nrow(z),
#' basehaz = "PGWPO",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,
#' log=FALSE)
#'
#' #Example #4
#' data("larynx")
#' time<-larynx$time
#' delta<-larynx$delta
#' larynx$age<-as.numeric(scale(larynx$age))
#' larynx$diagyr<-as.numeric(scale(larynx$diagyr))
#' larynx$stage<-as.factor(larynx$stage)
#' z<-model.matrix(~ stage+age+diagyr, data = larynx)
#' MLEPO(init = c(1.0,1.0,0.5,0.5,0.5,0.5,0.5,0.5),times = time,status = delta,n=nrow(z),
#' basehaz = "ATLLPO",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'

MLEPO <- function(init, times, status, n,basehaz, z, method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000, log=FALSE){
  # Required variables
  times <- as.vector(times)
  status <- as.vector(as.logical(status))
  z <- as.matrix(z)
  conf.int<-0.95
  hessian<- TRUE
  n<-nrow(z)
  times.obs <- times[status]
  if(!is.null(z))  z.obs <- z[status,]
  p0 <- dim(z)[2]

  # NGLL - PO Model
  if(basehaz == "NGLLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); de0<-exp(par[4]);beta <- par[5:(4+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rNGLL(times.obs,ae0,be0,ce0,de0, log=FALSE)))/((exp.x.beta.obs*pNGLL(times.obs,ae0,be0,ce0,de0))+SNGLL(times.obs,ae0,be0,ce0,de0)))
      lsf0<-1/(1+(exp.x.beta*((pNGLL(times,ae0,be0,ce0,de0)/SNGLL(times,ae0,be0,ce0,de0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }

  # GLL - PO Model
  if(basehaz == "GLLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rGLL(times.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.beta.obs*pGLL(times.obs,ae0,be0,ce0))+(sGLL(times.obs,ae0,be0,ce0))))
      lsf0<-1/(1+(exp.x.beta*((pGLL(times,ae0,be0,ce0)/sGLL(times,ae0,be0,ce0)))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # MLL - PO Model
  if(basehaz == "MLLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rMLL(times.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.beta.obs*pMLL(times.obs,ae0,be0,ce0))+(sMLL(times.obs,ae0,be0,ce0))))
      lsf0<-1/(1+(exp.x.beta*((pMLL(times,ae0,be0,ce0)/sMLL(times,ae0,be0,ce0)))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # GG - PO Model
  if(basehaz == "GGPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rGG(times.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.beta.obs*pGG(times.obs,ae0,be0,ce0))+(sGG(times.obs,ae0,be0,ce0))))
      lsf0<-1/(1+(exp.x.beta*((pGG(times,ae0,be0,ce0)/sGG(times,ae0,be0,ce0)))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # EW - PO Model
  if(basehaz == "EWPO"){
    log.lik <- function(par){
      ae0 <- par[1]; be0 <- par[2];  ce0 <- par[3]; beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rEW(times.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.beta.obs*pEW(times.obs,ae0,be0,ce0))+(sEW(times.obs,ae0,be0,ce0))))
      lsf0<-1/(1+(exp.x.beta*((pEW(times,ae0,be0,ce0)/sEW(times,ae0,be0,ce0)))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # PGW - PO Model
  if(basehaz == "PGWPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rPGW(times.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.beta.obs*pPGW(times.obs,ae0,be0,ce0))+(sPGW(times.obs,ae0,be0,ce0))))
      lsf0<-1/(1+(exp.x.beta*((pPGW(times,ae0,be0,ce0)/sPGW(times,ae0,be0,ce0)))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # MKW - PO Model
  if(basehaz == "MKWPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rMKW(times.obs,ae0,be0,ce0, log=FALSE)))/((exp.x.beta.obs*pMKW(times.obs,ae0,be0,ce0))+(sMKW(times.obs,ae0,be0,ce0))))
      lsf0<-1/(1+(exp.x.beta*((pMKW(times,ae0,be0,ce0)/sMKW(times,ae0,be0,ce0)))))
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # TLL - PO Model
    if(basehaz == "TLLPO"){
      log.lik <- function(par){
        ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
        x.beta <- as.vector(z%*%beta)
        exp.x.beta <- exp(x.beta)
        exp.x.beta.obs <- exp(x.beta[status])
        lhaz0 <- log((exp.x.beta.obs*(rTLL(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pTLL(times.obs,ae0,be0))+sTLL(times.obs,ae0,be0)))
        lsf0<-1/(1+(exp.x.beta*((pTLL(times,ae0,be0)/sTLL(times,ae0,be0)))))
        val <- - sum(lhaz0) - sum(log(lsf0))
        return(sum(val))
      }
    }

  # CLL - PO Model
  if(basehaz == "CLLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rCLL(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pCLL(times.obs,ae0,be0))+sCLL(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pCLL(times,ae0,be0)/sCLL(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }

  # SLL - PO Model
  if(basehaz == "SLLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rSLL(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pSLL(times.obs,ae0,be0))+sSLL(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pSLL(times,ae0,be0)/sSLL(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # SCLL - PO Model
  if(basehaz == "SCLLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rSCLL(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pSCLL(times.obs,ae0,be0))+sSCLL(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pSCLL(times,ae0,be0)/sSCLL(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # ATLL - PO Model
  if(basehaz == "ATLLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rATLL(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pATLL(times.obs,ae0,be0))+sATLL(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pATLL(times,ae0,be0)/sATLL(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # ASLL - PO Model
  if(basehaz == "ASLLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rASLL(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pASLL(times.obs,ae0,be0))+sASLL(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pASLL(times,ae0,be0)/sASLL(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # LL - PO Model
  if(basehaz == "LLPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rLL(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pLL(times.obs,ae0,be0))+sLL(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pLL(times,ae0,be0)/sLL(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # W - PO Model
  if(basehaz == "WPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rW(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pW(times.obs,ae0,be0))+sW(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pW(times,ae0,be0)/sW(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # G - PO Model
  if(basehaz == "GPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rG(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pG(times.obs,ae0,be0))+sG(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pG(times,ae0,be0)/sG(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # LN - PO Model
  if(basehaz == "LNPO"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      lhaz0 <- log((exp.x.beta.obs*(rLL(times.obs,ae0,be0, log=FALSE)))/((exp.x.beta.obs*pLN(times.obs,ae0,be0))+sLN(times.obs,ae0,be0)))
      lsf0<-1/(1+(exp.x.beta*((pLN(times,ae0,be0)/sLN(times,ae0,be0)))))
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  if(method != "optim") OPT <-   optim(init,log.lik,control=list(maxit=maxit), method = method,hessian = TRUE,)
  if(method == "optim") OPT <- nlminb(init,log.lik,hessian=TRUE,control=list(iter.max=maxit))

  pars=length(OPT$par)
  l=OPT$value
  MLE<-OPT$par
  MLE<-MLE
  hessian<-OPT$hessian
  se<-sqrt(diag(solve(hessian)))
  SE<-se
  zval <- pars/ se
  zvalue=zval
  ci.lo<-MLE-SE*qnorm((1-conf.int)/2,lower.tail=FALSE)
  ci.lo<-ci.lo
  ci.up<-MLE+SE*qnorm((1-conf.int)/2,lower.tail=FALSE)
  ci.up<-ci.up
  p.value = 2*stats::pnorm(-abs(zval))
  p.value=p.value
  estimates<-data.frame(MLE,SE,zvalue,
                        "p.value"=ifelse(p.value<0.001,"<0.001",round(p.value,digits=3)),lower.95=ci.lo,upper.95=ci.up)
  AIC=2*l + 2*pars
  CAIC=AIC+(2*pars*(pars+1)/(n-l+1))
  HQIC= 2*l+2*log(log(n))*pars
  BCAIC=2*l+(pars*(log(n)+1))
  BIC=(2*l)+(pars*(log(n)))
  informationcriterions=cbind(AIC,CAIC,BCAIC,BIC,HQIC)
  value<-OPT$value
  convergence<-OPT$convergence
  counts<-OPT$counts
  message<-OPT$message
  loglikelihood<-cbind(value,convergence,message)
  counts<-cbind(counts)
  result <- list(estimates=estimates, informationcriterions=informationcriterions,loglikelihood=loglikelihood,counts=counts)
  return(result)
}






###############################################################################################
###############################################################################################
###############################################################################################
#' Accelerated Failure Time (AFT) Model.
###############################################################################################
###############################################################################################
###############################################################################################

########################################################################################################
#' @description  Tractable Parametric accelerated failure time (AFT) model's maximum likelihood estimation, log-likelihood, and information criterion.
#'  Baseline hazards: NGLL,GLL,MLL,PGW, GG, EW, MKW, LL, TLL, SLL,CLL,SCLL,ATLL, and ASLL
########################################################################################################
#' @param init  : initial points for optimisation
#' @param z     : design matrix for covariates (p x n), p >= 1
#' @param status : vital status (1 - dead, 0 - alive)
#' @param n : The number of the data set
#' @param times  : survival times
#' @param basehaz : baseline hazard structure including baseline (New generalized log-logistic accelerated failure time "NGLLAFT" model,  generalized log-logisitic accelerated failure time "GLLAFT" model, modified log-logistic accelerated failure time "MLLAFT" model,
#' exponentiated Weibull accelerated failure time "EWAFT" model, power generalized weibull accelerated failure time "PGWAFT" model, generalized gamma accelerated failure time "GGAFT" model,
#' modified kumaraswamy Weibull proportional odds "MKWAFT" model, log-logistic accelerated failure time "LLAFT" model,
#' tangent-log-logistic accelerated failure time "TLLAFT" model, sine-log-logistic accelerated failure time "SLLAFT" model, cosine log-logistic accelerated failure time "CLLAFT" model,
#' secant-log-logistic accelerated failure time "SCLLAFT" model, arcsine-log-logistic accelerated failure time "ASLLAFT" model, arctangent-log-logistic accelerated failure time "ATLLAFT" model,
#' Weibull accelerated failure time "WAFT" model, gamma accelerated failure time "GAFT", and log-normal accelerated failure time "LNAFT")
#' @param hessian :A function to return (as a matrix) the hessian for those methods that can use this information.
#' @param conf.int : confidence level
#' @param method :"optim" or a method from "nlminb".The methods supported are: BFGS (default), "L-BFGS", "Nelder-Mead", "SANN", "CG", and "Brent".
#' @param maxit :The maximum number of iterations. Defaults to 1000
#' @param log     :log scale (TRUE or FALSE)
#' @return a list containing the output of the optimisation (OPT) and the log-likelihood function (loglik)
#' @export
#'
#' @author  Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa, Christophe Chesneau  \email{abdisalam.hassan@amoud.edu.so}
#'
#' @examples
#'
#' #Example #1
#' data(alloauto)
#' time<-alloauto$time
#' delta<-alloauto$delta
#' z<-alloauto$type
#' MLEAFT(init = c(1.0,0.20,0.05),times = time,status = delta,n=nrow(z),
#' basehaz = "WAFT",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,
#' log=FALSE)
#'
#' #Example #2
#' data(bmt)
#' time<-bmt$Time
#' delta<-bmt$Status
#' z<-bmt$TRT
#' MLEAFT(init = c(1.0,1.0,0.5),times = time,status = delta,n=nrow(z),
#' basehaz = "LNAFT",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,log=FALSE)
#'
#' #Example #3
#' data("gastric")
#' time<-gastric$time
#' delta<-gastric$status
#' z<-gastric$trt
#' MLEAFT(init = c(1.0,0.50,0.5),times = time,status = delta,n=nrow(z),
#' basehaz = "LLAFT",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,
#' log=FALSE)
#'
#' #Example #4
#' data("larynx")
#' time<-larynx$time
#' delta<-larynx$delta
#' larynx$age<-as.numeric(scale(larynx$age))
#' larynx$diagyr<-as.numeric(scale(larynx$diagyr))
#' larynx$stage<-as.factor(larynx$stage)
#' z<-model.matrix(~ stage+age+diagyr, data = larynx)
#' MLEAFT(init = c(1.0,0.5,0.5,0.5,0.5,0.5,0.5,0.5),times = time,status = delta,n=nrow(z),
#' basehaz = "LNAFT",z = z,method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000,
#' log=FALSE)
#'

MLEAFT <- function(init, times, status, n,basehaz, z, method = "BFGS",hessian=TRUE, conf.int=0.95,maxit = 1000, log=FALSE){
  # Required variables
  times <- as.vector(times)
  status <- as.vector(as.logical(status))
  z <- as.matrix(z)
  conf.int<-0.95
  hessian<- TRUE
  n<-nrow(z)
  times.obs <- times[status]
  if(!is.null(z))  z.obs <- z[status,]
  p0 <- dim(z)[2]

  # NGLL - AFT Model
  if(basehaz == "NGLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); de0<-exp(par[4]);beta <- par[5:(4+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rNGLL(times.obs*exp.x.beta.obs,ae0,be0,ce0,de0, log=TRUE)+x.beta.obs
      lsf0<-SNGLL(times*exp.x.beta,ae0,be0,ce0,de0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }

  # GLL - AFT Model
  if(basehaz == "GLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rGLL(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=TRUE)+x.beta.obs
      lsf0<-sGLL(times*exp.x.beta,ae0,be0,ce0)
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # MLL - AFT Model
  if(basehaz == "MLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rMLL(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=TRUE)+x.beta.obs
      lsf0<-sMLL(times*exp.x.beta,ae0,be0,ce0)
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # GG - AFT Model
  if(basehaz == "GGAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rGG(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=TRUE)+x.beta.obs
      lsf0<-sGG(times*exp.x.beta,ae0,be0,ce0)
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # EW - AFT Model
  if(basehaz == "EWAFT"){
    log.lik <- function(par){
      ae0 <- par[1]; be0 <- par[2];  ce0 <- par[3]; beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rEW(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=TRUE)+x.beta.obs
      lsf0<-sEW(times*exp.x.beta,ae0,be0,ce0)
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }

  # PGW - AFT Model
  if(basehaz == "PGWAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rPGW(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=TRUE)+x.beta.obs
      lsf0<-sPGW(times*exp.x.beta,ae0,be0,ce0)
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # MKW - AFT Model
  if(basehaz == "MKWAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); beta <- par[4:(3+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rMKW(times.obs*exp.x.beta.obs,ae0,be0,ce0, log=TRUE)+x.beta.obs
      lsf0<-sMKW(times*exp.x.beta,ae0,be0,ce0)
      val <- - sum(lhaz0) -sum(log(lsf0))
      return(sum(val))
    }
  }
  # TLL - AFT Model
  if(basehaz == "TLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rTLL(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sTLL(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }

  # CLL - AFT Model
  if(basehaz == "CLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rCLL(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sCLL(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }

  # SLL - AFT Model
  if(basehaz == "SLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rSLL(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sSLL(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # SCLL - AFT Model
  if(basehaz == "SCLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rSCLL(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sSCLL(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # ATLL - AFT Model
  if(basehaz == "ATLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rATLL(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sATLL(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # ASLL - AFT Model
  if(basehaz == "ASLLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rASLL(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sASLL(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # LL - AFT Model
  if(basehaz == "LLAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rLL(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sLL(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # Weibull - AFT Model
  if(basehaz == "WAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rW(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sW(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  # Gamma - AFT Model
  if(basehaz == "GAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rG(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sG(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }

  # Lognormal - AFT Model
  if(basehaz == "LNAFT"){
    log.lik <- function(par){
      ae0 <- exp(par[1]); be0 <- exp(par[2]);  beta <- par[3:(2+p0)];
      x.beta <- as.vector(z%*%beta)
      exp.x.beta <- exp(x.beta)
      exp.x.beta.obs <- exp(x.beta[status])
      x.beta.obs <- x.beta[status]
      lhaz0 <- rLN(times.obs*exp.x.beta.obs,ae0,be0, log=TRUE)+x.beta.obs
      lsf0<-sLN(times*exp.x.beta,ae0,be0)
      val <- - sum(lhaz0) - sum(log(lsf0))
      return(sum(val))
    }
  }
  if(method != "optim") OPT <-   optim(init,log.lik,control=list(maxit=maxit), method = method,hessian = TRUE,)
  if(method == "optim") OPT <- nlminb(init,log.lik,hessian=TRUE,control=list(iter.max=maxit))

  pars=length(OPT$par)
  l=OPT$value
  MLE<-OPT$par
  MLE<-MLE
  hessian<-OPT$hessian
  se<-sqrt(diag(solve(hessian)))
  SE<-se
  zval <- pars/ se
  zvalue=zval
  ci.lo<-MLE-SE*qnorm((1-conf.int)/2,lower.tail=FALSE)
  ci.lo<-ci.lo
  ci.up<-MLE+SE*qnorm((1-conf.int)/2,lower.tail=FALSE)
  ci.up<-ci.up
  p.value = 2*stats::pnorm(-abs(zval))
  p.value=p.value
  estimates<-data.frame(MLE,SE,zvalue,
                        "p.value"=ifelse(p.value<0.001,"<0.001",round(p.value,digits=3)),lower.95=ci.lo,upper.95=ci.up)
  AIC=2*l + 2*pars
  CAIC=AIC+(2*pars*(pars+1)/(n-l+1))
  HQIC= 2*l+2*log(log(n))*pars
  BCAIC=2*l+(pars*(log(n)+1))
  BIC=(2*l)+(pars*(log(n)))
  informationcriterions=cbind(AIC,CAIC,BCAIC,BIC,HQIC)
  value<-OPT$value
  convergence<-OPT$convergence
  counts<-OPT$counts
  message<-OPT$message
  loglikelihood<-cbind(value,convergence,message)
  counts<-cbind(counts)
  result <- list(estimates=estimates, informationcriterions=informationcriterions,loglikelihood=loglikelihood,counts=counts)
  return(result)
}
