#' Simulate stratified clustered competing risks data
#' @description The function \code{simulate_CR_data} simulates stratified competing risks data with two causes based 
#' on a proportional subdistribution hazard model based on \cite{Logan et al. (2011)}.
#' Three covariates (Bernoulli, Normal and Uniform) are considered. 
#' @param n Number of observations in each cluster. Default is 4.
#' @param m Total number of clusters. Default is 100.
#' @param alpha Parameter for a positive stable distribution. It controls correlation within a cluster. 
#' \code{1/alpha} must be an integer such that \code{alpha = 0.25, 0.5 and 1}. \code{alpha=1} generates independent data.
#' As \code{alpha} decreases, the correlation within a cluster increases. Default is 1.
#' @param beta1 This is a vector of values of length 3. This value multiplied by \code{alpha} is a true covariate 
#' effect for Cause 1.
#' @param beta2 This is a vector of values of length 3. It is a true covariate 
#' effect for Cause 2.
#' @param betaC This is a vector of values of length 3. This value multiplied by \code{alpha} is a true covariate 
#' effect for censoring. A marginal proportional hazards model is used to generate clustered censoring times
#' based on \cite{Logan et al. (2011)}.
#' @param lambdaC Constant baseline hazard for censoring for the marginal proportional hazards model.
#' @param stratified It is \code{TRUE} for stratified data. Two strata are considered.
#' If \code{TRUE}, the remaining parameters must be provided.
#' @param rho Baseline hazard for each stratum. Must be a vector of length two.
#' @param lambdaC0 Constant baseline hazard of censoring for each stratum. Must be a vector of length two.
#'
#' @return Returns a data frame with the following variables:
#' \item{time}{Survival times}
#' \item{cause}{Different causes of an event. Censoring is 0. The main cause is 1}
#' \item{Z1}{Bernoulli distributed covariate with probability 0.6}
#' \item{Z2}{Standard normal covariate}
#' \item{Z3}{Uniform distributed covariate}
#' \item{cluster}{Cluster variable}
#' \item{strata}{Strata variable. Only if \code{stratified=TRUE}.}
#' @export
#' @references{Logan BR, Zhang MJ, Klein JP. Marginal models for clustered time-to-event data with competing risks using pseudovalues. Biometrics. 2011;67(1):1-7. doi:10.1111/j.1541-0420.2010.01416.x}
#' @examples
#' alpha = 0.5
#' 
#' #Simulate unstratified clustered competing risks data
#' d1 = simulate_CR_data(n=4,m=100,alpha=alpha,beta1=c(0.7,-0.7,-0.5)*1/alpha,beta2=c(0.5,-0.5,1),
#' betaC=c(0,0,0)*1/alpha,lambdaC=0.59,stratified=FALSE)
#' 
#' #Simulate stratified clustered competing risks data
#' d2 = simulate_CR_data(n=4,m=100,alpha=alpha,beta1=c(0.7,-0.7,-0.5)*1/alpha,beta2=c(0.5,-0.5,1),
#' betaC=c(0,0,0)*1/alpha,lambdaC=0.59,stratified=TRUE,rho=c(2,4),lambdaC0=c(0.9,2.5))

simulate_CR_data <- function(n=4, 
                         m=100,
                         alpha=1,
                         beta1=c(0.7,-0.7,-0.5)*1/alpha, 
                         beta2=c(0.5,-0.5,1),
                         betaC=c(0,0,0)*1/alpha, 
                         lambdaC=0.59,
                         stratified=TRUE,
                         rho=c(2,4),
                         lambdaC0=c(0.9,2.5)
                         ){
  
  ######################## positive stable function #########################
  posStable <- function(alpha){ #       alpha - correlation within cluster
    n <- 1/alpha
    if( n==1 )
    {
      return(1)
    }
    else
    {
      delta <- 1:(n-1)/n
      theta <- 1
      shape <- delta
      scale <- 1/theta
      Y <- rgamma(n-1,shape=shape, scale=scale)
      Y <- 1/(n^n*prod(Y))
      return(Y)
    }
  }
  ######################## positive stable function end #########################
  
  if(stratified==FALSE){
    #################### Unstratified ############################################
    p = 0.65
    tempdata <- NULL
    for(j in 1 : m){
      Z1 = rbinom(n,1,prob=0.6)
      Z2 = rnorm(n)
      Z3 = runif(n,0,1)
      Z<-cbind(Z1,Z2,Z3)
      Z.c<-cbind(Z1,Z2,Z3)
      
      omega <- posStable(alpha)  #omega: random effect
      
      #generate P1 and P2
      p2 = (1-p)^(omega * exp(Z%*%beta1))
      p1 = 1 - p2
      
      #Create failure cause indicator
      cause <- 1 + (runif(n) > p1)
      
      #Generate failure times 
      T.k1 = c(); T.k2 = c(); U <- runif(n);
      
      
      #W.k1 = (1-U*(1-(1-p)^(exp(Z%*%beta1)*omega)))^(1/exp(Z%*%beta1)/omega)
      #T.k1 = -log(1-(1-W.k1)/p)
      W.k1 = log(1-U*p1)/(omega * exp(Z%*%beta1))
      T.k1 = log(p/(exp(W.k1) - 1 +p ))
      
      T.k2 = -log(1- U)/(exp(Z%*%beta2))  #don't need omega
      T.k = (cause==1)*T.k1 + (cause==2)*T.k2
      
      #Generate censoring time based on Cox model  
      u<- runif(n)
      omegaC <- posStable(alpha)  
      C <- -log(u)/(exp(Z.c%*%betaC)*lambdaC*omegaC)
      
      #Generate censoring indicator
      delta=(T.k<=C)*1
      
      #observed time
      time= (T.k <= C)*T.k + (C < T.k)*C
      
      disease = cause*delta
      
      data = data.frame(time,cause=disease,Z,cluster=j)
      tempdata <- rbind(tempdata, data)
      
    }
    tempdata = tempdata[order(tempdata[,1]),] 
    
    #tempdata = cbind(ID=1:nrow(tempdata),tempdata)
    return(tempdata)
    
    #################### Unstratified end ############################################
  }else{
    #################### Stratified ############################################
    p = 0.65
    tempdata <- NULL
    
    for(j in 1 : m){
      #For each cluster and stratum we have same omega
      omega <- posStable(alpha)  #omega: random effect
      
      #Generate stratum for each cluster
      v <- sample(c(1,1,0,0),n)
      
      #Generate covariate
      Z1 = rbinom(n,1,prob=0.60)
      Z2 = rnorm(n)
      Z3 = runif(n,0,1)
      Z<-cbind(Z1,Z2,Z3)
      Z.c<-cbind(Z1,Z2,Z3)
      
      
      #generate P1 and P2
      p2<-c(); p1<- c();
      p2[v==0] = (1-p)^(omega * exp(Z[v==0,]%*%beta1))
      p1[v==0] = 1 - p2[v==0]
      
      p2[v==1] = (1-p)^(omega * exp(Z[v==1,]%*%beta1))
      p1[v==1] = 1 - p2[v==1]
      
      #Create failure cause indicator
      cause <- c()
      cause[v==0] <- 1 + (runif(n/2) > p1[v==0])
      cause[v==1] <- 1 + (runif(n/2) > p1[v==1])
      
      
      #Generate failure times
      T.k1 = c(); T.k2 = c(); U1 <- runif(n/2);U2 <- runif(n/2);
      W.k1 = c();W.k2 = c(); T.k = c()
      
      #Stratum 1(baseline hazard is different)
      W.k1[v==0] = log(1-U1*p1[v==0])/(omega * exp(Z[v==0,]%*%beta1))
      T.k1[v==0] = log(p/(exp(W.k1[v==0]) - 1 +p ))/rho[1]
      
      T.k2[v==0] = -log(1- U2)/(exp(Z[v==0,]%*%beta2))/rho[1]  #don't need omega
      T.k[v==0] = (cause[v==0]==1)*T.k1[v==0] + (cause[v==0]==2)*T.k2[v==0]
      
      #Stratum 2(baseline hazard is different)
      W.k1[v==1] = log(1-U1*p1[v==1])/(omega * exp(Z[v==1,]%*%beta1))
      T.k1[v==1] = log(p/(exp(W.k1[v==1]) - 1 +p ))/rho[2]
      
      T.k2[v==1] = -log(1- U2)/(exp(Z[v==1,]%*%beta2))/rho[2]  #don't need omega
      T.k[v==1] = (cause[v==1]==1)*T.k1[v==1] + (cause[v==1]==2)*T.k2[v==1]
      
      
      #Generate censoring time based on Cox model 
      u<- runif(n/2)
      omegaC <- posStable(alpha) 
      C <- c()
      C[v==0] <- -log(u)/(exp(Z.c[v==0,]%*%betaC)*lambdaC0[1]*omegaC)
      C[v==1] <- -log(u)/(exp(Z.c[v==1,]%*%betaC)*lambdaC0[2]*omegaC)
      
      #Generate censoring indicator
      delta = c()
      delta[v==0]=(T.k[v==0]<=C[v==0])*1
      delta[v==1]=(T.k[v==1]<=C[v==1])*1
      
      #observed time
      time = c()
      time[v==0]= (T.k[v==0] <= C[v==0])*T.k[v==0] + (C[v==0] < T.k[v==0])*C[v==0]
      time[v==1]= (T.k[v==1] <= C[v==1])*T.k[v==1] + (C[v==1] < T.k[v==1])*C[v==1]
      
      disease = cause*delta
      
      data = data.frame(time,cause=disease,Z,cluster=j,strata=v)
      tempdata <- rbind(tempdata, data)
      
    }
    tempdata = tempdata[order(tempdata[,1]),]
    
    #tempdata = cbind(ID=1:nrow(tempdata),tempdata)
    return(tempdata)
    #################### Stratified end ############################################
  }

}
