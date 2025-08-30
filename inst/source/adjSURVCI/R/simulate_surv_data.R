#' Simulate stratified and clustered survival data
#' @description The function \code{simulate_surv_data} simulates survival data based 
#' on a marginal proportional hazards model based on \cite{Logan et al. (2011)}.
#' 
#' @param N Total number of clusters. Default is 100.
#' @param alpha Parameter for a positive stable distribution. It controls correlation within a cluster. 
#' \code{1/alpha} must be an integer such that \code{alpha = 0.25, 0.5 and 1}. \code{alpha=1} generates independent data.
#' As \code{alpha} decreases, the correlation within a cluster increases. Default is 1.
#' @param beta1 This value multiplied by alpha is the true value of normally distributed covariate effect.
#' @param beta2 This value multiplied by alpha is the true value of uniformly distributed covariate effect.
#' @param beta3 This value multiplied the alpha is the true value of bernoulli distributed covariate effect.
#' @param rateC Rate of exponential distribution to generate censoring times. Default is 0.01.
#' @param stratified It is \code{TRUE} for stratified data. Two strata are considered.
#' @param lambda0 Constant baseline hazard for first stratum. If \code{stratified=FALSE}, then \code{lambda0}
#' is used as a constant basline hazard.
#' @param lambda1 Constant baseline hazard for second stratum.
#' @return Returns a data frame with the following variables:
#' \item{cluster}{Cluster variable}
#' \item{times}{Survival times}
#' \item{delta}{Event indicator with Event=1 and Censoring=0}
#' \item{Z1}{Standard normal covariate}
#' \item{Z2}{Cluster level covariate generated from uniform distribution}
#' \item{Z3}{Bernoulli distributed covariate with probability 0.6}
#' \item{s}{Stratification variable. This is provided only when \code{stratified=TRUE}}
#' @export
#' @references{Logan BR, Zhang MJ, Klein JP. Marginal models for clustered time-to-event data with competing risks using pseudovalues. Biometrics. 2011;67(1):1-7. doi:10.1111/j.1541-0420.2010.01416.x}
#' @examples
#' #Stratified data
#' alpha = 0.5
#' d = simulate_surv_data(N=200,alpha=alpha,beta1=0.5*1/alpha,beta2=-0.5*1/alpha,
#' beta3=1/alpha,rateC=1.3,lambda0=1,lambda1=2,stratified = TRUE)
#'
#' #Unstratified data
#' d = simulate_surv_data(N=200,alpha=alpha,beta1=0.5*1/alpha,beta2=-0.5*1/alpha,
#' beta3=1/alpha,rateC=0.9,lambda0=1,lambda1=2,stratified = FALSE)

simulate_surv_data <- function(N=100,
                              alpha=1,
                              beta1=1*1/alpha,
                              beta2=-1*1/alpha,
                              beta3=0.5*1/alpha,
                              rateC=0.01,
                              stratified=TRUE,
                              lambda0=1, 
                              lambda1=2
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
  n <- 4
  if(stratified==TRUE){
    tempdata <- NULL
    middle_start <- 0.25 * N
    middle_end <- 0.75 * N
    #First 25% clusters only belong to Stratum 0.
    for(i in 1 : (middle_start-1)){
      s <- sample(c(0,0),n/2)
      w = posStable(alpha)
      wc = posStable(alpha)
      
      #Stratum 0
      Z1 <- rep(NA,n/2); Z2 <- rep(NA,n/2); Z3 <- rep(NA,n/2); 
      Z1[s==0] <- rnorm(n/2);Z2[s==0] = rep(runif(1,0,1),n/2); Z3[s==0] <- rbinom(n/2,1,prob=0.6);
      event.time <- rep(NA,n/2)
      event.time[s==0] <- c(-log(runif(n/2))/(lambda0*w*exp(cbind(Z1[s==0],Z2[s==0],Z3[s==0])%*% c(beta1,beta2,beta3) ))  )
      cens.time <- rep(NA,n/2)
      cens.time[s==0] <- rexp(n/2,rate=rateC)
      
      time <- rep(NA,n/2)
      time[s==0] <- pmin(event.time[s==0],cens.time[s==0])
      delta <- rep(NA,n/2)
      delta[s==0] <- as.numeric(event.time[s==0] <= cens.time[s==0])
      
      # #Stratum 1
      # Z1[s==1] <- rnorm(n/2); Z2[s==1] <- rbinom(n/2,1,prob=0.6);
      # event.time[s==1] <- c(-log(runif(n/2))/(lambda1*w*exp(cbind(Z1[s==1],Z2[s==1])%*% c(beta1,beta2) )) )
      # cens.time[s==1] <- rexp(n/2,rate=rateC)
      # 
      # time[s==1] <- pmin(event.time[s==1],cens.time[s==1])
      # delta[s==1] <- as.numeric(event.time[s==1] <= cens.time[s==1])
      
      
      cluster.data <- data.frame(cluster=i,times=time,delta=delta,Z1=Z1,Z2=Z2,Z3=Z3,s)
      tempdata <- rbind(tempdata,cluster.data)
    }    
    tempdata1 = cbind(ID=1:nrow(tempdata),tempdata)
    
    #Stratum 0 and 1 both contain the middle 50% clusters
    tempdata <- NULL
    for(i in middle_start : middle_end){
      s <- sample(c(0,1,1,0),n)
      w = posStable(alpha)
      wc = posStable(alpha)
      
      #Stratum 0
      Z1 <- rep(NA,n); Z2 <- rep(NA,n); Z3 <- rep(NA,n)
      Z1[s==0] <- rnorm(n/2); Z2 = rep(runif(1,0,1),n); Z3[s==0] <- rbinom(n/2,1,prob=0.6);
      event.time <- rep(NA,n)
      event.time[s==0] <- c(-log(runif(n/2))/(lambda0*w*exp(cbind(Z1[s==0],Z2[s==0],Z3[s==0])%*% c(beta1,beta2,beta3) ))  )
      cens.time <- rep(NA,n)
      cens.time[s==0] <- rexp(n/2,rate=rateC)
      
      time <- rep(NA,n)
      time[s==0] <- pmin(event.time[s==0],cens.time[s==0])
      delta <- rep(NA,n)
      delta[s==0] <- as.numeric(event.time[s==0] <= cens.time[s==0])
      
      #Stratum 1
      Z1[s==1] <- rnorm(n/2);  Z3[s==1] <- rbinom(n/2,1,prob=0.6);
      event.time[s==1] <- c(-log(runif(n/2))/(lambda1*w*exp(cbind(Z1[s==1],Z2[s==1],Z3[s==1])%*% c(beta1,beta2,beta3) )) )
      cens.time[s==1] <- rexp(n/2,rate=rateC)
      
      time[s==1] <- pmin(event.time[s==1],cens.time[s==1])
      delta[s==1] <- as.numeric(event.time[s==1] <= cens.time[s==1])
      
      
      cluster.data <- data.frame(cluster=i,times=time,delta=delta,Z1=Z1,Z2=Z2,Z3=Z3,s)
      tempdata <- rbind(tempdata,cluster.data)
    }
    
    tempdata2 = cbind(ID=1:nrow(tempdata),tempdata)
    
    #Last 25% clusters belong to Stratum 1
    tempdata=NULL
    for(i in (middle_end+1) : N){
      s <- sample(c(1,1),n/2)
      w = posStable(alpha)
      wc = posStable(alpha)
      
      # #Stratum 0
      Z1 <- rep(NA,n/2); Z2 <- rep(NA,n/2); Z3 <- rep(NA,n/2); 
      # Z1[s==0] <- rnorm(n/2);Z2[s==0] = rep(runif(1,0,1),n/2); Z3[s==0] <- rbinom(n/2,1,prob=0.6);
      event.time <- rep(NA,n/2)
      # event.time[s==0] <- c(-log(runif(n/2))/(lambda0*w*exp(cbind(Z1[s==0],Z2[s==0],Z3[s==0])%*% c(beta1,beta2,beta3) ))  )
      cens.time <- rep(NA,n/2)
      # cens.time[s==0] <- rexp(n/2,rate=rateC)
      # 
      time <- rep(NA,n/2)
      # time[s==0] <- pmin(event.time[s==0],cens.time[s==0])
      delta <- rep(NA,n/2)
      # delta[s==0] <- as.numeric(event.time[s==0] <= cens.time[s==0])
      
      #Stratum 1
      Z1[s==1] <- rnorm(n/2); Z2[s==1] = rep(runif(1,0,1),n/2); Z3[s==1] <- rbinom(n/2,1,prob=0.6);
      event.time[s==1] <- c(-log(runif(n/2))/(lambda1*w*exp(cbind(Z1[s==1],Z2[s==1],Z3[s==1])%*% c(beta1,beta2,beta3) )) )
      cens.time[s==1] <- rexp(n/2,rate=rateC)
      
      time[s==1] <- pmin(event.time[s==1],cens.time[s==1])
      delta[s==1] <- as.numeric(event.time[s==1] <= cens.time[s==1])
      
      
      cluster.data <- data.frame(cluster=i,times=time,delta=delta,Z1=Z1,Z2=Z2,Z3=Z3,s)
      tempdata <- rbind(tempdata,cluster.data)
    }    
    tempdata3 = cbind(ID=1:nrow(tempdata),tempdata)
    
    #return(data.frame(tempdata))
    return(rbind(tempdata1,tempdata2,tempdata3))
  }else{
    tempdata <- NULL
    for(i in 1 : N){
      w = posStable(alpha)
      Z1 <- rnorm(n); Z2 <- runif(n); Z3 <- rbinom(n,1,prob=0.6);
      event.time <- c(-log(runif(n))/(lambda0*w*exp(cbind(Z1,Z2,Z3)%*% c(beta1,beta2,beta3) )))
      cens.time <- rexp(n,rate=rateC)
      
      time <- pmin(event.time,cens.time)
      delta <- as.numeric(event.time <= cens.time)
      cluster.data <- data.frame(cluster=i,times=time,delta=delta,Z1=Z1,Z2=Z2,Z3=Z3)
      tempdata <- rbind(tempdata,cluster.data)
    }
    return(data.frame(tempdata))
  }
  
}
