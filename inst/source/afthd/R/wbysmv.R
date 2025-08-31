#' @title
#'
#' Posterior multivariate estimates of AFT model with weibull distribution using MCMC.
#'
#' @description Provides estimate of AFT model with weibull distribution using MCMC for multivariable (maximum 5 covariates of column at a time) in high dimensional gene expression data. It also deals covariates with missing values.
#'
#' @details
#' This function deals covariates (in data) with missing values. Missing value in any column (covariate) is replaced by mean of that particular covariate.
#' AFT model is log-linear regression model for survival time \eqn{ T_{1}},\eqn{ T_{2}},..,\eqn{T_{n}}.
#' i.e., \deqn{log(T_i)= x_i'\beta +\sigma\epsilon_i ;~\epsilon_i \sim F_\epsilon (.)~which~is~iid }
#' Where \eqn{ F_\epsilon } is known cdf which is defined on real line.
#' Here, when baseline distribution is extreme value then T follows weibull distribution.
#' To make interpretation of regression coefficients simpler, using extreme value distribution with median 0.
#' So using weibull distribution that leads to AFT model when
#' \deqn{ T \sim Weib(\sqrt{\tau},log(2)\exp(-x'\beta \sqrt{\tau})) }
#'
#'
#' @param m Starting column number of covariates of study from high dimensional entered data.
#' @param n Ending column number of covariates of study from high dimensional entered data.
#' @param STime name of survival time in data.
#' @param Event name of event status in data. 0 is for censored and 1 for occurrence of event.
#' @param nc number of markov chain.
#' @param ni number of iteration for MCMC.
#' @param data High dimensional gene expression data that contains event status, survival time and and set of covariates.
#' @return Data frame is containing mean, sd, n.eff, Rhat and credible intervals for beta's, sigma, alpha, tau and deviance of the model for the chosen covariates. beta[1] is for intercept and others are for covariates (which is/are chosen as columns in data). sigma is the scale parameter of the distribution. alpha is shape parameter of the distribution.
#' @import R2jags
#'
#' @references Prabhash et al(2016) <doi:10.21307/stattrans-2016-046>
#'
#' @examples
#' ##
#' data(hdata)
#' wbysmv(9,13,STime="os",Event="death",2,10,hdata)
#' ##
#' @export
#' @author Atanu Bhattacharjee, Gajendra Kumar Vishwakarma and Pragya Kumari
#' @seealso pvaft, wbysuni, rglwbysm, wbyscrkm, wbyAgmv
#'

wbysmv=function(m,n,STime,Event,nc,ni,data){

  nr<-nrow(data)

  if(STime!="os"){
    names(data)[names(data) == STime] <- "os"
  }
  if(Event!="death"){
    names(data)[names(data) == Event] <- "death"
  }

  d11 <- subset(data, select = c(m:n))
  le<-length(d11)
  for(i in 1:nr) {
    for(j in 1:le) {
      d11[i,j] = ifelse(is.na(d11[i,j])=="TRUE", mean(d11[,j], na.rm=TRUE), d11[i,j])
    }
  }

  pnt<-NULL
  for(j in 1:le)
  {
    if(sum(d11[,j])==0) {
      pnt<-c(pnt,j)
    }
  }

  if(is.null(pnt)==F){
    d11 <- d11[,-pnt]
  }
  len<-length(d11)
  d12<-data.frame(data[,c('death','os')],d11)

  mx<-max(d12$os) + 10
  surt<-ifelse(d12$death == 1, d12$os, NA)
  stcen<-ifelse(d12$death == 0, d12$os, mx)
  d12$os<-surt
  cen<-as.numeric(is.na(surt))
  d12<-data.frame(d12,stcen,cen)

  if(len>5){
    cat("Estimates for first 5 covariates:")
    vv<-subset(d11,select = c(1:5))
  } else {
    vv<-d11
  }
  vname<-colnames(vv)

  if(len==1){
    data1<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], N = nr )
    modelj1<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i]
      }
      alpha <- sqrt(tau)

      for(i in 1:2){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }

    inits1 <- function() {
      list(beta=c(0,0), tau=1)
    }
    jagsft <- jags(model.file=modelj1, data=data1, inits = inits1,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  } else if(len==2){
    data2<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], N = nr )
    modelj2<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i] + beta[3]*sV2[i]
      }
      alpha <- sqrt(tau)

      for(i in 1:3){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }

    inits2 <- function() {
      list(beta=c(0,0,0), tau=1)
    }
    jagsft <- jags(model.file=modelj2, data=data2, inits = inits2,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  } else if(len==3){
    data3<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], v3=vv[,3], N = nr )
    modelj3<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i] + beta[3]*sV2[i] + beta[4]*sV3[i]
      }
      alpha <- sqrt(tau)

      for(i in 1:4){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }
    inits3 <- function() {
      list(beta=c(0,0,0,0), tau=1)
    }
    jagsft <- jags(model.file=modelj3, data=data3, inits = inits3,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  } else if(len==4){
    data4<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], v3=vv[,3], v4=vv[,4], N = nr )
    modelj4<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <- log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i] + beta[3]*sV2[i] + beta[4]*sV3[i]
        + beta[5]*sV4[i]
      }
      alpha <- sqrt(tau)

      for(i in 1:5){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }

    inits4 <- function() {
      list(beta=c(0,0,0,0,0), tau=1)
    }
    jagsft <- jags(model.file=modelj4, data=data4, inits = inits4,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  } else {
    data5<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], v3=vv[,3], v4=vv[,4], v5=vv[,5], N = nr )
    modelj5<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        sV5[i] <- (v5[i]-mean(v5[]))/sd(v5[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <- log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i] + beta[3]*sV2[i] + beta[4]*sV3[i]
        + beta[5]*sV4[i] + beta[6]*sV5[i]
      }
      alpha <- sqrt(tau)
      for(i in 1:6){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }

    inits5 <- function() {
      list(beta=c(0,0,0,0,0,0), tau=1)
    }
    jagsft <- jags(model.file=modelj5, data=data5, inits = inits5,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  }
  cat("Estimates for covariates :- ", vname,"\n")
  f=data.frame(jagsft$BUGSoutput$summary)
  return(f)
}


utils::globalVariables(c("N","v1","sd","v2","v3","v4","v5","tau","step"))


