#'
#' @title
#' Bayesian multivariate estimates for competing risk gene expression data using AFT model.
#'
#' @description Provides multivariate(maximum 5 covariates of column at a time) posterior estimates of AFT model using MCMC for competing risk high dimensional gene expression data. It also deal with missing values.
#'
#' @details
#' Here AFT model has been used with weibull distribution.
#' This function deals covariates (in data) with missing values. Missing value in any column(covariate) is replaced by mean of that particular covariate.
#'
#' @param m Starting column number of covariates of study from high dimensional entered data.
#' @param n Ending column number of covariates of study from high dimensional entered data.
#' @param STime name of survival time in data.
#' @param Event name of event status in data. 0 is for censored and 1 for occurrence of event of interest and 2 for occurrence of event due to other causes.
#' @param nc number of markov chain.
#' @param ni number of iteration for MCMC.
#' @param data High dimensional gene expression data that contains event status with competing risk, survival time and and set of covariates.
#' @return Data frame is containing posterior estimates mean, sd, credible intervals, n.eff and Rhat for beta's, sigma, alpha, tau and deviance (DIC information) of the model for the chosen covariates as columns between m and n. beta's of regression coefficient of the model. beta[1] is for intercept and others are for covariates (which is/are chosen order as columns in data). alpha is shape parameter of the distribution. 'sigma' is the scale parameter of the distribution.
#' @import R2jags
#'
#' @references Prabhash et al(2016) <doi:10.21307/stattrans-2016-046>
#'
#' @examples
#' ##
#' data(hdata)
#' wbyscrkm(9,11,STime="os",Event="death2",2,10,hdata)
#' ##
#' @export
#' @author Atanu Bhattacharjee, Gajendra Kumar Vishwakarma and Pragya Kumari
#' @seealso wbysmv, wbyscrku
#'


wbyscrkm=function(m,n,STime,Event,nc,ni,data){
  nr<-nrow(data)

  if(STime!="os"){
    names(data)[names(data) == STime] <- "os"
  }
  if(Event!="death2"){
    names(data)[names(data) == Event] <- "death2"
  }

  u<-unique(data$death2)
  leu<-length(u)

  if(leu==3){

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

  d12<-data.frame(data[,c('death2','os')],d11)
  len<-length(d11)
  if(len>5){
    cat("Estimates for first 5 covariates : ")
    vv<-subset(d11,select = c(1:5))
  } else {
    vv<-d11
  }

  vn<-colnames(vv)

    dt01 <- d12[ which(d12$death2==0 | d12$death2==1),]
    dt02 <- d12[ which(d12$death2==0 | d12$death2==2),]
    vv1<-dt01[vn]
    vv2<-dt02[vn]
    nr1<-nrow(dt01)
    nr2<-nrow(dt02)

  if(sum(dt01$death2)>0){
    dstatus1<-1
  } else {
    dstatus1<- "char"
  }

  if(sum(dt02$death2)>0){
    dstatus2<-2
  } else {
    dstatus2<- "char"
  }

  if(is.numeric(dstatus1)==T){

    mx<-max(dt01$os) + 10
    surt1<-ifelse(dt01$death2 == 1, dt01$os, NA)
    stcen1<-ifelse(dt01$death2 == 0, dt01$os, mx)
    dt01$os<-surt1
    cen1<-as.numeric(is.na(surt1))
    dt01<-data.frame(dt01,stcen1,cen1)

    if(len==1){
      data1<-list(os=dt01$os, stcen1=dt01$stcen1, cen1=dt01$cen1, v1=vv1[,1], N = nr1 )
      modelj1<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          os[i] ~ dweib(alpha,lambda[i])
          cen1[i] ~ dinterval(os[i],stcen1[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta1[1] + beta1[2]*sV1[i]
        }
        alpha <- sqrt(tau)

        for(i in 1:2){
          beta1[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta1[i])
          prob[i] <- step(beta1[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }

      inits1 <- function() {
        list(beta1=c(0,0), tau=1)
      }
      jagsft1 <- jags(model.file=modelj1, data=data1, inits = inits1,
                      parameters.to.save = c('beta1','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    } else if(len==2){
      data2<-list(os=dt01$os, stcen1=dt01$stcen1, cen1=dt01$cen1, v1=vv1[,1], v2=vv1[,2], N = nr1 )
      modelj2<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
          os[i] ~ dweib(alpha,lambda[i])
          cen1[i] ~ dinterval(os[i],stcen1[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta1[1] + beta1[2]*sV1[i] + beta1[3]*sV2[i]
        }
        alpha <- sqrt(tau)

        for(i in 1:3){
          beta1[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta1[i])
          prob[i] <- step(beta1[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }

      inits2 <- function() {
        list(beta1=c(0,0,0), tau=1)
      }
      jagsft1 <- jags(model.file=modelj2, data=data2, inits = inits2,
                      parameters.to.save = c('beta1','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    } else if(len==3){
      data3<-list(os=dt01$os, stcen1=dt01$stcen1, cen1=dt01$cen1, v1=vv1[,1], v2=vv1[,2], v3=vv1[,3], N = nr1 )
      modelj3<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
          sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
          os[i] ~ dweib(alpha,lambda[i])
          cen1[i] ~ dinterval(os[i],stcen1[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta1[1] + beta1[2]*sV1[i] + beta1[3]*sV2[i] + beta1[4]*sV3[i]
        }
        alpha <- sqrt(tau)

        for(i in 1:4){
          beta1[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta1[i])
          prob[i] <- step(beta1[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }
      inits3 <- function() {
        list(beta1=c(0,0,0,0), tau=1)
      }
      jagsft1 <- jags(model.file=modelj3, data=data3, inits = inits3,
                      parameters.to.save = c('beta1','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    } else if(len==4){
      data4<-list(os=dt01$os, stcen1=dt01$stcen1, cen1=dt01$cen1, v1=vv1[,1], v2=vv1[,2], v3=vv1[,3], v4=vv1[,4], N = nr1 )
      modelj4<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
          sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
          sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
          os[i] ~ dweib(alpha,lambda[i])
          cen1[i] ~ dinterval(os[i],stcen1[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta1[1] + beta1[2]*sV1[i] + beta1[3]*sV2[i] + beta1[4]*sV3[i] + beta1[5]*sV4[i]
        }
        alpha <- sqrt(tau)

        for(i in 1:5){
          beta1[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta1[i])
          prob[i] <- step(beta1[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }

      inits4 <- function() {
        list(beta1=c(0,0,0,0,0), tau=1)
      }
      jagsft1 <- jags(model.file=modelj4, data=data4, inits = inits4,
                      parameters.to.save = c('beta1','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    } else {
      data5 <- list(os=dt01$os, stcen1=dt01$stcen1, cen1=dt01$cen1, v1=vv1[,1], v2=vv1[,2], v3=vv1[,3], v4=vv1[,4], v5=vv1[,5], N = nr1 )
      modelj5<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
          sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
          sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
          sV5[i] <- (v5[i]-mean(v5[]))/sd(v5[])
          os[i] ~ dweib(alpha,lambda[i])
          cen1[i] ~ dinterval(os[i],stcen1[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta1[1] + beta1[2]*sV1[i] + beta1[3]*sV2[i] + beta1[4]*sV3[i]
          + beta1[5]*sV4[i] + beta1[6]*sV5[i]
        }
        alpha <- sqrt(tau)
        for(i in 1:6){
          beta1[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta1[i])
          prob[i] <- step(beta1[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }

      inits5 <- function() {
        list(beta1=c(0,0,0,0,0,0), tau=1)
      }
      jagsft1 <- jags(model.file=modelj5, data=data5, inits = inits5,
                      parameters.to.save = c('beta1','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    }

    cat("Estimation for event status (0,1) for variables:- ", vn,"\n")
    f1=data.frame(jagsft1$BUGSoutput$summary)
    print(f1)

  }

  if(is.numeric(dstatus2)==T){

    mx2<-max(dt02$os) + 10
    surt2<-ifelse(dt02$death2 == 2, dt02$os, NA)
    stcen2<-ifelse(dt02$death2 == 0, dt02$os, mx2)
    dt02$os<-surt2
    cen2<-as.numeric(is.na(surt2))
    dt02<-data.frame(dt02,stcen2,cen2)

    if(len==1){
      data21<-list(os=dt02$os, stcen2=dt02$stcen2, cen2=dt02$cen2, v1=vv2[,1], N = nr2 )
      modelj21<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          os[i] ~ dweib(alpha,lambda[i])
          cen2[i] ~ dinterval(os[i],stcen2[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta2[1] + beta2[2]*sV1[i]
        }
        alpha <- sqrt(tau)

        for(i in 1:2){
          beta2[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta2[i])
          prob[i] <- step(beta2[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }

      inits21 <- function() {
        list(beta2=c(0,0), tau=1)
      }
      jagsft2 <- jags(model.file=modelj21, data=data21, inits = inits21,
                      parameters.to.save = c('beta2','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    } else if(len==2){
      data22<-list(os=dt02$os, stcen2=dt02$stcen2, cen2=dt02$cen2, v1=vv2[,1], v2=vv2[,2], N = nr2 )
      modelj22<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
          os[i] ~ dweib(alpha,lambda[i])
          cen2[i] ~ dinterval(os[i],stcen2[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta2[1] + beta2[2]*sV1[i] + beta2[3]*sV2[i]
        }
        alpha <- sqrt(tau)

        for(i in 1:3){
          beta2[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta2[i])
          prob[i] <- step(beta2[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }

      inits22 <- function() {
        list(beta2=c(0,0,0), tau=1)
      }
      jagsft2 <- jags(model.file=modelj22, data=data22, inits = inits22,
                      parameters.to.save = c('beta2','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    } else if(len==3){
      data23<-list(os=dt02$os, stcen2=dt02$stcen2, cen2=dt02$cen2, v1=vv2[,1], v2=vv2[,2], v3=vv2[,3], N = nr2 )
      modelj23<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
          sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
          os[i] ~ dweib(alpha,lambda[i])
          cen2[i] ~ dinterval(os[i],stcen2[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta2[1] + beta2[2]*sV1[i] + beta2[3]*sV2[i] + beta2[4]*sV3[i]
        }
        alpha <- sqrt(tau)

        for(i in 1:4){
          beta2[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta2[i])
          prob[i] <- step(beta2[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }
      inits23 <- function() {
        list(beta2=c(0,0,0,0), tau=1)
      }
      jagsft2 <- jags(model.file=modelj23, data=data23, inits = inits23,
                      parameters.to.save = c('beta2','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    } else if(len==4){
      data24<-list(os=dt02$os, stcen2=dt02$stcen2, cen2=dt02$cen2, v1=vv2[,1], v2=vv2[,2], v3=vv2[,3], v4=vv2[,4], N = nr2 )
      modelj24<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
          sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
          sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
          os[i] ~ dweib(alpha,lambda[i])
          cen2[i] ~ dinterval(os[i],stcen2[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta2[1] + beta2[2]*sV1[i] + beta2[3]*sV2[i] + beta2[4]*sV3[i]
          + beta2[5]*sV4[i]
          med[i] <- exp(mu[i])
        }
        alpha <- sqrt(tau)

        for(i in 1:5){
          beta2[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta2[i])
          prob[i] <- step(beta2[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }

      inits24 <- function() {
        list(beta2=c(0,0,0,0,0), tau=1)
      }
      jagsft2 <- jags(model.file=modelj24, data=data24, inits = inits24,
                      parameters.to.save = c('beta2','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    } else {
      data25 <- list(os=dt02$os, stcen2=dt02$stcen2, cen2=dt02$cen2, v1=vv2[,1], v2=vv2[,2], v3=vv2[,3], v4=vv2[,4], v5=vv2[,5], N = nr2 )
      modelj25<-function(){
        for (i in 1:N) {
          sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
          sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
          sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
          sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
          sV5[i] <- (v5[i]-mean(v5[]))/sd(v5[])
          os[i] ~ dweib(alpha,lambda[i])
          cen2[i] ~ dinterval(os[i],stcen2[i])
          lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
          mu[i] <-  beta2[1] + beta2[2]*sV1[i] + beta2[3]*sV2[i] + beta2[4]*sV3[i]
          + beta2[5]*sV4[i] + beta2[6]*sV5[i]
        }
        alpha <- sqrt(tau)
        for(i in 1:6){
          beta2[i] ~ dnorm(0,0.000001)
          rm[i] <- exp(beta2[i])
          prob[i] <- step(beta2[i])
        }
        tau ~ dgamma(0.001,0.001)
        sigma <- sqrt(1/tau)
      }

      inits25 <- function() {
        list(beta2=c(0,0,0,0,0,0), tau=1)
      }
      jagsft2 <- jags(model.file=modelj25, data=data25, inits = inits25,
                      parameters.to.save = c('beta2','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
    }

    cat("Estimation for event status (0,2) for variables: ", vn,"\n")
    f2=data.frame(jagsft2$BUGSoutput$summary)
    print(f2)

  }
  } else{
   stop("This is not the case of compating risk with event status (0,1,2)")
 }

}

utils::globalVariables(c("N","v1","sd","v2","v3","v4","v5","tau","step","beta1","beta2"))

