#' @title
#'
#'  Bayesian multivariate analysis of parametric AFT model with minimum deviance (DIC) among
#'  weibull, log normal and log logistic distribution.
#'
#' @description Provides better estimates (which has minimum deviance(DIC) ) for survival data among weibull,
#' log normal and log logistic distribution of parametric AFT model using MCMC for
#' multivariable (maximum 5 at a time) in high dimensional data.
#'
#' @details
#' This function deals covariates (in data) with missing values. Missing value in any column (covariate) is replaced by mean of that particular covariate.
#' AFT model is log-linear regression model for survival time \eqn{ T_{1}},\eqn{ T_{2}},..,\eqn{T_{n}}.
#' i.e., \deqn{log(T_i)= x_i'\beta +\sigma\epsilon_i ;~\epsilon_i \sim F_\epsilon (.)~which~is~iid }
#' i.e., \deqn{T_i \sim AFT(F_\epsilon ,\beta,\tau|x_i)}
#' Where \eqn{ F_\epsilon } is known cdf which is defined on real line.
#' Here, when baseline distribution is extreme value then T follows weibull distribution.
#' To make interpretation of regression coefficients simpler, using extreme value distribution with median 0.
#' So using weibull distribution that leads to AFT model when
#' \deqn{ T \sim Weib(\sqrt{\tau},log(2)\exp(-x'\beta \sqrt{\tau})) }
#'
#' When baseline distribution is normal then T follows log normal distribution.
#' \deqn{ T \sim LN(x'\beta,1/\tau) }
#' When baseline distribution is logistic then T follows log logistic distribution.
#' \deqn{ T \sim Log-Logis(x'\beta,\sqrt{\tau)} }
#'
#' @param m Starting column number of covariates of study from high dimensional entered data.
#' @param n Ending column number of covariates of study from high dimensional entered data.
#' @param STime name of survival time in data.
#' @param Event name of event in data. 0 is for censored and 1 for occurrence of event.
#' @param nc number of MCMC chain.
#' @param ni number of MCMC iteration to update the outcome.
#' @param data High dimensional gene expression data that contains event status, survival time and and set of covariates.
#' @return Data frame is containing posterior estimates mean, sd, credible intervals, n.eff and Rhat for beta's, sigma, tau and deviance of the model for the selected covariates. beta's of regression coefficient of the model. beta[1] is for intercept and others are for covariates (which is/are chosen order as columns in data). 'sigma' is the scale parameter of the distribution. DIC is the estimate of expected predictive error (so lower deviance denotes better estimation).
#' @import  R2jags
#'
#' @references Prabhash et al(2016) <doi:10.21307/stattrans-2016-046>
#'
#' @examples
#' ##
#' data(hdata)
#' aftbybmv(10,12,STime="os",Event="death",2,100,hdata)
#' ##
#' @export
#' @author Atanu Bhattacharjee, Gajendra Kumar Vishwakarma and Pragya Kumari
#' @seealso wbysmv, lgnbymv, lgstbymv
#'

 aftbybmv=function(m,n,STime,Event,nc,ni,data){
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
      d11[i,j] = ifelse(is.na(d11[i,j])=="TRUE", mean(d11[,j], na.rm=TRUE),
                        d11[i,j])
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
  nnr<-nrow(d12)

  mx<-max(d12$os) + 100
  surt<-ifelse(d12$death == 1, d12$os, NA)
  stcen<-ifelse(d12$death == 0, d12$os, mx)
  stcen1<-log(stcen)
  ls<-log(surt)
  d12$os<-surt
  cen<-as.numeric(is.na(surt))
  d12<-data.frame(d12,stcen,cen,ls,stcen1)

  if(len>5){
    cat("Outcome for first 5 covariates : ")
    vv<-subset(d11,select = c(1:5))
  } else {
    vv<-d11
  }
  vname<-colnames(vv)

  if(len==1){
    data1<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], N = nr)
    modelj1<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <- log(2)*exp(-mu[i]*sqrt(tau))
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
    jagsft1 <- jags(model.file=modelj1, data=data1, inits = inits1,
                   parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f1=data.frame(jagsft1$BUGSoutput$summary)

    data2<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], N = nr)
    modelj2<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        os[i] ~ dlnorm(mu[i], tau)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <-  beta[1] + beta[2]*sV1[i]
      }
      for(i in 1:2){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }
    inits2 <- function() {
      list(beta=c(0,0), tau=1)
    }
    jagsft2 <- jags(model.file=modelj2, data=data2, inits = inits2,
                   parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f2=data.frame(jagsft2$BUGSoutput$summary)

    data3<-list(os=d12$ls, stcen=d12$stcen1, cen=d12$cen, v1=vv[,1], N = nr)
    modelj3<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        os[i] ~ dlogis(mu[i], taustar)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <- beta[1] + beta[2]*sV1[i]
      }
      taustar <- sqrt(tau)
      for(i in 1:2){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
      junk1 <- exp(os[1])
    }
    inits3 <- function() {
      list(beta=c(0,0), tau=1)
    }
    jagsft3 <- jags(model.file=modelj3, data=data3, inits = inits3,
                   parameters.to.save = c('beta','tau','sigma'), n.chains=nc,
                   n.iter = ni)
    f3=data.frame(jagsft3$BUGSoutput$summary)

  } else if(len==2){
    data1<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], N = nr)
    modelj1<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i]+beta[3]*sV2[i]
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
    inits1 <- function() {
      list(beta=c(0,0,0), tau=1)
    }
    jagsft1 <- jags(model.file=modelj1, data=data1, inits = inits1,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f1=data.frame(jagsft1$BUGSoutput$summary)

    data2<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1],v2=vv[,2], N = nr)
    modelj2<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        os[i] ~ dlnorm(mu[i], tau)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <-  beta[1] + beta[2]*sV1[i] + beta[3]*sV2[i]
      }
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
    jagsft2 <- jags(model.file=modelj2, data=data2, inits = inits2,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f2=data.frame(jagsft2$BUGSoutput$summary)

    data3<-list(os=d12$ls, stcen=d12$stcen1, cen=d12$cen, v1=vv[,1],v2=vv[,2], N = nr)
    modelj3<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        os[i] ~ dlogis(mu[i], taustar)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <- beta[1] + beta[2]*sV1[i] +beta[3]*sV2[i]
      }
      taustar <- sqrt(tau)
      for(i in 1:3){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
      junk1 <- exp(os[1])
    }
    inits3 <- function() {
      list(beta=c(0,0,0), tau=1)
    }
    jagsft3 <- jags(model.file=modelj3, data=data3, inits = inits3,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc,
                    n.iter = ni)
    f3=data.frame(jagsft3$BUGSoutput$summary)

  } else if(len==3){
    data1<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2],v3=vv[,3], N = nr)
    modelj1<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i]+beta[3]*sV2[i] + beta[4]*sV3[i]
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
    inits1 <- function() {
      list(beta=c(0,0,0,0), tau=1)
    }
    jagsft1 <- jags(model.file=modelj1, data=data1, inits = inits1,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f1=data.frame(jagsft1$BUGSoutput$summary)

    data2<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1],v2=vv[,2], v3=vv[,3], N = nr)
    modelj2<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        os[i] ~ dlnorm(mu[i], tau)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <-  beta[1] + beta[2]*sV1[i] + beta[3]*sV2[i] + beta[4]*sV3[i]
      }
      for(i in 1:4){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }

    inits2 <- function() {
      list(beta=c(0,0,0,0), tau=1)
    }
    jagsft2 <- jags(model.file=modelj2, data=data2, inits = inits2,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f2=data.frame(jagsft2$BUGSoutput$summary)

    data3<-list(os=d12$ls, stcen=d12$stcen1, cen=d12$cen, v1=vv[,1],v2=vv[,2], v3=vv[,3], N = nr)
    modelj3<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        os[i] ~ dlogis(mu[i], taustar)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <- beta[1] + beta[2]*sV1[i] +beta[3]*sV2[i] + beta[4]*sV3[i]
      }
      taustar <- sqrt(tau)
      for(i in 1:4){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
      junk1 <- exp(os[1])
    }
    inits3 <- function() {
      list(beta=c(0,0,0,0), tau=1)
    }
    jagsft3 <- jags(model.file=modelj3, data=data3, inits = inits3,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc,
                    n.iter = ni)
    f3=data.frame(jagsft3$BUGSoutput$summary)
  } else if(len==4){
    data1<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2],v3=vv[,3],v4=vv[,4], N = nr)
    modelj1<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i]+beta[3]*sV2[i] + beta[4]*sV3[i] + beta[5]*sV4[i]
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
    inits1 <- function() {
      list(beta=c(0,0,0,0,0), tau=1)
    }
    jagsft1 <- jags(model.file=modelj1, data=data1, inits = inits1,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f1=data.frame(jagsft1$BUGSoutput$summary)

    data2<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1],v2=vv[,2], v3=vv[,3],v4=vv[,4], N = nr)
    modelj2<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        os[i] ~ dlnorm(mu[i], tau)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <-  beta[1] + beta[2]*sV1[i] + beta[3]*sV2[i] + beta[4]*sV3[i] + beta[5]*sV4[i]
      }
      for(i in 1:5){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }
    inits2 <- function() {
      list(beta=c(0,0,0,0,0), tau=1)
    }
    jagsft2 <- jags(model.file=modelj2, data=data2, inits = inits2,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f2=data.frame(jagsft2$BUGSoutput$summary)

    data3<-list(os=d12$ls, stcen=d12$stcen1, cen=d12$cen, v1=vv[,1],v2=vv[,2], v3=vv[,3],v4=vv[,4], N = nr)
    modelj3<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        os[i] ~ dlogis(mu[i], taustar)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <- beta[1] + beta[2]*sV1[i] +beta[3]*sV2[i] + beta[4]*sV3[i] + beta[5]*sV4[i]
      }
      taustar <- sqrt(tau)
      for(i in 1:5){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
      junk1 <- exp(os[1])
    }
    inits3 <- function() {
      list(beta=c(0,0,0,0,0), tau=1)
    }
    jagsft3 <- jags(model.file=modelj3, data=data3, inits = inits3,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc,
                    n.iter = ni)
    f3=data.frame(jagsft3$BUGSoutput$summary)
  } else{
    data1<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2],v3=vv[,3],v4=vv[,4],v5=vv[,5], N = nr)
    modelj1<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        sV5[i] <- (v5[i]-mean(v5[]))/sd(v5[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
        mu[i] <-  beta[1] + beta[2]*sV1[i]+beta[3]*sV2[i] + beta[4]*sV3[i] + beta[5]*sV4[i]+ beta[6]*sV5[i]
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
    inits1 <- function() {
      list(beta=c(0,0,0,0,0,0), tau=1)
    }
    jagsft1 <- jags(model.file=modelj1, data=data1, inits = inits1,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f1=data.frame(jagsft1$BUGSoutput$summary)

    data2<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1],v2=vv[,2], v3=vv[,3],v4=vv[,4], v5=vv[,5],N = nr)
    modelj2<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        sV5[i] <- (v5[i]-mean(v5[]))/sd(v5[])
        os[i] ~ dlnorm(mu[i], tau)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <-  beta[1] + beta[2]*sV1[i] + beta[3]*sV2[i] + beta[4]*sV3[i] + beta[5]*sV4[i]+ beta[6]*sV5[i]
      }
      for(i in 1:6){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
    }

    inits2 <- function() {
      list(beta=c(0,0,0,0,0,0), tau=1)
    }
    jagsft2 <- jags(model.file=modelj2, data=data2, inits = inits2,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc, n.iter = ni)
    f2=data.frame(jagsft2$BUGSoutput$summary)

    data3<-list(os=d12$ls, stcen=d12$stcen1, cen=d12$cen, v1=vv[,1],v2=vv[,2], v3=vv[,3],v4=vv[,4], v5=vv[,5],N = nr)
    modelj3<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        sV5[i] <- (v5[i]-mean(v5[]))/sd(v5[])
        os[i] ~ dlogis(mu[i], taustar)
        cen[i] ~ dinterval(os[i],stcen[i])
        mu[i] <- beta[1] + beta[2]*sV1[i] +beta[3]*sV2[i] + beta[4]*sV3[i] + beta[5]*sV4[i] + beta[6]*sV5[i]
      }
      taustar <- sqrt(tau)
      for(i in 1:6){
        beta[i] ~ dnorm(0,0.000001)
        rm[i] <- exp(beta[i])
        prob[i] <- step(beta[i])
      }
      tau ~ dgamma(0.001,0.001)
      sigma <- sqrt(1/tau)
      junk1 <- exp(os[1])
    }
    inits3 <- function() {
      list(beta=c(0,0,0,0,0,0), tau=1)
    }
    jagsft3 <- jags(model.file=modelj3, data=data3, inits = inits3,
                    parameters.to.save = c('beta','tau','sigma'), n.chains=nc,
                    n.iter = ni)
    f3=data.frame(jagsft3$BUGSoutput$summary)
  }

  cat("Estimates for variables: ", vname,"\n")
  f <- min(f1[(len+2),1],f2[(len+2),1],f3[(len+2),1])
  if(f1[(len+2),1]==f){
    cat("First initialization belongs to Weibull distribution.","\n")
    cat("Second initialization belongs to log normal distribution.","\n")
    cat("Third initialization belongs to log logistic distribution.","\n")
    cat("Estimates for weibull distribution, which is found suitable with minimal DIC value: ","\n")
    return(f1)
  }
  if(f2[(len+2),1]==f){
    cat("First initialization belongs to Weibull distribution.","\n")
    cat("Second initialization belongs to log normal distribution.","\n")
    cat("Third initialization belongs to log logistic distribution.","\n")
    cat("Estimates for log normal distribution, which is found suitable with minimal DIC value:","\n")
    return(f2)
  }
  if(f3[(len+2),1]==f){
    cat("First initialization belongs to Weibull distribution.","\n")
    cat("Second initialization belongs to log normal distribution.","\n")
    cat("Third initialization belongs to log logistic distribution.","\n")
    cat("Estimates for log logistic, which is found suitable with minimal DIC value:","\n")
    return(f3)
  }
}
utils::globalVariables(c("N","v1","sd","v2","v3","v4","v5","tau","step","phi"))

