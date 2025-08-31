#' @title
#'
#' Bayesian multivariate analysis of AFT model for selected covariates using regularization method.
#'
#' @description Provides posterior Estimates of selected variable in AFT model for multivariable(maximum 5 at a time) in high dimensional gene expression data with MCMC.Incorporated variable selection has been done with regularization technique. It also deals covariates with missing values.
#'
#' @details
#'
#' Here weibull distribution has been used for AFT model with MCMC.
#' This function deals covariates (in data) with missing values. Missing value in any column (covariate) is replaced by mean of that particular covariate.
#'
#' @param m Starting column number of covariates of study from high dimensional entered data.
#' @param n Ending column number of covariates of study from high dimensional entered data.
#' @param STime name of survival time in data
#' @param Event name of event status in data. 0 is for censored and 1 for occurrence of event.
#' @param nc number of markov chain.
#' @param ni number of iteration for MCMC.
#' @param alpha It is chosen value between 0 and 1 to know the regularization method. alpha=1 for Lasso,
#' alpha=0 for Ridge and alpha between 0 and 1 for elastic net regularization.
#' @param data High dimensional gene expression data that contains event status, survival time and and set of covariates.
#' @return Data frame is containing posterior estimates mean, sd, credible intervals, n.eff and Rhat for beta's, sigma, alpha, tau and deviance (DIC information) of the model for the selected covariates using regularization technique. beta's of regression coefficient of the model. beta[1] is for intercept and others are for covariates (which is/are chosen order as columns in data). alpha is shape parameter of the distribution. 'sigma' is the scale parameter of the distribution.
#' @import R2jags
#' @import survival
#' @import glmnet
#'
#' @references Prabhash et al(2016)<doi:10.21307/stattrans-2016-046>
#'
#' @examples
#' ##
#' data(hdata)
#' set.seed(1000)
#' rglwbysm(9,45,STime="os",Event="death",2,10,1,hdata)
#' ##
#'
#' @export
#' @author Atanu Bhattacharjee, Gajendra Kumar Vishwakarma and Pragya Kumari
#' @seealso wbysuni,wbysmv, rglwbysu, aftbybmv
#'

rglwbysm=function(m,n,STime,Event,nc,ni,alpha,data){
  a<-alpha
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
  os<-data$os
  death<-data$death
  x<-as.matrix(d11)
  #set.seed(5000)
  cv.lassoVar<-cv.glmnet(x,Surv(os,death),alpha = a,family = "cox", maxit=1000)

  var_minlmd = coef(cv.lassoVar, cv.lassoVar$lambda.min)
  p <- as.matrix(var_minlmd)
  sg<-subset(p,p[,1]!=0)

  if(nrow(sg)==0){
    stop("Increase number of variables, chosen are not sufficient for variable selection using regularization")
  } else{
    sgnf_var <- rownames(sg)
  }

  ln<-length(sgnf_var)
  pnt1<-NULL
  for(i in 1:ln){
    for(j in 1:len)
    {
      if(colnames(d11[j])==sgnf_var[i]) {
        pnt1<-c(pnt1,j)
      }
    }
  }
  fdt<-subset(d11,select = pnt1)
  d12<-data.frame(data[,c('death','os')],fdt)

  mx<-max(d12$os) + 10
  surt<-ifelse(d12$death == 1, d12$os, NA)
  stcen<-ifelse(d12$death == 0, d12$os, mx)
  d12$os<-surt
  cen<-as.numeric(is.na(surt))
  d12<-data.frame(d12,stcen,cen)

  if(ln>5){
    cat("Outcome for selected first 5 covariates : ")
    vv<-subset(fdt,select = c(1:5))
  } else {
    vv<-fdt
  }
  vn<-colnames(vv)
  if(ln==1){
    data1<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], N = nr)
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
    jagsftt <- jags(model.file=modelj1, data=data1, inits = inits1,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  } else if(ln==2){
    data2<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], N = nr)
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
    jagsftt <- jags(model.file=modelj2, data=data2, inits = inits2,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  } else if(ln==3){
    data3<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], v3=vv[,3], N = nr)
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
    jagsftt <- jags(model.file=modelj3, data=data3, inits = inits3,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  } else if(ln==4){
    data4<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], v3=vv[,3], v4=vv[,4], N = nr)
    modelj4<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
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
    jagsftt <- jags(model.file=modelj4, data=data4, inits = inits4,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  } else {
    data5<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,1], v2=vv[,2], v3=vv[,3], v4=vv[,4], v5=vv[,5], N = nr)
    modelj5<-function(){
      for (i in 1:N) {
        sV1[i] <- (v1[i]-mean(v1[]))/sd(v1[])
        sV2[i] <- (v2[i]-mean(v2[]))/sd(v2[])
        sV3[i] <- (v3[i]-mean(v3[]))/sd(v3[])
        sV4[i] <- (v4[i]-mean(v4[]))/sd(v4[])
        sV5[i] <- (v5[i]-mean(v5[]))/sd(v5[])
        os[i] ~ dweib(alpha,lambda[i])
        cen[i] ~ dinterval(os[i],stcen[i])
        lambda[i] <-  log(2)*exp(-mu[i]*sqrt(tau))
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
    jagsftt <- jags(model.file=modelj5, data=data5, inits = inits5,
                   parameters.to.save = c('beta','tau','alpha','sigma'), n.chains=nc, n.iter = ni)
  }

  cat("Estimates of variables:  ", vn,"\n")
  f=data.frame(jagsftt$BUGSoutput$summary)
  return(f)

}
utils::globalVariables(c("N","v1","sd","v2","v3","v4","v5","tau","step"))
