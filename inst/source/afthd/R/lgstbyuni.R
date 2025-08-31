#' @title
#' Univariate estimates of AFT model with log logistic distribution using MCMC.
#'
#' @description Provides estimate of AFT model with log logistic distribution using MCMC for univariate in high dimensional gene expression data. It also deals covariates with missing values.
#'
#' @details
#'
#' This function deals covariates (in data) with missing values. Missing value in any column (covariate) is replaced by mean of that particular covariate.
#' AFT model is log-linear regression model for survival time \eqn{ T_{1}},\eqn{ T_{2}},..,\eqn{T_{n}}.
#' i.e., \deqn{log(T_i)= x_i'\beta +\sigma\epsilon_i ;~\epsilon_i \sim F_\epsilon (.)~which~is~iid }
#' Where \eqn{ F_\epsilon } is known cdf which is defined on real line.
#' When baseline distribution is logistic then T follows log logistic distribution.
#' \deqn{ T \sim Log-Logis(x'\beta,\sqrt{\tau)} }
#'
#' @param m Starting column number of covariates of study from high dimensional entered data.
#' @param n Ending column number of covariates of study from high dimensional entered data.
#' @param STime name of survival time in data
#' @param Event name of event in data
#' @param nc number of chain used in model.
#' @param ni number of iteration used in model.
#' @param data High dimensional gene expression data that contains event status, survival time and and set of covariates.
#' @return Data frame is containing posterior estimates (Coef, SD, Credible Interval, Rhat, n.eff) of regression coefficient of selected covariates and deviance. Result shows together for all covariates chosen from column m to n.
#'
#' @import R2jags
#'
#' @references Prabhash et al(2016) <doi:10.21307/stattrans-2016-046>
#'
#' @examples
#' ##
#' data(hdata)
#' lgstbyuni(12,14,STime="os",Event="death",3,100,hdata)
#' ##
#' @export
#' @author Atanu Bhattacharjee, Gajendra Kumar Vishwakarma and Pragya Kumari
#' @seealso wbysmv, lgnbymv, lgstbymvs
#'

lgstbyuni=function(m,n,STime,Event,nc,ni,data){
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

  mx<-max(d12$os) + 100
  surt<-ifelse(d12$death == 1, d12$os, NA)
  stcen<-ifelse(d12$death == 0, d12$os, mx)
  stcen<-log(stcen)
  ls<-log(surt)
  d12$os<- ls
  cen<-as.numeric(is.na(surt))
  d12<-data.frame(d12,stcen,cen)

  vv<-d11

  mtx<-matrix(nrow=len, ncol = 10)
  colnames(mtx)<-c("coef","SD","2.5%","25%","50%","75%","97.5%","Rhat","n.eff","deviance")
  rownames(mtx)<-colnames(d11)
  mtx<-data.frame(mtx)

  for(j in 1:len){
    data1<-list(os=d12$os, stcen=d12$stcen, cen=d12$cen, v1=vv[,j], N = nr)
    modelj1<-function(){
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

    inits1 <- function() {
      list(beta=c(0,0), tau=1)
    }
    jagsft <- jags(model.file=modelj1, data=data1, inits = inits1,
                   parameters.to.save = c('beta','tau','sigma'), n.chains = nc, n.iter = ni)
    f=data.frame(jagsft$BUGSoutput$summary)

    for(k in 1:9){
      mtx[j,k]<-f[2,k]
    }
    mtx[j,10]<-f[3,1]
  }
  if(nrow(mtx)!=0){
  cat("Estimates for variables:  ", colnames(d11),"\n")
  return(mtx)
  } else{
    warning("No return value, check for right entry or called for side effects")
  }
}

utils::globalVariables(c("N","v1","sd","tau","step","os"))
