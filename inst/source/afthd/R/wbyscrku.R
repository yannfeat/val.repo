#'
#' @title
#' Bayesian univariate estimates for competing risk gene expression data using AFT model.
#'
#' @description Provides univariate estimate of AFT model using MCMC for competing risk high dimensional gene expression data. It also dea with missing values.
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
#' @return posterior estimates (Coef, SD, Credible Interval) of regression coefficient of covariate(which is/are chosen as columns in data) in model and deviance are there. Outcome shows together for all covariates chosen from column m to n.
#' @import R2jags
#'
#' @references Prabhash et al(2016) <doi:10.21307/stattrans-2016-046>
#'
#' @examples
#' ##
#' data(hdata)
#' #1<=p<=q<=nrow(data)
#' wbyscrku(9,13,STime="os",Event="death2",2,100,hdata)
#' ##
#' @export
#' @author Atanu Bhattacharjee, Gajendra Kumar Vishwakarma and Pragya Kumari
#' @seealso wbysuni, wbyscrkm
#

 wbyscrku=function(m,n,STime,Event,nc,ni,data){

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

    vn<-colnames(d11)

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

      mtx1<-matrix(nrow=len, ncol = 8)
      colnames(mtx1)<-c("coef","SD","2.5%","25%","50%","75%","97.5%","deviance")
      rownames(mtx1)<-colnames(vv1)

      for(j in 1:len){
        data1<-list(os=dt01$os, stcen1=dt01$stcen1, cen1=dt01$cen1, v1=vv1[,j], N = nr1)
        model1<-function(){
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
        jagsft1 <- jags(model.file=model1, data=data1, inits = inits1,
                        parameters.to.save = c('beta1','tau','sigma'), n.chains=nc, n.iter = ni)
        f1=data.frame(jagsft1$BUGSoutput$summary)
        for(k in 1:7){
          mtx1[j,k]<-f1[2,k]
        }
        mtx1[j,8]<-f1[3,1]
      }
      cat("Estimates for event status (0,1) for variables:-  ", colnames(vv1),"\n")
      print(mtx1)
    }

    if(is.numeric(dstatus2)==T){

      mx2<-max(dt02$os) + 10
      surt2<-ifelse(dt02$death2 == 2, dt02$os, NA)
      stcen2<-ifelse(dt02$death2 == 0, dt02$os, mx2)
      dt02$os<-surt2
      cen2<-as.numeric(is.na(surt2))
      dt02<-data.frame(dt02,stcen2,cen2)

      mtx2<-matrix(nrow=len, ncol = 8)
      colnames(mtx2)<-c("coef","SD","2.5%","25%","50%","75%","97.5%","deviance")
      rownames(mtx2)<-colnames(vv2)

      for(j in 1:len){
        data2<-list(os=dt02$os, stcen2=dt02$stcen2, cen2=dt02$cen2, v1=vv2[,j], N = nr2)
        model2<-function(){
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

        inits2 <- function() {
          list(beta2=c(0,0), tau=1)
        }
        jagsft2 <- jags(model.file=model2, data=data2, inits = inits2,
                        parameters.to.save = c('beta2','tau','sigma'), n.chains=nc, n.iter = ni)
        f2=data.frame(jagsft2$BUGSoutput$summary)

        for(k in 1:7){
          mtx2[j,k]<-f2[2,k]
        }
        mtx2[j,8]<-f2[3,1]
      }
      cat("Estimates for event status (0,2) for variables:-  ", colnames(vv2),"\n")
      print(mtx2)
    }

  } else{
    stop("This is not the case of compating risk with event status (0,1,2)")
  }

}
utils::globalVariables(c("N","v1","sd","tau","step","sd"))
