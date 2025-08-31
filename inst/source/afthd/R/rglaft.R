#' @title
#' Estimates of selected univariate covariates(using regularization) in AFT model without MCMC.
#'
#' @description Provides Estimates of selected variable in parametric AFT model with smooth time functions for univariate in high dimensional gene expression data without MCMC.Incorporated variable selection has been done with regularization technique. It also deals covariates with missing values.
#'
#'  @details
#' Survival time T for covariate x, is modelled as AFT model using
#' \deqn{S(T|x)=S_0(T\exp(-\eta(x;\beta)))}
#' and baseline survival function is modelled as
#' \deqn{S_0(T)=\exp(-\exp(\eta_0(log(T);\beta_0)))}
#' Where \eqn{\eta} and \eqn{\eta} are linear predictor.
#'
#' @param m Starting column number of covariates of study from high dimensional entered data.
#' @param n Ending column number of covariates of study from high dimensional entered data.
#' @param STime name of survival time in data.
#' @param Event name of event in data. 0 is for censored and 1 for occurrence of event.
#' @param alpha It is chosen value between 0 and 1 to know the regularization method. alpha=1 for Lasso,
#' alpha=0 for Ridge and alpha between 0 and 1 for elastic net regularization.
#' @param data High dimensional gene expression data that contains event status, survival time and and set of covariates.
#'
#' @return Matrix that contains survival information of selected covariates(selected from chosen columns using regularization) on AFT model. Uppermost covariates are more significant  than lowerone, as covariates are ordered as their increasing order of p value.
#'
#' @import rstpm2
#' @import photobiology
#' @import survival
#' @import glmnet
#'
#' @examples
#' ##
#' data(hdata)
#' set.seed(1000)
#' rglaft(9,50,STime="os",Event="death",1,hdata)
#' ##
#' @export
#' @author Atanu Bhattacharjee, Gajendra Kumar Vishwakarma and Pragya Kumari
#' @seealso pvaft, rglwbysu, rglwbysm
#'

rglaft<-function(m,n,STime,Event,alpha,data){
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
  cv.ReglVar<-cv.glmnet(x,Surv(os,death),alpha = a,family = "cox")

  var_minlmd = coef(cv.ReglVar, cv.ReglVar$lambda.min)
  p <- as.matrix(var_minlmd)
  sg<-subset(p,p[,1]!=0)

  if(nrow(sg)==0){
    stop("Increase number of variables, chosen are not sufficient")
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
  ht<-colnames(d12)[3:length(d12)]
  mtx<-matrix(nrow=ln,ncol = 4)
  for(i in 1:ln)
  {
    rglaft<-aft(Surv(os,death==1)~get(ht[i]),data=d12)
    q1<-coef(summary(rglaft))[1,]
    mtx[i,]<-q1
  }
  colnames(mtx)<-colnames(coef(summary(rglaft)))
  rownames(mtx)<-colnames(fdt)
  mtx<-data.frame(mtx)
  mtx<-mtx[order(mtx[,4]),]
  return(mtx)
}

