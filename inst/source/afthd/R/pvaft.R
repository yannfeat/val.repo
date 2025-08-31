#' @title
#'
#' Estimates of univariate covariates using Accelerated Failure time (AFT) model without MCMC.
#'
#' @description Provides list of covariates and their estimates of parametric AFT model with smooth time functions, whose p value is less than chosen value (by default p=1 that is all chosen covariates come in result). Using AFT model for univariate in high dimensional data without MCMC.
#'
#' @details
#' Survival time T for covariate x, is modelled as AFT model using
#' \deqn{S(T|x)=S_0(T\exp(-\eta(x;\beta)))}
#' and baseline survival function is modelled as
#' \deqn{S_0(T)=\exp(-\exp(\eta_0(log(T);\beta_0)))}
#' Where \eqn{\eta} and \eqn{\eta} are linear predictor.
#'
#' @param m Starting column number of covariates of study in high dimensional entered data.
#' @param n Ending column number of covariates of study in high dimensional entered data.
#' @param STime name of survival time in data.
#' @param Event name of event in data. 0 is for censored and 1 for occurrence of event.
#' @param p p-value, to make restriction for selection of covariates, default value is 1.
#' @param data High dimensional gene expression data that contains event status, survival time and and set of covariates.
#' @return Matrix that contains survival information of selected covariates(selected from chosen columns whose p value is <= p) on AFT model. Result shows together for all covariates chosen from column m to n.
#' @import rstpm2
#' @import photobiology
#' @import survival
#'
#' @examples
#' ##
#' data(hdata)
#' pvaft(9,30,STime="os",Event="death",0.1,hdata)
#' ##
#' @export
#' @author Atanu Bhattacharjee, Gajendra Kumar Vishwakarma and Pragya Kumari
#' @seealso wbysuni,wbysmv, rglaft

pvaft<-function(m,n,STime,Event,p=1,data)
{
  data<-na.omit(data)
  nr<-nrow(data)

  if(STime!="os"){
    names(data)[names(data) == STime] <- "os"
  }
  if(Event!="death"){
    names(data)[names(data) == Event] <- "death"
  }

  pnt<-NULL
  for(i in m:n)
  {
    if(sum(data[,i])==0) {
      pnt<-c(pnt,i)
    }
  }
  if(is.null(pnt)==F){
    data<-data[,-pnt]
  }
  else{
    data<-data
  }
  count<-length(pnt)
  n=n-count

  ht<-colnames(data)[m:n]
  le<-length(ht)
  covariates<-NULL
  mtx<-matrix(nrow=le,ncol = 4)
  colnames(mtx)<-c("Estimate","std.Error","z value","p_value")
  rownames(mtx)<-ht
  for(i in 1:le)
  {
    ftt<-aft(Surv(os,death==1)~get(ht[i]),data=data)
    q1<-coef(summary(ftt))[1,]
    mtx[i,]<-q1
  }

  mtx<-data.frame(mtx)
  mtx<-mtx[order(mtx$p_value),]
  fmtx<-subset(mtx,p_value<=p)
  return(fmtx)
}

utils::globalVariables(c("na.omit","death","p_value"))

