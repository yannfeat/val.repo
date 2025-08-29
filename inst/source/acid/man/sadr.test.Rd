\name{sadr.test}
\alias{sadr.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Misspecification Test assessing a Parametric Conditional Income Distribution
%%  ~~function to do ... ~~
}
\description{ This function performs a misspecificaton test for a parametrically specified cdf estimated by (Bayesian) Structured Additive Distributional Regression. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sadr.test(data, y.pos = NULL, dist1, dist2, params.m, mcmc = TRUE, mcmc.params.a,
 ygrid, bsrep = 10, n.startvals = 300, dist.para.table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a dataframe including dependent variable and all explanatory variables. 
%%     ~~Describe \code{data} here~~
}
  \item{y.pos}{ an integer indicating the position of the dependent variable in the dataframe. 
%%     ~~Describe \code{y.pos} here~~
}
  \item{dist1}{ character string with the name of the first continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
%%     ~~Describe \code{dist1} here~~
}
  \item{dist2}{  character string with the name of the second continuous distribution used. Must be listed in dist.para.table. Must be equivalent to the respective function of that distribution, e.g. norm for the normal distribution.
%%     ~~Describe \code{dist2} here~~
}
  \item{params.m}{ a matrix with the estimated parameter values (in colums) for each individual (in rows). The order of the parameters must be as follows: parameters for the first distribution, parameters for the second distribution, probability of zero income, probability of dist1, probability of dist2 and probability of dist1 given employment/non-zero income.
%%     ~~Describe \code{params.m} here~~
}
  \item{mcmc}{ logical; if TRUE, uncertainty as provided by the MCMC samples is considered.
%%     ~~Describe \code{mcmc} here~~
}
  \item{mcmc.params.a}{ an array, with the mcmc samples for all the parameters specified by structured additive distributional regression. In the first dimension should be the MCMC realisations, in the second dimension the individuals and in the third the parameters. The order of the parameters must be as follows: parameters for the first distribution, parameters for the second distribution, probability of zero income, probability of dist1, probability of dist2 and probability of dist1 given employment/non-zero income.
%%     ~~Describe \code{mcmc.params.a} here~~
}
  \item{ygrid}{ vector yielding the grid on which the cdf is specified.
%%     ~~Describe \code{ygrid} here~~
}
  \item{bsrep}{ integer giving the number of bootstrap repitions in order to determine the distributions of the test statistics under the null.
%%     ~~Describe \code{bsrep} here~~
}
  \item{n.startvals}{ integer giving the maximum number of observations used to estimate the test statistic.
%%     ~~Describe \code{n.startvals} here~~
}
  \item{dist.para.table}{ a table of the same form as \code{\link{dist.para.t}} with distribution name, function name and number of parameters.
%%     ~~Describe \code{dist.para.table} here~~
}
}
\value{
\item{teststat.ks}{ Kolmogorov-Smirnov test statistic.}
\item{pval.ks}{ p-value based on the Kolmogorov-Smirnov test statistic.}
\item{teststat.cvm}{ Cramer-von-Mises test statistic.}
\item{pval.cvm}{ p-value based on the Cramer-von-Mises test statistic.}
\item{test}{ type cdf considered for the test.}
\item{param.distributions}{ parametric distributions assumed for dist1 and dist2.}
\item{teststat.ks.bs}{ bootstrap results of Kolmogorov-Smirnov test statistic under null.}
\item{teststat.cvm.bs}{ bootstrap results of Cramer-von-Mises test statistic under null.}
}
\references{
Rothe, C. and Wied, D. (2013): Misspecification Testing in a Class of Conditional Distributional Models, in: Journal of the American Statistical Association, Vol. 108(501), pp.314-324.

Sohn, A. (forthcoming): Scars from the Past and Future Earning Distributions.
}
\author{ Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# ### functions not run - take considerable time!
# 
# library(acid)
# data(dist.para.t)
# data(params)
# ### example one - two normals, no mcmc
# dist1<-"norm"
# dist2<-"norm"
# ## generating data
# set.seed(1234)
# n<-1000
# sigma<-0.1
# X.theta<-c(1,10,1,10)
# X.gen<-function(n,paras){
#   X<-matrix(c(round(runif(n,paras[1],paras[2])),round(runif(n,paras[3],
#             paras[4]))),ncol=2)
#   return(X)
# }
# X <- X.gen(n,X.theta)
# beta.mu1   <- 1
# beta.sigma1<- 0.1
# beta.mu2   <- 2
# beta.sigma2<- 0.1
# pi0        <- 0.3
# pi01       <- 0.8
# pi1        <- (1-pi0)*pi01
# pi2        <- 1-pi0-pi1
# 
# params.m<-matrix(NA,n,8)
# params.m[,1]<-(0+beta.mu1)*X[,1]
# params.m[,2]<-(0+beta.sigma1)*X[,1]
# params.m[,3]<-(0+beta.mu2)*X[,2]
# params.m[,4]<-(0+beta.sigma2)*X[,2]
# params.m[,5]<-pi0
# params.m[,6]<-pi1
# params.m[,7]<-pi2
# params.m[,8]<-pi01
# 
# params.mF<-matrix(NA,n,8)
# params.mF[,1]<-(10+beta.mu1)*X[,1]
# params.mF[,2]<-(0+beta.sigma1)*X[,1]
# params.mF[,3]<-(0+beta.mu2)*X[,2]
# params.mF[,4]<-(2+beta.sigma2)*X[,2]
# params.mF[,5]<-pi0
# params.mF[,6]<-pi1
# params.mF[,7]<-pi2
# params.mF[,8]<-pi01
# # starting repititions
# reps<-30
# tsreps1T<-rep(NA,reps)
# tsreps2T<-rep(NA,reps)
# tsreps1F<-rep(NA,reps)
# tsreps2F<-rep(NA,reps)
# sys.t<-Sys.time()
# for(r in 1:reps){
#   Y <- rep(NA,n)
#   for(i in 1:n){
#     Y[i] <- ysample.md(1,dist1,dist2,theta=params.m[i,1:4],params.m[i,5],
#     params.m[i,6],params.m[i,7],dist.para.t)
#   }
#   dat<-cbind(Y,X)
#   y.pos<-1
#   ygrid<-seq(min(Y),round(max(Y)*1.2,-1),by=1)  
#   tsT<-sadr.test(data=dat,y.pos=NULL,dist1="norm",dist2="norm",
#   params.m=params.m,mcmc=FALSE,mcmc.params=NA,ygrid=ygrid, bsrep=100,
#   n.startvals=30000,dist.para.table=dist.para.t)
#   tsreps1T[r]<-tsT$pval.ks
#   tsreps2T[r]<-tsT$pval.cvm
#   tsF<-sadr.test(data=dat,y.pos=NULL,dist1="norm",dist2="norm",
#   params.m=params.mF,mcmc=FALSE,mcmc.params=NA,ygrid=ygrid, bsrep=100,
#   n.startvals=30000,dist.para.table=dist.para.t)
#   tsreps1F[r]<-tsF$pval.ks
#   tsreps2F[r]<-tsF$pval.cvm
# }
# time.taken<-Sys.time()-sys.t
# time.taken
# cbind(tsreps1T,tsreps2T,tsreps1F,tsreps2F)
# 
# data(dist.para.t)
# data(params)
# 
# ### example two - Dagum and log-normal - no mcmc
# ##putting list elements from params into matrix form for params.m
# params.m<-matrix(NA,length(params$aft.v),6+4)
# params.m[,1]<-params[[which(names(params)=="bft.v")]]
# params.m[,2]<-params[[which(names(params)=="aft.v")]]
# params.m[,3]<-params[[which(names(params)=="cft.v")]]
# params.m[,4]<-1
# params.m[,5]<-params[[which(names(params)=="mupt.v")]]
# params.m[,6]<-params[[which(names(params)=="sigmapt.v")]]
# params.m[,7]<-params[[which(names(params)=="punemp.v")]]
# params.m[,8]<-params[[which(names(params)=="pft.v")]]
# params.m[,9]<-params[[which(names(params)=="ppt.v")]]
# params.m[,10]<-params[[which(names(params)=="pemp.v")]]
# 
# set.seed(123)
# reps<-30
# tsreps1T<-rep(NA,reps)
# tsreps2T<-rep(NA,reps)
# tsreps1F<-rep(NA,reps)
# tsreps2F<-rep(NA,reps)
# sys.t<-Sys.time()
# for(r in 1:reps){ 
#   ## creates variables under consideration and dimnames
#   n  <- dim(params.m)[1]
#   mcmcsize<-params$mcmcsize
#   ages <- params$ages
#   unems <- params$unems
#   educlvls <- params$educlvls
#   OW <- params$OW
#   ## simulate two samples
#   ages.s <- sample(ages,n,replace=TRUE)
#   unems.s<- sample(unems,n,replace=TRUE)
#   edu.s  <- sample(c(-1,1),n,replac=TRUE)
#   OW.s   <- sample(c(-1,1),n,replac=TRUE)
#   y.sim<-rep(NA,n)
#   p.sel<-sample(1:dim(params.m)[1],n)
#   for(i in 1:n){
#     p<-p.sel[i]
#     #p<-sample(1:n,1) #select a random individual
#     y.sim[i]<-ysample.md(1,"GB2","LOGNO",
#                          theta=c(params$bft.v[p],params$aft.v[p],
#                                  params$cft.v[p],1,
#                                  params$mupt.v[p],params$sigmapt.v[p]),
#                          params$punemp.v[p],params$pft.v[p],params$ppt.v[p],
#                          dist.para.t)
#   }
#   dat<-cbind(y.sim,ages.s,unems.s,edu.s,OW.s)
#   y.simF<- rnorm(n,mean(y.sim),sd(y.sim))
#   y.simF[y.simF<0]<-0
#   datF<-dat
#   datF[,1]<-y.simF
#   ygrid <- seq(0,1e6,by=1000) #quantile(y,taus)
#   ##executing test
#   tsT<-sadr.test(data=dat,y.pos=NULL,dist1="GB2",dist2="LOGNO",params.m=
#                  params.m[p.sel,],mcmc=FALSE,mcmc.params=NA,ygrid=ygrid, 
#                  bsrep=100,n.startvals=30000,dist.para.table=dist.para.t)
#   tsreps1T[r]<-tsT$pval.ks
#   tsreps2T[r]<-tsT$pval.cvm
#   tsF<-sadr.test(data=datF,y.pos=NULL,dist1="GB2",dist2="LOGNO",
#                  params.m=params.m[p.sel,],mcmc=FALSE,mcmc.params=NA,
#                  ygrid=ygrid, 
#                  bsrep=100,n.startvals=30000,dist.para.table=dist.para.t)
#   tsreps1F[r]<-tsF$pval.ks
#   tsreps2F[r]<-tsF$pval.cvm
# }
# time.taken<-Sys.time()-sys.t
# time.taken
# cbind(tsreps1T,tsreps2T,tsreps1F,tsreps2F)
# 
# 
# 
# 
# 
# ### example three - two normals, with mcmc
# set.seed(1234)
# n<-1000 #no of observations
# m<-100 #no of mcmc samples
# sigma<-0.1
# X.theta<-c(1,10,1,10)
# #without weights
# X.gen<-function(n,paras){
#   X<-matrix(c(round(runif(n,paras[1],paras[2])),round(runif(n,paras[3],
#             paras[4]))),ncol=2)
#   return(X)
# }
# X <- X.gen(n,X.theta)
# 
# beta.mu1   <- 1
# beta.sigma1<- 0.1
# beta.mu2   <- 2
# beta.sigma2<- 0.1
# pi0        <- 0.3
# pi01       <- 0.8
# pi1        <- (1-pi0)*pi01
# pi2        <- 1-pi0-pi1
# 
# mcmc.params.a<-array(NA,dim=c(m,n,8))
# mcmc.params.a[,,1]<-(0+beta.mu1+rnorm(m,0,beta.mu1/10))%*%t(X[,1]) 
      #assume sd of mcmc as 10% of parameter value
# mcmc.params.a[,,2]<-(0+beta.sigma1+rnorm(m,0,beta.sigma1/10))%*%t(X[,1]) 
      #must not be negative!, may be for other seed!
# mcmc.params.a[,,3]<-(0+beta.mu2+rnorm(m,0,beta.mu2/10))%*%t(X[,2])
# mcmc.params.a[,,4]<-(0+beta.sigma2+rnorm(m,0,beta.sigma2/10))%*%t(X[,2]) 
      #must not be negative!, may be for other seed!
# mcmc.params.a[,,5]<-(pi0+rnorm(m,0,pi0/10))%*%t(rep(1,n))
# mcmc.params.a[,,8]<-(pi01+rnorm(m,0,pi01/10))%*%t(rep(1,n))
# mcmc.params.a[,,6]<-(1-mcmc.params.a[,,5])*mcmc.params.a[,,8]
# mcmc.params.a[,,7]<-1-mcmc.params.a[,,5]-mcmc.params.a[,,6]
# 
# params.m<-apply(mcmc.params.a,MARGIN=c(2,3),FUN=quantile,probs=0.5)
# 
# mcmc.params.aF<-array(NA,dim=c(m,n,8))
# mcmc.params.aF[,,1]<-(10+beta.mu1+rnorm(m,0,beta.mu1/10))%*%t(X[,1]) 
      #assume sd of mcmc as 10% of parameter value
# mcmc.params.aF[,,2]<-(0+beta.sigma1+rnorm(m,0,beta.sigma1/10))%*%t(X[,1]) 
      #must not be negative!, may be for other seed!
# mcmc.params.aF[,,3]<-(0+beta.mu2+rnorm(m,0,beta.mu2/10))%*%t(X[,2])
# mcmc.params.aF[,,4]<-(2+beta.sigma2+rnorm(m,0,beta.sigma2/10))%*%t(X[,2]) 
      #must not be negative!, may be for other seed!
# mcmc.params.aF[,,5]<-(pi0+rnorm(m,0,pi0/10))%*%t(rep(1,n))
# mcmc.params.aF[,,8]<-(pi01+rnorm(m,0,pi01/10))%*%t(rep(1,n))
# mcmc.params.aF[,,6]<-(1-mcmc.params.aF[,,5])*mcmc.params.aF[,,8]
# mcmc.params.aF[,,7]<-1-mcmc.params.aF[,,5]-mcmc.params.aF[,,6]
# 
# params.mF<-apply(mcmc.params.aF,MARGIN=c(2,3),FUN=quantile,probs=0.5)
# 
# reps<-30
# tsreps1T<-rep(NA,reps)
# tsreps2T<-rep(NA,reps)
# tsreps1F<-rep(NA,reps)
# tsreps2F<-rep(NA,reps)
# sys.t<-Sys.time()
# for(r in 1:reps){
#   Y <- rep(NA,n)
#   for(i in 1:n){
#     Y[i] <- ysample.md(1,dist1,dist2,theta=params.m[i,1:4],params.m[i,5],
#                        params.m[i,6],params.m[i,7],dist.para.t)
#   }  
#   dat<-cbind(Y,X)
#   y.pos<-1
#   ygrid<-seq(min(Y),round(max(Y)*1.2,-1),by=1)  
#   tsT<-sadr.test(data=dat,y.pos=NULL,dist1="norm",dist2="norm",params.m=
#                  params.m,mcmc=TRUE,mcmc.params=mcmc.params.a,ygrid=ygrid, 
#                  bsrep=100,n.startvals=30000,dist.para.table=dist.para.t)
#   tsreps1T[r]<-tsT$pval.ks
#   tsreps2T[r]<-tsT$pval.cvm
#   tsF<-sadr.test(data=dat,y.pos=NULL,dist1="norm",dist2="norm",
#                  params.m=params.mF,mcmc=TRUE,mcmc.params=mcmc.params.aF,
#                  ygrid=ygrid, bsrep=100,n.startvals=30000,
#                  dist.para.table=dist.para.t)
#   tsreps1F[r]<-tsF$pval.ks
#   tsreps2F[r]<-tsF$pval.cvm
#   #c(ts$teststat.ks,ts$teststat.cvm)
#   #c(ts$pval.ks,ts$pval.cvm)
#   
# }
# time.taken<-Sys.time()-sys.t
# time.taken
# cbind(tsreps1T,tsreps2T,tsreps1F,tsreps2F)
# 
# 
# 
# ### example four - two normals, with mcmc and slight deviation from truth 
#     in true params
# library(acid)
# data(dist.para.t)
# data(params)
# dist1<-"norm"
# dist2<-"norm"
# 
# set.seed(1234)
# n<-1000 #no of observations
# m<-100 #no of mcmc samples
# sigma<-0.1
# X.theta<-c(1,10,1,10)
# #without weights
# X.gen<-function(n,paras){
#   X<-matrix(c(round(runif(n,paras[1],paras[2])),round(runif(n,paras[3],
#             paras[4]))),ncol=2)
#   return(X)
# }
# X <- X.gen(n,X.theta)
# 
# beta.mu1   <- 1
# beta.sigma1<- 0.1
# beta.mu2   <- 2
# beta.sigma2<- 0.1
# pi0        <- 0.3
# pi01       <- 0.8
# pi1        <- (1-pi0)*pi01
# pi2        <- 1-pi0-pi1
# 
# mcmc.params.a<-array(NA,dim=c(m,n,8))
# mcmc.params.a[,,1]<-(beta.mu1/10+beta.mu1+rnorm(m,0,beta.mu1/10))%*%t(X[,1]) 
       #assume sd of mcmc as 10% of parameter value
# mcmc.params.a[,,2]<-(0+beta.sigma1+rnorm(m,0,beta.sigma1/10))%*%t(X[,1]) 
       #must not be negative!, may be for other seed!
# mcmc.params.a[,,3]<-(0+beta.mu2+rnorm(m,0,beta.mu2/10))%*%t(X[,2])
# mcmc.params.a[,,4]<-(beta.sigma2/10+beta.sigma2+rnorm(m,0,
#                      beta.sigma2/10))%*%t(X[,2]) 
       #must not be negative!, may be for other seed!
# mcmc.params.a[,,5]<-(pi0+rnorm(m,0,pi0/10))%*%t(rep(1,n))
# mcmc.params.a[,,8]<-(pi01+rnorm(m,0,pi01/10))%*%t(rep(1,n))
# mcmc.params.a[,,6]<-(1-mcmc.params.a[,,5])*mcmc.params.a[,,8]
# mcmc.params.a[,,7]<-1-mcmc.params.a[,,5]-mcmc.params.a[,,6]
# 
# params.m<-apply(mcmc.params.a,MARGIN=c(2,3),FUN=quantile,probs=0.5)
# 
# mcmc.params.aF<-array(NA,dim=c(m,n,8))
# mcmc.params.aF[,,1]<-(10+beta.mu1+rnorm(m,0,beta.mu1/10))%*%t(X[,1]) 
       #assume sd of mcmc as 10% of parameter value
# mcmc.params.aF[,,2]<-(0+beta.sigma1+rnorm(m,0,beta.sigma1/10))%*%t(X[,1]) 
       #must not be negative!, may be for other seed!
# mcmc.params.aF[,,3]<-(0+beta.mu2+rnorm(m,0,beta.mu2/10))%*%t(X[,2])
# mcmc.params.aF[,,4]<-(2+beta.sigma2+rnorm(m,0,beta.sigma2/10))%*%t(X[,2]) 
       #must not be negative!, may be for other seed!
# mcmc.params.aF[,,5]<-(pi0+rnorm(m,0,pi0/10))%*%t(rep(1,n))
# mcmc.params.aF[,,8]<-(pi01+rnorm(m,0,pi01/10))%*%t(rep(1,n))
# mcmc.params.aF[,,6]<-(1-mcmc.params.aF[,,5])*mcmc.params.aF[,,8]
# mcmc.params.aF[,,7]<-1-mcmc.params.aF[,,5]-mcmc.params.aF[,,6]
# 
# params.mF<-apply(mcmc.params.aF,MARGIN=c(2,3),FUN=quantile,probs=0.5)
# 
# reps<-30
# tsreps1T<-rep(NA,reps)
# tsreps2T<-rep(NA,reps)
# tsreps1F<-rep(NA,reps)
# tsreps2F<-rep(NA,reps)
# sys.t<-Sys.time()
# for(r in 1:reps){
#   Y <- rep(NA,n)
#   for(i in 1:n){
#     Y[i] <- ysample.md(1,dist1,dist2,theta=params.m[i,1:4],params.m[i,5],
#                        params.m[i,6],params.m[i,7],dist.para.t)
#   }
#   
#   dat<-cbind(Y,X)
#   y.pos<-1
#   ygrid<-seq(min(Y),round(max(Y)*1.2,-1),by=1)  
#   tsT<-sadr.test(data=dat,y.pos=NULL,dist1="norm",dist2="norm",
#                  params.m=params.m,mcmc=TRUE,mcmc.params=mcmc.params.a,
#                  ygrid=ygrid, bsrep=100,n.startvals=30000,
#                  dist.para.table=dist.para.t)
#   tsreps1T[r]<-tsT$pval.ks
#   tsreps2T[r]<-tsT$pval.cvm
#   tsF<-sadr.test(data=dat,y.pos=NULL,dist1="norm",dist2="norm",
#                  params.m=params.mF,mcmc=TRUE,mcmc.params=mcmc.params.aF,
#                  ygrid=ygrid, bsrep=100,n.startvals=30000,
#                  dist.para.table=dist.para.t)
#   tsreps1F[r]<-tsF$pval.ks
#   tsreps2F[r]<-tsF$pval.cvm
#   #c(ts$teststat.ks,ts$teststat.cvm)
#   #c(ts$pval.ks,ts$pval.cvm)
#   
# }
# time.taken<-Sys.time()-sys.t
# time.taken
# cbind(tsreps1T,tsreps2T,tsreps1F,tsreps2F)



}