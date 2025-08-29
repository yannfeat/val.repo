\name{midks.test}
\alias{midks.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Kolmogorov-Smirnov Test assessing a Parametric Mixture for a Conditional Income Distribution
%%  ~~function to do ... ~~
}
\description{ This function performs a Kolmogorov-Smirnov test for a parametrically specified cdf composed of a mixture distribution either by cdf.mix.dag or cdf.mix.LN.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
midks.test(x, y, ..., w = NULL, pmt = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of observed incomes.
%%     ~~Describe \code{x} here~~
}
  \item{y}{ a function specifying the parametric cdf.
%%     ~~Describe \code{y} here~~
}
  \item{\dots}{ arguments to be passed to y.
%%     ~~Describe \code{\dots} here~~
}
  \item{w}{ the weights of the observations contained in y.
%%     ~~Describe \code{w} here~~
}
  \item{pmt}{ point mass threshold equivalent to thres0 in y. 
%%     ~~Describe \code{pmt} here~~
}
}

\value{
\item{statistic}{ returns the test statistic.}
\item{method}{ returns the methodology - currently always One-sample KS-test.}
\item{diffpm}{ the difference of the probability for the point mass.}
\item{diff1}{ the upper difference between for the continuous part of the cdfs.}
\item{diff2}{ the lower difference between for the continuous part of the cdfs.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Sohn, A., Klein, N. and Kneib. T. (2014): A New Semiparametric Approach to Analysing Conditional Income Distributions, in: SOEPpapers, No. 676.
%% ~put references to the literature/web site here ~
}
\author{  Alexander Sohn
%%  ~~who you are~~
}

\examples{
# parameter values
pi0.s<-0.2
pi1.s<-0.1
thres0.s<-0
thres1.s<-25000
mu.s<-20000
sigma.s<-5
nu.s<-0.5
tau.s<-1

# generate sample
n<-100
s<-as.data.frame(matrix(NA,n,3))
names(s)<-c("cat","y","w")
s[,1]<-sample(1:3,n,replace=TRUE,prob=c(pi0.s,pi1.s,1-pi0.s-pi1.s))
s[,3]<-rep(1,n)
for(i in 1:n){
  if(s$cat[i]==1){s$y[i]<-0
  }else if(s$cat[i]==2){s$y[i]<-runif(1,thres0.s,thres1.s)
  }else s$y[i]<-rGB2(1,mu=mu.s,sigma=sigma.s,nu=nu.s,tau=tau.s)+thres1.s
}

# midks.test
midks.test(s$y,cdf.mix.dag,pi0=pi0.s,thres0=thres0.s,pi1=pi1.s,thres1=thres1.s,mu=mu.s,
sigma=sigma.s,nu=nu.s,tau=tau.s,w=s$w,pmt=thres0.s)$statistic

}