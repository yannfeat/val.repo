\name{confband.pw}
\alias{confband.pw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Pointwise Confidence Bands
%%  ~~function to do ... ~~
}
\description{ This function computes pointwise confidence bands for samples of the presumed distribution of the parameter estimator.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
confband.pw(samples, level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samples}{ matrix containing samples of the presumed distribution of the parameter estimator.
%%     ~~Describe \code{samples} here~~
}
  \item{level}{ the desired confidence level.
%%     ~~Describe \code{level} here~~
}
}

\value{ 

\item{lower }{a vector containing the lower bound of the confidence band.}
\item{upper }{a vector containing the lower bound of the confidence band.}

}
\references{ T. Krivobokova, T. Kneib, G. Claeskens (2010): Simultaneous Confidence Bands for Penalized Spline Estimators, in: Journal of the American Statistical Association, Vol. 105(490), pp.852-863.
%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
%%  ~~who you are~~
}
\note{ This function is mainly derived from the work of T. Krivobokova, T. Kneib and G. Claeskens. 
%%  ~~further notes~~
}

\examples{
mu<-1:20
n<-1000
mcmc<-matrix(NA,n,20)
for(i in 1:20){
  mcmc[,i]<- rnorm(n,mu[i],sqrt(i))
}

plot(mu,type="l",ylim=c(-10,30),lwd=3)
lines(confband.pw(mcmc)$lower,lty=2)
lines(confband.pw(mcmc)$upper,lty=2)
lines(confband.kneib(mcmc)$lower,lty=3)
lines(confband.kneib(mcmc)$upper,lty=3)

}