\name{cdf.mix.dag}
\alias{cdf.mix.dag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Cumulative Density Function of Dagum Mixture Distribution
%%  ~~function to do ... ~~
}
\description{ This function yields the cdf of a mixture distribution consisting of a point mass (at the lower end), a uniform distribution
 (above the point mass and below the Dagum distribution) and a Dagum distribution. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cdf.mix.dag(q, pi0, thres0 = 0, pi1, thres1, mu, sigma, nu, tau)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{ a vector of quantiles.
%%     ~~Describe \code{q} here~~
}
  \item{pi0}{ the probability mass at thres0.
%%     ~~Describe \code{pi0} here~~
}
  \item{thres0}{ the location of the probability mass at the lower end of the distribution.
%%     ~~Describe \code{thres0} here~~
}
  \item{pi1}{ the probability mass of the uniform distribution. 
%%     ~~Describe \code{pi1} here~~
}
  \item{thres1}{ the upper bound of the uniform distribution. 
%%     ~~Describe \code{thres1} here~~
}
  \item{mu}{ the parameter mu of the Dagum distribution as defined by the function GB2. 
%%     ~~Describe \code{mu} here~~
}
  \item{sigma}{ the parameter sigma of the Dagum distribution as defined by the function GB2.
%%     ~~Describe \code{sigma} here~~
}
  \item{nu}{ the parameter nu of the Dagum distribution as defined by the function GB2.
%%     ~~Describe \code{nu} here~~
}
  \item{tau}{ the parameter tau of the Dagum distribution as defined by the function GB2.
%%     ~~Describe \code{tau} here~~
}
}

\value{ returns the cumulative density for the given quantiles.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Alexander Sohn
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{  \code{\link[gamlss.dist]{gamlss.dist}} , \code{\link[gamlss.dist]{gamlss.family}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
pi0.s<-0.2
pi1.s<-0.1
thres0.s<-0
thres1.s<-25000
mu.s<-20000
sigma.s<-5
nu.s<-0.5
tau.s<-1

cdf.mix.dag(50000,pi0.s,thres0.s,pi1.s,thres1.s,mu.s,sigma.s,nu.s,tau.s)
}

\references{ 
Sohn, A., Klein, N. and Kneib. T. (2014): A New Semiparametric Approach to Analysing Conditional Income Distributions, in: SOEPpapers, No. 676.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
