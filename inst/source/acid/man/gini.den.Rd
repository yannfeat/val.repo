\name{gini.den}
\alias{gini.den}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gini Coefficient for an Income Distribution
%%  ~~function to do ... ~~
}
\description{  This function approximates the Gini coefficient for a distribution specified by a vector of densities and a corresponding income vector. A point mass at zero is allowed.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gini.den(incs, dens, pm0 = NA, 
lower = NULL, upper = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{incs}{  a vector with sorted income values.
%%     ~~Describe \code{incs} here~~
}
  \item{dens}{ a vector with the corresponding densities.
%%     ~~Describe \code{dens} here~~
}
  \item{pm0}{ the point mass for zero incomes. If not specified no point mass is assumed.
%%     ~~Describe \code{pm0} here~~
}
  \item{lower}{ the lower bound of the income range considered. 
%%     ~~Describe \code{lower} here~~
}
  \item{upper}{ the upper bound of the income range considered.
%%     ~~Describe \code{upper} here~~
}
}
\value{
\item{Gini}{the approximation of the Gini coefficient.}
\item{pm0}{the point mass for zero incomes used.}
\item{lower}{the lower bound of the income range considered used.}
\item{upper}{the upper bound of the income range considered used.}

}
\references{
Cowell, F.A. (2000): Measurement of Inequality, in: Atkinson and Bourguignon (eds.), Handbook of Income Distribution, pp. 1-86, Elsevier, Amsterdam.

%% ~put references to the literature/web site here ~
}
\author{Alexander Sohn
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{weighted.gini}}
}
\examples{
mu<-2
sigma<-1
incs<-c(seq(0,500,by=0.01),seq(501,50000,by=1))
dens<-dLOGNO(incs,mu,sigma)
plot(incs,dens,type="l",xlim=c(0,100))
gini.den(incs=incs,dens=dens)$Gini
gini(rLOGNO(5000000,mu,sigma))$Gini
2*pnorm(sigma/sqrt(2))-1 #theoretical Gini
}