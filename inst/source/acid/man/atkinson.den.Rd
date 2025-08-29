\name{atkinson.den}
\alias{atkinson.den}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Atkinson Index for an Income Distribution
%%  ~~function to do ... ~~
}
\description{ This function approximates the Atkinson index for a distribution specified by a vector of densities and a corresponding income vector. A point mass at zero is allowed.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
atkinson.den(incs, dens, epsilon = 1, pm0 = NA, 
lower = NULL, upper = NULL, zero.approx = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{incs}{ a vector with income values.
%%     ~~Describe \code{incs} here~~
}
  \item{dens}{ a vector with the corresponding densities.
%%     ~~Describe \code{dens} here~~
}
  \item{epsilon}{  inequality aversion parameter as denoted by Atkinson (1970). The default is epsilon=1. 
%%     ~~Describe \code{epsilon} here~~
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
  \item{zero.approx}{ a scalar which replaces zero-incomes, such that the Atkinson index involving a logarithm return finite values.
%%     ~~Describe \code{zero.approx} here~~
}
}
\value{
\item{AIM}{the approximation of the selected Atkinson inequality measure.}
\item{epsilon}{the inequality aversion parameter used.}
\item{mean}{the approximated expected value of the distribution.}
\item{pm0}{the point mass for zero incomes used.}
\item{lower}{the lower bound of the income range considered used.}
\item{upper}{the upper bound of the income range considered used.}
\item{zero.approx}{the zero approximation used.}

}
\references{ Atkinson, A.B. (1970): On the Measurment of Inequality, in: Journal of Economic Theory, Vol. 2(3), pp. 244-263.

%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
%%  ~~who you are~~
}


\seealso{  \code{\link{atkinson}} , \code{\link{atkinson.md}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## without point mass at zero
incs<-seq(0,500,by=0.01)
dens<-dLOGNO(incs,2,1)
plot(incs,dens,type="l",xlim=c(0,100))
atkinson.den(incs=incs,dens=dens,epsilon=1)$AIM
atkinson(rLOGNO(50000,2,1),epsilon=1)
atkinson.den(incs=incs,dens=dens,epsilon=0.5)$AIM
atkinson(rLOGNO(50000,2,1),epsilon=0.5)

## with point mass at zero
incs<-c(seq(0,100,by=0.1),seq(100.1,1000,by=1),seq(1001,10000,by=10))
dens<-dLOGNO(incs,2,1)/2
dens[1]<-0.5
plot(incs,dens,type="l",ylim=c(0,max(dens[-1])),xlim=c(0,100))
#without zero approx zeros
atkinson.den(incs=incs,dens=dens,epsilon=1,pm0=0.5)$AIM
atkinson(c(rep(0,25000),rLOGNO(25000,2,1)),epsilon=1)
atkinson.den(incs=incs,dens=dens,epsilon=0.5,pm0=0.5)$AIM
atkinson(c(rep(0,25000),rLOGNO(25000,2,1)),epsilon=0.5)
#with zero approximation 
atkinson.den(incs=incs,dens=dens,epsilon=0.5,pm0=0.5,zero.approx=1)$AIM
atkinson(c(rep(1,25000),rLOGNO(25000,2,1)),epsilon=0.5)
atkinson.den(incs=incs,dens=dens,epsilon=1,pm0=0.5,zero.approx=0.01)$AIM
atkinson(c(rep(0.01,250000),rLOGNO(250000,2,1)),epsilon=1)

}