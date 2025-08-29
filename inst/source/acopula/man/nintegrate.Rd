\name{nintegrate}
\alias{nintegrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Numerical integration
}
\description{
Trapezoidal integration of an arbitrarily dimensional function.
}
\usage{
nintegrate(fun, lower, upper, subdivisions=100, differences=(upper-lower)/subdivisions)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
function. Arguments can be in sequence or single vector.
}
  \item{lower,upper}{
numeric (vector). Upper and lower bound of integration. Either one of these or \code{differences} should have length of \code{fun}ction dimension.
}
  \item{subdivisions}{
numeric (vector). Number of bins.
}
  \item{differences}{
numeric. Width of bins.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Numeric.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Tomas Bacigal
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{nderive}}
}
\examples{
##cumulative distribution function of a bivariate normal copula 
##evaluated at point c(0.5,0.6); compare pCopula(c(0.5,0.6),cop=copNormal(),par=0.5)
nintegrate(function(x) dCopula(x,cop=copNormal(),par=0.5), 
  lower=0.001, upper=c(0.5,0.6), subdivisions=20) 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ integral }
\keyword{ trapezoid }% __ONLY ONE__ keyword per line
