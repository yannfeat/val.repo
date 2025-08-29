\name{nderive}
\alias{nderive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Numerical derivative
}
\description{
Linear approximation to function partial derivatives of arbitrary order and dimension.
}
\usage{
nderive(fun, point = c(0), order = c(1), difference = 1e-04, area = c(0, 1, -1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
function. Arguments can be in sequence or single vector.
}
  \item{point}{
numeric vector. Where to evaluate the derivative.
}
  \item{order}{
numeric vector of orders for each variable. 
}
  \item{difference}{
numeric. A change in the variable that (by limit) is to approach zero.
}
  \item{area}{
numeric. One of \{0,1,-1\} representing two-sided, right-sided or left-sided limit, respectively.
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
\code{\link{nintegrate}}
}
\examples{
##density of a bivariate Gumbel copula evaluated in point c(0.5,0.6)
nderive(fun = function(x) pCopula(x,genGumbel(),gpar=3.5), point = c(0.5,0.6),
        order = c(1,1))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ derivative }
\keyword{ linear approximation }% __ONLY ONE__ keyword per line
