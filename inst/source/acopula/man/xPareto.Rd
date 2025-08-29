\name{xPareto}
\alias{pPareto}
\alias{qPareto}

\title{
4-parametric univariate Pareto distribution
}
\description{
Probability (cumulative) distribution function \code{pPareto} and quantile function \code{qPareto} of Pareto type IV distribution.
}
\usage{
pPareto(t,pars)
qPareto(t,pars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{numeric. Argument of the functions.}
  \item{pars}{numeric vector of length 4. Parameters of the Pareto distribution (see Details).}
}
\details{
Cumulative distribution function of 4-parametric Pareto is defined as
\deqn{cdf(x)=1-\left(1+\left(\frac{t-p_4}/p_1\right)^{1/p_3}\right)^{-p_2}\quad \textrm{for } t\geq p_4}{cdf(x)=1-(1+((t-p[4])/p[1])^(1/p[3]))^(-p[2]) for t>=p[4]}
and results to 0 otherwise.
}
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
\code{\link{generator}}, \code{\link{depfun}}, \code{\link{copula}}
}
\examples{
## probability P(X<q)=p
pPareto(t = 2.5, pars = c(10.,5.,3.,1))  # 0.8823436
qPareto(t = .Last.value, pars = c(10.,5.,3.,1))  # 2.5
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Pareto distribution }
\keyword{ CDF }
\keyword{ quantile }
