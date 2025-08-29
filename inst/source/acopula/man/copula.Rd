\name{copula}

\alias{copula}
\alias{copFGM}
\alias{copGumbel}
\alias{copNormal}
\alias{copPlackett}
\alias{copProduct}
\title{
Generic copulae definitions
}
\description{
Produce a list containing CDF and/or PDF of an copula with parameters bounds.
}
\usage{
copula(name,...)

copFGM(...)
copGumbel(...)
copNormal(dim=2,...)
copPlackett(...)
copProduct(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
character. Code name for copula, identical with the part after \code{cop}.
}
  \item{dim}{
integer. Copula dimension.
}
  \item{\dots}{
named arguments. Items of the copula definition list to be redefined. Argument names must NOT be cropped.
}
}
\details{
Currently implemented copula families:
\tabular{lccc}{
  \bold{family}\tab \bold{CDF} \eqn{C(t)=} \tab  \eqn{p}\bold{ar.range} \tab \bold{special cases} \cr
  Farlie-Gumbel-Morgenstern \tab  \eqn{\prod_i t_i (p\prod_i(1-t)+1)}{prod(t)*(p*prod(1-t)+1)} \tab [-1,1] \tab 0(\eqn{\Pi}) \cr
  Gumbel-Hougaard \tab \eqn{\exp((-\sum_i(-\log t_i)^p)^{1/p})}{exp(-sum((-log(t))^p)^(1/p))} \tab [1,Inf] \tab 1(\eqn{\Pi}),Inf(M) \cr 
  normal \tab \eqn{\Phi_P(\Phi^{-1}(t_1),...)}{\Phi_P(\Phi^-1(t_1),...)} \tab ]-1,1[ \tab -1(W),0(\eqn{\Pi}),1(M) \cr 
  Plackett \tab \eqn{\frac{A-\sqrt{A^2-4\prod_i t_ip(p-1)}}{2(p-1)}; A=1+(p-1)\sum_i t_i}{(A-sqrt(A^2-4*prod(t)*p*(p-1)))/(2*(p-1)); A=1+(p-1)*sum(t)} \tab [0,Inf] \tab 0(W),1(\eqn{\Pi}),Inf(M) \cr
  product \tab \eqn{-\prod_i t_i}{prod(t)} \tab  \tab \eqn{\Pi} \cr 
}
where \eqn{\Phi^{-1}}{\Phi^-1} is quantile function of standard normal and \eqn{\Phi_P}{\Phi_P} is CDF of multivariate normal distribution with correlation matrix \eqn{P} containing copula parameters.
}
\value{
  \item{parameters}{numeric vector to be used whenever parameters of copula are not supplied to procedure that use it, or as starting values in estimation.}
  \item{pcopula,dcopula}{function of two arguments. The first is vector of copula arguments, the another is vector of parameters.}
  \item{rcopula}{function of two arguments. The first is copula dimension or length of random sample (normal copula), the another is vector of parameters.}  
  \item{kendall,spearman}{list. Correlation coefficient as function of copula parameter (\code{coef}), its inverse (\code{icoef}) and range (\code{bounds}). Available only for 1-parameter families.}
  \item{lower,upper}{numeric. Parameters boundary.}
  \item{id}{character. Identification of copula family.}
}
\references{
Nelsen, R. B. (2006): An introduction to copulas. Springer.
}
\author{
Tomas Bacigal
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pCopula}}, \code{\link{generator}}, \code{\link{depfun}}
}
\examples{
## the following gives the same definition list
copFGM()
copula("FGM")

## any list item can be modified upon function call
copPlackett(parameters=2.2,upper=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ copula }% __ONLY ONE__ keyword per line
