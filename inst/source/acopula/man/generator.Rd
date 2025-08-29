\name{generator}

\alias{generator}
\alias{genAMH}
\alias{genClayton}
\alias{genFrank}
\alias{genGumbel}
\alias{genJoe}
\alias{genLog}
\title{
Generator of Archimedean copula
}
\description{
Produce a list containing generator of specified Archimedean family, its inverse and derivatives with parameters bounds.
}
\usage{
generator(name,...)

genAMH(...)
genClayton(...)
genFrank(...)
genGumbel(...)
genJoe(...)
genLog(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
character; code name for generator, identical with the part after 'gen'
}
  \item{\dots}{
named arguments; items of the generator definition list to be redefined
}
}
\details{
Currently implemented families of Archimedean copula generator:
\tabular{lccc}{
  \bold{family}\tab \bold{generator} \eqn{\phi(t)=} \tab  \eqn{p}\bold{ar.range} \tab \bold{Archimed.case} \cr
  Ali-Mikhail-Haq \tab  \eqn{\log\left(\frac{1-(1-t)p}{t}\right)}{log([1-(1-t)p]/t)} \tab [-1,1[ \tab -1(\eqn{\Pi}) \cr
  Clayton \tab \eqn{t^(-p) - 1 } \tab [0,Inf] \tab 0(\eqn{\Pi}),Inf(M) \cr
  Frank \tab \eqn{-\log\left( \frac{\exp(-p t)-1}{\exp(-p)-1} \right)}{-log[(exp(-p t)-1)/(exp(-p)-1)]} \tab [-Inf,Inf] \tab -Inf(W),0(\eqn{\Pi}),Inf(M) \cr
  Gumbel-Hougaard \tab \eqn{(-\log(t))^{p}}{(-log(t))^p} \tab [1,Inf] \tab 1(\eqn{\Pi}),Inf(M) \cr 
  Joe \tab \eqn{-\log(1-(1-t)^p)}{-log(1-(1-t)^p)} \tab [1,Inf] \tab 1(\eqn{\Pi}),Inf(M) \cr
  Log \tab \eqn{-\log(t)}{-log(t)} \tab  \tab \eqn{\Pi} \cr 
}
}
\value{
  \item{parameters}{numeric vector to be used whenever parameters of generator are not supplied to procedure that use it, or as starting values in estimation.}
  \item{gen}{function of two arguments. The first is generator argument, the another
is genereator parameters.}
  \item{gen.der}{function. Generator first derivative.}
  \item{gen.der2}{function. Generator second derivative.}
  \item{gen.inv}{function. Generator inverse.}
  \item{gen.inv.der}{function. First derivative of generator inverse.}
  \item{gen.inv.der2}{function. second derivative of generator inverse.}
  \item{kendall,spearman}{list. Correlation coefficient as function of copula parameter (\code{coef}), its inverse (\code{icoef}) and range (\code{bounds}). Available only for 1-parameter families.}
  \item{lower,upper}{numeric; parameters boundary}
  \item{id}{character; identification of generator family}
}
\references{
Nelsen, R. B.: An introduction to copulas. Springer (2006).
}
\author{
Tomas Bacigal
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pCopula}}, \code{\link{depfun}}, \code{\link{copula}}
}
\examples{
## the following gives the same definition list
genGumbel()
generator("Gumbel")

## any list item can be modified upon function call
genGumbel(parameters=2.2,upper=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ generator }
\keyword{ Archimedean copula }% __ONLY ONE__ keyword per line
