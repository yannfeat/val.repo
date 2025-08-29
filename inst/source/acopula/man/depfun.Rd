\name{depfun}

\alias{depfun}
\alias{dep1}
\alias{depGalambos}
\alias{depGumbel}
\alias{depHuslerReiss}
\alias{depMax}
\alias{depTawn}

\alias{depCC}
\alias{depGCC}
\alias{ldepPartition3D}

\title{
Dependence function of Extreme-Value copula
}
\description{
Produce a list containing dependence function of specified EV copula family, its derivatives and parameters bounds.
Only Hussler-Reiss family is limited to two dimensions.

\code{ldepPartition3D} returns set of 5 dependence functions (see details).
}
\usage{
depfun(name, ...)

dep1(...)
depGalambos(...)
depGumbel(...)
depHuslerReiss(...)
depMax(power = 10, ...)
depTawn(dim = 2, ...)

depCC(depfun = list(dep1(),depGumbel()), 
  dparameters = lapply(depfun,
    function(x) rep(list(NULL),max(1,length(x$parameters)))),
  dim = 2)
depGCC(depfun=list(dep1(),depGumbel()), 
  dparameters = lapply(depfun,
    function(x) rep(list(NULL),max(1,length(x$parameters)))),
  dim = 2, symmetry = FALSE)
    
ldepPartition3D(power = 8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
character. Code name for Pickands' dependence function, identical with the part after \code{dep}.
}
  \item{power}{
numeric. Parameter of Gumbel family dependence function, which approximates the weakest dependence function in order to bring smoothness.
}
  \item{dim}{
numeric. Dimension (of copula) of random vector.
}
  \item{depfun}{
list of dependence function definition lists, also \code{ldepPartition3D} can be used.
}
  \item{dparameters}{
list of dependence function parameters; defaults to list of \code{NULL}s which means the parameters are to be estimated.
}
  \item{symmetry}{
logical. If TRUE, then GCC reduces to standard convex sum and \code{depCC} is used.
}
  \item{\dots}{
named arguments. Items of the dependence function definition list to be redefined.
}
}
\details{
Currently implemented families of EV copula dependence functions:
\tabular{lccc}{
  \bold{family}\tab \bold{dependence function} \eqn{A(t)=} \tab \bold{domain} \tab {EV.case} \cr
  1 \tab  \eqn{ 1 } \tab  \tab \eqn{\Pi} \cr
  Galambos \tab \eqn{1 - (\sum_i t_i^{-p})^{-1/p} }{1-sum(t^{-p})^{-1/p}} \tab [0,10] \tab 1(\eqn{\Pi}),Inf(M) \cr
  Gumbel-Hougaard \tab \eqn{(\sum_i(t_i^{p}))^{1/p}}{sum(t^p)^{1/p}} \tab [1,Inf] \tab 1(\eqn{\Pi}),Inf(M) \cr 
  Husler-Reiss \tab \eqn{t_1 \Phi(1/p + p \log(t_1/t_2)/2) + \atop + t_2 \Phi(1/p - p \log(t_1/t_2)/2)}{t[1]*pnorm(1/p+p*log(t[1]/t[2])/2)+t[2]*pnorm(1/p-p*log(t[1]/t[2])/2)} \tab [0,Inf] \tab 0(\eqn{\Pi}),Inf(M) \cr
  Max \tab \eqn{(\sum_i{t_i^{10}})^{1/10}}{sum(t^10)^(1/10)} \tab  \tab \eqn{M} \cr 
  Tawn \tab \eqn{1 - \sum_i{p_i t_i} + (\sum_i{(p_i t_i)^{p_0}})^{1/p_0}}{1 - sum(p[-1]*t) + sum((p[-1]*t)^p[1])^(1/p[1])} \tab [1,Inf]x[0,1]x... \tab \{1,0,\ldots\}(W),\{Inf,1,\ldots\}(M) \cr
}
Since \eqn{\sum_i t_i=1}{sum(t)=1} a dependence function accepts argument vector with the last element omitted.
}
\value{
  \item{parameters}{numeric vector to be used whenever parameters of depfun are not supplied to procedure that use it, or as starting values in estimation}
  \item{dep}{function of two arguments; the first is depfun argument, the another is depfun parameters}
  \item{dep.der}{function; depfun first derivative}
  \item{dep.der2}{function; depfun second derivative}
  \item{kendall,spearman}{list. Correlation coefficient as function of copula parameter (\code{coef}), its inverse (\code{icoef}) and range (\code{bounds}). Available only for 1-parameter families.}
  \item{lower,upper}{numeric; parameters boundary}
  \item{id}{character; identification of depfun family}
  \item{combpars,rescalepars}{function; extract the combination parameters from the set of provided parameters and rescale them if not fulfilling inner conditions of the (general) convex combination}
}
\references{
\enc{Bacigál}{Bacigal}, T., Mesiar, R.: 3-dimensional Archimax copulas and their fitting to real
data. In: \emph{COMPSTAT 2012, 20th International conference on computational statistics}.
Limassol,Cyprus,27.-31.8.2012. The International Statistical Institute, 81--88 (2012).

Gudendorf, G., Segers, J. (2010): Extreme-value copulas. In \emph{Copula Theory and Its Applications}. Springer Berlin Heidelberg, 127-145.

Insightful Corp.: EVANESCE Implementation in S-PLUS FinMetrics Module (2002).
\url{https://faculty.washington.edu/ezivot/book/QuanCopula.pdf} Cited 6th July 2013.

}
\author{
Tomas Bacigal
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pCopula}}, \code{\link{generator}}, \code{\link{copula}}
}
\examples{
## the following gives the same definition list
depGumbel()
depfun("Gumbel")

## any list item can be modified upon function call
depGumbel(parameters=2.2,upper=10)

## general convex combination of 5 basic depfuns that arise from 
## partitioning method for 3 dimensions; it results in 
## (3x5)-parametric Pickand's dependence function definition list
depGCC(depfun=ldepPartition3D(), dim = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Pickands' dependence function }
\keyword{ Extreme-Value copula}
\keyword{ EV copula}
