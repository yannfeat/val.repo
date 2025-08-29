\name{sgini.den}
\alias{sgini.den}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{   Single-parameter Gini Coefficient for an Income Distribution
%%  ~~function to do ... ~~
}
\description{  This function approximates the Single-parameter Gini coefficient for a distribution specified by a vector of densities and a corresponding income vector. A point mass at zero is allowed.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sgini.den(incs, dens, nu = 2, pm0 = NA, lower = NULL, upper = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{incs}{  a vector with sorted income values.
%%     ~~Describe \code{incs} here~~
}
  \item{dens}{  a vector with the corresponding densities.
%%     ~~Describe \code{dens} here~~
}
  \item{nu}{  a scalar entailing the parameter that tunes the degree of the policy maker's aversion to inequality. See Yaari, 1988 for details.
%%     ~~Describe \code{nu} here~~
}
  \item{pm0}{   the point mass for zero incomes. If not specified no point mass is assumed.
%%     ~~Describe \code{pm0} here~~
}
  \item{lower}{  the lower bound of the income range considered. 
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
van Kerm, P. (2009): sgini - Generalized Gini and Concentration coefficients (with factor decomposition) in Stata', CEPS/INSTEAD, Differdange, Luxembourg. 

Yaari, M.E. (1988): A Controversal Proposal Concerning Inequality Measurement, Journal of Economic Theory, Vol. 44, pp. 381-397.

}
\author{
Alexander Sohn
}

\seealso{ \code{\link{gini}}
}
%% ~~objects to See Also as \code{\link{help}}, ~~~
\examples{
## without point mass
set.seed(123)
x <- rnorm(1000,10,1)
incs <- seq(1,20,length.out=1000)
dens <- dnorm(incs,10,1)
lower=NULL;upper=NULL;pm0<-NA
gini(x)$Gini
sgini(x,nu=2)$Gini
sgini.den(incs,dens)$Gini


## with point mass
set.seed(123)
x <- c(rep(0,1000),rnorm(1000,10,1))
incs <- c(0,seq(1,20,length.out=1000))
dens <- c(0.5,dnorm(incs[-1],10,1)/2)
gini(x)$Gini
sgini(x,nu=2)$Gini
sgini.den(incs,dens,pm = 0.5)$Gini
 }