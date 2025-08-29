\name{sgini}
\alias{sgini}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Single-parameter Gini Coefficient
%%  ~~function to do ... ~~
}
\description{  This function computes the Single-parameter Gini coefficient (a.k.a. generalised Gini coefficient or extended Gini coefficient) for a vector of observations.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sgini(x, nu = 2, w = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of observations.
%%     ~~Describe \code{x} here~~
}
  \item{nu}{ a scalar entailing the parameter that tunes the degree of the policy maker's aversion to inequality. See Yaari, 1988 for details.
%%     ~~Describe \code{nu} here~~
}
  \item{w}{ a vector of weights.
%%     ~~Describe \code{w} here~~
}
}
\value{ 
\item{Gini }{the Gini coefficient for the sample.}
\item{bcGini }{the bias-corrected Gini coefficient for the sample.}
}
\references{
van Kerm, P. (2009): sgini - Generalized Gini and Concentration coefficients (with factor decomposition) in Stata', CEPS/INSTEAD, Differdange, Luxembourg. 

Yaari, M.E. (1988): A Controversal Proposal Concerning Inequality Measurement, Journal of Economic Theory, Vol. 44, pp. 381-397.
%% ~put references to the literature/web site here ~
}
\author{ Alexander Sohn
}

\examples{
set.seed(123)
x <- rnorm(100,10,1)
gini(x)$Gini
sgini(x,nu=2)$Gini
}