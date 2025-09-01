\encoding{UTF-8}
\name{hbay.fun}
\alias{hbay.fun}
\alias{hbay.fun.default}

\title{
Local Bayesian procedure for bandwidth selection
}
\description{
The (S3) generic function \code{hbay.fun} computes the local Bayesian procedure for bandwidth selection. 
}
\usage{
hbay.fun(Vec, \dots)
\method{hbay.fun}{default}(Vec, x = NULL, \dots)
}

\arguments{
  \item{Vec}{The data sample from which the estimate is to be computed.

}
  \item{x}{The points of the grid where the density  is to be estimated.

}
  \item{\dots}{ Further arguments for (non-default) methods.

}

}
\details{
\code{hbay.fun} implements the choice of the bandwidth \eqn{h} using the local Bayesian approach
  of a kernel density estimator.
  
}
\value{
Returns the bandwidth selected using the local Bayesian procedure.
}
\references{
Chen, S. X. (1999). Beta kernels estimators for density functions, \emph{Computational Statistics and Data Analysis}  \bold{31},  131 - 145.

 Zougab, N., Adjabi, S. and Kokonendji, C.C. (2014). Bayesian approach in nonparametric count regression with binomial kernel, \emph{ Communications in Statistics - Simulation and Computation }  \bold{43}, 1052 - 1063.



}
\author{
W. E. Wansouwé, S. M. Somé and C. C. Kokonendji 
}

\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
