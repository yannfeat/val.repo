\name{fitaep}
\alias{fitaep}
\title{Estimating the parameters of AEP distribution through the expectation-maximization (EM) algorithm}
\description{Estimates the parameters of AEP distribution.}
\usage{fitaep(x, initial = FALSE, starts)}
\arguments{
\item{x}{Vector of observations.}
\item{initial}{By default is \code{FALSE}. If the initial values are given by user, then set \code{initial=TRUE}.}
\item{starts}{If initial values \code{starts}=\eqn{\bigl(\alpha^{(0)}, \sigma^{(0)}, \mu^{(0)}, \epsilon^{(0)} \bigr)}, are given by user, i.e., \code{initial=TURE}, then vector \code{starts} must contain the initial values of the parameter vector, i.e., for starting the EM algorithm.}
}
\value{A list of objects in two parts as
\enumerate{
\item The EM estimator for the parameters of AEP distribution.
\item A sequence of goodness-of-fit measures consist of Akaike Information Criterion (\code{AIC}), Consistent Akaike Information Criterion (\code{CAIC}), Bayesian Information Criterion (\code{BIC}), Hannan-Quinn information criterion (\code{HQIC}), Anderson-Darling (\code{AD}), Cram\'{e}er-von Misses (\code{CVM}), Kolmogorov-Smirnov (\code{KS}), and log-likelihood (\code{log-likelihood}) statistics.
}
}
\references{
A. P. Dempster, N. M. Laird, and D. B. Rubin, 1977. Maximum likelihood from incomplete data via the EM algorithm, \emph{Journal of the Royal Statistical Society Series B}, 39, 1-38.
}
\author{Mahdi Teimouri}
\examples{
x <- raep(n=50, alpha=.8, sigma=1, mu=0, epsilon=0.5)
fitaep(x, initial = FALSE, starts)
}
