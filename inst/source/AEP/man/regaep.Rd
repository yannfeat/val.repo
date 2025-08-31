\name{regaep}
\alias{regaep}
\title{Robust linear regression analysis when error term follows AEP distribution}
\description{Estimates parameters of the multiple linear regression model through EM algorithm when error term follows AEP distribution. The regression model is given by
\deqn{y_{i}=\beta_{0}+\beta_{1} x_{i1}+\cdots+ \beta_{k} x_{ik}+\nu_{i},~ i=1,\cdots,n,}
where \eqn{{\boldsymbol{\beta}}=\bigl(\beta_{0},\beta_{1},\cdots,\beta_{k}\bigr)^{T}} are the
regression coefficients and \eqn{\nu_i} is the error term follows a zero-location AEP distibution.
}
\usage{regaep(y, x)}
\arguments{
\item{y}{Vector of response observations of length \eqn{n}.}
\item{x}{An \eqn{n\times k} array of covariate(s).}
}
\value{
A list of estimated regression coefficients, summary of residuals, F statistic, R-square (\eqn{R^2}), adjusted R-square, and inverted observed Fisher information matrix.
}
\references{
A. P. Dempster, N. M. Laird, and D. B. Rubin, 1977. Maximum likelihood from incomplete data via the EM algorithm, \emph{Journal of the Royal Statistical Society Series B}, 39, 1-38.
}
\author{Mahdi Teimouri}
\examples{
x <- seq(-5, 5, 0.1)
y <- 2 + 2*x + raep( length(x), alpha = 1, sigma = 0.5, mu = 0, epsilon = 0.5)
regaep(y, x)
}

