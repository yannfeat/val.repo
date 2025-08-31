\name{qaep}
\alias{qaep}
\title{Computing the quantile function of asymmetric exponential power (AEP) distribution.}
\description{Computes the quantile function of AEP distribution given by
\deqn{
F_{X}^{-1}(u|\Theta)=
\mu-\sigma(1-\epsilon)\biggl[\frac{\gamma\bigl(\frac{1-\epsilon-2u}{1-\epsilon},\frac{1}{\alpha}\bigr)}{\Gamma\bigl(\frac{1}{\alpha}\bigr)}\biggr]^{\frac{1}{\alpha}},~{{}}~u\leq \frac{1-\epsilon}{2},
}
\deqn{
F_{X}^{-1}(u|\Theta)=
\mu+\sigma(1+\epsilon)\biggl[\frac{\gamma\bigl(\frac{2u+\epsilon-1}{1+\epsilon},\frac{1}{\alpha}\bigr)}{\Gamma\bigl(\frac{1}{\alpha}\bigr)}\biggr]^{\frac{1}{\alpha}},~{{}}~u> \frac{1-\epsilon}{2}.\\
}
where
\eqn{-\infty<x<+\infty}, \eqn{\Theta=(\alpha,\sigma,\mu,\epsilon)^T} with \eqn{0<\alpha \leq 2, \sigma> 0}, \eqn{-\infty<\mu<\infty}, \eqn{-1<\epsilon<1},
and
\deqn{\gamma(u,\nu) =\int_{0}^{u}t^{\nu-1}\exp\bigl\{-t\bigr\}dt, ~\nu>0.}
}
\usage{qaep(u, alpha, sigma, mu, epsilon)}
\arguments{
\item{u}{Numeric vector with values in \eqn{(0,1)} whose quantiles are desired.}
\item{alpha}{Tail thickness  parameter.}
\item{sigma}{Scale parameter.}
\item{mu}{Location parameter.}
\item{epsilon}{Skewness parameter.}
}
\value{
A vector of length \code{n}, consists of the random generated values from AEP distribution.
}
\author{Mahdi Teimouri}
\examples{
qaep(runif(1), alpha = 1.5, sigma = 1, mu = 0, epsilon = 0.5)
}
