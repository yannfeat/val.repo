\name{paep}
\alias{paep}
\title{Computing the cumulative distribution function (cdf) of asymmetric exponential power (AEP) distribution.}
\description{Computes the cdf of AEP distribution given by
\deqn{
F_{X}(x|\Theta)=
\frac{1-\epsilon}{2}-\frac{1-\epsilon}{2 \Gamma\bigl(1+\frac{1}{\alpha}\bigr)} \gamma\Bigl(\Big|\frac{\mu-x}{\sigma(1-\epsilon)}\Big|^{\alpha},\frac{1}{\alpha}\Bigr),~{}~x < \mu,
}
\deqn{
F_{X}(x|\Theta)=
\frac{1-\epsilon}{2}+\frac{1+\epsilon}{2 \Gamma\bigl(1+\frac{1}{\alpha}\bigr)} \gamma\Bigl(\Big|\frac{x-\mu}{\sigma(1+\epsilon)}\Big|^{\alpha},\frac{1}{\alpha}\Bigr),~{{}}~x \geq \mu,
}
where \eqn{-\infty<x<+\infty}, \eqn{\Theta=(\alpha,\sigma,\mu,\epsilon)^T} with \eqn{0<\alpha \leq 2}, \eqn{\sigma> 0}, \eqn{-\infty<\mu<\infty}, and \eqn{-1<\epsilon<1}.
}
\usage{paep(x, alpha, sigma, mu, epsilon, log.p = FALSE, lower.tail = TRUE)}
\arguments{
\item{x}{Vector of observations.}
\item{alpha}{Tail thickness parameter.}
\item{sigma}{Scale parameter.}
\item{mu}{Location parameter.}
\item{epsilon}{Skewness parameter.}
\item{log.p}{If \code{TRUE}, then \code{log}\eqn{ \bigl(F_{X}(x|\Theta)\bigr)} is returned.}
\item{lower.tail}{If \code{FALSE}, then \eqn{1-F_{X}(x|\Theta)} is returned.}
}
\value{Computed cdf of AEP distribution at points of vector \eqn{x}.}
\author{Mahdi Teimouri}
\examples{
paep(x = 2, alpha = 1.5, sigma = 1, mu = 0, epsilon = 0.5, log.p = FALSE, lower.tail = TRUE)
}
