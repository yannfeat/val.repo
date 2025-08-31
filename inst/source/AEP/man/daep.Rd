\name{daep}
\alias{daep}
\title{Computing the probability density function (pdf) of asymmetric exponential power (AEP) distribution.}
\description{The pdf of AEP distribution given by
\deqn{
f_{X}(x|\Theta)=
\frac{1}{2\sigma \Gamma\bigl(1+\frac{1}{\alpha}\bigr)}\exp\biggl\{-\bigg|\frac{\mu-x}{\sigma(1-\epsilon)}\bigg|^{\alpha}\biggr\},~~{}~x < \mu,
}
\deqn{
f_{X}(x|\Theta)=
\frac{1}{2\sigma \Gamma\bigl(1+\frac{1}{\alpha}\bigr)}\exp\biggl\{-\bigg|\frac{x-\mu}{\sigma(1+\epsilon)}\bigg|^{\alpha}\biggr\},~~{}~x \geq\mu,
}
where \eqn{-\infty<x<+\infty}, \eqn{\Theta=(\alpha,\sigma,\mu,\epsilon)^T} with \eqn{0<\alpha \leq 2}, \eqn{\sigma> 0}, \eqn{-\infty<\mu<\infty}, \eqn{-1<\epsilon<1},
and
\deqn{\Gamma(u)=\int_{0}^{+\infty} x^{u-1}\exp\bigl\{-x\bigr\}dx,~u>0.}
}
\usage{daep(x, alpha, sigma, mu, epsilon, log = FALSE)}
\arguments{
\item{x}{Vector of observation of requested random realizations.}
\item{alpha}{Tail thickness parameter.}
\item{sigma}{Scale parameter.}
\item{mu}{Location parameter.}
\item{epsilon}{Skewness parameter.}
\item{log}{If \code{TRUE}, then \code{log}\eqn{\bigl(f_{X}(x|\Theta)\bigr)} is returned.}
}
\details{The AEP distribution is a special case of asymmetric exponential power distribution studied by Dongming and Zinde-Walsh (2009) when \eqn{p_1=p_2=\alpha}. Also, note that if \eqn{\epsilon=0}, then the AEP distribution turns into a normal distribution with mean \eqn{\mu} and standard deviation \eqn{\sqrt{2}\sigma}. When \eqn{\alpha=2}, the AEP distribution is a slight variant of epsilon-skew-normal distribution introduced by Mudholkar and Huston (2001).}

\value{
Computed pdf of AEP distribution at points of vector \eqn{x}.
}
\references{
Z. Dongming and V. Zinde-Walsh, 2009. Properties and estimation of asymmetric exponential power distribution, \emph{Journal of Econometrics}, 148(1), 86-99.

G. S. Mudholkar and A. D. Huston, 2001. The epsilon-skew-normal distribution for analyzing near-normal data, \emph{Journal of Statistical Planning and
Inference}, 83, 291-309.
}
\author{Mahdi Teimouri}
\examples{
daep(x = 2, alpha = 1.5, sigma = 1, mu = 0, epsilon = 0.5, log = FALSE)
}
