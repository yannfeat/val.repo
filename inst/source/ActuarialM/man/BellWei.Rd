\name{BellW distribution}
\alias{vBellW}
\alias{eBellW}
\title{Bell Weibull distribution
}
\description{Computes the value at risk and expected shortfall based on the Bell Weibull (BellW) distribution.
The CDF of the Bell G family is as follows:
\deqn{
H(x)=\frac{1-\exp\left[-e^{\lambda}\left(1-e^{-\lambda K(x)}\right)\right]}{1-\exp\Bigl(1-e^{\lambda}\Bigr)};\qquad\lambda>0,
}
where K(x) represents the baseline Weibull CDF, it is given by
\deqn{
K(x)=1-\exp(-\alpha x^{\beta});\qquad\alpha,\beta>0.
}
By setting K(x) in the above Equation, yields the CDF of the BellW distribution.
The following expression can be used to calculate the VaR:
\deqn{VaR_{p}(X)=\left[\frac{-1}{\alpha}\ln\left(\frac{1}{\lambda}\left\{ \ln\left[\ln\left(1-p\left\{ 1-\exp(1-e^{\lambda})\right\} \right)+e^{\lambda}\right]\right\} \right)\right]^{1/\beta};\qquad p\in(0,1).}

The ES can be computed from the following expression:
\deqn{ES_{p}(X)=\frac{1}{p}\intop_{0}^{p}\left[\frac{-1}{\alpha}\ln\left(\frac{1}{\lambda}\left\{ \ln\left[\ln\left(1-z\left\{ 1-\exp(1-e^{\lambda})\right\} \right)+e^{\lambda}\right]\right\} \right)\right]^{1/\beta}dz.
}
}


\usage{
vBellW(p, alpha, beta, lambda, log.p = FALSE, lower.tail = TRUE)
eBellW(p, alpha, beta, lambda)
}

\arguments{
\item{p}{
A vector of probablities \eqn{p \in (0,1)}.
}

\item{lambda}{
The strictly positive parameter of the Bell G family of distributions \eqn{\lambda > 0}.
}
\item{alpha}{
The strictly positive scale parameter of the baseline Weibull distribution (\eqn{\alpha > 0}).
}
\item{beta}{
The strictly positive shape parameter of the baseline Weibull distribution (\eqn{\beta > 0}).
}

\item{lower.tail}{if FALSE then 1-H(x) are returned and quantiles are computed for 1-p.
}

\item{log.p}{if TRUE then log(H(x)) are returned and quantiles are computed for exp(p).
}

}

\details{The functions allow to compute the value at risk and the expected shortfall of the BellW distribution.
}

\value{
vBellW gives the values at risk.
eBellW gives the expected shortfall.
}

\references{
Fayomi, A., Tahir, M. H., Algarni, A., Imran, M., & Jamal, F. (2022). A new useful exponential model with applications to quality control and actuarial data. Computational Intelligence and Neuroscience, 2022.

Alsadat, N., Imran, M., Tahir, M. H., Jamal, F., Ahmad, H., & Elgarhy, M. (2023). Compounded Bell-G class of statistical models with applications to COVID-19 and actuarial data. Open Physics, 21(1), 20220242.

Hallinan Jr, A. J. (1993). A review of the Weibull distribution. Journal of Quality Technology, 25(2), 85-93.

Rinne, H. (2008). The Weibull distribution: a handbook. CRC press.
}

\author{
Muhammad Imran and M.H. Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and M.H. Tahir \email{mht@iub.edu.pk}.
}


%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{eBellEW}, \link{eBellE}
}
}

\examples{
p=runif(10,min=0,max=1)
vBellW(p,1,2,1)
eBellW(p,1,2,1)
}

