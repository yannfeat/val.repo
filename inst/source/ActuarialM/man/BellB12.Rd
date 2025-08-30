\name{BellB12 distribution}
\alias{vBellB12}
\alias{eBellB12}
\title{Bell Burr-12 distribution
}
\description{Computes the value at risk and expected shortfall based on the Bell Burr-12 (BellB12) distribution.
The CDF of the Bell G family is as follows:
\deqn{
H(x)=\frac{1-\exp\left[-e^{\lambda}\left(1-e^{-\lambda K(x)}\right)\right]}{1-\exp\Bigl(1-e^{\lambda}\Bigr)};\qquad\lambda>0,
}
where K(x) represents the baseline Burr-12 CDF, it is given by
\deqn{
K\left(x\right)=1-\left[1+\left(\frac{x}{a}\right)^{b}\right]^{-k};\qquad a,b,k>0.
}
By setting K(x) in the above Equation, yields the CDF of the BellB12 distribution.
The following expression can be used to calculate the VaR:
\deqn{
VaR_{p}(X)=a\left(\left[\left(\frac{1}{\lambda}\left[\ln\left(\left[\ln\left(1-p\left[1-\exp\Bigl(1-e^{\lambda}\Bigr)\right]\right)\right]+e^{\lambda}\right)\right]\right)\right]^{-1/k}-1\right)^{1/b},}

where \eqn{p \in (0,1)}. The ES can be computed from the following expression:
\deqn{ES_{p}(X)=\frac{a}{p}\intop_{0}^{p}\left(\left[\left(\frac{1}{\lambda}\left[\ln\left(\left[\ln\left(1-z\left[1-\exp\Bigl(1-e^{\lambda}\Bigr)\right]\right)\right]+e^{\lambda}\right)\right]\right)\right]^{-1/k}-1\right)^{1/b}dz.}
}


\usage{
vBellB12(p, a, b, k, lambda, log.p = FALSE, lower.tail = TRUE)
eBellB12(p, a, b, k, lambda)
}

\arguments{
\item{p}{
A vector of probablities \eqn{p \in (0,1)}.
}
\item{lambda}{
The strictly positive parameter of the Bell G family (\eqn{\lambda > 0}).
}
\item{a}{
The strictly positive scale parameter of the baseline Burr-12 distribution (\eqn{a > 0}).
}
\item{b}{
The strictly positive shape parameter of the baseline Burr-12 distribution (\eqn{b > 0}).
}
\item{k}{
The strictly positive shape parameter of the baseline Burr-12 distribution (\eqn{k > 0}).
}
\item{lower.tail}{if FALSE then 1-H(x) are returned and quantiles are computed for 1-p.
}
\item{log.p}{if TRUE then log(H(x)) are returned and quantiles are computed for exp(p).
}
}

\details{The functions allow to compute the value at risk and the expected shortfall of the BellB12 distribution.
}

\value{
vBellB12 gives the value at risk.
eBellB12 gives the expected shortfall.
}

\references{
Fayomi, A., Tahir, M. H., Algarni, A., Imran, M., & Jamal, F. (2022). A new useful exponential model with applications to quality control and actuarial data. Computational Intelligence and Neuroscience, 2022.

Alsadat, N., Imran, M., Tahir, M. H., Jamal, F., Ahmad, H., & Elgarhy, M. (2023). Compounded Bell-G class of statistical models with applications to COVID-19 and actuarial data. Open Physics, 21(1), 20220242.

Zimmer, W. J., Keats, J. B., & Wang, F. K. (1998). The Burr XII distribution in reliability analysis. Journal of quality technology, 30(4), 386-394.



}

\author{
Muhammad Imran and M.H. Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and M.H. Tahir \email{mht@iub.edu.pk}.
}


%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{eBellBX}, \link{eBellL}
}
}

\examples{
p=runif(10,min=0,max=1)
vBellB12(p,1,1,2,1.2)
eBellB12(p,1,1,2,1.2)
}

