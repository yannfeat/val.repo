\name{powertestplot}
\alias{powertestplot}
\title{
A Function to Plot the Power of a UMP Test for Normal Distribution
}
\description{
A simple function for obtaining the plot of power of UMP test. 
}
\usage{
powertestplot(mu0, sigma, n, alpha)
}
\arguments{
  \item{mu0}{
the value of mean
}
  \item{sigma}{
standard deviation
}
  \item{n}{
sample size
}
  \item{alpha}{
size of the test
}
}
\author{
Prabhanjan N. Tattar
}
\seealso{
t.test
}
\examples{
UMPNormal <- function(mu0, sigma, n,alpha)  {
  qnorm(alpha)*sigma/sqrt(n)+mu0
}
UMPNormal(mu0=0, sigma=1,n=1,alpha=0.5)
powertestplot <- function(mu0,sigma,n,alpha)	{
  mu0seq <- seq(mu0-3*sigma, mu0+3*sigma,(6*sigma/100))
  betamu <- pnorm(sqrt(n)*(mu0seq-mu0)/sigma-qnorm(1-alpha))
  plot(mu0seq,betamu,"l",xlab=expression(mu),ylab="Power of UMP Test",
    main = expression(paste("H:",mu <= mu[0]," vs K:",mu>mu[0])))
  abline(h=alpha)
  abline(v=mu0)
}
powertestplot(mu0=0,sigma=1,n=10,alpha=0.05)
# H:mu > mu_0 vs K: mu <= mu_0
UMPNormal <- function(mu0, sigma, n,alpha)	{
  mu0-qnorm(alpha)*sigma/sqrt(n)
}
UMPNormal(mu0=0, sigma=1,n=1,alpha=0.5)
powertestplot <- function(mu0,sigma,n,alpha)	{
  mu0seq <- seq(mu0-3*sigma, mu0+3*sigma,(6*sigma/100))
  betamu <- pnorm(sqrt(n)*(mu0-mu0seq)/sigma-qnorm(1-alpha))
  plot(mu0seq,betamu,"l",xlab=expression(mu),ylab="Power of UMP Test",
    main=expression(paste("H:",mu >= mu[0]," vs K:",mu<mu[0])))
  abline(h=alpha)
  abline(v=mu0)
}
powertestplot(mu0=0,sigma=1,n=10,alpha=0.05)
}
\keyword{power plot}
