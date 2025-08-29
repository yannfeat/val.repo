\name{ns}
\alias{ns}
\docType{data}
\title{
Simulated Sample from Normal Distribution
}
\description{
The data set is used to understand the sampling variation of the score function. The simulated data is available in Pawitan (2001). 
}
\usage{data(ns)}
\format{
  A data frame with 10 observations on the following 20 variables.
  \describe{
    \item{\code{Sample.1}}{a numeric vector}
    \item{\code{Sample.2}}{a numeric vector}
    \item{\code{Sample.3}}{a numeric vector}
    \item{\code{Sample.4}}{a numeric vector}
    \item{\code{Sample.5}}{a numeric vector}
    \item{\code{Sample.6}}{a numeric vector}
    \item{\code{Sample.7}}{a numeric vector}
    \item{\code{Sample.8}}{a numeric vector}
    \item{\code{Sample.9}}{a numeric vector}
    \item{\code{Sample.10}}{a numeric vector}
    \item{\code{Sample.11}}{a numeric vector}
    \item{\code{Sample.12}}{a numeric vector}
    \item{\code{Sample.13}}{a numeric vector}
    \item{\code{Sample.14}}{a numeric vector}
    \item{\code{Sample.15}}{a numeric vector}
    \item{\code{Sample.16}}{a numeric vector}
    \item{\code{Sample.17}}{a numeric vector}
    \item{\code{Sample.18}}{a numeric vector}
    \item{\code{Sample.19}}{a numeric vector}
    \item{\code{Sample.20}}{a numeric vector}
  }
}
\source{
Pawitan, Y. (2001). In All Likelihood. Oxford Science Publications.
}
\references{
Pawitan, Y. (2001). In All Likelihood. Oxford Science Publications.
}
\examples{
library(stats4)
data(ns)
x <- ns[,1]
nlogl <- function(mean,sd) { -sum(dnorm(x,mean=mean,sd=sd,log=TRUE)) }
norm_mle <- mle(nlogl,start=list(mean=median(x),sd=IQR(x)),nobs=length(x))
summary(norm_mle)
}
\keyword{maximum likelihood estimation}
