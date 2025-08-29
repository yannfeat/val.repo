\name{ps}
\alias{ps}
\docType{data}
\title{
Simulated Sample from Poisson Distribution
}
\description{
The data set is used to understand the sampling variation of the score function. The simulated data is available in Pawitan (2001). 
}
\usage{data(ps)}
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
data(ps)
n <- 10
sample_means <- colMeans(ps)
poisson_score_fn <- function(theta,xbar) n*(xbar-theta)/theta
theta <- seq(from=2,to=8,by=0.2)
plot(theta,sapply(theta,poisson_score_fn,xbar=sample_means[1]),"l",xlab=
  expression(lambda),ylab=expression(S(lambda)),ylim=c(-5,15))
title(main="B: Score Function Plot of the Poisson Model")
for(i in 2:20) 
lines(theta,sapply(theta,poisson_score_fn,xbar=sample_means[i]),"l")
abline(v=4)
abline(h=0)}
\keyword{maximum likelihood estimator}
