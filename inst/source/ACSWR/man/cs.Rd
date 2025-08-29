\name{cs}
\alias{cs}
\docType{data}
\title{
Random Samples from Cauchy Distribution
}
\description{
The data set is used to understand the sampling variation of the score function. The simulated data is available in Pawitan (2001).
}

\usage{data(cs)}
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
\references{
Pawitan, Y. (2001). In All Likelihood. Oxford Science Publications.
}
\examples{
data(cs)
n <- 10
cauchy_score_fn  <-  function(mu,x)
      sum(2*(x-mu)/(1+(x-mu)^{2}))
mu <- seq(from=-15,to=20,by=0.5)
plot(mu,sapply(mu,cauchy_score_fn,x=cs[,1]),"l",xlab=expression(mu),
ylab=expression(S(mu)),ylim=c(-10,10))
title(main="D: Score Function Plot of Cauchy Model")
for(i in 2:20) lines(mu,sapply(mu,
cauchy_score_fn,x=cs[,i]),"l")
abline(v=4)
abline(h=0)
}
\keyword{Cauchy random samples, score function}
