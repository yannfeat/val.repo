\name{mfp}
\alias{mfp}
\docType{data}
\title{
Psychological Tests for Males and Females
}
\description{
A psychological study consisting of four tests was conducted on males and females group and the results were noted. Since the four tests are correlated and each one is noted for all the individuals, one is interested to know if the mean vector of the test scores is same across the gender group.
}
\usage{data(mfp)}
\format{
  A data frame with 32 observations on the following 8 variables.
  \describe{
    \item{\code{M_y1}}{pictorial inconsistencies for males}
    \item{\code{M_y2}}{paper form board test for males}
    \item{\code{M_y3}}{tool recognition test for males}
    \item{\code{M_y4}}{vocabulary test for males}
    \item{\code{F_y1}}{pictorial inconsistencies for females}
    \item{\code{F_y2}}{paper form board test for females}
    \item{\code{F_y3}}{tool recognition test for females}
    \item{\code{F_y4}}{vocabulary test for females}
  }
}
\examples{
data(mfp)
males <- mfp[,1:4]; females <- mfp[,5:8]
nm <- nrow(males);nf <- nrow(females)
p <- 4; k <- 2
vm <- nm-1; vf <- nf-1
meanm <- colMeans(males); meanf <- colMeans(females)
sigmam <- var(males); sigmaf <- var(females)
sigmapl <- (1/(nm+nf-2))*((nm-1)*sigmam+(nf-1)*sigmaf)
ln_M <- .5*(vm*log(det(sigmam))+vf*log(det(sigmaf))) -.5*(vm+vf)*log(det(sigmapl))
exact_test <- -2*ln_M # the Exact Test
exact_test
}
\keyword{hypothesis testing for equality of covariance matrices}
