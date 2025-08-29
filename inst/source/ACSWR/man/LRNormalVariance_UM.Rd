\name{LRNormalVariance_UM}
\alias{LRNormalVariance_UM}
\title{
Likelihood ratio test for the variance of normal distribution with mean is unknown
}
\description{
This function returns the LR test for the variance of normal distribution with the mean being unknown. Refer Chapter 7 for more details. }
\usage{
LRNormalVariance_UM(x, sigma0, alpha)
}
\arguments{
  \item{x}{
the vector of sample values
}
  \item{sigma0}{
the standard deviation of interest under the hypothesis
}
  \item{alpha}{
the required level of significance
}
}
\author{
Prabhanjan Tattar}
\examples{
LRNormalVariance_UM(rnorm(20),1,0.05)
}
\keyword{Likelihood ratio test}
\keyword{normal distribution}
