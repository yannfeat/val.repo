\name{LRNormalMean_KV}
\alias{LRNormalMean_KV}
\title{
Likelihood ratio test for equality of mean when the variance is known
}
\description{ Likelihood ratio test for equality of mean when the variance is known for a sample from normal distribution is setup here. For details, refer Chapter 7 of the book. 
}
\usage{
LRNormalMean_KV(x, mu0, alpha, sigma)
}
\arguments{
  \item{x}{
the variable of interest
}
  \item{mu0}{
the mean of interest 
}
  \item{alpha}{
size of the LR test
}
  \item{sigma}{
value of the known standard deviation
}
}
\author{
Prabhanjan N. Tattar 
}
\seealso{
t.test }
\examples{
data(hw)
LRNormalMean_KV(hw$Height,mu0=70, alpha=0.05, sigma=sqrt(20))
}
\keyword{Likelihood Ratio Test}
\keyword{Normal Distribution}
