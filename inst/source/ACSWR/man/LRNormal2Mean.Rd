\name{LRNormal2Mean}
\alias{LRNormal2Mean}
\title{
Likelihood Ratio Test for Equality of Means when Variance Unknown}
\description{
This function sets up the likelihood ratio test for equality of means when the variance term is unknown. Refer Chapter 7 for more details. }
\usage{
LRNormal2Mean(x, y, alpha)
}
\arguments{
  \item{x}{
Observations from Population 1
}
  \item{y}{
Observations from Population 2
}
  \item{alpha}{
Size alpha test
}
}
\details{
Likelihood ratio test is setup through this function. For more details, refer Chapter 7 of the book. 
}
\author{
Prabhanjan N. Tattar
}
\seealso{
t.test
}
\examples{
lisa <- c(234.26, 237.18, 238.16, 259.53, 242.76, 237.81, 250.95, 277.83)
mike <- c(187.73, 206.08, 176.71, 213.69, 224.34, 235.24)
LRNormal2Mean(mike,lisa,0.05)
}
\keyword{Likelihood Ratio test}
\keyword{Normal Distribution}
