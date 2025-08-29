\name{ST_Unordered}
\alias{ST_Unordered}
\title{
Simulating Random Observations from an Arbitrary Distribution (ordered probabilities)}
\description{
Simulation observations from an arbitrary discrete distribution with probabilities arranged in desending/ascending order. 
}
\usage{
ST_Unordered(N, x, p_x)
}
\arguments{
  \item{N}{
number of required random observations
}
  \item{x}{
the possible values of the RV
}
  \item{p_x}{
the probability vector associated with x
}
}
\author{
Prabhanjan N. Tattar
}
\seealso{
sample
}
\examples{
N <- 1e2
x <- 1:10
p_x <- c(0.05,0.17,0.02,0.14,0.11,0.06,0.05,0.04,0.17,0.19)
ST_Unordered(N,x,p_x)
}
\keyword{Arbitrary discrete distribution}
\keyword{Simulation unordered probabilities}
