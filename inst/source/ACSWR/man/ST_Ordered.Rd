\name{ST_Ordered}
\alias{ST_Ordered}
\title{
Simulating Random Observations from an Arbitrary Distribution}
\description{
An implementation of the algorithm for simulation of observations from an arbitrary discrete distribution is provided here. 
}
\usage{
ST_Ordered(N, x, p_x)
}
\arguments{
  \item{N}{
number of required random observations}
  \item{x}{
the possible values of the RV}
  \item{p_x}{
the probability vector associated with x
}
}

\author{
Prabhanjan N. Tattar}
\seealso{
sample
}
\examples{
N <- 1e4
x <- 1:10
p_x <- c(0.05,0.17,0.02,0.14,0.11,0.06,0.05,0.04,0.17,0.19)
table(ST_Ordered(N, x, p_x))
}
\keyword{Arbitrary discrete distribution}
\keyword{Simulation unordered probabilities}
