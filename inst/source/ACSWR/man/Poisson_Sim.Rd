\name{Poisson_Sim}
\alias{Poisson_Sim}
\title{
Simulation for Poisson Distribution
}
\description{
A simple function to understand the algorithm to simulate (psuedo-)observations from binomial distribution. It is an implementation of the algorithm given in Section 11.3.1 "Simulation from Discrete Distributions". This function is not an alternative to the "rpois" function.
}
\usage{
Poisson_Sim(lambda, n)
}
\arguments{
  \item{lambda}{
rate of the Poisson RV}
  \item{n}{
required number of pseudo-observations}
}

\author{
Prabhanjan N. Tattar}

\seealso{
rpois}
\examples{
set.seed(123)
mean(Poisson_Sim(4,1000))
}
\keyword{Poisson Distribution}
\keyword{Simulation}
