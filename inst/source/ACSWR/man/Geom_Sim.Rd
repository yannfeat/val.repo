\name{Geom_Sim}
\alias{Geom_Sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation for Geometric Distribution
}
\description{
A simple function to understand the algorithm to simulate (psuedo-)observations from binomial distribution. It is an implementation of the algorithm given in Section 11.3.1 "Simulation from Discrete Distributions", and as such the function is not an alternative to the "rgeom" function. }
\usage{
Geom_Sim(p, n)
}
\arguments{
  \item{p}{
probability of success
}
  \item{n}{
number of pseudo-observations required
}
}
\details{
To simulate a random number from geometric RV, we make use of the algorithm described in the book. 
}
\author{
Prabhanjan N. Tattar
}
\seealso{
rgeom
}
\examples{
mean(Geom_Sim(0.01,10))
}
\keyword{Geometric distribution}
\keyword{Simulation}
