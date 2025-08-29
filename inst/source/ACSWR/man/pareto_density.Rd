\name{pareto_density}
\alias{pareto_density}
\title{
Pareto density
}
\description{
A simple function is given here which returns the density function values for a Pareto RV. A more efficient implementation is obtainable in the function "dpareto" from the "VGAM" package. }
\usage{
pareto_density(x, scale, shape)
}
\arguments{
  \item{x}{
the x value
}
  \item{scale}{
the scale parameter of Pareto RV
}
  \item{shape}{
the shape parameter of Pareto RV
}
}
\author{
Prabhanjan N. Tattar}
\seealso{
VGAM::dpareto
}
\examples{
m <- 9184
n <- 103
b <- 10000
K <- 10
theta <- seq(1000,20000,500)
plot(theta,as.numeric(sapply(theta,pareto_density,scale=b,shape=K)),"l",
     xlab=expression(theta),ylab="The Posterior Density")
(n+1)*m/n
}
\keyword{Pareto Density}
