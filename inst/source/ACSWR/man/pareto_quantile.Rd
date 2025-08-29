\name{pareto_quantile}
\alias{pareto_quantile}
\title{
Quantile of Pareto RV}
\description{
A simple function is given here which returns the quantiles for a Pareto RV. A more efficient implementation is obtainable in the function "qpareto" from the "VGAM" package.}
\usage{
pareto_quantile(p, scale, shape)
}
\arguments{
  \item{p}{
the percentiles required}
  \item{scale}{
scale of Pareto RV 
}
  \item{shape}{
shape of Pareto RV
}
}
\author{
Prabhanjan N. Tattar
}
\seealso{
VGAM::qpareto
}
\examples{
pareto_quantile(c(0.05,0.95),scale=10000,shape=10)
}
\keyword{Quantile of Pareto RV}
