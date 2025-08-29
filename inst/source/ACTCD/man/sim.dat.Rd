\name{sim.dat}
\alias{sim.dat}
\docType{data}
\title{
Simulated data
}
\description{
The data are simulated based on the DINA model (Junker & Sijtsma, 2001).
}
\usage{data(sim.dat)}
\format{
A \eqn{500 \times 14} binary matrix. 
}
\details{
This data set contains responses of 500 examinees to 14 items and is used to demonstrate the functions in this package. The DINA model with a given Q-matrix (\code{\link{sim.Q}}) was used to generate data and the true values for guessing and slipping parameters are fixed to 0.2.
}

\references{

Junker, B., & Sijtsma, K. (2001). Cognitive Assessment Models with Few Assumptions, and Connections with Nonparametric Item Response Theory. \emph{Applied Psychological Measurement, 25}(3), 258-272.

}

\examples{
data(sim.dat)
}
\keyword{datasets}
