\name{stationdistTPM}
\alias{stationdistTPM}
\title{
A function which will return the stationary distribution of an ergodic Markov chain
}
\description{
This function returns the stationary distribution of an ergodic Markov chain. For details, refer Chapter 11 of the book. 
}
\usage{
stationdistTPM(M)
}
\arguments{
  \item{M}{
a transition probability matrix (TPM)
}
}
\author{
Prabhanjan N. Tattar}
\seealso{
eigen
}
\examples{
P <- matrix(nrow=3,ncol=3) # An example
P[1,] <- c(1/3,1/3,1/3)
P[2,] <- c(1/4,1/2,1/4)
P[3,] <- c(1/6,1/3,1/2)
stationdistTPM(P)
}
\keyword{stationary distribution}
\keyword{Ergodic Markov chain}
