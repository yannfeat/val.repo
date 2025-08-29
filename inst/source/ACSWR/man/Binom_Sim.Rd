\name{Binom_Sim}
\alias{Binom_Sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation for Binomial Distribution
}
\description{
A simple function to understand the algorithm to simulate psuedo-observations from binomial distribution. It is an implementation of the algorithm given in Section 11.3.1. This function is not an alternative to the rbinom function. 
}
\usage{
Binom_Sim(size, p, N)
}
\arguments{
  \item{size}{
Size of the binomial distribution
}
  \item{p}{
Denotes the probability of success
}
  \item{N}{
The number of observations required from b(n,p)
}
}
\author{
Prabhanjan N. Tattar
}
\note{
This function is to simply explain the algorithm described in the text. For efficient results, the user should use the rbinom function. 
}
\seealso{
rbinom
}
\examples{
Binom_Sim(10,0.5,100)
}
\keyword{Simulation}
\keyword{Binomial Distribution}
