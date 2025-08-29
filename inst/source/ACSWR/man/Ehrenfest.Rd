\name{Ehrenfest}
\alias{Ehrenfest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate transition probability matrix of Ehrenfest model
}
\description{
The Ehrenfest model is an interesting example of a Markov chain. Though the probabilities in decimals are not as interesting as expressed in fractions, the function will help the reader generate the transition probability matrices of 2n balls among two urns. 
}
\usage{
Ehrenfest(n)
}
\arguments{
  \item{n}{
2n will be the number of balls in the urns. 
}
}
\details{
In this experiment there are i balls in Urn I, and remaining 2n-i balls in Urn II. Then at any instance, the probability of selecting a ball from Urn I and placing it in Urn II is i/2n, and the other way of placing a ball from Urn II to Urn I is (2n-i)/2n. At each instant we let the number i of balls in the Urn I to be the state of the system. Thus, the state space is S = { 0, 1, 2, \ldots, 2n }. Then we can pass from state i only to either of the states i-1 or i+1.
Here, S = {0, 1, \ldots, 2n}. 
}
\author{
Prabhanjan N. Tattar 
}

\examples{
Ehrenfest(2)
Ehrenfest(3)
}
\keyword{Ehrenfest}
\keyword{Markov chain}
