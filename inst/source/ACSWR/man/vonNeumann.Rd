\name{vonNeumann}
\alias{vonNeumann}
\title{
von Neumann Random Number Generator}
\description{
The "vonNeumann" function implements the von Neumann random generator as detailed in Section 11.2. 
}
\usage{
vonNeumann(x, n)
}
\arguments{
  \item{x}{
the initial seed
}
  \item{n}{
number of required observations}
}

\author{
Prabhanjan N. Tattar
}
\examples{
vonNeumann(x=11,n=10)
vonNeumann(x=675248,n=10)
vonNeumann(x=8653,n=100)
}
\keyword{von Neumann}
\keyword{random generator}