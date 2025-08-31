\name{gcd.mult.ref}
\alias{gcd.mult.ref}
\title{
Greatest common divisor of chip sizes
}
\description{
Function to compute the greatest common divisor of the chip sizes of the reference products at a fixed precision.
}
\usage{
gcd.mult.ref(A, prec = 2)
}
\arguments{
  \item{A}{
vector of chip sizes for each reference product (in mm^2).
}
  \item{prec}{
precision for greatest common divisor is 10^-\code{prec} (default: 2).
}
}

\value{
\item{A.gcd}{greatest common divisor of the sizes in mm^2 (at specified precision).}
}

\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}

\examples{
A<-c(48, 30, 42) 
gcd.mult.ref(A) 

A<-c(2.2, 3.01, 5)
gcd.mult.ref(A, prec = 2) 
gcd.mult.ref(A, prec = 1) 
gcd.mult.ref(A, prec = 0)
}
\keyword{ greatest }
\keyword{ common }
\keyword{ divisor }
