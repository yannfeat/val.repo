\name{phi.mult.ref}
\alias{phi.mult.ref}
\title{
Downscaling of failures to greatest common chip size
}
\description{
Function to scale the numbers of failures in burn-in studies of differently sized reference products down to the greatest common chip size of the products and to merge the downscaled information.
}
\usage{
phi.mult.ref(k, n, A.ref, prec = 2, tailcut = 1e-08)
}
\arguments{
  \item{k}{
vector of total numbers of failures for each reference product.
}
  \item{n}{
vector of numbers of inspected devices for each reference product.
}
  \item{A.ref}{
vector of chip sizes for each reference product (in mm^2).
}
  \item{prec}{
precision for greatest common divisor is 10^-\code{prec} (default: 2).
}
\item{tailcut}{
probabilities for scaled failures smaller than \code{tailcut} are set to zero for each reference product (default: 1e-08). Too small values for \code{tailcut} might  cause increased computation times.
}
}

\value{
\item{phi}{data frame with possible numbers of failures \code{k.gcd} and probabilities \code{phi(k.gcd)}. Only failure numbers \code{k.gcd} with \code{phi(k.gcd)}>0 are printed out.}
\item{A.gcd}{greatest common divisor of the sizes of the reference products.}
}
\references{
D. Kurz, H. Lewitschnig and J. Pilz: \emph{Failure Probability Estimation with Differently Sized Reference Products for Semiconductor Burn-in Studies}. Applied Stochastic Models in Business and Industry, 31(5): 732-744, 2015. DOI: 10.1002/asmb.2100.
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}

\seealso{
\code{\link{ci.mult.ref}}
\code{\link{phi.mult.ref.cm}}
\code{\link{ci.mult.ref.cm}}
}
\examples{
k<-c(1,2)
n<-c(10,15)
A.ref<-c(2,3)
phi.mult.ref(k,n,A.ref)

k<-c(1,1)
n<-c(110000,220000)
A.ref<-c(5.21,10.71)
phi.mult.ref(k,n,A.ref)

k<-c(1,2,3,4)
n<-c(10,15,20,30)
A.ref<-c(1,2,3,4)
phi.mult.ref(k,n,A.ref)
}
\keyword{ area }
\keyword{ scaling }
