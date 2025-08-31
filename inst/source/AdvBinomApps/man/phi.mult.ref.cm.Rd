\name{phi.mult.ref.cm}
\alias{phi.mult.ref.cm}
\title{
Downscaling of failures tackled by countermeasures to greatest common chip size
}
\description{
Function to scale failures tackled by countermeasures in burn-in studies of differently sized reference products down to the greatest common chip size of the products and to merge the downscaled information.
}
\usage{
phi.mult.ref.cm(k, n, A.ref, K, theta, prec = 2, tailcut = 1e-08)
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
  \item{K}{
matrix with entries K[j,i] denoting the number of failures of the j-th reference product tackled with the i-th countermeasure. If two or more countermeasures
have the same efficiency, they can be handled as one countermeasure for several failures. If the i-th countermeasure does not apply to the j-th reference product, then set K[j,i]=0. If there is no countermeasure for a failure at all, then it does not need to be considered in \code{K} (the failure itself is already considered in \code{k}).
}
  \item{theta}{
vector of (different) effectivenesses of countermeasures.
}
  \item{prec}{
precision for greatest common divisor is 10^-\code{prec} (default: 2).
}
\item{tailcut}{
probabilities for scaled failures smaller than \code{tailcut} are set to zero for each reference product (default: 1e-08). Too small values for \code{tailcut} might  cause increased computation times.  
}
}

\value{
\item{phi.cm}{data frame with possible number of failures \code{k.gcd} (after the implementation of countermeasures) and probabilities \code{phi.cm(k.gcd)}. Only failure numbers \code{k.gcd} with \code{phi.cm(k.gcd)}>0 are printed out.}
\item{A.gcd}{greatest common divisor of the sizes of the reference products.}
}
\references{
D. Kurz, H. Lewitschnig and J. Pilz: \emph{Failure Probability Estimation with Differently Sized Reference Products for Semiconductor Burn-in Studies}. Applied Stochastic Models in Business and Industry, 31(5): 732-744, 2015. DOI: 10.1002/asmb.2100.

D. Kurz, H. Lewitschnig and J. Pilz: \emph{Decision-Theoretical Model for Failures Tackled by Countermeasures}. IEEE Transactions on Reliability, 63(2): 583-592, 2014. DOI: 10.1109/TR.2014.2315952.
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}

\seealso{
\code{\link{phi.mult.ref}}
\code{\link{ci.mult.ref}}
\code{\link{ci.mult.ref.cm}}
}
\examples{
k<-c(1,2)
n<-c(10,10)
K<-matrix(c(1,0,1,1),2,2,byrow=TRUE)
theta<-c(0.7,0.8)
A.ref<-c(1,2)
phi.mult.ref.cm(k,n,A.ref,K,theta)

k<-c(1,2)
n<-c(110000,220000)
K<-matrix(c(1,0,0,1),2,2,byrow=TRUE) #no CM for one fail!
theta<-c(0.7,0.8)
A.ref<-c(2,3)
phi.mult.ref.cm(k,n,A.ref,K,theta)
}

\keyword{ area }
\keyword{ scaling }
\keyword{ countermeasures }

