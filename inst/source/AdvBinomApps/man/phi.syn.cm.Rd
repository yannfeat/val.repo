\name{phi.syn.cm}
\alias{phi.syn.cm}
\title{
Assembling of devices with countermeasures
}
\description{
Function to compute the probability of having a certain number of failures out of \code{min(n)} randomly assembled devices with countermeasures.
}
\usage{
phi.syn.cm(k, n, K, theta)
}
\arguments{
  \item{k}{
vector of total numbers of failures for each subset.
}
  \item{n}{
vector of numbers of inspections for each subset.
}
  \item{K}{
matrix with entries K[j,i] denoting the number of failures of the j-th subset tackled with the i-th countermeasure. If two or more countermeasures
have the same efficiency, they can be handled as one countermeasure for several failures. If the i-th countermeasure does not apply to the j-th subset, then set K[j,i]=0. If there is no countermeasure for a failure at all, then it does not need to be considered in \code{K} (the failure itself is already considered in \code{k}).
}
  \item{theta}{
vector of (different) effectivenesses of countermeasures.
}
}
\value{
\item{phi.cm}{data frame with possible numbers of failures (out of \code{min(n)} assembled devices) and associated probabilities considering the implemented countermeasures.}
}
\references{
D. Kurz, H. Lewitschnig and J. Pilz: \emph{Failure probability estimation under additional subsystem information with application to semiconductor burn-in}. Resubmitted to: Journal of Applied Statistics, 2015.

D. Kurz, H. Lewitschnig and J. Pilz: \emph{Decision-Theoretical Model for Failures Tackled by Countermeasures}. IEEE Transactions on Reliability, 63(2): 583-592, 2014. DOI: 10.1109/TR.2014.2315952.
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}
\seealso{
\code{\link{phi.syn}}
\code{\link{ci.syn}}
\code{\link{ci.syn.cm}}
}
\examples{
k<-c(0,1)
K<-matrix(c(0,1),2,1,byrow=TRUE)
theta<-0.8
n<-c(110000,330000)
phi.syn.cm(k,n,K,theta)

k<-c(1,1,2)
K<-matrix(c(0,0,0,1,1,1),3,2,byrow=TRUE)
theta<-c(0.7,0.5)
n<-c(10,15,20)
phi.syn.cm(k,n,K,theta)

}
\keyword{ chip }
\keyword{ synergies }
\keyword{ countermeasures }
