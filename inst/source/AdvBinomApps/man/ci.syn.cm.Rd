\name{ci.syn.cm}
\alias{ci.syn.cm}
\title{
Upper Clopper-Pearson confidence limits under chip synergies and countermeasures
}
\description{
Function to compute upper Clopper-Pearson confidence limits of failure probabilities on the basis of burn-in studies with countermeasures for each subset of a chip. Optionally, the required number of additional inspections for reaching a predefined target failure probability with countermeasures is returned.
}
\usage{
ci.syn.cm(k, n, K, theta, alpha = 0.1, p.target = 1, tol = 1e-10)
}
\arguments{
  \item{k}{
vector of numbers of failures for each subset.
}
  \item{n}{
vector of numbers of inspections for each subset.
}
  \item{K}{
matrix with entries K[j,i] denoting the number of failures in the j-th subset tackled with the i-th countermeasure. If two or more countermeasures have the same efficiency, they can be handled as one countermeasure for several failures. If the i-th countermeasure does not apply to the j-th subset, then set K[j,i]=0. If there is no countermeasure for a failure at all, then it does not need to be considered in \code{K} (the failure itself is already considered in \code{k}).
}
  \item{theta}{
vector of (different) effectivenesses of countermeasures.
}
  \item{alpha}{
alpha-level (1-alpha confidence level, default: 0.1).
}
  \item{p.target}{
target failure probability (optional).
}
\item{tol}{
tolerance of \code{uniroot}-function used for computing \code{p.hat.cm} (default: 1e-10).
}
}


\value{
\item{p.hat.cm}{upper Clopper-Pearson confidence limit of the failure probability of the assembled devices with countermeasures.}
\item{n.add.cm}{required number of additional inspections of each subset for reaching \code{p.target} with countermeasures.}
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
\code{\link{phi.syn.cm}}
}
\examples{
#Subset 1: no failures.
#Subset 2: 1 failure - failure tackled with 80% efficiency.
k<-c(0,1)
K<-matrix(c(0,1),2,1,byrow=TRUE)
theta<-0.8
n<-c(110000,330000)
ci.syn.cm(k,n,K,theta,0.1,20e-06)

#Subset 1: 1 failure - failure tackled with 80% efficiency.
#Subset 2: 1 failure - failure tackled with 70% efficiency.
#Subset 3: 2 failures - 1 failure tackled with 80%, 
#1 failure with 70% efficiency.
k<-c(1,1,2)
K<-matrix(c(1,0,0,1,1,1),3,2,byrow=TRUE)
theta<-c(0.8,0.7)
n<-c(110000,150000,220000)
ci.syn.cm(k,n,K,theta,0.1,20e-06)

#Subset 1: 1 failure - failure tackled with 80% efficiency.
#Subset 2: 1 failure - failure without countermeasure.
#Subset 3: 2 failures - 1 failure tackled with 70% efficiency, 
#1 failure without countermeasure.
k<-c(1,1,2)
K<-matrix(c(1,0,0,0,0,1),3,2,byrow=TRUE)
theta<-c(0.8,0.7)
n<-c(110000,150000,220000)
ci.syn.cm(k,n,K,theta,0.1,20e-06)

}
\keyword{ chip }
\keyword{ synergies }
\keyword{ countermeasures }

