\name{ci.sas.cm}
\alias{ci.sas.cm}
\title{
Separate area scaling for upper Clopper-Pearson confidence limits with countermeasures
}
\description{
Function to compute upper Clopper-Pearson confidence limits of failure probabilities of follower products by means of separate area scaling (SAS) with failures tackled by countermeasures. Furthermore, the validity of the SAS in comparison to the classical area scaling (CAS) is evaluated taking into account the implemented countermeasures. Optionally, the required numbers of additional inspections of the reference product in order to reach the predefined target failure probability of the follower product according to the CAS and SAS with countermeasures are returned.
}
\usage{
ci.sas.cm(k, n, A.ref, A.follow, K, theta, alpha = 0.1, 
p.target = 1, atol = 1e-08)
}
\arguments{
  \item{k}{
vector of total numbers of failures for each subset on reference product (total number of failures on reference product = sum of entries of \code{k}).
}
  \item{n}{
number of inspected devices in burn-in study of reference product.
}
  \item{A.ref}{
vector of sizes for each subset on reference product (in mm^2).
}
  \item{A.follow}{
vector of sizes for each subset on follower product (in mm^2).
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
target failure probability of follower product (optional).
}
\item{atol}{
tolerance of \code{multiroot}-function used for computing \code{p.sas} (default: 1e-08). For further details, see description of \code{multiroot}-function.
}
}

\details{
Function makes use of \code{multiroot}-function of the package \code{rootSolve} to solve non-linear equation system for the subset failure probabilities. 

Appropriate starting values for \code{multiroot} are chosen automatically. 

In case of non-convergence of \code{multiroot}-function, \code{NA} is returned.

Function designed and verified for number of subsets < 6.
}



\value{
\item{p.cas.cm}{vector of upper Clopper-Pearson confidence limits of failure probabilities for each subset on reference product according to CAS with countermeasures.}
\item{p.sas.cm}{vector of upper Clopper-Pearson confidence limits of failure probabilities for each subset on reference product according to SAS with countermeasures.}
\item{p.follow.cas.cm}{upper Clopper-Pearson confidence limit of the failure probability of the follower product according to CAS with countermeasures.}
\item{p.follow.sas.cm}{upper Clopper-Pearson confidence limit of the failure probability of the follower product according to SAS with countermeasures.}
\item{delta.cm}{evidence factor against CAS with countermeasures.}
\item{n.add.cas.cm}{required number of additional inspections of the reference product in order to reach \code{p.target} of the follower product according to CAS with countermeasures.}
\item{n.add.sas.cm}{required number of additional inspections of the reference product in order to reach \code{p.target} of the follower product according to SAS with countermeasures.}
}

\references{
D. Kurz, H. Lewitschnig and J. Pilz: \emph{An Advanced Area Scaling Approach for Semiconductor Burn-in}. Microelectronics Reliability, 55(1): 129-137, 2015. DOI: 10.1016/j.microrel.2014.09.007.

D. Kurz, H. Lewitschnig and J. Pilz: \emph{Decision-Theoretical Model for Failures Tackled by Countermeasures}. IEEE Transactions on Reliability, 63(2): 583-592, 2014. DOI: 10.1109/TR.2014.2315952.
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}

\seealso{
\code{\link{ci.sas}}
}
\examples{
#Reference product: 1 failure.
#Subset 1: 1 failure - failure tackled with 80% efficiency.
#Subset 2: no failures.
k<-c(1,0)
K<-matrix(c(1,0),2,1)
n<-100000
A.ref<-c(1,1)
theta<-0.8
A.follow<-c(1,2)
p.target<-20e-06
ci.sas.cm(k,n,A.ref,A.follow,K,theta,0.1,p.target)

#Reference product: 3 failures.
#Subset 1: 2 failures - 1 failure tackled with 80%, 
#1 failure with 70% efficiency.
#Subset 2: 1 failure - failure tackled with 70% efficiency.
#Subset 3: no failures.
k<-c(2,1,0)
K<-matrix(c(1,1,0,1,0,0),3,2,byrow=TRUE)
n<-100000
A.ref<-c(2,3,4)
theta<-c(0.8,0.7)
A.follow<-c(1,2,3)
p.target<-20e-06
ci.sas.cm(k,n,A.ref,A.follow,K,theta,0.1,p.target)

#Reference product: 4 failures.
#Subset 1: 2 failures - 1 failure tackled with 80% efficiency, 
#1 failure without countermeasure.
#Subset 2: 1 failure - failure tackled with 70% efficiency.
#Subset 3: 1 failure - failure without countermeasure.
k<-c(2,1,1)
K<-matrix(c(1,0,0,1,0,0),3,2,byrow=TRUE)
n<-100000
A.ref<-c(2,3,4)
theta<-c(0.8,0.7)
A.follow<-c(1,2,3)
p.target<-20e-06
ci.sas.cm(k,n,A.ref,A.follow,K,theta,0.1,p.target)

}
\keyword{ separate }
\keyword{ area }
\keyword{ scaling }
\keyword{ countermeasures }
