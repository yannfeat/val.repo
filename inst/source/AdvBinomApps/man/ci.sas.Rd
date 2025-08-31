\name{ci.sas}
\alias{ci.sas}
\title{
Separate area scaling for upper Clopper-Pearson confidence limits
}
\description{
Function to compute upper Clopper-Pearson confidence limits of failure probabilities of follower products by means of separate area scaling (SAS). Furthermore, the validity of the SAS in comparison to the classical area scaling (CAS) is evaluated. Optionally, the required numbers of additional inspections of the reference product in order to reach the predefined target failure probability of the follower product according to the CAS and SAS are returned.
}
\usage{
ci.sas(k, n, A.ref, A.follow, alpha = 0.1, p.target = 1, atol = 1e-08)
}
\arguments{
  \item{k}{
vector of numbers of failures for each subset on reference product (total number of failures on reference product = sum of entries of \code{k}).
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
\item{p.cas}{vector of upper Clopper-Pearson confidence limits of failure probabilities for each subset on reference product according to CAS.}
\item{p.sas}{vector of upper Clopper-Pearson confidence limits of failure probabilities for each subset on reference product according to SAS.}
\item{p.follow.cas}{upper Clopper-Pearson confidence limit of the failure probability of the follower product according to CAS.}
\item{p.follow.sas}{upper Clopper-Pearson confidence limit of the failure probability of the follower product according to SAS.}
\item{delta}{evidence factor against CAS.}
\item{n.add.cas}{required number of additional inspections of the reference product in order to reach \code{p.target} of the follower product according to CAS.}
\item{n.add.sas}{required number of additional inspections of the reference product in order to reach \code{p.target} of the follower product according to SAS.}
}
\references{
D. Kurz, H. Lewitschnig and J. Pilz: \emph{An Advanced Area Scaling Approach for Semiconductor Burn-in}. Microelectronics Reliability, 55(1): 129-137, 2015. DOI: 10.1016/j.microrel.2014.09.007.
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}

\seealso{
\code{\link{ci.sas.cm}}
}
\examples{
k<-c(1,0)
n<-100000
A.ref<-c(1,1.5)
A.follow<-c(1,2)
p.target<-20e-06
ci.sas(k,n,A.ref,A.follow,0.1,p.target)

k<-c(2,1,0)
n<-100000
A.ref<-c(2,3,4)
A.follow<-c(1,2,3)
p.target<-20e-06
ci.sas(k,n,A.ref,A.follow,0.1,p.target)

k<-c(1,1,0,2)
n<-100000
A.ref<-c(1,1,1,1)
A.follow<-c(3,4,5,6)
p.target<-20e-06
ci.sas(k,n,A.ref,A.follow,0.1,p.target)
}
\keyword{ separate }
\keyword{ area }
\keyword{ scaling }
