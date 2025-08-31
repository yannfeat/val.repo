\name{ci.mult.ref.cm}
\alias{ci.mult.ref.cm}
\title{
Upper Clopper-Pearson confidence limits for area scaling with differently sized reference products and countermeasures
}
\description{
Function to compute upper Clopper-Pearson confidence limits of failure probabilities on the basis of differently sized reference products and failures tackled by countermeasures. Optionally, the required numbers of additional inspections for each reference product to reach a predefined target failure probability of the follower product are returned.
}
\usage{
ci.mult.ref.cm(k, n, A.ref, A.follow, K, theta, alpha = 0.1, 
p.target = 1, prec = 2, tailcut = 1e-08, tol = 1e-12)
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
  \item{A.follow}{
size of follower product.
}
  \item{K}{
matrix with entries K[j,i] denoting the number of failures of the j-th reference product tackled with the i-th countermeasure. If two or more countermeasures
have the same efficiency, they can be handled as one countermeasure for several failures. If the i-th countermeasure does not apply to the j-th reference product, then set K[j,i]=0. If there is no countermeasure for a failure at all, then it does not need to be considered in \code{K} (the failure itself is already considered in \code{k}).
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
  \item{prec}{
precision for greatest common divisor is 10^-\code{prec} (default: 2).
}
\item{tailcut}{
probabilities for scaled failures smaller than \code{tailcut} are set to zero for each reference product (default: 1e-08). Too small values for \code{tailcut} might  cause increased computation times.
}
\item{tol}{
tolerance of \code{uniroot}-function used for computing failure probability per greatest common chip size with countermeasures (default: 1e-12).
}
}


\value{
\item{p.ref.cm}{vector of upper Clopper-Pearson confidence limits for each reference product with countermeasures (without the other reference products).}
\item{p.mm.cm}{upper Clopper-Pearson confidence limit of the failure probability per mm^2 with countermeasures (on the basis of all reference products).}
\item{p.follow.cm}{upper Clopper-Pearson confidence limit of the failure probability of the follower product with countermeasures (on the basis of all reference products).}
\item{n.add.cm}{vector of required numbers of additional inspections for each reference product in order to reach p.target for the follower product with countermeasures.}
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
\code{\link{phi.mult.ref.cm}}
}
\examples{
#Reference product 1: 1 failure - failure tackled with 80% efficiency.
#Reference product 2: 2 failures - 1 failure tackled with 80%, 
#1 failure with 60% efficiency.
k<-c(1,2)
K<-matrix(c(1,0,1,1),2,2,byrow=TRUE)
n<-c(110000,138000)
theta<-c(0.8,0.6)
A.ref<-c(5.21,10.71)
A.follow<-8.5
p.target<-20e-06
ci.mult.ref.cm(k,n,A.ref,A.follow,K,theta,p.target=p.target)

#Reference product 1: 1 failure - failure tackled with 20% efficiency.
#Reference product 2: 2 failures - 1 failure tackled with 20%, 
#1 failure with 40% efficiency.
#Reference product 3: 2 failures - both tackled with 60% efficiency.
k<-c(1,2,2)
n<-c(110000,138000,170000)
K<-matrix(c(1,0,0,1,1,0,0,0,2),3,3,byrow=TRUE)
theta<-c(0.2,0.4,0.6)
A.ref<-c(5.21,10.71,7.89)
A.follow<-8.5
p.target<-20e-06
ci.mult.ref.cm(k,n,A.ref,A.follow,K,theta,p.target=p.target)

#Reference product 1: 1 failure - failure tackled with 20% efficiency.
#Reference product 2: 2 failures - 1 failure tackled with 40% efficiency, 
#1 failure without countermeasure.
#Reference product 3: 3 failures - 1 failure tackled with 60% efficiency, 
#2 failures without countermeasures.
k<-c(1,2,3)
n<-c(110000,138000,170000)
K<-matrix(c(1,0,0,0,1,0,0,0,1),3,3,byrow=TRUE)
theta<-c(0.2,0.4,0.6)
A.ref<-c(5.21,10.71,7.89)
A.follow<-8.5
p.target<-20e-06
ci.mult.ref.cm(k,n,A.ref,A.follow,K,theta,p.target=p.target)

}
\keyword{ area }
\keyword{ scaling }
\keyword{ countermeasures }
