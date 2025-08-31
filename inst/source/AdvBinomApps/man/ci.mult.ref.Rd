\name{ci.mult.ref}
\alias{ci.mult.ref}
\title{
Upper Clopper-Pearson confidence limits for area scaling with differently sized reference products
}
\description{
Function to compute upper Clopper-Pearson confidence limits of failure probabilities on the basis of differently sized reference products. Optionally, the required numbers of additional inspections for each reference product to reach a predefined target failure probability of the follower product are returned.
}
\usage{
ci.mult.ref(k, n, A.ref, A.follow, alpha = 0.1, p.target = 1, 
prec = 2, tailcut = 1e-08, tol = 1e-12)
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
tolerance of \code{uniroot}-function used for computing failure probability per greatest common chip size (default: 1e-12).
}
}


\value{
\item{p.ref}{vector of upper Clopper-Pearson confidence limits for each reference product (without the other reference products).}
\item{p.mm}{upper Clopper-Pearson confidence limit of the failure probability per mm^2 (on the basis of all reference products).}
\item{p.follow}{upper Clopper-Pearson confidence limit of the failure probability of the follower product (on the basis of all reference products).}
\item{n.add}{vector of required numbers of additional inspections for each reference product in order to reach p.target for the follower product.}
}
\references{
D. Kurz, H. Lewitschnig and J. Pilz: \emph{Failure Probability Estimation with Differently Sized Reference Products for Semiconductor Burn-in Studies}. Applied Stochastic Models in Business and Industry, 31(5): 732-744, 2015. DOI: 10.1002/asmb.2100.
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}



\seealso{
\code{\link{phi.mult.ref}}
\code{\link{ci.mult.ref.cm}}
\code{\link{phi.mult.ref.cm}}
}
\examples{
k<-c(1,2)
n<-c(110000,138000)
A.ref<-c(5.21,10.71)
A.follow<-8.5
p.target<-20e-06
ci.mult.ref(k,n,A.ref,A.follow,p.target=p.target)

k<-c(1,2,2)
n<-c(110000,138000,170000)
A.ref<-c(5.21,10.71,7.89)
A.follow<-8.5
p.target<-20e-06
ci.mult.ref(k,n,A.ref,A.follow,p.target=p.target)
}
\keyword{ area }
\keyword{ scaling }
