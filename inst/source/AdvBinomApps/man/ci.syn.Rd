\name{ci.syn}
\alias{ci.syn}
\title{
Upper Clopper-Pearson confidence limits under chip synergies
}
\description{
Function to compute upper Clopper-Pearson confidence limits of failure probabilities on the basis of burn-in studies for each subset of a chip. Optionally, the required number of additional inspections for reaching a predefined target failure probability is returned.
}
\usage{
ci.syn(k, n, alpha = 0.1, p.target = 1, tol = 1e-10)
}
\arguments{
  \item{k}{
vector of numbers of failures for each subset.
}
  \item{n}{
vector of numbers of inspections for each subset.
}
  \item{alpha}{
alpha-level (1-alpha confidence level, default: 0.1).
}
  \item{p.target}{
target failure probability (optional).
}
\item{tol}{
tolerance of \code{uniroot}-function used for computing \code{p.hat} (default: 1e-10).
}
}
\value{
\item{p.hat}{upper Clopper-Pearson confidence limit of the failure probability of the assembled devices.}
\item{n.add}{required number of additional inspections of each subset for reaching \code{p.target}.}
}
\references{
D. Kurz, H. Lewitschnig and J. Pilz: \emph{Failure probability estimation under additional subsystem information with application to semiconductor burn-in}. Resubmitted to: Journal of Applied Statistics, 2015.
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}
\seealso{
\code{\link{phi.syn}}
\code{\link{phi.syn.cm}}
\code{\link{ci.syn.cm}}
}
\examples{
k<-c(0,1)
n<-c(110000,330000)
ci.syn(k,n,0.1,20e-06)

k<-c(1,0,1,5)
n<-c(330000,240000,240000,400000)
ci.syn(k,n,0.1,20e-06)
}

\keyword{ chip }
\keyword{ synergies }
