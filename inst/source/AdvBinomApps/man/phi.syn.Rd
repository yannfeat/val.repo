\name{phi.syn}
\alias{phi.syn}
\title{
Assembling of devices
}
\description{
Function to compute probability of having a certain number of failures out of \code{min(n)} devices, which are randomly assembled out of a certain number of chip subsets.
}
\usage{
phi.syn(k, n)
}
\arguments{
  \item{k}{
vector of numbers of failures for each subset.
}
  \item{n}{
vector of numbers of inspections for each subset.
}
}
\value{
\item{phi}{data frame with possible numbers of failures (out of \code{min(n)} assembled devices) and associated probabilities.}
}
\references{
D. Kurz, H. Lewitschnig and J. Pilz: \emph{Failure probability estimation under additional subsystem information with application to semiconductor burn-in}. Resubmitted to: Journal of Applied Statistics, 2015.
}
\author{
Daniel Kurz, Horst Lewitschnig

Maintainer: Horst Lewitschnig \email{horst.lewitschnig@infineon.com}
}
\seealso{
\code{\link{ci.syn}}
\code{\link{phi.syn.cm}}
\code{\link{ci.syn.cm}}
}
\examples{
k<-c(1,2)
n<-c(10,15)
phi.syn(k,n)

k<-c(0,1,1,1)
n<-c(110000,220000,150000,330000)
phi.syn(k,n)
}
\keyword{ chip }
\keyword{ synergies }
