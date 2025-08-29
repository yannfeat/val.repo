\name{Ac3net.getDirectedOrDualLinks}
\alias{Ac3net.getDirectedOrDualLinks}

\title{
Find the unique directed or dual links in the network.
}
\description{
\code{Ac3net.getDirectedOrDualLinks} takes a network list matrix and returns a unique network; either directionals or the duals. 
}
\usage{
Ac3net.getDirectedOrDualLinks(net1, dual_=FALSE)
}

\arguments{
  \item{net1}{A matrix that represents the links between two variables at each row. Namely, the element at row i and column 1 and the element at row i and column 2 are assumed to be paired or linked. There is no connection assumed among rows.}
  \item{dual_}{ It is FALSE by default, which means there is  directionality assumed from column 1 to column 2 at each matrices. A--B and B--A is cases filtered out. If TRUE, then only the dual pairs selected.}
}
\details{
\code{Ac3net.getDirectedOrDualLinks} takes a network list matrix and returns a unique network; either directionals or the duals. If directed=TRUE then it is the unique pairs from column 1 to column 2. If directed=FALSE then it is the unique dual pairs (selects if and only if A--B and B--A exist). 
directional or undirectional common pairs between the two regarding the first two columns of the networks. 
}
\value{
  \code{Ac3net.getDirectedOrDualLinks} returns a unique network matrix.
}
\references{
G. Altay,"Directed Conservative Causal Core Gene Networks", bioRxiv, 2018.
G. Altay, F. Emmert-Streib, "Inferring the conservative causal core of gene regulatory networks", BMC Systems Biology (2010) 4:132. 
}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{Ac3net.maxmim}}, \code{\link{Ac3net.cutoff}},
}

\examples{
# net1
# net <-  Ac3net.getDirectedOrDualLinks(net1, dual_=FALSE)
}

\keyword{misc}
