\name{Ac3net.commonlinks}
\alias{Ac3net.commonlinks}

\title{
Find the common links between the network lists.
}
\description{
\code{Ac3net.commonlinks} takes two network list matrices and returns directional or undirectional common pairs between the two regarding the first two columns of the networks.
}
\usage{
Ac3net.commonlinks(net1,net2, directed=TRUE)
}

\arguments{
  \item{net1}{A matrix that represents the links between two variables at each row. Namely, the element at row i and column 1 and the element at row i and column 2 are assumed to be paired or linked. There is no connection assumed among rows.}
  \item{net2}{A matrix that represents the links between two variables at each row. Namely, the element at row i and column 1 and the element at row i and column 2 are assumed to be paired or linked. There is no connection assumed among rows.}
  \item{directed}{ It is TRUE by default, which means there is  directionality assumed from column 1 to column 2 at each matrices. Therefore common links are the directionale common links. If it is FALSE, then the pairs assumed to be undirectional and all the common pairs are taken as common. E.g. If there is A--B in net1 and B--A in net2 then they are considered as common.}
}
\details{
\code{Ac3net.commonlinks} takes two network list matrices and based on the first two columns it makes comparison. If the argument directed is equal to TRUE, as in the default case, the it returns only directionally common pairs from column 1 to column 2 of the matrices. E.g. For a pair of A-->B in the first network, if there is also A-->B in the second network then it is taken as common but the opposite direction B-->A is not counted. If the argument directed is equal to FALSE, then B-->A is also counted, namely all the common pairs, with no directional considerations, is taken as common between the two network. 
}
\value{
  \code{Ac3net.commonlinks}  returns a network matrix that correspond to the common pairs between net1 and net2. 
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
# net2
# net <- Ac3net.commonlinks(net1,net2, directed=TRUE)
}

\keyword{misc}
