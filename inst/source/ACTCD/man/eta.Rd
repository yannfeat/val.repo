\name{eta}
\alias{eta}


\title{
Ideal Response Patterns for all possible attribute profiles
}
\description{
This function is used to calculate ideal response patterns for all possible attribute profiles based on the DINA model (Junker & Sijtsma, 2001) or conjunctive-type cognitive diagnostic models.
}
\usage{
eta(K, J, Q)
}

\arguments{
  \item{K}{
The number of attributes.
}
  \item{J}{
The number of items.
}
  \item{Q}{
A required \eqn{J \times K} binary item-by-attribute association matrix (Q-matrix), where \eqn{K} is the number of attributes. The \eqn{j^{th}} row of the matrix is an indicator vector, 1 indicating attributes are required and 0 indicating attributes are not required to master item \eqn{j}.
}
}

\value{
A \eqn{2^K \times J} binary matrix will be returned. Each row of ideal response patterns is corresponding to each of the \eqn{2^K} possible attribute patterns, which can be obtained from \code{\link{alpha}}.
}
\references{

Junker, B., & Sijtsma, K. (2001). Cognitive Assessment Models with Few Assumptions, and Connections with Nonparametric Item Response Theory. \emph{Applied Psychological Measurement, 25}(3), 258-272.

}


\seealso{
\code{\link{alpha}}
}
\examples{
# Generating ideal response patterns
data(sim.Q)
K <- ncol(sim.Q)
J <- nrow(sim.Q)
IRP <- eta(K, J, sim.Q) 
}