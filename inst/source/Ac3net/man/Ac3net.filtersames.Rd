\name{Ac3net.filtersames}
\alias{Ac3net.filtersames}

\title{
The correlations of the same variables are set to zero.
}
\description{
\code{Ac3net.filtersames} The correlations of the same variables are set to zero as they are likly to have the maximum correlations already.
}
\usage{
Ac3net.filtersames(mim)
}

\arguments{
  \item{mim}{An adjacency matrix, where the element at row i and column j corresponds to the correlation or mutual information between variables row i and column j. Row and columns of the matrix must have the variable names. Row names must be available.}
}
\details{
\code{Ac3net.filtersames} takes an adjacency matrix with variable names are on the rows. Then it sets 0 for the self correlations of the variables by checking their names.
}
\value{
  \code{Ac3net.filtersames} returns an adjacency matrix with no self dependency. 
}
\references{
G. Altay,"Directed Conservative Causal Core Gene Networks", bioRxiv, 2018.
}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{Ac3net.maxmim}}, \code{\link{Ac3net.commonlinks}},
}

\examples{
# data(expdata)
# mim <- cor(t(expdata), method = 'pearson')
# mim <- Ac3net.filtersames(mim)
}

\keyword{misc}
