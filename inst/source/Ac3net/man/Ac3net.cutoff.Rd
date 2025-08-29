\name{Ac3net.cutoff}
\alias{Ac3net.cutoff}

\title{
Calculates a quick cutoff value from an adjacency matrix
}
\description{
\code{Ac3net.cutoff} A quick way to get a very rough estimate of the cutoff value for the significants of the input adjacency matrix
}
\usage{
Ac3net.cutoff(mim, ratio_ = 0.002, PCmincutoff=0.6, PCmaxcutoff=0.96)
}

\arguments{
  \item{mim}{An adjacency matrix, where the element at row i and column j corresponds to the correlation or mutual information between variables row i and column j. Row and columns of the matrix must have the variable names.}
  \item{ratio_}{The ratio of all the elements in the input adjacency matrix (even if the matrix is symmetric), which the user considers as significant portion.}
  \item{PCmincutoff}{The minimum absolute Pearson correlation value ( by default but can be different) where the below of is not considered as significant.}
  \item{PCmaxcutoff}{The maximum absolute Pearson correlation value ( by default but can be different) where all the above of is considered as significant.}
  
}
\details{
\code{Ac3net.cutoff} takes an adjacency matrix, ratio and minimum absolute Pearson correlation information. Then return a cutoff value that either correspond to the input ratio or the minimum.
}
\value{
  \code{Ac3net.cutoff} returns a scaler as a very rough estimate of the cutoff value for the significants of the input adjacency matrix. 
}
\references{
G. Altay,"Directed Conservative Causal Core Gene Networks", bioRxiv, 2018.
G. Altay, F. Emmert-Streib, "Inferring the conservative causal core of gene regulatory networks", BMC Systems Biology (2010) 4:132. 
}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{Ac3net.maxmim}}, \code{\link{Ac3net.commonlinks}},
}

\examples{
 data(expdata)
 mim <- cor(t(expdata), method = 'pearson')
 cutoff <- Ac3net.cutoff(mim=mim, ratio_ = 0.002,
              PCmincutoff=0.6, PCmaxcutoff=0.96)
}

\keyword{misc}
