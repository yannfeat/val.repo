\name{Ac3net.MTC}
\alias{Ac3net.MTC}

\title{
Return adjusted p-values of the adjancency matrix.
}
\description{
\code{Ac3net.MTC} Return adjusted p-values of the adjancency matrix.
}
\usage{
Ac3net.MTC(data, iterations=10, MTC=TRUE, MTCmethod="BH",
estmethod='pearson')
}

\arguments{
  \item{data}{Data matrix where rows are variables and columns are the samples. Rows must have the variable names}
  \item{iterations}{Number of iterations to get null distribuiton from multiple shuffling of the data.}  
  \item{MTC}{If TRUE,as the default status, the the returned p-value matrix has the adjusted p-values}
  \item{MTCmethod}{The Multiple Testing Correction method. BH is default. See R p.adjust function for options.}
  \item{estmethod}{The default method is 'pearson' assuming that the data was normalized (at least with Log-2). If the data is not normalized, its recomended setting is 'spearman'.}  
}
\details{
\code{Ac3net.MTC} takes a data matrix and returns the adjusted p-values matrix that correspond to the adjacency matrix of the input data. This way varios p-values can be tried to get different cutoff values without re-calculating the p-values.
}
\value{
  \code{Ac3net.MTC} returns a default adjusted p-values matrix that corresponds to the input adjacency matrix. If MTC is set to FALSE then it returns raw p-values.  
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
#data(expdata)
#mim.pvals <- Ac3net.MTC(data = expdata, iterations=10,
#            MTC=TRUE, MTCmethod="BH", estmethod='pearson')
}

\keyword{misc}
