\name{Ac3net}
\alias{Ac3net}

\title{
Implement Ac3net algorithm with a single function.
}
\description{
\code{Ac3net} A quick way to implement Ac3net algorithm with a single function.
}
\usage{
Ac3net(DataOrMim, processed=FALSE, ratio_ = 0.002, PCmincutoff=0.6,
PCmaxcutoff=0.96, cutoff=0, estmethod='pearson', pval=1, 
iterations=10, MTC=FALSE, MTCmethod="BH" )
}

\arguments{
  \item{DataOrMim}{It can be an adjacency matrix or data matrix. If adjacency matrix then the element at row i and column j corresponds to the correlation or mutual information between variables row i and column j. Rows and columns must have the variable names. Also the 'processed' parameter must be set to TRUE. If it is data matrix then rows are variables and columns are the samples. Rows must have the variable names. Also the 'processed' parameter is already set to FALSE as the default usage case is data matrix. }
  \item{processed}{Set to FALSE if the input is a data matrix, set to TRUE if the input is adjacency matrix.}  
  \item{ratio_}{The ratio of all the elements in the input adjacency matrix (even if the matrix is symmetric), which the user considers as significant portion.}
  \item{PCmincutoff}{The minimum absolute Pearson correlation value ( by default but can be different) where the below of is not considered as significant.}
  \item{PCmaxcutoff}{The maximum absolute Pearson correlation value ( by default but can be different) where all the above of is considered as significant.}
  \item{cutoff}{Default is 0 means inactive. If set, it must be betwen 0 and 1, and the input ise used directly as cutoff.}
  \item{estmethod}{The default method is 'pearson' assuming that the data was normalized (at least with Log-2). If the data is not normalized, its recomended setting is 'spearman'.}  
  \item{pval}{Default is 1, means do not use p-values cutoff. If < 1, then the corresponding cutoff is calculated and the adjacency matrix is filtered based on this cutoff calculated by the input pval p-value.}  
  \item{iterations}{Number of iterations to get null distribuiton from multiple shuffling of the data.}  
  \item{MTC}{If TRUE,as the default status, the the returned p-value matrix has the adjusted p-values}
  \item{MTCmethod}{The Multiple Testing Correction method. BH is default. See R p.adjust function for options.}
}
\details{
\code{Ac3net} The input can be an adjacency matrix or data matrix. If adjacency matrix then the element at row i and column j corresponds to the correlation or mutual information between variables row i and column j. Rows and columns must have the variable names. Also the 'processed' parameter must be set to TRUE. If it is data matrix then rows are variables and columns are the samples. Rows must have the variable names. Also the 'processed' parameter is already set to FALSE as the default usage case is data matrix.
If MTC is set to TRUE then it sliminates non-significants based on adjusted p-values but this process takes too long.
If MTC is set to FALSE as default then cutoff parameter is used.
If the cutoff parameter is set to a value other than 0, then it is used as cutoff directly.
If the cutoff wanted to be calculated very quicly, for comparison purposes for example, then first it is set to 0 as default and then it is calculated between minimum and maximum input cutoffs as the value that correspond to the input ratio_ parameter from top to down.
}
\value{
  \code{Ac3net} returns a network matrix where the first column is the variable names of the input adjacency matrix and second columns have the variables names that correspond to the absolute maximum value for the variable in the first column.  
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
# data(expdata)
# ac3net.net <- Ac3net(DataOrMim=expdata, processed=FALSE, ratio_ = 0.002, PCmincutoff=0.6,
#              PCmaxcutoff=0.96, cutoff=0, estmethod='pearson', pval=1, 
# iterations=10, MTC=FALSE, MTCmethod="BH" )
}

\keyword{misc}
