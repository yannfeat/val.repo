\name{Ac3net.maxmim}
\alias{Ac3net.maxmim}

\title{
Find the maximum valued partner of each variable.
}
\description{
\code{Ac3net.maxmim} takes an adjacency matrix and returns the pairs with the absolute maximum correlated partner of each variable on the rows.
}
\usage{
Ac3net.maxmim(mim_, net_= TRUE, cutoff_=0)
}

\arguments{
  \item{mim_}{An adjacency matrix, where the element at row i and column j corresponds to the correlation or mutual information between variables row i and column. Row and columns of the matrix must have the variable names.}
  \item{net_}{If equal to TRUE, then it returns a sorted list of interaction of Ac3net algorithm. The significance cutoff values needs to be set in this case. In this case the resulting network directions is shown by Source and Target columns to make it look intuitive and avoid confusion in general. If equal to FALSE, then it only returns the maximum valued partner for each gene by the rows of the matrix, not sorted and no filtered by the cutoff. The gene of interest is in the second column and its maximum valued partner is in the first column, similar to above.}
  \item{cutoff_}{A scaler cutoff magnitude above which is considered as significant dependecny score.}
}
\details{
\code{Ac3net.maxmim} takes an adjacency matrix and returns the pairs with the absolute maximum correlated partner.
}
\value{
  \code{Ac3net.maxmim} returns a network matrix where the second column is the variable names of the row names of the input adjacency matrix and second columns have the variables names that correspond to the absolute maximum value for the variables in the second column. If net_ is TRUE, then this list of gene pairs are further filtered out based on the cutoff and sorted, which results the inferred network of Ac3net algorithm. If net_ is FALSE, the it only returns the maximum valued partner of each variable of the input matrix.
}
\references{
G. Altay,"Directed Conservative Causal Core Gene Networks", bioRxiv, 2018.
G. Altay, F. Emmert-Streib, "Inferring the conservative causal core of gene regulatory networks", BMC Systems Biology (2010) 4:132. 
}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{Ac3net.commonlinks}}, \code{\link{Ac3net.cutoff}},
}

\examples{
# data(expdata)
# mim <- cor(t(expdata), method = 'pearson')
# mim <- Ac3net.filtersames(mim)
# cutoff <- Ac3net.cutoff(mim=mim, ratio_ = 0.002, PCmincutoff=0.6, PCmaxcutoff=0.96)
# Ac3net.net <-  Ac3net.maxmim(mim_= mim, net_=TRUE, cutoff_= cutoff)
}

\keyword{misc}
