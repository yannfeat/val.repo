\name{labeling}
\alias{labeling}
\title{
labeling for clusters
}
\description{
This function is used to label the clusters obtained from \code{\link{cd.cluster}}.
}
\usage{
labeling(Y, Q, cd.cluster.object, method = c("2b","2a","1","3"),perm=NULL)
}

\arguments{
  \item{Y}{
A required \eqn{N \times J} response matrix with binary elements (1=correct, 0=incorrect), where \eqn{N} is the number of examinees and \eqn{J} is the number of items.
}
  \item{Q}{
A required \eqn{J \times K} binary item-by-attribute association matrix (Q-matrix), where \eqn{K} is the number of attributes. The \eqn{j^{th}} row of the matrix is an indicator vector, 1 indicating attributes are required and 0 indicating attributes are not required to master item \eqn{j}.
}
  \item{cd.cluster.object}{
An object of \code{cd.cluster}. 
}
  \item{method}{
The algorithm used for labeling. It should be one of "1","2a", "2b" and "3" corresponding to four different labeling methods in Chiu and Ma (2013). The default is "2b". See details for more information.
}
  \item{perm}{
The data matrix of the partial orders of the attribute patterns. 
}
}
\details{
Because cluster analysis such as \emph{K}-means or HACA can only classify examinees into unlabeled clusters, labeling algorithms are needed to identify the underlying attribute patterns of each latent cluster. Four labeling algorithms proposed in Chiu and Ma (2013) can be implemented using this function. 

The first method is the Inconsistency Index method (\code{method="1"}). The Inconsistency Index, \eqn{IC}, quantifies the amount of deviation of an ordering of clusters due to a specific \eqn{\bm{W}} (See details in \code{\link{cd.cluster}}) from an arrangement of clusters that is suggested by simple assumptions about the (possible) underlying model. Among all feasible assignments of attribute patterns to clusters, the one that minimizes \eqn{IC} is chosen. Refer to Chiu and Ma (2013) for details. Note that this method appears to be more time-consuming when \eqn{K} is large and thus only the cases of \eqn{K=3} and \eqn{K=4} are implemented in the current function. To implement this algorithm, the partial order matrix of the attribute patterns should be provided. See \code{\link{perm}} for details. 

For \code{method="2a"} and \code{method="2b"}, the label of a latent class is obtained by minimizing the average distance between observed responses and ideal responses. Specifically, let \eqn{\bm{y}=(y_1, y_2, \ldots, y_J)} be the observed response pattern for a particular examinee and \eqn{\bm{\eta}=(\eta_1,\eta_2,\ldots,\eta_J)} be the ideal response pattern corresponding to a particular attribute pattern \eqn{\bm{\alpha}}. The Weighted Hamming distance \eqn{d} between \eqn{\bm{y}} and \eqn{\bm{\eta}} is given by
\deqn{
d(\bm{y}, \bm{\eta})=\sum_{j=1}^J\frac{1}{\bar{p_j}(1-\bar{p_j})}|y_j-\eta_j|.
}
where \eqn{\bar{p_j}} denotes the proportion correction on the \eqn{j^{th}} item.
Then the best label or attribute pattern (\eqn{\hat{\bm{\alpha}}}) can be obtained through
\deqn{
\hat{\bm{\alpha}}=\mbox{arg} \min_{\bm{\alpha}_k \in \Omega}D.
}
where \eqn{D} is the average weighted Hamming distance within each cluster and \eqn{\Omega} is the set of \eqn{\bm{\alpha}}. In practice, the largest cluster will be labeled first and the smallest cluster will be labeled last.

For \code{method="2a"}, The selected label \eqn{\bm{\alpha}} will be eliminated from \eqn{\Omega} after each labeling iteration, implying that different clusters will obtain different labels. 

For \code{method="2b"}, The selected label \eqn{\bm{\alpha}} will not be eliminated from \eqn{\Omega} after each labeling iteration, implying that different clusters may obtain the same label. 

For \code{method="3"}, it combines the technique of the partial order and  \code{"2a"} method such that some labels can be eliminated from \eqn{\Omega} before each labeling iteration. Refer to Chiu and Ma (2013) for details.

It should be noted that \code{method} \code{"1"}, \code{"2a"} and \code{"3"} all assume that different latent clusters are distinct in nature, which means different clusters will be given different labels using these methods. But \code{method} \code{"2b"} relaxes this assumption and allow the same label for different clusters. In addition, \code{method} \code{"1"} and \code{"3"} may be used when number of clusters is \eqn{2^K} only. If it is not the case, \code{method} \code{"2a"} or \code{method} \code{"2b"} should be used.

}


\value{
\item{att.pattern}{A \eqn{N \times K} binary attribute patterns, where \eqn{N} is the number of examinees and \eqn{K} is the number of attributes.}
\item{att.dist}{A \eqn{2^K \times 2} data frame, where the first column is the attribute pattern, the second column is its frequency.}
}
\references{

Chiu, C. Y., Douglas, J. A., & Li, X. (2009). Cluster analysis for cognitive diagnosis: theory and applications. \emph{Psychometrika, 74}(4), 633-665.

Chiu, C. Y., & Ma, W. (2013). \emph{Assignment of clusters to attribute profiles for cognitive diagnosis}. Manuscript in preparation.
}

\seealso{
\code{\link{print.labeling}}, \code{\link{cd.cluster}}, \code{\link{npar.CDM}}
}
\examples{
#Labeling based on simulated data and Q matrix
data(sim.dat)
data(sim.Q)

# Information about the dataset
N <- nrow(sim.dat) #number of examinees
J <- nrow(sim.Q) #number of items
K <- ncol(sim.Q) #number of attributes

# Assume 2^K latent clusters
cluster.obj <- cd.cluster(sim.dat, sim.Q)
# Different clusters may have the same attribute patterns
labeled.obj.2b <- labeling(sim.dat, sim.Q, cluster.obj, method="2b")
# Different clusters mhave different attribute patterns
labeled.obj.2a <- labeling(sim.dat, sim.Q, cluster.obj, method="2a")
# labeling using method 1
data(perm3)  #since the number of attributes in this example is 3, perm3 is used here
labeled.obj.1 <- labeling(sim.dat, sim.Q, cluster.obj, method="1",perm=perm3)
remove(perm3) #remove perm3 

# Assume 5 attribute patterns exist
M <- 5
cluster.obj <- cd.cluster(sim.dat, sim.Q, method="HACA", HACA.cut=M) 
labeled.obj <- labeling(sim.dat, sim.Q, cluster.obj, method="2b")


}

