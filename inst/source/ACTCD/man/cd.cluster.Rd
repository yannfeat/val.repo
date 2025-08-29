\name{cd.cluster}
\alias{cd.cluster}


\title{
Cluster analysis for cognitive diagnosis based on the Asymptotic Classification Theory 
}
\description{
\code{\link{cd.cluster}} is used to classify examinees into unlabeled clusters based on cluster analysis. Available options include \emph{K}-means and Hierarchical Agglomerative Cluster Analysis (HACA) with various links.
}
\usage{
cd.cluster (Y, Q, method = c("HACA", "Kmeans"), Kmeans.centers = NULL,
Kmeans.itermax = 10, Kmeans.nstart = 1, HACA.link = c("complete", "ward", "single",
 "average", "mcquitty", "median", "centroid"), HACA.cut = NULL)
}

\arguments{
  \item{Y}{
A required \eqn{N \times J} response matrix with binary elements (1=correct, 0=incorrect), where \eqn{N} is the number of examinees and \eqn{J} is the number of items.
}
  \item{Q}{
A required \eqn{J \times K} binary item-by-attribute association matrix (Q-matrix), where \eqn{K} is the number of attributes. The \eqn{j^{th}} row of the matrix is an indicator vector, 1 indicating attributes are required and 0 indicating attributes are not required to master item \eqn{j}.
}
  \item{method}{
The clustering algorithm used to classify data. Two options are available, including \code{"Kmeans"} and  \code{"HACA"}, where \code{"HACA"} is the default method.
}
  \item{Kmeans.centers}{
The number of clusters when \code{"Kmeans"} argument is selected. It must be not less than 2 and not greater than \eqn{2^K} where \eqn{K} is the number of attributes. The default is \eqn{2^K}.
}
  \item{Kmeans.itermax}{
The maximum number of iterations allowed when \code{"Kmeans"} argument is selected.
}
  \item{Kmeans.nstart}{
The number of random sets to be chosen when \code{"Kmeans"} argument is selected.
}
  \item{HACA.link}{
The link to be used with HACA.  It must be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. The default \code{"HACA.link"} is \code{"complete"}.
}
  \item{HACA.cut}{
The number of clusters when \code{"HACA"} argument is specified. It must be not less than 2 and not greater than \eqn{2^K}, where \eqn{K} is the number of attributes. The default is \eqn{2^K}.
}
}

\details{
Based on the Asymptotic Classification Theory (Chiu, Douglas & Li, 2009), A sample statistic \eqn{\bm{W}} (See \code{\link{ACTCD}}) is calculated using the response matrix and Q-matrix provided by the users and then taken as the input for cluster analysis (i.e. \eqn{K}-means and HACA). 

The number of latent clusters can be specified by the users in \code{Kmeans.centers} or \code{HACA.cut}. It must be not less than 2 and not greater than \eqn{2^K}, where \eqn{K} is the number of attributes. Note that if the number of latent clusters is less than the default value (\eqn{2^K}), the clusters cannot be labeled in \code{\link{labeling}} using \code{method="1"} and \code{method="3"} algorithms. See \code{\link{labeling}} for more information. 
}
\value{
\item{W}{The \eqn{N \times K} sample statistic \eqn{\bm{W}} for the clustering algorithm. See details for more information.}
\item{size}{A set of integers, indicating the sizes of latent clusters.}
\item{mean.w}{A matrix of cluster centers, representing the average \eqn{\bm{W}} of the latent clusters.}
\item{wss.w}{The vector of within-cluster sum of squares of \eqn{\bm{W}}.}
\item{sqmwss.w}{The vector of square root of mean of within-cluster sum of squares of \eqn{\bm{W}}.}
\item{mean.y}{The vector of the mean of sum scores of the clusters.}
\item{class}{The vector of estimated memberships for examinees.}

}
\references{
Chiu, C. Y., Douglas, J. A., & Li, X. (2009). Cluster analysis for cognitive diagnosis: theory and applications. \emph{Psychometrika, 74}(4), 633-665.
}

\seealso{
\code{\link{print.cd.cluster}}, \code{\link{labeling}}, \code{\link{npar.CDM}}, \code{\link{ACTCD}}
}
\examples{
# Classification based on the simulated data and Q matrix
data(sim.dat)
data(sim.Q)
# Information about the dataset
N <- nrow(sim.dat) #number of examinees
J <- nrow(sim.Q) #number of items
K <- ncol(sim.Q) #number of attributes

#the default number of latent clusters is 2^K
cluster.obj <- cd.cluster(sim.dat, sim.Q)
#cluster size
sizeofc <- cluster.obj$size
#W statistics
W <- cluster.obj$W

#User-specified number of latent clusters
M <- 5  # the number of clusters is fixed to 5
cluster.obj <- cd.cluster(sim.dat, sim.Q, method="HACA", HACA.cut=M) 
#cluster size
sizeofc <- cluster.obj$size
#W statistics
W <- cluster.obj$W

M <- 5 # the number of clusters is fixed to 5
cluster.obj <- cd.cluster(sim.dat, sim.Q, method="Kmeans", Kmeans.centers =M)  
#cluster size
sizeofc <- cluster.obj$size
#W statistics
W <- cluster.obj$W
}

