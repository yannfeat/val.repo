\name{npar.CDM}
\alias{npar.CDM}

\title{
Main function for ACTCD package
}
\description{
This function is used to classify examinees into labeled classes given responses and the Q-matrix.
}
\usage{
npar.CDM(Y, Q, cluster.method = c("HACA", "Kmeans"), Kmeans.centers = NULL, 
Kmeans.itermax = 10, Kmeans.nstart = 1, HACA.link = c("complete", "ward", "single",
 "average", "mcquitty", "median", "centroid"), HACA.cut = NULL, label.method = 
c("2b", "2a", "1", "3"),perm=NULL)
}

\arguments{
  \item{Y}{
A required \eqn{N \times J} response matrix with binary elements (1=correct, 0=incorrect), where \eqn{N} is the number of examinees and \eqn{J} is the number of items.
}
  \item{Q}{
A required \eqn{J \times K} binary item-by-attribute association matrix (Q-matrix), where \eqn{K} is the number of attributes. The \eqn{j^{th}} row of the matrix is an indicator vector, 1 indicating attributes are required and 0 indicating attributes are not required to master item \eqn{j}.
}
  \item{cluster.method}{
The cluster algorithm used to classify data. Two options are available, including \code{"Kmeans"} and  \code{"HACA"}, where \code{"HACA"} is the default method. See \code{\link{cd.cluster}} for details.
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
The link to be used with HACA.  It must be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. The default is \code{"complete"}.
}
  \item{HACA.cut}{
The number of clusters when \code{"HACA"} argument is selected. It must be not less than 2 and not greater than \eqn{2^K}, where \eqn{K} is the number of attributes. The default is \eqn{2^K}.
}
  \item{label.method}{
The algorithm used for labeling. It should be one of "1","2a", "2b" and "3" corresponding to different labeling methods in Chiu and Ma (2013). The default is "2b". See \code{\link{labeling}} for details.
}
  \item{perm}{
The data matrix of the partial orders of the attribute patterns. 
}
}

\value{
\item{att.pattern}{A \eqn{N \times K} binary attribute patterns, where \eqn{N} is the number of examinees and \eqn{K} is the number of attributes.}
\item{att.dist}{A \eqn{2^K \times 2} data frame, where the first column is the attribute pattern, the second column is its frequency.}
\item{cluster.size}{A set of integers, indicating the sizes of latent clusters.}
\item{cluster.class}{A vector of estimated memberships for examinees.}
}

\seealso{
 \code{\link{print.npar.CDM}}, \code{\link{cd.cluster}},\code{\link{labeling}}
}
\examples{
# Classification based on the simulated data and Q matrix
data(sim.dat)
data(sim.Q)
# Information about the dataset
N <- nrow(sim.dat) #number of examinees
J <- nrow(sim.Q) #number of items
K <- ncol(sim.Q) #number of attributes

# Compare the difference in results among different labeling methods
# Note that the default cluster method is HACA
labeled.obj.2a <- npar.CDM(sim.dat, sim.Q, label.method="2a")
labeled.obj.2b <- npar.CDM(sim.dat, sim.Q, label.method="2b")
labeled.obj.3 <- npar.CDM(sim.dat, sim.Q, label.method="3")
data(perm3)
labeled.obj.1 <- npar.CDM(sim.dat, sim.Q, label.method="1",perm=perm3)
remove(perm3)

#User-specified number of latent clusters
M <- 5
labeled.obj.2b <- npar.CDM(sim.dat, sim.Q, cluster.method="HACA", 
HACA.cut=M, label.method="2b")
labeled.obj.2a <- npar.CDM(sim.dat, sim.Q, cluster.method="HACA", 
HACA.cut=M, label.method="2a")
#The attribute pattern for each examinee
attpatt <- labeled.obj.2b$att.pattern


}

