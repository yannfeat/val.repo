\name{Ac3net.performance}
\alias{Ac3net.performance}

\title{
Find the performance of the first network based on the second.
}
\description{
\code{Ac3net.performance} takes two network list matrices. First is the predicted network and second is the reference true network. Returns performance scores.
}
\usage{
Ac3net.performance(predictNet, referenceNet, data_, directed=TRUE)
}

\arguments{
  \item{predictNet}{The predicted network. A matrix that represents the links between two variables at each row. Namely, the element at row i and column 1 and the element at row i and column 2 are assumed to be paired or linked. There is no connection assumed among rows.}
  \item{referenceNet}{The reference network over which the performance will ve evaluated. A matrix that represents the links between two variables at each row. Namely, the element at row i and column 1 and the element at row i and column 2 are assumed to be paired or linked. There is no connection assumed among rows.}
  \item{data_}{The data matrix where the variables are available as row names of it. Only row names will be used to get all the variable names for the analaysis.}
  \item{directed}{ It is TRUE by default, which means there is  directionality assumed from column 1 to column 2 at each matrices. Therefore common links are the directionale common links. If it is FALSE, then the pairs assumed to be undirectional and all the common pairs are taken as common. E.g. If there is A--B in net1 and B--A in net2 then they are considered as common.}
}
\details{
\code{Ac3net.performance} takes two network list matrices and based on the first two columns it makes comparison betwee the predicted network and the reference network. If the argument directed is equal to TRUE, as in the default case, then it assumes directionally from column 1 to column 2 of the matrices and computes the performance based on it. E.g. For a pair of A-->B in the predicted network, if there is also A-->B in the reference network then it is taken as True Positive (TP) but the opposite direction B-->A is not accepted as TP. If the argument directed is equal to FALSE, then eitherA-->B or B-->A is acepted as TP, with no directional considerations. 
}
\value{
  \code{Ac3net.performance}  returns an object with all the performance scores. 
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
 # data(expdata)
 # data(truenet)
 # data(zeroindx) # zero indices for the example dataset, not in general.
 ## step by step to get Ac3net network
 # mim <- cor(t(expdata), method = "pearson")
 # mim[zeroindx, zeroindx] <- 0 #for the example dataset, not in general.
 # mim <- Ac3net.filtersames(mim=mim) #in case if there are same variables.
 #diag(mim) <-0
 #cutoff <- Ac3net.cutoff(mim, ratio_ = 0.002, PCmincutoff=0.6, PCmaxcutoff=0.96)
 #mim[abs(mim) < cutoff] <- 0
 #Ac3net.net <- Ac3net.maxmim(mim=mim, net_=TRUE, cutoff_= cutoff)
 ### one command could do the same as below if zeroindx was not available. ###
 ## ac3net.net <- Ac3net(DataOrMim=expdata, processed=FALSE, ratio_ = 0.002, PCmincutoff=0.6,
 #          PCmaxcutoff=0.96, cutoff=0, estmethod='pearson', pval=1, 
 #          iterations=10, MTC=FALSE, MTCmethod="BH" )
 
# performance <- Ac3net.performance(predictNet=ac3net.net, 
#                referenceNet = truenet, data_, directed=TRUE)
# performance
}

\keyword{misc}
