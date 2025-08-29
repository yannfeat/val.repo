\name{sample}
\alias{sample}
\docType{data}
\title{
Simulated Dataset}
\description{
In the data set sample, we have data from five different probability distributions. Histograms are used to intuitively understand the underlying probability model. 
}
\usage{data(sample)}
\format{
  A data frame with 100 observations on the following 5 variables.
  \describe{
    \item{\code{Sample_1}}{A sample 1}
    \item{\code{Sample_2}}{A sample 2}
    \item{\code{Sample_3}}{A sample 3}
    \item{\code{Sample_4}}{A sample 4}
    \item{\code{Sample_5}}{A sample 5}
  }
}
\examples{
data(sample)
layout(matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), 2, 6, byrow=TRUE),respect=FALSE) 
matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), 2, 6, byrow=TRUE)
hist(sample[,1],main="Histogram of Sample 1",xlab="sample1", ylab="frequency")
hist(sample[,2],main="Histogram of Sample 2",xlab="sample2", ylab="frequency")
hist(sample[,3],main="Histogram of Sample 3",xlab="sample3", ylab="frequency")
hist(sample[,4],main="Histogram of Sample 4",xlab="sample4", ylab="frequency")
hist(sample[,5],main="Histogram of Sample 5",xlab="sample5", ylab="frequency")
}
\keyword{histogram}
