\name{somesamples}
\alias{somesamples}
\docType{data}
\title{
Scatter Plots for Understanding Correlations
}
\description{
A cooked data tailor made for the use of scatter plots towards understanding correlations.
}
\usage{data(somesamples)}
\format{
  A data frame with 200 observations on the following 12 variables.
  \describe{
    \item{\code{x1}}{x of Sample 1}
    \item{\code{y1}}{y of Sample 1}
    \item{\code{x2}}{x of Sample 2}
    \item{\code{y2}}{y of Sample 2}
    \item{\code{x3}}{x of Sample 3}
    \item{\code{y3}}{y of Sample 3}
    \item{\code{x4}}{x of Sample 4}
    \item{\code{y4}}{y of Sample 4}
    \item{\code{x5}}{x of Sample 5}
    \item{\code{y5}}{y of Sample 5}
    \item{\code{x6}}{x of Sample 6}
    \item{\code{y6}}{y of Sample 6}
  }
}

\examples{
data(somesamples)
attach(somesamples)
par(mfrow=c(2,3))
plot(x1,y1,main="Sample I",xlim=c(-4,4),ylim=c(-4,4))
plot(x2,y2,main="Sample II",xlim=c(-4,4),ylim=c(-4,4))
plot(x3,y3,main="Sample III",xlim=c(-4,4),ylim=c(-4,4))
plot(x4,y4,main="Sample IV",xlim=c(-4,4),ylim=c(-4,4))
plot(x5,y5,main="Sample V",xlim=c(-4,4),ylim=c(-4,4))
plot(x6,y6,main="Sample VI",xlim=c(-4,4),ylim=c(-4,4))
}
\keyword{scatterplot}
