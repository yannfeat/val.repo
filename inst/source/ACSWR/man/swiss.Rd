\name{swiss}
\alias{swiss}
\docType{data}
\title{
Forged Swiss Bank Notes
}
\description{
The swiss data set consists of measurements on the width of bottom margin and image diagonal length for forged and real notes. Histogram smoothing method is applied to understand the width of bottom margins for the forged notes.
}
\usage{data(swiss)}
\format{
  A data frame with 100 observations on the following 4 variables.
  \describe{
    \item{\code{Bottforg}}{bottom margin of forged notes}
    \item{\code{Diagforg}}{diagonal margin of forged notes}
    \item{\code{Bottreal}}{bottom margin of real notes}
    \item{\code{Diagreal}}{diagonal margin of real notes}
  }
}
\references{
Simonoff, J.S. (1996). Smoothing Methods in Statistics. Springer.
}
\examples{
data(swiss)
par(mfrow=c(1,3))
hist(swiss$Bottforg,breaks=28,probability=TRUE,col=0,ylim=c(0,.5),
  xlab="Margin width (mm)",ylab="Density")
hist(swiss$Bottforg,breaks=12,probability=TRUE,col=0,ylim=c(0,.5),
  xlab="Margin width (mm)",ylab="Density")
hist(swiss$Bottforg,breaks=6,probability=TRUE,col=0,ylim=c(0,.5),
  xlab="Margin width (mm)",ylab="Density")
}
\keyword{histogram smoothing}
