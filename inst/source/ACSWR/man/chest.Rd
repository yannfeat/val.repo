\name{chest}
\alias{chest}
\docType{data}
\title{
The Militiamen's Chest Dataset
}
\description{
Militia means an army composed of ordinary citizens and not of professional soldiers. This data set is available in an 1846 book published by the Belgian statistician Adolphe Quetelet, and the data is believed to have been collected some thirty years before that. 
}
\usage{data(chest)}
\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{Chest}}{chest width measured in inches}
    \item{\code{Count}}{the corresponding frequencies}
  }
}
\references{
Velleman, P.F., and Hoaglin, D.C. (2004). ABC of Exploratory Data Analysis.
Duxbury Press, Boston.
}
\examples{
data(chest)
attach(chest)
names(chest)
militiamen <- rep(Chest,Count)
length(militiamen)
bins <- seq(33,48)
bins
bin.mids <- (bins[-1]+bins[-length(bins)])/2
par(mfrow=c(1,2))
h <- hist(militiamen, breaks = bins, xlab= "Chest Measurements (Inches)",
main= "A: Histogram for the Militiamen")
h$counts <- sqrt(h$counts)
plot(h,xlab= "Chest Measurements (Inches)",ylab= "ROOT FREQUENCY",
main= "B: Rootogram for the Militiamen")
}
\keyword{rootogram, militiamen}
