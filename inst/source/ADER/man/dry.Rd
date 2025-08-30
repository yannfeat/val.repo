\name{dry}
\alias{dry}
\docType{data}
\title{
Productivity in Chiapas Dry Forests
}
\description{
Richness of forest trees and average productivity for the 2001-2007 period estimated from MODIS images (NDVI, Normalized Difference Vegetation Index for 250 x 250 m pixels) in 96 plots of 0.1 ha in the dry forests of  Chiapas (Mexico).
}
\usage{data("dry")}
\format{
  A data frame with 96 observations on the following 2 variables.
  \describe{
    \item{\code{richness}}{Number of tree species per plot.}
    \item{\code{ndvi}}{Average productivity.}
  }
}
\details{
NDVI values range between 0 an 1, with higher values indicating higher productiviy.
}
\examples{
data(dry)
str(dry)
}
\keyword{datasets}
