\name{cardata}
\alias{cardata}
\docType{data}
\title{
Car Data}
\description{
The data is used to show the effectiveness of Chernoff faces. }
\usage{data(cardata)}
\format{
  A data frame with 74 observations on the following 14 variables.
  \describe{
    \item{\code{Model}}{various car models }
    \item{\code{P}}{Price}
    \item{\code{M}}{Mileage (in miles per gallon)}
    \item{\code{R78}}{Repair record 1978}
    \item{\code{R77}}{Repair record 1977}
    \item{\code{H}}{Headroom (in inches)}
    \item{\code{R}}{Rear seat clearance}
    \item{\code{Tr}}{Trunk space}
    \item{\code{W}}{Weight (in pound)}
    \item{\code{L}}{Length (in inches)}
    \item{\code{T}}{Turning diameter}
    \item{\code{D}}{Displacement (in cubic inches)}
    \item{\code{G}}{Gear ratio for high gear}
    \item{\code{C}}{Company headquarter}
  }
}
\examples{
data(cardata)
pairs(cardata)
}
\keyword{visualizing multivariate data}
