\name{RadiosondeA}
\alias{RadiosondeA}
\docType{data}
\title{
Radiosonde A
}
\description{
Contains the information measured by a sounding in Santander (Station 08023) in 2010, June 16th at 12:00 UTC. It was not a really unstable day but a great amount of (large scale) precipitation was measured.
}
\usage{data("RadiosondeA")}
\format{
  A data frame with 74 observations on the following 11 variables.
  \describe{
    \item{\code{V1}}{a vector with pressure values (hPa).}
    \item{\code{V2}}{a vector with height (m).}
    \item{\code{V3}}{a vector with temperature values (Celsius).}
    \item{\code{V4}}{a vector with dew point temperature values (Celsius).}
    \item{\code{V5}}{a vector with relative humidity values (\%).}
    \item{\code{V6}}{a vector with mixing ratio values (g/kg).}
    \item{\code{V7}}{a vector with wind direction values (degrees).}
    \item{\code{V8}}{a vector with wind speed values (knots).}
    \item{\code{V9}}{a vector with potential temperature (K). }
    \item{\code{V10}}{a vector with equivalent potential temperature (K). }
    \item{\code{V11}}{a vector with virtual potential temperature (K). }
  }
}
\seealso{
\code{\link{RadiosondeD}} and \code{\link{RadiosondeDavenport}} 
}
\examples{
data(RadiosondeA)
#Calculate the pressure in Pa
RadiosondeA$V1*100

#Calculate the temperature in K
C2K(RadiosondeA$V3)
}
\keyword{Datasets}
