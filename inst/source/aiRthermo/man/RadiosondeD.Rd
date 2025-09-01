\name{RadiosondeD}
\alias{RadiosondeD}
\docType{data}
\title{
Radiosonde D
}
\description{
Contains the information measured by a sounding in Barcelona (station 05190) in 2013, August 7th at 12:00 UTC. According to the university of Wyoming, the CAPE was higher than 3000 J/kg and a great amount of (convective) precipitation was measured.
}
\usage{data("RadiosondeD")}
\format{
  A data frame with 70 observations on the following 11 variables.
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
\code{\link{RadiosondeA}} and \code{\link{RadiosondeDavenport}} 
}
\examples{
data(RadiosondeD)
#Calculate the pressure in Pa
RadiosondeD$V1*100

#Calculate the temperature in K
C2K(RadiosondeD$V3)
}
\keyword{Datasets}
