\name{RadiosondeDavenport}
\alias{RadiosondeDavenport}
\docType{data}
\title{
Radiosonde Davenport
}
\description{
Contains the information measured by a sounding in Davenport (station 74455) in 1997, June 21st at 00:00 UTC. That day was a very unstable situation.
}
\usage{data("RadiosondeDavenport")}
\format{
  A data frame with 67 observations on the following 11 variables.
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
\code{\link{RadiosondeA}} and \code{\link{RadiosondeD}} 
}
\examples{
data(RadiosondeDavenport)
#Calculate the pressure in Pa
RadiosondeDavenport$V1*100

#Calculate the temperature in K
C2K(RadiosondeDavenport$V3)
}
\keyword{Datasets}
