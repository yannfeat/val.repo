\name{TTdP2rh}
\alias{TTdP2rh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Relative Humidity from temperature, pressure and dew point temperature
}
\description{
This function calculates the relative humidity from given temperature, dew point temperature and pressure.
}
\usage{
TTdP2rh(Temp, Td, P, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Temp}{
A vector with temperature values in Kelvin.
}
  \item{Td}{
A vector with dew point temperature in Kelvin.
}
  \item{P}{
A vector with pressure values in Pa.
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}

\value{
This function returns a vector with relative humidity values.
}
\seealso{
\code{\link{saturation_mixing_ratio}}
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
dTds<-w2Td(dPs,dws)
rhs<-TTdP2rh(dTs,dTds,dPs)

}
\keyword{Functions}