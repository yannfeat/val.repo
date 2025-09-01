\name{rh2w}
\alias{rh2w}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mixing Ratio from relative humidity
}
\description{
This function gets the mixing ratio (kg/kg) from a given relative humidity (\%), pressure (Pa) and temperature (K).
}
\usage{
rh2w(P, Temp, rh, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
A vector with pressure values in Pa.
}
  \item{Temp}{
A vector with temperature values in Kelvin.
}
  \item{rh}{
A vector with relative humidity values in (\%).
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}
\value{
This function returns a vector with mixing ratio values (kg/kg).
}
\seealso{
\code{\link{saturation_mixing_ratio}
}
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
dTds<-w2Td(dPs,dws)
rhs<-TTdP2rh(dTs,dTds,dPs)
wfromrh<-rh2w(dPs,dTs,rhs)
}
\keyword{Functions}