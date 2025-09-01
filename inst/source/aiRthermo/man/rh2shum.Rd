\name{rh2shum}
\alias{rh2shum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Specific Humidity from relative humidity
}
\description{
This function calculates the specific humidity from a given relative humidity.
}
\usage{
rh2shum(P, Temp, rh, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
A vector with pressure values (Pa).
}
  \item{Temp}{
A vector with temperature values (Kelvin).
}
  \item{rh}{
A vector with relative humidity values (\%).
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary.
}
}
\value{
This function returns a vector with specific humidity (kg/kg).
}
\seealso{
\code{\link{rh2shum}}
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
dTds<-w2Td(dPs,dws)
rhs<-TTdP2rh(dTs,dTds,dPs)
rh2shum(dPs,dTs,rhs)
}
\keyword{Functions}