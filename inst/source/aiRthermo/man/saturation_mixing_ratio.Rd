\name{saturation_mixing_ratio}
\alias{saturation_mixing_ratio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Saturation Mixing Ratio
}
\description{
This function calculates the saturation mixing ratio from a given temperature and pressure.}
\usage{
saturation_mixing_ratio(P, Temp, consts = export_constants())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
A vector with pressure values in Pa.
}
  \item{Temp}{
A vector with temperature values in Kelvin.
}
  \item{consts}{
The constants defined in \emph{aiRthermoConstants} data are necessary. 
}
}
\value{
This function returns a vector with saturation mixing ratio values (kg/kg).
}
\examples{
data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
saturation_mixing_ratio(dPs,dTs)
}
\keyword{Functions}