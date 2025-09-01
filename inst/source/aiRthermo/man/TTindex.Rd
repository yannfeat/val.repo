\name{TTindex}
\alias{TTindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Total-Totals Instability Index 
}
\description{
Total-Totals instability index (Celsius) from parameters (1D arrays) Ps (pressure, Pa) Ts (temperature, Kelvin) and ws (mixing ratio, kg/kg) obtained from a vertical sounding.


}
\usage{
TTindex(Ps, Ts, ws, doLog = 0)
}

\arguments{
  \item{Ps}{
A vector with pressure values in Pa.
}
  \item{Ts}{
A vector with temperature values in Kelvin.
}
  \item{ws}{
A vector with mixing ratio values in kg/kg.
}
  \item{doLog}{
Use logarithmic vertical interpolation between sounding levels. A default value is 0.
}
}
\details{
If the needed levels (850 hPa or 500 hPa) are not exactly found in the input sounding, logarithmic/linear vertical interpolation is run depending on the value of doLog (TRUE or FALSE).

If the sounding does not enclose the needed levels and the interpolation fails, the function returns -99999999.
}
\value{
This function returns the Total-Totals instability index (Celsius).
}

\examples{
data(RadiosondeDavenport)
aPs<-RadiosondeDavenport[,1]*100
aTs<-C2K(RadiosondeDavenport[,3])
aws<-RadiosondeDavenport[,6]/1000
aTT<-TTindex(aPs,aTs,aws,0)
}
\keyword{Functions}