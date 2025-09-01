\name{Kindex}
\alias{Kindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
K Instability Index
}
\description{
This function calculates the K instability index (Celsius) from a sounding given by the measured arrays pressure Ps (Pa) temperature Ts (K) and mixing ratio ws (kg/kg).


}
\usage{
Kindex(Ps, Ts, ws, doLog = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ps}{
    A vector with pressure values (Pa) measured by the radiosonde.
}
  \item{Ts}{
    A vector with temperature values (K) measured by the radiosonde.
}
  \item{ws}{
    A vector with mixing ratio values (kg/kg) measured by the radiosonde.
}
  \item{doLog}{
Use logarithmic vertical interpolation between sounding levels. The default value is 0.
}
}
\details{
If needed levels (850, 700 and 500 hPa) are not found in the input sounding (without extrapolation), the function returns -99999999.

Use/do not use logarithmic interpolation in pressure (if needed because mandatory levels such as 700 hPa or 500 hPa are not given in the sounding) when finding the requested levels.
}
\value{
%%???
This function returns the K index.
}

\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
aws<-RadiosondeA[,6]/1000
aK<-Kindex(aPs,aTs,aws,0)

data(RadiosondeD)
dPs<-RadiosondeD[,1]*100
dTs<-C2K(RadiosondeD[,3])
dws<-RadiosondeD[,6]/1000
dK<-Kindex(dPs,dTs,dws,0)
}
\keyword{Functions}