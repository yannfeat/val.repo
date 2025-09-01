\name{LIindex}
\alias{LIindex}

\title{
Lifted index 
}
\description{
This function calculates the instability parameter Lifted index (Celsius) from pressure, temperature and mixing ratio values described by a vertical sounding.

}
\usage{
LIindex(Ps, Ts, ws, Psurface, deltaP, PWIDTH, doLog = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ps}{
Pressure (Pa) of the sounding.
}
  \item{Ts}{
Temperature (K) of the sounding.
}
  \item{ws}{
Mixing ratio (kg/kg) of the sounding.
}
  \item{Psurface}{
Surface pressure (Pa). If not available, the first level of the sounding can be used.
}
  \item{deltaP}{
The width (Pa) of the layers used in the numerical solution of the vertical evolution (integration of the ODE). A default value of 5 Pa is used. It must be positive.
}
  \item{PWIDTH}{
PWIDTH represents the width (Pa) of the lower layer that will be averaged for P, T and w in order to calculate a "mixed-layer" average parcel that will be used for the vertical evolution. Typically 5000-10000 Pa are used.
}
  \item{doLog}{
Use logarithmic vertical interpolation between sounding levels if doLog=1. It is not used by default (doLog=0).
}
}
\details{
If the 500 hPa needed level is not exactly found in the input sounding, logarithmic/linear
vertical interpolation is run to get the corresponding T/w from the Ps/Ts/ws depending on the value of doLog 0/1.

The evolution of the lifted particle is computed by integrating the dT/dP ordinary differential equation (applying the Runge-Kutta 4th order method), that represents the vertical adiabatic evolution from the initial condition to 500 hPa using a pressure step deltaP (Pa). The vertical adiabatic evolution is either dry (before saturation) or pseudoadiabatic at every vertical step with a correction for moisture in \eqn{c_p} using the value of the mixing ratio (\eqn{c_{pm}} as in Tsonis, eq 7.11).

If the sounding does not enclose the needed level of 500 hPa and the interpolation fails, the function returns -99999999.
}
\value{

This function returns the LI index (Celsius).
}
\references{
Tsonis, A. A. (2002). An Introduction to Atmospheric Thermodynamics, Cambridge University Press, Cambridge. Eq. 7.11.
}

\examples{
data(RadiosondeA)
aPs<-RadiosondeA[,1]*100
aTs<-C2K(RadiosondeA[,3])
aws<-RadiosondeA[,6]/1000
LIindex(aPs,aTs,aws,max(aPs),5,2500,0)
}
\keyword{Functions}